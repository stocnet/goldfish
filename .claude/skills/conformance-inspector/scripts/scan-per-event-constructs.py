#!/usr/bin/env python3
"""Scan the lines a commit range ADDED to goldfish's per-event files for the
constructs that break the walk cost rules (.claude/rules/walk-cost.md).

    python3 scan-per-event-constructs.py <base> <head> [--all-files] [--paths P ...]

A hit is a prompt, not a verdict: most hits in these files are setup code
that runs once per walk. For every hit the script prints the enclosing
function so the inspector can trace whether that function runs once per
event (the walk step, an effect update closure, compute_step.*, a gather
loop, a C++ kernel's event loop) or once per walk. Only the former is a
finding.

Output: one block per file, one line per hit:
    <file>:<line>  [rule N] <construct>   in <function>
        <the added line>
"""

import re
import subprocess
import sys

# The files walk-cost.md declares as per-event code (its frontmatter paths).
DEFAULT_PATHS = [
    "R/preprocess_*.R",
    "R/model_preprocess*.R",
    "R/support_mask*.R",
    "R/broadcast_kind.R",
    "R/walk_handle.R",
    "R/simulate_*.R",
    "R/process_state_evaluators.R",
    "R/event_streams.R",
    "R/state_at.R",
    "R/cpp_interface.R",
    "R/estimation_core.R",
    "R/functions_effects_*.R",
    "src/*.cpp",
    "src/*.h",
]

# (regex, rule number, label). Rule numbers follow walk-cost.md:
# 1 one value per event / grid where a vector serves; 2 grid built to read a
# row or cell; 3 copy on write (second reference); 4 allocation per event;
# 5 lookup by name per event; 6 second pass / second walk.
R_PATTERNS = [
    (r"\bdata\.frame\(", 4, "data.frame()"),
    (r"\bas\.data\.frame\(", 4, "as.data.frame()"),
    (r"\brbind\(", 4, "rbind()"),
    (r"\bcbind\(", 4, "cbind()"),
    (r"\bdo\.call\(", 4, "do.call()"),
    (r"\bappend\(", 4, "append()"),
    (r"\b(\w+)\s*<-\s*c\(\s*\1\s*,", 4, "x <- c(x, ...) growth"),
    (r"\[\[\s*length\([^)]*\)\s*\+\s*1L?\s*\]\]\s*<-", 4, "x[[length(x) + 1]] <- growth"),
    (r"\$\w+\[\[[^\]]+\]\]\s*<-", 4, "env/list field element append (copies the field)"),
    (r"\b\w+\[\s*\w+\s*,\s*\]\s*<-", 4, "df[i, ] <- row assignment"),
    (r"\bmatrix\(", 2, "matrix()"),
    (r"\barray\(", 2, "array()"),
    (r"\bdim\(\s*\w+\s*\)\s*<-", 2, "dim<-"),
    (r"\bsupport_to_grid\(", 2, "support_to_grid()"),
    (r"\bproject_value\([^)]*to\s*=\s*0L?", 2, "project_value(to = 0) grid expansion"),
    (r"\bexpand_operand_update\(", 2, "expand_operand_update() to cells"),
    (r"\bouter\(", 2, "outer()"),
    (r"\bexpand\.grid\(", 2, "expand.grid()"),
    (r"\brep\([^)]*each\s*=", 2, "rep(each =) grid expansion"),
    (r"\brep\(", 4, "rep() vector built"),
    (r"\bas\.(integer|numeric|logical|double)\(", 4, "coercion copies the vector"),
    (r"\b(logical|integer|numeric|double|character|vector)\(\s*[A-Za-z_]", 4, "typed vector constructor"),
    (r"\bwhich\(", 4, "which() builds an index vector"),
    (r"\.?(rowSums|colSums|rowMeans|colMeans)\(", 6, "full-grid reduction per call"),
    (r"\bseq_len\(\s*n[12]?\b", 6, "loop over an actor axis"),
    (r"\b(rowSums|colSums)\(\s*(matrix|array|support_to_grid)", 2, "row sums over a built grid"),
    (r"\bapply\(", 2, "apply() over a matrix"),
    (r"<<-", 3, "<<- (superassignment)"),
    (r"\bassign\(", 3, "assign()"),
    (r"\bget\(", 3, "get() (binds a second reference before a write)"),
    (r"\bmget\(", 3, "mget()"),
    (r"\beval\(\s*parse\(", 5, "eval(parse())"),
    (r"\bmatch\.arg\(", 5, "match.arg() per call"),
    (r"\bmatch\(", 5, "match() lookup"),
    (r"%in%", 5, "%in% lookup"),
    (r"\bwhich\(\s*names\(", 5, "which(names(...) ==) lookup by name"),
    (r"\[\[\s*\"[A-Za-z_.]+\"\s*\]\]", 5, "[[\"name\"]] lookup by string"),
    (r"\bpaste0?\(", 5, "paste() string building"),
    (r"\bsprintf\(", 5, "sprintf() string building"),
    (r"\b(lapply|sapply|vapply|Map|mapply|Reduce|Filter)\(", 6, "higher-order loop (per-row closure?)"),
    (r"\bfor\s*\(", 6, "for loop (second pass over rows/events?)"),
    (r"\bunique\(", 6, "unique() pass"),
    (r"\b(sort|order)\(", 6, "sort()/order() pass"),
    (r"\bunlist\(", 6, "unlist() rebuild"),
]

CPP_PATTERNS = [
    (r"\barma::(mat|vec|rowvec|colvec|umat|uvec|cube)\b[^;]*\(", 4, "Armadillo object constructed"),
    (r"\barma::(zeros|ones|eye)\b", 4, "arma::zeros/ones/eye allocation"),
    (r"\b(NumericMatrix|NumericVector|IntegerVector|LogicalVector|List)\s+\w+\s*\(", 4, "Rcpp object constructed"),
    (r"\bstd::vector<[^>]*>\s+\w+", 4, "std::vector declared"),
    (r"\.push_back\(", 4, "push_back() growth"),
    (r"\.resize\(", 4, "resize()"),
    (r"\bnew\s+\w", 4, "new allocation"),
    (r"\bclone\(", 3, "Rcpp::clone() copy"),
    (r"\.t\(\)\s*;", 3, "transpose materialized"),
    (r"\bRcpp::(as|wrap)<", 4, "as<>/wrap<> conversion (copies)"),
]

R_FUN_DEF = re.compile(r"^\s*([A-Za-z_.][A-Za-z0-9_.]*)\s*(<-|=)\s*function\s*\(")
# A C++ definition starts at column 0 with a return type and a name followed
# by "("; the parameter list may span lines, so only the opening is matched,
# and a line ending in ";" is a declaration, not a definition.
CPP_FUN_DEF = re.compile(
    r"^[A-Za-z_][\w:<>,&\*\s]*?\b([A-Za-z_]\w*)\s*\((?!.*;\s*$)"
)
HUNK = re.compile(r"^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@")


def git(*args):
    return subprocess.run(
        ["git", *args], check=True, capture_output=True, text=True
    ).stdout


def added_lines(base, head, paths):
    """Yield (file, new_line_number, text) for every line the range added."""
    diff = git("diff", "-U0", "--no-color", f"{base}..{head}", "--", *paths)
    current = None
    lineno = 0
    for raw in diff.splitlines():
        if raw.startswith("+++ "):
            current = raw[4:]
            current = current[2:] if current.startswith("b/") else current
        elif raw.startswith("@@"):
            m = HUNK.match(raw)
            lineno = int(m.group(1)) if m else 0
        elif raw.startswith("+") and not raw.startswith("+++"):
            if current and current != "/dev/null":
                yield current, lineno, raw[1:]
            lineno += 1
        elif raw.startswith("-"):
            continue


def function_index(head, path):
    """Return a list of (line_number, name) for every function definition in
    the file at <head>, so a hit can be attributed to its enclosing function."""
    try:
        text = git("show", f"{head}:{path}")
    except subprocess.CalledProcessError:
        return []
    is_cpp = path.endswith((".cpp", ".h"))
    pattern = CPP_FUN_DEF if is_cpp else R_FUN_DEF
    defs = []
    for i, line in enumerate(text.splitlines(), start=1):
        m = pattern.match(line)
        if m and not line.lstrip().startswith(("//", "#", "*")):
            defs.append((i, m.group(1)))
    return defs


def enclosing(defs, lineno):
    name = "<top level>"
    for start, fname in defs:
        if start <= lineno:
            name = fname
        else:
            break
    return name


def is_comment(text, is_cpp):
    s = text.lstrip()
    return s.startswith("//") if is_cpp else s.startswith("#")


def main(argv):
    if len(argv) < 3:
        sys.exit(__doc__)
    base, head = argv[1], argv[2]
    paths = DEFAULT_PATHS
    if "--all-files" in argv:
        paths = ["R/", "src/"]
    if "--paths" in argv:
        paths = argv[argv.index("--paths") + 1 :]
    hits_by_file = {}
    for path, lineno, text in added_lines(base, head, paths):
        if path.endswith(("RcppExports.cpp", "RcppExports.R")):
            continue
        is_cpp = path.endswith((".cpp", ".h"))
        if is_comment(text, is_cpp):
            continue
        patterns = CPP_PATTERNS if is_cpp else R_PATTERNS
        labels = [f"[rule {rule}] {label}" for regex, rule, label in patterns if re.search(regex, text)]
        if labels:
            # One output line per source line, all matched constructs joined.
            hits_by_file.setdefault(path, []).append((lineno, "; ".join(labels), text))
    if not hits_by_file:
        print(f"No per-event constructs in the lines {base}..{head} added to:")
        print("  " + "\n  ".join(paths))
        return
    total = 0
    for path in sorted(hits_by_file):
        defs = function_index(head, path)
        print(f"== {path}")
        for lineno, labels, text in sorted(hits_by_file[path]):
            total += 1
            print(f"{path}:{lineno}  {labels}   in {enclosing(defs, lineno)}")
            print(f"    {text.strip()}")
        print()
    print(
        f"{total} hit(s). Each is a prompt to trace the enclosing function to "
        "the per-event path (walk step, effect update closure, compute_step, "
        "gather loop, kernel event loop) or to setup. Only per-event hits are "
        "findings; quantify them with the reference costs in walk-cost.md."
    )


if __name__ == "__main__":
    main(sys.argv)
