# a refuse cell names the class, the reason and the alternative

    Code
      refuse_fit_generic("coef_layout", "goldfishFit")
    Condition
      Error:
      ! `coef_layout()` is not defined for a <goldfishFit> object.
      x A single-process fit has no per-process blocks: the layout is a coefficient surface over the fids of a joint specification.
      i Use coef() for the coefficient vector, or coef_layout() on the joint fit this process belongs to.

# refusing a cell the table does not refuse is reported as a bug

    Code
      refuse_fit_generic("coef", "goldfishFit")
    Condition
      Error:
      ! The fit-class contract has no "refuse" verdict for `coef()` on <goldfishFit>.
      i This is a bug in goldfish: the method refuses, the table does not say so.

---

    Code
      refuse_fit_generic("coef", "goldfishNoSuchFit")
    Condition
      Error:
      ! The fit-class contract has no "refuse" verdict for `coef()` on <goldfishNoSuchFit>.
      i This is a bug in goldfish: the method refuses, the table does not say so.

