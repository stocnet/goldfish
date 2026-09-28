* Fixed `estimate_dynes()` and `simulate()` announcing a completed pinned
  intercept-only rate with two warnings instead of one, the second naming
  the process by its bare fid (e.g. `Rate 3`) rather than its
  `layer › rate` label. A completed rate is now announced once, naming the
  process by its label and stating the count source the pin was actually
  built from (the layer's observed event count, or the net wave Hamming
  diff between observed states) rather than a source implied by which
  consumer is running. No pinned value changes.
