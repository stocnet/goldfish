# render_process_label elides a NA flavor instead of printing it

    Code
      render_process_label(map, 1L)
    Output
      [1] "friendship › rate"

---

    Code
      render_process_label(map, 2L)
    Output
      [1] "calls › creation › rate"

