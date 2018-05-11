exams2openolat <- function(file, n = 1L, dir = ".", name = "olattest",
  qti = "2.1", converter = "pandoc-mathjax", ...)
{
  ## post-process mathjax output for display in OpenOLAT
  .exams_set_internal(pandoc_mathjax_fixup = TRUE)
  on.exit(.exams_set_internal(pandoc_mathjax_fixup = FALSE))

  ## call exams2qti12 or exams2qti21
  qti <- match.arg(qti, c("1.2", "2.1"))
  rval <- switch(qti,
    "1.2" = exams::exams2qti12(file = file, n = n, dir = dir, name = name,
      converter = converter, ...),
    "2.1" = exams::exams2qti21(file = file, n = n, dir = dir, name = name,
      converter = converter, ...)
  )
  
  ## save exams list generated
  ## saveRDS(rval, file = file.path(dir, paste(name, ".rds", sep = "")))

  invisible(rval)
}
