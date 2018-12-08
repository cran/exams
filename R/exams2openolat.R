exams2openolat <- function(file, n = 1L, dir = ".", name = "olattest",
  qti = "2.1", converter = "pandoc-mathjax", table = TRUE,
  maxattempts = 1, ...)
{
  ## post-process mathjax output for display in OpenOLAT
  .exams_set_internal(pandoc_mathjax_fixup = TRUE)
  on.exit(.exams_set_internal(pandoc_mathjax_fixup = FALSE))
  .exams_set_internal(pandoc_table_class_fixup = table)
  on.exit(.exams_set_internal(pandoc_table_class_fixup = FALSE))

  if(qti == "2.1" && maxattempts >= 100000) {
    warning("'maxattempts' must be smaller than 100000 in OpenOLAT with QTI 2.1")
    maxattempts <- 99999
  }

  ## call exams2qti12 or exams2qti21
  qti <- match.arg(qti, c("1.2", "2.1"))
  rval <- switch(qti,
    "1.2" = exams::exams2qti12(file = file, n = n, dir = dir, name = name,
      converter = converter, maxattempts = maxattempts, ...),
    "2.1" = exams::exams2qti21(file = file, n = n, dir = dir, name = name,
      converter = converter, maxattempts = maxattempts, ...)
  )
  
  ## save exams list generated
  ## saveRDS(rval, file = file.path(dir, paste(name, ".rds", sep = "")))

  invisible(rval)
}
