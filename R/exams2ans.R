exams2ans <- function(file, n = 1L, dir = ".", name = "anstest",
  converter = "pandoc-mathjax", table = TRUE,
  maxattempts = 1, cutvalue = NULL, ...)
{

  ## call exams2qti21
  rval <- exams2qti21(file = file, n = n, dir = dir, name = name,
      converter = converter, maxattempts = maxattempts, cutvalue = cutvalue, base64 = FALSE,
      flavor = "ans", eval = list(partial = FALSE), cloze_schoice_display = "buttons", ...)

  invisible(rval)
}
