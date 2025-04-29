exams2canvas <- function(file, n = 1L, dir = ".", name = "canvasquiz",
  maxattempts = 1, duration = NULL, points = NULL, converter = "pandoc-mathjax",
  template = "canvas_qti12.xml", quiztype = "assignment", ...)
{
  ## call exams2qti12
  rval <- exams2qti12(file = file, n = n, dir = dir, name = name,
    maxattempts = maxattempts, duration = duration, points = points,
    converter = converter, flavor = "canvas", base64 = FALSE,
    eval = list(partial = TRUE, negative = FALSE),
    template = template, quiztype = quiztype, ...)

  invisible(rval)
}
