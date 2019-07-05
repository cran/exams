exams2canvas <- function(file, n = 1L, dir = ".", name = "canvasquiz",
  maxattempts = 1, duration = NULL, points = NULL, converter = NULL, ...)
{
  ## enforce MathML for mathematical notation
  if(any(tolower(tools::file_ext(unlist(file))) == "rmd")) {
    if(is.null(converter)) converter <- "pandoc-mathml"
    if(!(converter %in% c("pandoc", "pandoc-mathml"))) {
      warning("'converter' must be 'pandoc-mathml' (or equivalently 'pandoc')")
      converter <- "pandoc-mathml"
    }
  } else {
    if(is.null(converter)) converter <- "ttm"
    if(!(converter %in% c("ttm", "pandoc", "pandoc-mathml"))) {
      warning("'converter' must either be 'ttm' or 'pandoc-mathml' (or equivalently 'pandoc')")
      converter <- "ttm"
    }  
  }

  ## call exams2qti12
  rval <- exams2qti12(file = file, n = n, dir = dir, name = name,
    maxattempts = maxattempts, duration = duration, points = points,
    converter = converter, flavor = "canvas", base64 = FALSE,
    eval = list(partial = TRUE, negative = FALSE), ...)

  invisible(rval)
}
