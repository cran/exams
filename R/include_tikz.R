include_tikz <- function(tikz, name = "tikzpicture", format = NULL,
  library = NULL, width = NULL, markup = "tex", ...)
{
  ## defaults
  if(is.null(format)) format <- "png"
  if(is.null(library)) library <- TRUE
  if(is.null(width)) width <- ""

  ## output markup
  markup <- match.arg(markup, c("tex", "markdown", "none"))
  if(format == "tex" && markup != "tex") {
    warning("'tex' format only supported within 'tex' markup, changed to 'png'")
    format <- "png"
  }
  wi <- if(markup == "tex") "[width=%s]" else "{width=%s}"
  if(nchar(width) > 1L && substr(width, 1L, 1L) != substr(wi, 1L, 1L)) width <- sprintf(wi, width)
  
  ## add {tikzpicture} if necessary
  if(!(any(grepl("begin{tikzpicture}", tikz, fixed = TRUE)) &
       any(grepl("end{tikzpicture}", tikz, fixed = TRUE)))) {
    tikz <- c("\\begin{tikzpicture}", tikz, "\\end{tikzpicture}")
  }

  if(format == "tex") {
    writeLines(tikz)
    invisible(tikz)
  } else {
    ## call tex2image
    tex2image(tikz, name = name, dir = ".", format = format, tikz = library, show = FALSE, ...)
    fig <- paste(name, format, sep = ".")
    if(markup == "tex") {
      writeLines(sprintf("\\includegraphics%s{%s}", width, fig))
      invisible(fig)
    } else if(markup == "markdown") {
      writeLines(sprintf("![](%s)%s", fig, width))
      invisible(fig)
    } else {
      return(fig)
    }
  }
}
