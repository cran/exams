## helper transformator function (FIXME: which output formats can handle base64?)
make_exercise_transform_pandoc <- function(to = "latex", base64 = to != "latex", attachfile = FALSE, ...)
{
  ## system requirement
  stopifnot(rmarkdown::pandoc_available())

  ## base64 checks
  if(is.null(base64)) base64 <- c("bmp", "gif", "jpeg", "jpg", "png", "svg")
  base64 <- if(isTRUE(base64)) {
    .fileURI_mime_types[, "ext"]
  } else {
    if(is.logical(base64)) NA_character_  else tolower(base64)
  }
  b64 <- !all(is.na(base64))

  ## function to apply ttx() on every
  ## element of a list in a fast way
  apply_pandoc_on_list <- function(object, from = "markdown",
    sep = "SEPARATOR007BETWEEN007LIST007ELEMENTS", ...)
  {
    ## add seperator as last line to each chunk
    object <- lapply(object, c, c("", sep, ""))

    ## call pandoc_convert() on collapsed chunks
    rval <- pandoc(unlist(object), from = from, to = to,
      fixup = TRUE, Sweave = TRUE, ...)

    ## split chunks again on sep
    ix <- grepl(sep, rval, fixed = TRUE)
    rval <- split(rval, c(0, head(cumsum(ix), -1L)))
    names(rval) <- rep(names(object), length.out = length(rval))

    ## omit sep from last line in each chunk
    cleansep <- function(x) {
      n <- length(x)
      if(n < 1L) return(x)
      del <- c(
        if(x[1L] == "") 1L else NULL,
        if(n > 1L && grepl(sep, x[n], fixed = TRUE) && x[n - 1L] == "") n - 1L else NULL,
	if(x[n] %in% c(sep, paste0("<p>", sep, "</p>"))) n else NULL
	## FIXME: Depending on the output format the relevant line
	## may just contain the 'sep' or '<p>sep</p>'. But
	## a '</p>' may also be in the line _after_ the 'sep'.
	## ...maybe lapply() rather than seperator-based?
      )
      x[n] <- gsub(sep, "", x[n], fixed = TRUE)
      if(length(del) > 0L) return(x[-del]) else return(x)
    }
    rval <- lapply(rval, cleansep)

    rval
  }

  ## exercise conversion with pandoc
  function(x)
  {
    owd <- getwd()
    setwd(sdir <- attr(x$supplements, "dir"))

    ## what needs to be transormed with pandoc?
    what <- c(
      "question" = list(x$question),
      "questionlist" = as.list(x$questionlist),
      "solution" = list(x$solution),
      "solutionlist" = as.list(x$solutionlist)
    )

    ## transform the .tex chunks
    trex <- if(x$metainfo$markup != to) {
      apply_pandoc_on_list(what, from = x$metainfo$markup, ...)
    } else {
      what
    }
    namtrex <- names(trex)

    ## base64 image/supplements handling
    if(b64 && length(sfiles <- dir(sdir))) {
      for(sf in sfiles) {
        if(any(grepl(sf, unlist(trex), fixed = TRUE)) && tolower(file_ext(sf)) %in% base64) {
	  ## replacement pattern pairs
          sf64 <- fileURI(file = sf)
	  ## always include HTML replacements (could also be in Markdown)
          sfx <- rbind(
	    c(sprintf('alt="%s"', sf),  'alt="\\007\\007_exams_supplement_\\007\\007"'),
	    c(sprintf('href="%s"', sf), sprintf('href="%s" download="\\007\\007_exams_supplement_\\007\\007"', sf)),
	    c(sprintf('="%s"', sf),	sprintf('="%s"', sf64)),
	    c('\\007\\007_exams_supplement_\\007\\007', sf)
	  )
	  ## Markdown replacements only for non-HTML
          if(substr(to, 1L, 4L) != "html") {
	    sfx <- rbind(
              c(sprintf('src="%s"', sf), sprintf('src="%s"', sf64)), ## images might still be embedded in <img> rather than ![]()
	      c(sprintf("](%s)", sf), sprintf("](%s)", sf64))
	    )
	  }	
	  ## replace (if necessary)
    	  for(i in seq_along(trex)) {
  	    if(length(j <- grep(sf, trex[[i]], fixed = TRUE))) {
  	      for(k in 1L:nrow(sfx)) trex[[i]][j] <- gsub(sfx[k, 1L], sfx[k, 2L], trex[[i]][j], fixed = TRUE)
  	    }
  	  }
  	  file.remove(file.path(sdir, sf))
  	  x$supplements <- x$supplements[!grepl(sf, x$supplements)]
        }
      }
      attr(x$supplements, "dir") <- sdir
    }
    
    ## optionally use \attachfile{...}{...} instead of \url{...} in LaTeX
    if(to == "latex" && attachfile && length(sfiles <- dir(sdir))) {
      sfiles <- intersect(sfiles, c(extract_latex(unlist(trex), "url"), extract_latex(unlist(trex), "href")))    
      for(sf in sfiles) {
    	for(i in seq_along(trex)) {
  	  if(length(j <- grep(sf, trex[[i]], fixed = TRUE))) {
            trex[[i]][j] <- gsub(sprintf("\\url{%s}", sf), sprintf("\\textattachfile{%s}{\\tt %s}", sf, sf), trex[[i]][j], fixed = TRUE)
            trex[[i]][j] <- gsub(sprintf("\\href{%s}{", sf), sprintf("\\textattachfile{%s}{\\tt ", sf), trex[[i]][j], fixed = TRUE)
	  }
	}
      }
    }

    ## replace .tex chunks with pandoc output
    x$question <- trex$question
    x$questionlist <- unname(sapply(trex[grep("questionlist", namtrex)], paste, collapse = "\n"))
    x$solution <- trex$solution
    x$solutionlist <- unname(sapply(trex[grep("solutionlist", namtrex)], paste, collapse = "\n"))

    for(j in c("question", "questionlist", "solution", "solutionlist")) {
      if(length(x[[j]]) < 1L) x[j] <- structure(list(NULL), .Names = j)
    }
    
    ## remove leading and trailing <p> tags in question/solution lists
    if(substr(to, 1L, 4L) == "html") {
      if(!is.null(x$questionlist)) x$questionlist <- gsub("^<p>", "", gsub("</p>$", "", x$questionlist))
      if(!is.null(x$solutionlist)) x$solutionlist <- gsub("^<p>", "", gsub("</p>$", "", x$solutionlist))
    }

    setwd(owd)

    x$metainfo$markup <- to
    return(x)
  }
}

## fixup Sweave environments when converting to something else than LaTeX
fixup_sweave_pandoc <- function(x, from = "latex", to = "html") {
  if(from != "latex" | to == "latex") return(x)
  ## replace/remove Sweave code environments	
  tab <- rbind(
      c("\\\\begin\\{Sinput}",  "\\\\begin{verbatim}"),
      c("\\\\end\\{Sinput}",	"\\\\end{verbatim}"),
      c("\\\\begin\\{Soutput}", "\\\\begin{verbatim}"),
      c("\\\\end\\{Soutput}",	"\\\\end{verbatim}"),
      c("\\\\begin\\{Schunk}",  ""),
      c("\\\\end\\{Schunk}",	""),
      c("\\\\textit\\{",        "\\\\emph{"),
      c("\\\\textnormal\\{",	"\\\\text{"),
      c("\\\\texttt\\{\\\\url\\{([^}]*)\\}\\}", "\\\\url{\\1}"),
      c("\\\\url\\{([^}]*)\\}", "\\\\href{\\1}{\\\\texttt{\\1}}")
  )
  for(i in 1:nrow(tab)) x <- gsub(tab[i,1L], tab[i,2L], x)
  return(x)  
}

pandoc <- function(x, ..., from = "latex", to = "html", options = NULL, fixup = TRUE, Sweave = TRUE)
{
  ## temporary files
  infile <- tempfile()
  outfile <- tempfile()
  on.exit(unlink(c(infile, outfile)))

  ## fixup Sweave and related special LaTeX to plain LaTeX  
  if(Sweave) x <- fixup_sweave_pandoc(x, from = from, to = to)
  
  ## fixup logical comparisons with \not to LaTeX commands
  if(fixup) {
    tab <- rbind(
      c("\\\\not",	   "\\\\not "),
      c("\\\\not +=",	   "\\\\neq"),
      c("\\\\not +&lt;",   "\\\\nless"),
      c("\\\\not +<",      "\\\\nless"),
      c("\\\\not +&gt;",   "\\\\ngtr"),
      c("\\\\not +>",      "\\\\ngtr"),
      c("\\\\not +\\\\le", "\\\\nleq"),
      c("\\\\not +\\\\ge", "\\\\ngeq")
    )
    for(i in 1:nrow(tab)) x <- gsub(tab[i, 1L], tab[i, 2L], x)
  }

  ## change some default options from pandoc (see https://pandoc.org/MANUAL.html)
  ## --wrap=preserve instead of --wrap=auto
  ## --columns=99999 instead of --columns=72
  if(is.null(options) || all(substr(options, 1L,  7L) != "--wrap=")) options <- c(options, "--wrap=preserve")
  if(is.null(options) || all(substr(options, 1L, 10L) != "--columns=")) options <- c(options, "--columns=99999")

  ## call pandoc_convert()
  writeLines(x, infile)
  rmarkdown::pandoc_convert(input = infile, output = outfile, from = from, to = to, options = options, ...)
  rval <- readLines(outfile)

  ## post-process output with certain fixups
  if(fixup) {
    ## fixup <span> in markdown (can occur for LaTeX -> Markdown)
    if(from == "latex" && substr(to, 1L, 8L) == "markdown") {
      rval <- gsub("<span>", "", rval, fixed = TRUE)
      rval <- gsub("</span>", "", rval, fixed = TRUE)
    }
    
    if(!identical(mathjax_fixup <- .exams_get_internal("pandoc_mathjax_fixup"), FALSE)) {
      if(isTRUE(mathjax_fixup)) mathjax_fixup <- "openolat"
      tab <- rbind(
        c('pandoc' = '<span class="math display">\\[', 'openolat' = '</p><div class="math">', 'blackboard' = '<br/>$$'),
	c(           '\\]</span>',                                  '</div><p>',                             '$$<br/>'),
	c(           '<span class="math inline">\\(',               '<span class="math">',                   '$$'),
	c(           '\\)</span>',                                  '</span>',                               '$$')
      )
      mathjax_fixup <- match.arg(tolower(mathjax_fixup), colnames(tab))
      for(i in 1:nrow(tab)) rval <- gsub(tab[i, "pandoc"], tab[i, mathjax_fixup], rval, fixed = TRUE)
    }
    if(!identical(cls <- .exams_get_internal("pandoc_table_class_fixup"), FALSE)) {
      if(isTRUE(cls)) cls <- "b_gray"
      rval <- gsub('<table>', sprintf('<table class="%s">', cls), rval, fixed = TRUE)

      tab <- rbind(
        c('align="right">',  'style="text-align: right;">'),
        c('align="left">',   'style="text-align: left;">'),
        c('align="center">', 'style="text-align: center;">')
      )
      for(i in 1:nrow(tab)) rval <- gsub(tab[i, 1L], tab[i, 2L], rval, fixed = TRUE)
    }
  }
  
  return(rval)
}
