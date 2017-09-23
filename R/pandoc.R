## helper transformator function (FIXME: which output formats can handle base64?)
make_exercise_transform_pandoc <- function(to = "latex", base64 = to != "latex", ...)
{
  ## base64 checks
  if(is.null(base64)) base64 <- TRUE
  base64 <- if(isTRUE(base64)) {
    c("bmp", "gif", "jpeg", "jpg", "png", "svg")
  } else {
    if(is.logical(base64)) NA_character_  else tolower(base64)
  }
  if(b64 <- !all(is.na(base64))) stopifnot(requireNamespace("base64enc"))

  ## function to apply ttx() on every
  ## element of a list in a fast way
  apply_pandoc_on_list <- function(object, from = "markdown",
    sep = "\007\007\007\007\007", ...)
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
	if(grepl(sep, x[n], fixed = TRUE)) n else NULL
      )
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
      if(substr(to, 1L, 4L) == "html") {
        pre <- suf <- '"'
      } else {
        pre <- '('
	suf <- ')'
      }
      for(sf in sfiles) {
  	for(i in seq_along(trex)) {
  	  if(length(j <- grep(sf, trex[[i]], fixed = TRUE)) && tools::file_ext(sf) %in% base64) {
  	    base64i <- fileURI(file = sf)
  	    trex[[i]][j] <- gsub(paste0(pre, sf, suf),
  	      paste0(pre, base64i, suf), trex[[i]][j], fixed = TRUE)
  	    file.remove(file.path(sdir, sf))
  	    x$supplements <- x$supplements[!grepl(sf, x$supplements)]
  	  }
  	}
      }
      attr(x$supplements, "dir") <- sdir
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
      x$questionlist <- gsub("^<p>", "", gsub("</p>$", "", x$questionlist))
      x$solutionlist <- gsub("^<p>", "", gsub("</p>$", "", x$solutionlist))
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

pandoc <- function(x, ..., from = "latex", to = "html", fixup = TRUE, Sweave = TRUE)
{
  ## temporary files
  infile <- tempfile()
  outfile <- tempfile()
  on.exit(unlink(c(infile, outfile)))

  ## fixup Sweave and related special LaTeX to plain LaTeX  
  if(Sweave) x <- fixup_sweave_pandoc(x, from = from, to = to)
  
  ## call pandoc_convert()
  writeLines(x, infile)
  rmarkdown::pandoc_convert(input = infile, output = outfile, from = from, to = to, ...)
  rval <- readLines(outfile)

  ## post-process output with certain fixups
  if(fixup) {
    ## fixup <span> in markdown (can occur for LaTeX -> Markdown)
    if(from == "latex" && substr(to, 1L, 8L) == "markdown") {
      rval <- gsub("<span>", "", rval, fixed = TRUE)
      rval <- gsub("</span>", "", rval, fixed = TRUE)
    }
    
    ## fixup logical comparisons with \not in html
    if(substr(to, 1L, 4L) == "html") {
      tab <- rbind(
        c("\\\\not",	     "\\\\not "),
        c("\\\\not +=",      "&#8800;"),
        c("\\\\not +&lt;",   "&#8814;"),
        c("\\\\not +&gt;",   "&#8815;"),
        c("\\\\not +\\\\le", "&#8816;"),
        c("\\\\nleq",	     "&#8816;"),
        c("\\\\not +\\\\ge", "&#8817;"),
        c("\\\\ngeq",	     "&#8817;")
      )
      for(i in 1:nrow(tab)) rval <- gsub(tab[i, 1L], tab[i, 2L], rval)
    }
  }
  
  return(rval)
}
