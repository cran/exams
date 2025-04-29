extract_environment <- function(x, env, value = TRUE, markup = c("latex", "markdown"))
{
  markup <- match.arg(markup)
  if(markup == "latex") {
    b <- grep(paste0("\\\\(begin|end)\\{", env, "\\}"), x)
    if(length(b) == 0L) return(NULL)
    if(length(b)!= 2L) stop(sprintf("no unique begin/end pair for %s found", sQuote(env)))
    s <- grep("[^[:space:]]", gsub(paste0("\\\\(begin|end)\\{", env, "\\}"), "", x[b]))
    if(length(s) > 0L) warning(sprintf("there should be no other text in lines with begin/end for %s", sQuote(env)))
    if(value) return(x[(b[1L] + 1L):(b[2L] - 1L)]) else return(b)
  } else {
    ## get all sections and subsections
    seclines <- grep("^====", x)
    sublines <- grep("^----", x)
    alllines <- sort(c(seclines, sublines))
    ## match environment names
    x_alllines_m1 <- tolower(x[alllines - 1L])
    x_alllines_m1 <- gsub("-", "", x_alllines_m1, fixed = TRUE)
    x_alllines_m1 <- gsub("[[:space:]]+$", "", x_alllines_m1)
    x_alllines_m1 <- gsub("questionlist", "answerlist", x_alllines_m1, fixed = TRUE)
    x_alllines_m1 <- gsub("solutionlist", "answerlist", x_alllines_m1, fixed = TRUE)
    ## find desired environment
    wi <- which(env == x_alllines_m1)
    if(length(wi) < 1L) return(NULL)

    ## begin/end
    b <- alllines[wi] - 1L
    e <- if(substr(x[b + 1L], 1L, 1L) == "=") {
      seclines[seclines > b + 1L]
    } else {
      alllines[alllines > b + 1L]
    }
    e <- if(length(e) > 0L) min(e) - 3L else length(x)
    if(value) {
      if((b + 2L) <= e) { ## check if section is empty
        return(x[(b + 2L):e])
      } else {
        return(NULL)
      }
    } else {
      return(c(b, e))
    }
  }
}

extract_command <- function(x, command, type = c("character", "logical", "numeric"), markup = c("latex", "markdown"))
{
  ## return type and markup type
  type <- match.arg(type, c("character", "logical", "numeric"))
  markup <- match.arg(markup, c("latex", "markdown"))

  ## find command line
  command <- if(markup == "latex") paste0("\\", command) else paste0(command, ":")
  rval <- x[grep(command, x, fixed = TRUE)]
  if(length(rval) < 1L) {
      if(type == "logical") return(FALSE) else return(NULL)
  }
  if(length(rval) > 1L) {
    warning("command", sQuote(command), "occurs more than once, last instance used")
    rval <- tail(rval, 1L)
  }
  
  if(markup == "latex") {
    ## strip off everything in brackets
    ## omit everything before \command{
    rval <- strsplit(rval, paste(command, "{", sep = ""), fixed = TRUE)[[1L]][2L]
    ## omit everything after last }
    rval <- gsub("[^\\}]+$", "", rval)
    ## get everthing within brackets
    rval <- gsub("{", "", strsplit(rval, "}")[[1L]], fixed = TRUE)
    ## omit leading and trailing white space
    rval <- gsub("^[ \t]+", "", rval)
    rval <- gsub("[ \t]+$", "", rval)
    ## split further with respect to other symbols (currently only |)

    ## split further with respect separator symbol |
    ## special case: if strings contain regular expressions with |, these need to be protected by surrounding (...|...)
    rval <- if(any(grepl("\\([^(]*\\|[^)]*\\)", rval))) {
      unlist(regmatches(rval, gregexpr("\\^?\\(([^()]+|(?R))*\\)\\$?|\\b[^()\\|]+(?:\\s+[^()\\|]+)*\\b", rval, perl = TRUE)))
    } else {
      unlist(strsplit(rval, "|", fixed = TRUE))
    }

    ## translate HTML scientific notation produced by knitr
    if(type == "numeric" && any(grepl("\\times 10^", x, fixed = TRUE) & grepl("\\ensuremath", x, fixed = TRUE))) {
      rval <- gsub("\\ensuremath", "", rval, fixed = TRUE)
      rval <- gsub("([ \t]*\\\\times[ \t]*10\\^)([-]*[0-9]+)", "e\\2", rval)
    }
  } else {
    ## strip off command
    rval <- gsub(command, "", rval, fixed = TRUE)
    ## omit leading and trailing white space
    rval <- gsub("^[ \t]+", "", rval)
    rval <- gsub("[ \t]+$", "", rval)

    ## split further with respect separator symbol |
    ## special case: if strings contain regular expressions with |, these need to be protected by surrounding (...|...)
    rval <- if(any(grepl("\\([^(]*\\|[^)]*\\)", rval))) {
      unlist(regmatches(rval, gregexpr("\\^?\\(([^()]+|(?R))*\\)\\$?|\\b[^()\\|]+(?:\\s+[^()\\|]+)*\\b", rval, perl = TRUE)))
    } else {
      unlist(strsplit(rval, "|", fixed = TRUE))
    }

    ## translate HTML scientific notation produced by knitr
    if(type == "numeric" && any(grepl("&times; 10<sup>", x, fixed = TRUE))) {
      rval <- gsub("([ \t]*&times;[ \t]*10<sup>)([-]*[0-9]+)(</sup>)", "e\\2", rval)
    }
  }

  ## convert to return type
  do.call(paste("as", type, sep = "."), list(rval))
}

extract_extra <- function(x, markup = c("latex", "markdown"))
{
  ## markup type
  markup <- match.arg(markup, c("latex", "markdown"))

  ## search for extra commands
  comm0 <- if(markup == "latex") "\\exextra[" else "exextra["
  comm <- x[grep(comm0, x, fixed = TRUE)]  
  if(length(comm) < 1L) return(list())
  
  ## extract command and type
  comm <- sapply(strsplit(comm, comm0, fixed = TRUE), "[", 2L)
  comm <- sapply(strsplit(comm, "]", fixed = TRUE), "[", 1L)
  nam <- strsplit(comm, ",", fixed = TRUE)
  typ <- sapply(nam, function(z) if(length(z) > 1L) z[2L] else "character")
  nam <- sapply(nam, "[", 1L)
  
  ## call extract_command
  rval <- lapply(seq_along(comm), function(i) extract_command(x,
    command = paste0("exextra[", comm[i], "]"), type = typ[i], markup = markup))

  names(rval) <- nam
  return(rval)
}

extract_items <- function(x, markup = c("latex", "markdown"))
{
  ## markup type
  markup <- match.arg(markup, c("latex", "markdown"))

  ## map markdown to tex
  if(markup == "markdown") {
    x <- gsub("^[\\*|-][[:space:]]*", "\\\\item ", x)
    x[x %in% c("*", "-")] <- "\\item "    
  }
    
  ## make sure we get items on multiple lines right
  x <- paste(x, collapse = " ")
  x <- gsub("^[[:space:]]*\\\\item[[:space:]]*", "", x)
  x <- paste0(x, " ")
  x <- strsplit(x, "[[:space:]]*\\\\item")[[1L]]
  x <- gsub("^[[:space:]]", "", x)
  gsub("[[:space:]]+$", "", x)
}

extract_latex <- function(x, command)
{
  pattern <- sprintf("\\\\%s(\\[.*?\\])?\\{.*?\\}", command)
  rval <- regmatches(x, gregexpr(pattern, x, perl = TRUE))
  rval <- lapply(rval, function(x) regmatches(x, regexpr("(?<=\\{).*?(?=\\})", x, perl = TRUE)))
  return(unlist(rval))
}

read_metainfo <- function(file, markup = NULL, exshuffle = NULL)
{
  ## read file
  x <- readLines(file)
  if(is.null(markup)) markup <- switch(tolower(tools::file_ext(file)),
    "tex"  = "latex",
    "rtex" = "latex",
    "rnw"  = "latex",
    "md"   = "markdown",
    "rmd"  = "markdown"
  )

  ## only read meta-information from corresponding environment/section in Rmd files
  ## (in Rnw files there is no meta-information environment)
  if(markup == "markdown") x <- extract_environment(x, "metainformation", markup = markup)

  ## overwrite exshuffle settings?
  exshuffle_overwrite <- !is.null(exshuffle)
  exshuffle_arg <- exshuffle

  ## Description ###################################
  extype <- match.arg(extract_command(x, "extype", markup = markup), ## exercise type: schoice, mchoice, num, string, or cloze
    c("schoice", "mchoice", "num", "string", "cloze"))  
  exname <- extract_command(x, "exname", markup = markup)            ## short name/description, only to be used for printing within R
  extitle <- extract_command(x, "extitle", markup = markup)          ## pretty longer title
  exsection <- extract_command(x, "exsection", markup = markup)      ## sections for groups of exercises, use slashes for subsections (like URL)
  extags <- extract_command(x, "extags", markup = markup)            ## arbitrary individual tags, can contain "=" for tag classes/groups
  exauthor <- extract_command(x, "exauthor", markup = markup)        ## author of exercise
  exversion <- extract_command(x, "exversion", markup = markup)      ## version of exercise

  ## Question & Solution ###########################
  exsolution <- extract_command(x, "exsolution", markup = markup)    ## solution, valid values depend on extype
  extol <- extract_command(x, "extol", "numeric", markup = markup)   ## optional tolerance limit for numeric solutions
  exclozetype <- extract_command(x, "exclozetype", markup = markup)  ## type of individual cloze solutions
  exstringtype <- extract_command(x, "exstringtype", markup = markup)## essay+file

  ## E-Learning & Exam ###################################
  expoints     <- extract_command(x, "expoints",    "numeric", markup = markup)   ## default points
  extime       <- extract_command(x, "extime",      "numeric", markup = markup)   ## default time in seconds
  exshuffle    <- extract_command(x, "exshuffle",   "character", markup = markup) ## shuffle schoice/mchoice answers?
  exmaxchars   <- extract_command(x, "exmaxchars",   markup = markup)             ## maximum number of characters in string answers
  exabstention <- extract_command(x, "exabstention", markup = markup)             ## string for abstention in schoice/mchoice answers

  ## User-Defined ###################################
  exextra <- extract_extra(x, markup = markup)

  ## shuffle can be logical or numeric
  exshuffle <- if(is.null(exshuffle)) {
    FALSE
  } else if(is.na(suppressWarnings(as.numeric(exshuffle)))) {
    as.logical(exshuffle)
  } else {
    as.numeric(exshuffle)
  }
  if(exshuffle_overwrite) exshuffle <- exshuffle_arg

  ## set default exname
  if(is.null(exname)) exname <- "R/exams exercise"

  ## process valid solution types (in for loop for each cloze element)
  slength <- length(exsolution)
  if(slength < 1L) stop("no exsolution specified")
  exsolution <- switch(extype,
    "schoice" = string2mchoice(exsolution, single = !is.numeric(exshuffle) && !exshuffle_overwrite), ## check for _single_ choice (unless sub-shuffling afterwards)
    "mchoice" = string2mchoice(exsolution),
    "num" = string2num(exsolution),
    "string" = exsolution,
    "cloze" = {
      if(is.null(exclozetype)) {
        warning("no exclozetype specified, taken to be string")
	exclozetype <- "string"
      }
      if(length(exclozetype) > 1L & length(exclozetype) != slength)
        warning("length of exclozetype does not match length of exsolution")
      exclozetype <- rep(exclozetype, length.out = slength)
      exsolution <- as.list(exsolution)
      for(i in 1L:slength) exsolution[[i]] <- switch(match.arg(exclozetype[i], c("schoice", "mchoice", "num", "string", "essay", "file", "verbatim")),
        "schoice" = string2mchoice(exsolution[[i]], single = TRUE),
        "mchoice" = string2mchoice(exsolution[[i]]),
        "num" = string2num(exsolution[[i]]),
        "string" = exsolution[[i]],
        exsolution[[i]])
      exsolution
    }
  )
  slength <- length(exsolution)

  ## merge exstringtype with exclozetype for cloze
  if(!is.null(exstringtype) && !all(exstringtype %in% c("string", "essay", "file"))) {
    warning("unknown exstringtype, must be 'string' (default) or 'essay' or 'file'")
    exstringtype[!(exstringtype %in% c("string", "essay", "file"))] <- "string"
  }
  if(!is.null(exstringtype) && extype == "cloze") {
    clozestring <- which(exclozetype == "string")
    if(!(length(exstringtype) %in% c(1L, length(clozestring), slength))) {
      warning(sprintf("length of exstringtype is %s but there are %s string items out of %s cloze items", length(exstringtype), length(clozestring), slength))
    }
    exclozetype[clozestring] <- if(length(exstringtype) == slength) exstringtype[clozestring] else rep_len(exstringtype, length.out = length(clozestring))
    warning("exstringtype now merged into exclozetype: ", paste(exclozetype, collapse = "|"))
    exstringtype <- NULL
  }
  if(!is.null(exstringtype) && (extype != "string")) {
    warning("exstringtype should only be specified for extype 'string'")
  }
  if(!is.null(exclozetype) && (extype != "cloze")) {
    warning("exclozetype should only be specified for extype 'cloze'")
  }

  ## tolerance value (expand to appropriate length or omit)
  if(is.null(extol)) extol <- 0
  if(extype == "num") {
    extol <- extol[1L]
  } else if(extype == "cloze") {
    clozenum <- which(exclozetype == "num")
    if(!(length(extol) %in% c(1L, length(clozenum), slength))) {
      warning(sprintf("length of extol is %s but there are %s num items out of %s cloze items", length(extol), length(clozenum), slength))
    }
    if(length(extol) != slength) {
      clozetol <- rep.int(0, slength)
      extol <- rep_len(extol, length.out = length(clozenum))
      clozetol[clozenum] <- extol
      extol <- clozetol
    }
  } else {
    extol <- NULL
  }
  if(!is.null(extol) && any(extol < 0)) {
    warning("'extol' must not be negative, using 0 instead")
    extol <- pmax(extol, 0)
  }

  ## compute "nice" string for printing solution in R
  sol_to_string <- function(sol, type, tol = 0) {
    slength <- length(sol)
    switch(type,
      "schoice" = if(slength <= 26L) letters[which(sol)] else paste0(which(sol)), ## FIXME: currently fixed
      "mchoice" = if(all(!sol)) "-" else paste0(if(slength <= 26L) letters[which(sol)] else which(sol), collapse = ", "), ## FIXME: currently fixed
      "num" = if(max(tol) <= 0) {
        paste0(sol)
      } else {
        if(slength == 1L) {
          paste0(sol, " (", sol - tol, "--", sol + tol, ")")
        } else {
	  paste0("[", sol[1L], ", ", sol[2L], "] ([", sol[1L] - tol[1L], "--", sol[1L] + tol[1L], ", ",
	    sol[2L] - tol[2L], "--", sol[2L] + tol[2L], "])")
        }
      },
      "string" = paste0(sol, collapse = "\n"),
      paste0(sapply(sol, paste0, collapse = ", "), collapse = " | ")
    )
  }
  string <- if(extype == "cloze") {
    paste0(sapply(seq_along(exclozetype), function(i) sol_to_string(exsolution[[i]], exclozetype[i], tol = extol[i])), collapse = " | ")
  } else {
    sol_to_string(exsolution, extype, tol = extol)
  }
  string <- paste0(exname, ": ", string)

  ## points should be a vector for cloze
  if(!is.null(expoints) & extype == "cloze") {
    expoints <- rep(expoints, length.out = slength)
  }

  ## possible char setting options
  if(!is.null(exmaxchars)) {
    exmaxchars <- rep(exmaxchars, length.out = slength)
    exmaxchars <- lapply(exmaxchars, function(x) {
      x <- gsub("\\s", ",", x)
      x <- strsplit(x, ",")[[1]]
      x <- x[x != ""]
      if(any(x == "NA"))
        x[x == "NA"] <- NA
      mode(x) <- "integer"
      x
    })
    if(slength < 2)
      exmaxchars <- exmaxchars[[1]]
  }

  ## return everything
  rval <- list(
    file = tools::file_path_sans_ext(file),
    markup = markup,

    type = extype,
    name = exname,
    title = extitle,
    section = exsection,
    tags = extags,
    author = exauthor,
    version = exversion,

    solution = exsolution,
    tolerance = extol,
    clozetype = exclozetype,
    stringtype = exstringtype,

    points = expoints,
    time = extime,
    shuffle = exshuffle,
    length = slength,
    string = string,
    maxchars = exmaxchars,
    abstention = exabstention
  )
  rval <- c(rval, exextra)
  return(rval)
}
