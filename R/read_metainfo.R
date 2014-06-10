extract_environment <- function(x, env, value = TRUE)
{
  b <- grep(paste("\\\\(begin|end)\\{", env, "\\}", sep = ""), x)
  if(length(b) == 0L) return(NULL)
  if(length(b)!= 2L) stop("no unique begin/end pair for", sQuote(env), "found")
  if(value) x[(b[1L] + 1L):(b[2L] - 1L)] else b
}

extract_command <- function(x, command, type = c("character", "logical", "numeric"))
{
  ## return type
  type <- match.arg(type)

  ## find command line
  command <- paste("\\", command, sep = "")
  rval <- x[grep(command, x, fixed = TRUE)]
  if(length(rval) < 1L) {
      if(type=="logical") return(FALSE) else return(NULL)
  }
  if(length(rval) > 1L) {
    warning("command", sQuote(command), "occurs more than once, last instance used")
    rval <- tail(rval, 1L)
  }
  
  ## strip off everything in brackets
  ## omit everything before \command{
  rval <- strsplit(rval, paste(command, "{", sep = ""), fixed = TRUE)[[1L]][2L]
  ## omit everything after last }
  rval <- gsub("[^\\}]+$", "", rval)
  ## get everthing within brackets
  rval <- gsub("{", "", strsplit(rval, "}")[[1L]], fixed = TRUE)
  ## split further with respect to other symbols (currently only |)
  rval <- unlist(strsplit(rval, "|", fixed = TRUE))

  ## convert to return type
  do.call(paste("as", type, sep = "."), list(rval))
}

extract_extra <- function(x) {
  comm <- x[grep("\\exextra[", x, fixed = TRUE)]
  if(length(comm) < 1L) return(list())
  comm <- sapply(strsplit(comm, "\\exextra[", fixed = TRUE), "[", 2L)
  comm <- sapply(strsplit(comm, "]", fixed = TRUE), "[", 1L)
  nam <- strsplit(comm, ",", fixed = TRUE)
  typ <- sapply(nam, function(z) if(length(z) > 1L) z[2L] else "character")
  nam <- sapply(nam, "[", 1L)
  rval <- lapply(seq_along(comm), function(i) extract_command(x,
    command = paste("exextra[", comm[i], "]", sep = ""), type = typ[i]))
  names(rval) <- nam
  return(rval)
}

extract_items <- function(x) {
    ## make sure we get items on multiple lines right
    x <- paste(x, collapse = " ")
    x <- gsub("^ *\\\\item *", "", x)
    x <- strsplit(x," *\\\\item")[[1L]] ## CHECKME, was: " *\\\\item *"
    x <- gsub("^ ", "", x)              ## CHECKME, now leading white space removed here
    gsub(" +$", "", x)
}

read_metainfo <- function(file)
{
  ## read file
  x <- readLines(file)

  ## Description ###################################
  extype <- match.arg(extract_command(x, "extype"), ## exercise type: schoice, mchoice, num, string, or cloze
    c("schoice", "mchoice", "num", "string", "cloze"))  
  exname <- extract_command(x, "exname")            ## short name/description, only to be used for printing within R
  extitle <- extract_command(x, "extitle")          ## pretty longer title
  exsection <- extract_command(x, "exsection")      ## sections for groups of exercises, use slashes for subsections (like URL)
  exversion <- extract_command(x, "exversion")      ## version of exercise

  ## Question & Solution ###########################
  exsolution <- extract_command(x, "exsolution")    ## solution, valid values depend on extype
  extol <- extract_command(x, "extol", "numeric")   ## optional tolerance limit for numeric solutions
  exclozetype <- extract_command(x, "exclozetype")  ## type of individual cloze solutions

  ## E-Learning & Exam ###################################
  expoints  <- extract_command(x, "expoints",  "numeric") ## default points
  extime    <- extract_command(x, "extime",    "numeric") ## default time in seconds
  exshuffle <- extract_command(x, "exshuffle", "logical") ## shuffle schoice/mchoice answers?
  exsingle  <- extract_command(x, "exsingle",  "logical") ## use radio buttons?
  exmaxchars  <- extract_command(x, "exmaxchars") ## use radio buttons?

  ## User-Defined ###################################
  exextra <- extract_extra(x)

  ## process valid solution types (in for loop for each cloze element)
  slength <- length(exsolution)
  if(slength < 1L) stop("no \\exsolution{} specified")
  exsolution <- switch(extype,
    "schoice" = string2mchoice(exsolution, single = TRUE),
    "mchoice" = string2mchoice(exsolution),
    "num" = as.numeric(exsolution),
    "string" = exsolution,
    "cloze" = {
      if(is.null(exclozetype)) {
        warning("no \\exclozetype{} specified, taken to be string")
	exclozetype <- "string"
      }
      if(length(exclozetype) > 1L & length(exclozetype) != slength)
        warning("length of \\exclozetype{} does not match length of \\exsolution{}")
      exclozetype <- rep(exclozetype, length.out = slength)
      exsolution <- as.list(exsolution)
      for(i in 1L:slength) exsolution[[i]] <- switch(match.arg(exclozetype[i], c("schoice", "mchoice", "num", "string")),
        "schoice" = string2mchoice(exsolution[[i]], single = TRUE), ## FIXME: like this?
        "mchoice" = string2mchoice(exsolution[[i]]),
        "num" = as.numeric(exsolution[[i]]),
        "string" = exsolution[[i]])
      exsolution
    })
  slength <- length(exsolution)

  ## lower/upper tolerance value
  if(is.null(extol)) extol <- 0
  extol <- rep(extol, length.out = slength)

  ## compute "nice" string for printing solution in R
  string <- switch(extype,
    "schoice" = paste(exname, ": ", which(exsolution), sep = ""),                                                      ## FIXME: currently fixed
    "mchoice" = paste(exname, ": ", paste(if(any(exsolution)) which(exsolution) else "-", collapse = ", "), sep = ""), ## FIXME: currently fixed
    "num" = if(max(extol) <= 0) {
      paste(exname, ": ", exsolution, sep = "")
    } else {
      if(slength == 1L) {
        paste(exname, ": ", exsolution, " (", exsolution - extol, "--", exsolution + extol, ")", sep = "")
      } else {
	paste(exname, ": [", exsolution[1L], ", ", exsolution[2L], "] ([", exsolution[1L] - extol[1L], "--", exsolution[1L] + extol[1L], ", ",
	  exsolution[2L] - extol[2L], "--", exsolution[2L] + extol[2L], "])", sep = "")
      }
    },
    "string" = paste(exname, ": ", paste(exsolution, collapse = "\n"), sep = ""),
    "cloze" = paste(exname, ": ", paste(sapply(exsolution, paste, collapse = ", "), collapse = " | "), sep = "")
  )

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

  ## return everything (backward compatible with earlier versions)
  rval <- list(
    file = file_path_sans_ext(file),
    type = extype,
    name = exname,
    title = extitle,
    section = exsection,
    version = exversion,
    solution = exsolution,
    tolerance = extol,
    clozetype = exclozetype,
    points = expoints,
    time = extime,
    shuffle = exshuffle,
    single = exsingle,
    length = slength,
    string = string,
    maxchars = exmaxchars
  )
  rval <- c(rval, exextra)
  return(rval)
}
