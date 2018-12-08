read_exercise <- function(file, markup = NULL)
{
  ## read all text
  x <- readLines(file)
  if(is.null(markup)) markup <- switch(tolower(tools::file_ext(file)),
    "tex"  = "latex",
    "rtex" = "latex",
    "rnw"  = "latex",
    "md"   = "markdown",
    "rmd"  = "markdown"
  )

  ## add linebreaks within \end{answerlist}\end{...}
  ## to work around Sweave dropping spaces between these
  if(markup == "latex") {
    sep <- "\\007\\007\\007\\007\\007"
    end <- c("\\end{answerlist}", "\\end")
    end1 <- paste(end, collapse = "")
    end2 <- paste(end, collapse = sep)
    x <- as.list(x)
    x <- lapply(x, function(y) {
      if(grepl(end1, y, fixed = TRUE)) {
        y <- gsub(end1, end2, y, fixed = TRUE)
        strsplit(y, sep, fixed = TRUE)[[1L]]
      } else {
        y
      }
    })
    x <- unlist(x)
  }
  
  ## convenience helper function
  zap_text_if_empty <- function(x) {
    if(length(x) < 1L) return("")
    if(all(grepl("^[[:space:]]*$", x))) return("")
    return(x)
  }

  ## process question
  question <- extract_environment(x, "question", markup = markup)
  questionlist <- ql <- extract_environment(question, "answerlist", value = FALSE, markup = markup)
  if(!is.null(questionlist)) {
    qli <- if(markup == "latex") (ql[1L] + 1L):(ql[2L] - 1L) else (ql[1L] + 2L):ql[2L]
    questionlist <- extract_items(question[qli], markup = markup)
    question <- zap_text_if_empty(question[-(ql[1L]:ql[2L])])
  }

  ## process solution
  solution <- extract_environment(x, "solution", markup = markup)
  solutionlist <- sl <- extract_environment(solution, "answerlist", value = FALSE, markup = markup)
  if(!is.null(solutionlist)) {
    sli <- if(markup == "latex") (sl[1L] + 1L):(sl[2L] - 1L) else (sl[1L] + 2L):sl[2L]
    solutionlist <- extract_items(solution[sli], markup = markup)
    solution <- zap_text_if_empty(solution[-(sl[1L]:sl[2L])])
  }

  metainfo <- read_metainfo(file)
  
  ## consistency checks
  if(!is.null(questionlist) && metainfo$type %in% c("schoice", "mchoice") && metainfo$length != length(questionlist))
    warning("length of exsolution and questionlist does not match")
  if(!is.null(solutionlist) && metainfo$type %in% c("schoice", "mchoice") && metainfo$length != length(solutionlist))
    warning("length of exsolution and solutionlist does not match")

  ## perform shuffling?
  if(!identical(metainfo$shuffle, FALSE) & metainfo$type %in% c("schoice", "mchoice")) {
    o <- sample(metainfo$length)
    if(is.numeric(metainfo$shuffle)) {
      ## subsample the choices: take the first TRUE and FALSE (if any)
      ## and then the first remaining ones (only FALSE ones for schoice)
      ns <- min(c(metainfo$length, metainfo$shuffle))
      os <- c(
        if(any(metainfo$solution)) which.max(metainfo$solution[o]),
        if(any(!metainfo$solution)) which.max(!metainfo$solution[o])
      )
      nos <- if(metainfo$type == "mchoice") {
        seq_along(o)[-os]
      } else {
        seq_along(o)[-unique(c(os, which(metainfo$solution[o])))]
      }
      os <- c(os, nos[1L:min(c(ns - length(os), length(nos)))])
      o <- o[sort(os)]
      if(length(o) < metainfo$shuffle) warning(sprintf("%s shuffled answers requested, only %s available", metainfo$shuffle, length(o)))
    }
    questionlist <- questionlist[o]
    solutionlist <- solutionlist[o]
    metainfo$solution <- metainfo$solution[o]
    metainfo$tolerance <- metainfo$tolerance[o]
    metainfo$string <- if(metainfo$type == "schoice") {
      ## copied from read_metainfo
      paste(metainfo$name, ": ", which(metainfo$solution), sep = "")
    } else {
      paste(metainfo$name, ": ", paste(if(any(metainfo$solution)) which(metainfo$solution) else "-", collapse = ", "), sep = "")
    }
    metainfo$length <- length(questionlist)
  }
  if(!identical(metainfo$shuffle, FALSE) & metainfo$type == "cloze") {
    gr <- rep.int(1L:metainfo$length, sapply(metainfo$solution, length))
    questionlist <- split(questionlist, gr)
    solutionlist <- split(solutionlist, gr)
    for(i in which(metainfo$clozetype %in% c("schoice", "mchoice"))) {
      o <- sample(length(questionlist[[i]]))
      if(is.numeric(metainfo$shuffle)) {
        ## subsample the choices: take the first TRUE and FALSE (if any)
        ## and then the first remaining ones
        ns <- min(c(length(questionlist[[i]]), metainfo$shuffle))
        os <- c(
          if(any(metainfo$solution[[i]])) which.max(metainfo$solution[[i]]),
          if(any(!metainfo$solution[[i]])) which.max(!metainfo$solution[[i]])
        )
        os <- c(os, (seq_along(o)[-os])[1L:(ns - length(os))])
        os
        o <- o[sort(os)]
      }
      questionlist[[i]] <- questionlist[[i]][o]
      solutionlist[[i]] <- solutionlist[[i]][o]
      metainfo$solution[[i]] <- metainfo$solution[[i]][o]
    }
    questionlist <- unlist(questionlist)
    solutionlist <- unlist(solutionlist)
    metainfo$string <- paste(metainfo$name, ": ", paste(sapply(metainfo$solution, paste, collapse = ", "), collapse = " | "), sep = "")
  }
  
  ## collect everything in one list
  list(
    question = question,
    questionlist = questionlist,
    solution = solution,
    solutionlist = solutionlist,
    metainfo = metainfo
  )
}
