read_exercise <- function(file, markup = NULL, exshuffle = NULL)
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

  metainfo <- read_metainfo(file, exshuffle = exshuffle)
  
  ## consistency checks
  if(!is.null(questionlist) && metainfo$type %in% c("schoice", "mchoice") && metainfo$length != length(questionlist))
    stop(sprintf("length of exsolution (= %s) and questionlist (= %s) does not match in '%s'", metainfo$length, length(questionlist), metainfo$file))
  if(!is.null(solutionlist) && metainfo$type %in% c("schoice", "mchoice") && metainfo$length != length(solutionlist))
    warning(sprintf("length of exsolution (= %s) and solutionlist (= %s) does not match in '%s'", metainfo$length, length(solutionlist), metainfo$file))

  ## cloze with placeholds: all placeholders available?
  if(metainfo$type == "cloze" && any(grepl(paste0(if(metainfo$markup == "markdown") "\\#\\#" else "##", "ANSWER"), question, fixed = TRUE))) {
    ii <- 1L:length(metainfo$clozetype)
    tag <- if(metainfo$markup == "markdown") "\\#\\#" else "##"
    frq <- sapply(ii, function(i) sum(grepl(paste0(tag, "ANSWER", i, tag), question, fixed = TRUE)))
    if(any(frq < 1L)) warning(paste("the ##ANSWERi## placeholders are missing for i in", paste(ii[frq < 1L], collapse = ", ")))
    if(any(frq > 1L)) warning(paste("the ##ANSWERi## placeholders are occuring more than once for i in", paste(ii[frq > 1L], collapse = ", ")))
  }

  ## check length of questionlist, adapt if necessary
  if((metainfo$type == "cloze") & (length(unlist(questionlist)) < length(unlist(metainfo$solution)))) {
    is_choice <- lapply(1:length(metainfo$solution), function(i) {
      rep(grepl("choice", metainfo$clozetype[i]), length(metainfo$solution[[i]]))
    })
    j <- seq_len(sum(unlist(is_choice)))
    ql2 <- list()
    for(i in seq_along(is_choice)) {
      if(all(is_choice[[i]])) {
        k <- 1:length(is_choice[[i]])
        ql2[[i]] <- questionlist[j[k]]
        j <- j[-k]
      } else {
        ql2[[i]] <- ""
      }
    }
    questionlist <- unlist(ql2)
  }

  ## perform shuffling?
  if(!identical(metainfo$shuffle, FALSE) & metainfo$type %in% c("schoice", "mchoice")) {
    o <- shuffle_choice(metainfo$solution, metainfo$shuffle, metainfo$type, metainfo$file)
    questionlist <- questionlist[o]
    solutionlist <- solutionlist[o]
    metainfo$solution <- metainfo$solution[o]
    metainfo$tolerance <- metainfo$tolerance[o]
    ## adapted from read_metainfo
    s <- if(length(metainfo$solution) <= 26) letters[which(metainfo$solution)] else which(metainfo$solution)
    metainfo$string <- if(metainfo$type == "schoice") {
      paste(metainfo$name, ": ", s, sep = "")
    } else {
      paste(metainfo$name, ": ", paste(if(any(metainfo$solution)) s else "-", collapse = ", "), sep = "")
    }
    metainfo$length <- length(questionlist)
  }
  if(!identical(metainfo$shuffle, FALSE) & metainfo$type == "cloze") {
    gr <- rep.int(1L:metainfo$length, sapply(metainfo$solution, length))
    ssol <- length(solutionlist) == length(questionlist) ## shuffle solutionlist?
    questionlist <- split(questionlist, gr)
    if(ssol) solutionlist <- split(solutionlist, gr)
    for(i in which(metainfo$clozetype %in% c("schoice", "mchoice"))) {
      ## in cloze exercises with numeric exshuffle: only warn if exshuffle is greater than maximum of available choices
      shuffle_i <- if(is.numeric(metainfo$shuffle) && metainfo$shuffle <= max(lengths(metainfo$solution))) {
        min(length(metainfo$solution[[i]]), metainfo$shuffle)
      } else {
        metainfo$shuffle
      }
      o <- shuffle_choice(metainfo$solution[[i]], shuffle_i, metainfo$clozetype[i], metainfo$file)
      questionlist[[i]] <- questionlist[[i]][o]
      if(ssol) solutionlist[[i]] <- solutionlist[[i]][o]
      metainfo$solution[[i]] <- metainfo$solution[[i]][o]
    }
    questionlist <- as.vector(unlist(questionlist))
    if(ssol) solutionlist <- as.vector(unlist(solutionlist))
    metainfo$string <- paste(metainfo$name, ": ", paste(sapply(metainfo$solution, paste, collapse = ", "), collapse = " | "), sep = "")
  }

  ## question list of schoice/mchoice items should usually not have duplicated items
  if(metainfo$type %in% c("schoice", "mchoice") && !is.null(questionlist)) {
    if(any(duplicated(questionlist))) warning(sprintf("duplicated items in question list in '%s'", metainfo$file))
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

shuffle_choice <- function(solution, shuffle, type = "mchoice", file = NULL) {
  len <- length(solution)
  o <- sample(len)
  if(is.numeric(shuffle)) {
    ## subsample the choices: take the first TRUE and FALSE (if any)
    ## and then the first remaining ones (only FALSE ones for schoice)
    ns <- min(c(len, shuffle))
    os <- c(
      if(any(solution)) which.max(solution[o]),
      if(any(!solution)) which.max(!solution[o])
    )
    nos <- if(type == "mchoice") {
      seq_along(o)[-os]
    } else {
      seq_along(o)[-unique(c(os, which(solution[o])))]
    }
    if(ns > 2L && length(nos) > 0L) os <- c(os, nos[1L:min(c(ns - length(os), length(nos)))])
    o <- o[sample(os)]
    if(length(o) < shuffle) warning(sprintf("%s shuffled answers requested, only %s available%s",
      shuffle, length(o), if(is.null(file)) "" else sprintf(" in '%s'", file)))
  }
  return(o)
}
