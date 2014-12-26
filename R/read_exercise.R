read_exercise <- function(file)
{
  ## read all text
  x <- readLines(file)
  
  ## convenience helper function
  zap_text_if_empty <- function(x) {
    if(length(x) < 1L) return("")
    if(all(grepl("^[[:space:]]*$", x))) return("")
    return(x)
  }

  ## process question
  question <- extract_environment(x, "question")
  questionlist <- ql <- extract_environment(question, "answerlist", value = FALSE)
  if(!is.null(questionlist)) {
    questionlist <- extract_items(question[(ql[1L] + 1L):(ql[2L] - 1L)])
    question <- zap_text_if_empty(question[-(ql[1L]:ql[2L])])
  }

  ## process solution
  solution <- extract_environment(x, "solution")
  solutionlist <- sl <- extract_environment(solution, "answerlist", value = FALSE)
  if(!is.null(solutionlist)) {
    solutionlist <- extract_items(solution[(sl[1L] + 1L):(sl[2L] - 1L)])
    solution <- zap_text_if_empty(solution[-(sl[1L]:sl[2L])])
  }

  metainfo <- read_metainfo(file)
  
  ## consistency checks
  if(!is.null(questionlist) && metainfo$type %in% c("schoice", "mchoice") && metainfo$length != length(questionlist))
    warning("length of \\exsolution and {answerlist} does not match")
  if(!is.null(solutionlist) && metainfo$type %in% c("schoice", "mchoice") && metainfo$length != length(solutionlist))
    warning("length of \\exsolution and {answerlist} does not match")

  ## perform shuffling?
  if(isTRUE(metainfo$shuffle) & metainfo$type %in% c("schoice", "mchoice")) {
    o <- sample(metainfo$length)
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
  }
  if(isTRUE(metainfo$shuffle) & metainfo$type == "cloze") {
    gr <- rep.int(1L:metainfo$length, sapply(metainfo$solution, length))
    questionlist <- split(questionlist, gr)
    solutionlist <- split(solutionlist, gr)
    for(i in which(metainfo$clozetype %in% c("schoice", "mchoice"))) {
      o <- sample(length(questionlist[[i]]))
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
