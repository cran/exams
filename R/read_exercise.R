read_exercise <- function(file)
{
  ## read all text
  x <- readLines(file)
  
  ## convenience helper function
  drop_text_if_empty <- function(x) {
    if(length(x) < 1L) return(NULL)
    if(all(grepl("^[[:space:]]*$", x))) return(NULL)
    return(x)
  }

  ## process question
  question <- extract_environment(x, "question")
  questionlist <- ql <- extract_environment(question, "answerlist", value = FALSE)
  if(!is.null(questionlist)) {
    questionlist <- extract_items(question[(ql[1L] + 1L):(ql[2L] - 1L)])
    question <- drop_text_if_empty(question[-(ql[1L]:ql[2L])])
  }

  ## process solution
  solution <- extract_environment(x, "solution")
  solutionlist <- sl <- extract_environment(solution, "answerlist", value = FALSE)
  if(!is.null(solutionlist)) {
    solutionlist <- extract_items(solution[(sl[1L] + 1L):(sl[2L] - 1L)])
    solution <- drop_text_if_empty(solution[-(sl[1L]:sl[2L])])
  }

  metainfo <- read_metainfo(file)
  
  ## consistency checks
  if(!is.null(questionlist) && metainfo$type %in% c("schoice", "mchoice") && metainfo$length != length(questionlist))
    warning("length of \\exsolution and {answerlist} does not match")
  if(!is.null(solutionlist) && metainfo$type %in% c("schoice", "mchoice") && metainfo$length != length(solutionlist))
    warning("length of \\exsolution and {answerlist} does not match")
  
  list(
    question = question,
    questionlist = questionlist,
    solution = solution,
    solutionlist = solutionlist,
    metainfo = metainfo
  )
}
