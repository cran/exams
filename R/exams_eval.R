exams_eval <- function(partial = TRUE, negative = FALSE, rule = c("false2", "false", "true", "all", "none"))
{
  ## rule for negative points in partial evaluation of mchoice answers
  rule <- match.arg(rule, c("false2", "false", "true", "all", "none"))

  ## negative value for wrong answers (or lower bound for sum of partial results)
  if(is.logical(negative)) negative <- ifelse(negative, -1, 0)
  negative <- -abs(as.numeric(negative))

  ## convenience function for determining exercise type
  extype <- function(correct, answer = NULL, type = NULL) {
    ## convenience function
    mchoice01 <- function(x) as.numeric(strsplit(unlist(x), "")[[1L]])

    ## if type not given: auto-detect from correct
    if(is.null(type)) {
      type <- if(is.numeric(correct)) {
        "num"
      } else if(is.logical(correct)) {
        "mchoice"
      } else if(is.character(correct)) {
        if(all(strsplit(correct, "")[[1L]] %in% c("0", "1"))) "mchoice" else "string"
      } else {
        "unknown"
      }
    }
    if(!(type %in% c("num", "mchoice", "schoice", "string"))) stop("Unknown exercise type.")
    
    ## canonicalize correct
    if(type != "string" && is.character(correct)) {
      correct <- if(type == "num") as.numeric(correct) else as.logical(mchoice01(correct))    
    }
    
    ## process answer (if any)
    if(!is.null(answer)) {
      answer <- switch(type,
        "num" = {
          if(is.character(answer)) answer <- gsub(",", ".", answer, fixed = TRUE)
          as.numeric(answer)	
	},
	"mchoice" = {
          if(is.character(answer)) answer <- mchoice01(answer)
	  as.logical(answer)
	},
	"schoice" = {
          if(is.character(answer)) answer <- mchoice01(answer)
	  as.logical(answer)
	},
	"string" = {
	  as.character(answer)
	}
      )
      if(!any(is.na(answer)) && (length(correct) != length(answer))) stop(
        "Length of 'correct' and given 'answer' do not match.")
    }
    
    return(list(type = type, correct = correct, answer = answer))
  }
  
  checkanswer <- function(correct, answer, tolerance = 0, type = NULL)
  {
    ## preprocess type of solution
    type <- extype(correct, answer, type = type)
    correct <- type$correct
    answer <- type$answer
    type <- type$type

    if(is.null(answer)) return(rep.int(0, length(correct)))
    
    ## numeric answer can be NA or needs to fall into tolerance interval
    if(type == "num") {
      if(any(is.na(answer))) return(0L)
      if(all(answer >= correct - tolerance & answer <= correct + tolerance)) {
        return(1L)
      } else {
        return(-1L)
      }
    }
    
    ## mchoice answer can be processed partially or as a whole pattern
    if(type %in% c("mchoice", "schoice")) {
      if(any(is.na(answer))) return(0L)
      if(partial && type == "mchoice") {
	rval <- rep.int(0L, length(answer))
        if(all(!answer)) return(rval)
	rval[which(correct & answer)] <- 1L
	rval[which(!correct & answer)] <- -1L
	return(rval)
      } else {
        if(any(is.na(answer))) return(0)
        if(negative < 0 & all(!answer)) return(0)
        return(ifelse(all(correct == answer), 1L, -1L))
      }
    }
    
    ## string answer is NA if empty, otherwise has to match exactly
    if(type == "string") {
      if(any(is.na(answer)) | all(grepl("^[[:space:]]*$", answer))) return(NA)
      return(ifelse(correct == answer, 1L, -1L))
    }
  }

  pointvec <- function(correct = NULL, type = NULL) {
    if(!partial) return(c("pos" = 1, "neg" = negative))
    if(is.null(correct)) stop("Need 'correct' answer to compute points vector.")
    type <- extype(correct, type = type)
    if(type$type == "mchoice") {
      n <- switch(rule,
        "false" = -1/sum(!type$correct),
	"false2" = -1/pmax(sum(!type$correct), 2),
	"true" = -1/sum(type$correct),
	"all" = -1,
	"none" = 0)
      return(c("pos" = 1/sum(type$correct), "neg" = n))
    } else {
      return(c("pos" = 1, "neg" = negative))
    }
  }
  
  pointsum <- function(correct, answer, tolerance = 0, type = NULL) {
    pts <- pointvec(correct, type = type)
    chk <- as.character(checkanswer(correct, answer, tolerance = tolerance, type = type))
    res <- rep(0, length.out = length(chk))
    res[which(chk == "1")] <- pts["pos"]
    res[which(chk == "-1")] <- pts["neg"]
    pmax(sum(res), negative)
  }

  ## return (processed) parameters and functions
  list(
    partial = partial,
    negative = negative,
    rule = rule,
    checkanswer = checkanswer,
    pointvec = pointvec,
    pointsum = pointsum
  )
}

