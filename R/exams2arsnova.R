exams2arsnova <- function(file, n = 1L, dir = ".",
  name = "R/exams", sname = NULL, qname = NULL,
  quiet = TRUE, resolution = 100, width = 4, height = 4, svg = FALSE,
  encoding = "UTF-8", envir = NULL, engine = NULL,
  url = "https://arsnova.eu/api", sessionkey = NULL, jsessionid = NULL,
  active = TRUE, votingdisabled = FALSE, showstatistic = FALSE, showanswer = FALSE, abstention = TRUE,
  variant = "lecture", ssl.verifypeer = TRUE, fix_choice = TRUE, ...)
{
  warning("ARSnova has been superseded by Particify, hence it is recommended to use exams2particify() instead, exams2arsnova() will be removed in future versions of R/exams")

  ## names (session, questions)
  if(is.null(sname)) sname <- name
  if(length(sname) != 2L) sname <- rep(sname, length.out = 2L)
  sname[1L] <- abbreviate(sname[1L], 50L)
  sname[2L] <- abbreviate(sname[2L], 8L)
  if(is.null(qname)) qname <- name
  if(length(qname) == 1L && length(file) > 1L) qname <- paste0(qname, "/",
    formatC(1:length(file), width = floor(log10(length(file))) + 1L, flag = "0"))
    
  if(length(qname) != length(file)) stop("length of 'file' and 'qname' do not match")

  ## Markdown transformer:
  ## - Github flavor
  ## - math mode with \( rather than $
  ## - avoid line wrapping
  mdtransform <- make_exercise_transform_pandoc(to = "gfm",
    options = "--wrap=none")

  ## create JSON/RCurl write with custom options
  arsnovawrite <- make_exams_write_arsnova(url = url, sessionkey = sessionkey, jsessionid = jsessionid,
    name = name, sname = sname, qname = qname,
    active = active, votingdisabled = votingdisabled, showstatistic = showstatistic,
    showanswer = showanswer, abstention = abstention, variant = variant,
    ssl.verifypeer = ssl.verifypeer, fix_choice = fix_choice)

  ## generate xexams
  rval <- xexams(file, n = n, dir = dir,
    driver = list(
      sweave = list(quiet = quiet, pdf = FALSE, png = !svg, svg = svg, resolution = resolution, width = width, height = height,
        encoding = encoding, envir = envir, engine = engine),
      read = NULL,
      transform = mdtransform,
      write = arsnovawrite),
    ...)

  ## return xexams object invisibly
  invisible(rval)
}

make_exams_write_arsnova <- function(url = "https://arsnova.eu/api", sessionkey = NULL, jsessionid = NULL,
  name = "R/exams", sname = NULL, qname = NULL, active = TRUE, votingdisabled = FALSE, showstatistic = FALSE,
  showanswer = FALSE, abstention = TRUE, variant = "lecture",
  ssl.verifypeer = TRUE, fix_choice = TRUE)
{
  warning("ARSnova has been superseded by Particify, make_exams_write_arsnova() will be removed in future versions of R/exams")

  ## check whether JSON data can actually be POSTed (by question)
  ## or should be exported to a file (full session)
  post <- !is.null(url) & !is.null(jsessionid) & !is.null(sessionkey)
  csv <- !post & !is.null(sessionkey)

  ## curl info
  if(post) {
    if(substr(url, nchar(url), nchar(url)) == "/") url <- substr(url, 1L, nchar(url) - 1L)
    url <- sprintf("%s/lecturerquestion/?sessionkey=%s", url, sessionkey)
    head <- c("Content-Type: application/json; charset=UTF-8",
      paste0("Cookie: JSESSIONID=", jsessionid))
  }

  ## question list template
  qtemp <- list(
    type = "skill_question",
    questionType = "abcd",
    questionVariant = match.arg(variant, c("lecture", "preparation")),
    subject = "name",
    text = NULL,
    active = active,
    releasedFor = "all",
    possibleAnswers = list(),
    noCorrect = FALSE,
    sessionKeyword = sessionkey,
    votingDisabled = votingdisabled,
    showStatistic = showstatistic,
    showAnswer = showanswer,
    abstention = abstention
  )
  ## session list template
  stemp <- list(
    exportData = list(
      session = list(
        name = if(is.null(sname)) name else sname[1L],
        shortName = if(is.null(sname)) name else sname[2L],
        active = active,
	publicPool = list(
          ppAuthorName = NULL,
          ppAuthorMail = NULL,
          ppUniversity = NULL,
          ppLogo = NULL,
          ppSubject = NULL,
          ppLicense = NULL,
          ppLevel = NULL,
          ppDescription = NULL,
          ppFaculty = NULL,
          name = if(is.null(sname)) name else sname[1L],
          shortName = if(is.null(sname)) name else sname[2L]
	)
      ),
      questions = list(),
      feedbackQuestions = list()
    )
  )
  
  fix_choice <- if(fix_choice) {
    function(x) {
      x <- gsub("\\(", "", x, fixed = TRUE)
      x <- gsub("\\)", "", x, fixed = TRUE)
      x <- gsub("\\%", "%", x, fixed = TRUE)
      x
    }
  } else {
    function(x) x
  }
  
  ## set up actual write function
  function(exm, dir, info)
  {
    ## basic indexes
    id <- info$id
    n <- info$n
    m <- length(exm)

    ## check whether all exercises are schoice/mchoice
    wrong_type <- sapply(1L:m, function(n) exm[[n]]$metainfo$file)[
      !sapply(1L:m, function(n) exm[[n]]$metainfo$type %in% c("schoice", "mchoice", "num", "string"))]
    if(length(wrong_type) > 0) {
      stop(paste("the following exercises are not supported:",
        paste(wrong_type, collapse = ", ")))
    }

    ## placeholder in session template
    stemp$exportData$questions <- vector(mode = "list", length = m)
  
    for(j in 1L:m) {
      ## copy question template and adapt subject
      json <- qtemp
      json$subject <- if(is.null(qname)) name else qname[j]

      ## collapse question text
      json$text <- paste(exm[[j]]$question, collapse = "\n")

      ## questionType and possibleAnswers
      if(exm[[j]]$metainfo$type %in% c("schoice", "mchoice")) {
        json$possibleAnswers <- lapply(seq_along(exm[[j]]$questionlist),
          function(i) list(text = fix_choice(as.vector(exm[[j]]$questionlist)[i]), correct = exm[[j]]$metainfo$solution[i]))
      }
      if(exm[[j]]$metainfo$type == "mchoice") {
        json$questionType <- "mc"
        json$noCorrect <- sum(exm[[j]]$metainfo$solution) > 0
      }
      if(exm[[j]]$metainfo$type %in% c("num", "string")) json$questionType <- "freetext"

      ## if there is an existing session, just add questions
      if(post) {
        ## convert to json
        json <- RJSONIO::toJSON(json)
	## POST via RCurl
        w <- RCurl::basicTextGatherer()
        rval <- RCurl::curlPerform(url = url, httpheader = head, postfields = json, writefunction = w$update,
          ssl.verifypeer = ssl.verifypeer)
	if(rval != 0) warning(paste("question", j, "could not be posted"))
	w$reset()
      } else {
        stemp$exportData$questions[[j]] <- json	
      }
    }
    
    ## collect json file for entire session (rather than individual questions only)
    if(!post) {
      ## assign names for output files
      fil <- gsub("/", "", name, fixed = TRUE)
      fil <- gsub(" ", "-", fil, fixed = TRUE)
      fil <- paste0(fil, "-", formatC(id, width = floor(log10(n)) + 1L, flag = "0"))

      if(is.null(sessionkey)) {
        ## json string for whole session
        json <- RJSONIO::toJSON(stemp)      
        ## create and copy output json
	fil <- paste0(fil, ".json")
        writeLines(json, fil)
        file.copy(fil, dir, overwrite = TRUE)
      } else {
        ans <- function(x, i) if(i > length(x$possibleAnswers)) "" else x$possibleAnswers[[i]]$text 
        df <- sapply(stemp$exportData$questions, function (x) {
	  c(
            "questionType" = x$questionType,
            "questionSubject" = x$subject,
            "question" = x$text,
            "answer1" = ans(x, 1L),
            "answer2" = ans(x, 2L),
            "answer3" = ans(x, 3L),
            "answer4" = ans(x, 4L),
            "answer5" = ans(x, 5L),
            "answer6" = ans(x, 6L),
            "answer7" = ans(x, 7L),
            "answer8" = ans(x, 8L),
            "correctAnswer" = if(x$questionType == "freetext") "" else {
	      paste0(which(sapply(x$possibleAnswers, "[[", "correct")), collapse = ",")
	    },
            "abstention" = ifelse(x$abstention, "y", "n"),
            "hint" = "",
            "solution" = ""
	  )
	})
	df <- as.data.frame(t(df), stringsAsFactors = FALSE)
        ## create and copy output json
	fil <- paste0(fil, ".csv")
        write.table(df, fil, quote = TRUE, col.names = TRUE, row.names = FALSE, sep = ",")
        file.copy(fil, dir, overwrite = TRUE)
        
      }
    }
  }
}
