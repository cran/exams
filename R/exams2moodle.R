## generate exams in Moodle 2.3 .xml format
## http://docs.moodle.org/23/en/Moodle_XML_format
exams2moodle <- function(file, n = 1L, nsamp = NULL, dir = ".",
  name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL,
  resolution = 100, width = 4, height = 4,
  iname = TRUE, stitle = NULL, testid = FALSE, zip = FALSE,
  num = NULL, mchoice = NULL, schoice = mchoice, string = NULL, cloze = NULL,
  ...)
{
  ## set up .html transformer
  htmltransform <- make_exercise_transform_html(...)

  ## generate the exam
  exm <- xexams(file, n = n, nsamp = nsamp,
   driver = list(
       sweave = list(quiet = quiet, pdf = FALSE, png = TRUE,
         resolution = resolution, width = width, height = height),
       read = NULL, transform = htmltransform, write = NULL),
     dir = dir, edir = edir, tdir = tdir, sdir = sdir)

  ## get the possible moodle question body functions and options
  moodlequestion = list(num = num, mchoice = mchoice, schoice = schoice, cloze = cloze, string = string)

  for(i in c("num", "mchoice", "schoice", "cloze", "string")) {
    if(is.null(moodlequestion[[i]])) moodlequestion[[i]] <- list()
    if(is.list(moodlequestion[[i]])) moodlequestion[[i]] <- do.call("make_question_moodle23", moodlequestion[[i]])
    if(!is.function(moodlequestion[[i]])) stop(sprintf("wrong specification of %s", sQuote(i)))
  }

  ## create a temporary directory
  dir <- path.expand(dir)
  if(is.null(tdir)) {
    dir.create(tdir <- tempfile())
    on.exit(unlink(tdir))
  } else {
    tdir <- path.expand(tdir)
  }
  if(!file.exists(tdir)) dir.create(tdir)

  ## obtain the number of exams and questions
  nx <- length(exm)
  nq <- length(exm[[1L]])

  ## create a name
  if(is.null(name)) name <- "moodlequiz"

  ## function for internal ids
  make_test_ids <- function(n, type = c("test", "section", "item"))
  {
    switch(type,
      "test" = if(testid) paste(name, make_id(9), sep = "_") else name,
      paste(type, formatC(1:n, flag = "0", width = nchar(n)), sep = "_")
    )
  }

  ## generate the test id
  test_id <- make_test_ids(type = "test")

  ## create the directory where the test is stored
  dir.create(test_dir <- file.path(tdir, name))

  exsecs <- rep(NA, length = nq)
  if(!is.null(stitle)) {
    if((ks <- length(stitle)) > nq) stop("more section titles than exercises specified!")
    exsecs[1:ks] <- stitle
  }

  ## start the quiz .xml
  xml <- c('<?xml version="1.0" encoding="UTF-8"?>', '<quiz>\n')

  ## cycle through all questions and samples
  for(j in 1:nq) {
    ## search for \exsection{}
    exsec <- if(is.null(exm[[1]][[j]]$metainfo$section)) {
      paste("Exercise", formatC(j, flag = "0", width = nchar(nq)))
    } else exm[[1]][[j]]$metainfo$section

    ## if specified, overule the section
    if(!is.na(exsecs[j])) exsec <- exsecs[j]

    ## first, create the category tag for the question
    xml <- c(xml,
      '\n<question type="category">',
      '<category>',
      paste('<text>$course$/', if(iname) paste(test_id, '/', sep = '') else NULL, exsec, '</text>', sep = ''),
      '</category>',
      '</question>\n')

    ## create ids for all samples
    sample_ids <- paste(exsec, make_test_ids(nx, type = "sample"), sep = "_")

    ## create the questions
    for(i in 1:nx) {
      ## get the question type
      type <- exm[[i]][[j]]$metainfo$type

      ## attach sample id to metainfo
      exm[[i]][[j]]$metainfo$id <- paste(sample_ids[i], type, sep = "_")

      ## add sample number to name
      exm[[i]][[j]]$metainfo$name <- paste("Replication",
        formatC(i, flag = "0", width = nchar(nx)), ":",
        if(!is.null(exm[[i]][[j]]$metainfo$title)) {
          exm[[i]][[j]]$metainfo$title
        } else exm[[i]][[j]]$metainfo$file)

      ## create the .xml
      question_xml <- moodlequestion[[type]](exm[[i]][[j]])

      ## include supplements using base64 encoding and data uri
      if(length(exm[[i]][[j]]$supplements)) {
        for(si in seq_along(exm[[i]][[j]]$supplements)) {
          if(any(grepl(f <- basename(exm[[i]][[j]]$supplements[si]), question_xml))) {
            question_xml <- gsub(paste(f, '"', sep = ''),
              paste(fileURI(exm[[i]][[j]]$supplements[si]), '"', sep = ''),
              question_xml, fixed = TRUE)
          }
        }
      }

      ## add question to quiz .xml
      xml <- c(xml, question_xml)
    }
  }

  ## finish the quiz
  xml <- c(xml, '</quiz>')

  ## write to dir
  writeLines(xml, file.path(test_dir, paste(name, "xml", sep = ".")))

  ## compress
  if(zip) {
    owd <- getwd()
    setwd(test_dir)
    zip(zipfile = zipname <- paste(name, "zip", sep = "."), files = list.files(test_dir))
    setwd(owd)
  } else zipname <- list.files(test_dir)

  ## copy the final .zip file
  file.copy(file.path(test_dir, zipname), dir, recursive = TRUE)

  ## assign test id as an attribute
  attr(exm, "test_id") <- test_id

  invisible(exm)
}


## Moodle 2.3 question constructor function
make_question_moodle23 <- function(name = NULL, solution = TRUE, shuffle = FALSE, penalty = 0,
  answernumbering = "abc", usecase = FALSE, cloze_mchoice_display = "MULTICHOICE",
  truefalse = c("True", "False"), enumerate = TRUE)
{
  function(x) {
    ## how many points?
    points <- if(is.null(x$metainfo$points)) 1 else x$metainfo$points

    ## match question type
    type <- switch(x$metainfo$type,
      "num" = "numerical",
      "mchoice" = "multichoice",
      "schoice" = "multichoice",
      "cloze" = "cloze",
      "string" = "shortanswer"
    )

    ## question name
    if(is.null(name)) name <- x$metainfo$name

    ## start the question xml
    xml <- c(
      paste('\n<question type="', type, '">', sep = ''),
      '<name>',
      paste('<text>', name, '</text>'),
      '</name>',
      '<questiontext format="html">',
      '<text><![CDATA[<p>', if(type != "cloze") x$question else '##QuestionText', '</p>]]></text>',
      '</questiontext>'
    )

    ## insert the solution
    if(length(x$solution) && solution) {
      xml <- c(xml,
        '<generalfeedback format="html">',
        '<text><![CDATA[<p>', x$solution,
        if(!type %in% c("mchoice", "schoice") && (nsol <- length(x$solutionlist))) {
          g <- rep(seq_along(x$metainfo$solution), sapply(x$metainfo$solution, length))
          soll <- sapply(split(x$solutionlist, g), paste, collapse = " / ")
          c(if(enumerate) '<ol type = "a">' else '</br>',
            paste(if(enumerate) "<li>" else NULL, soll, if(enumerate) "</li>" else NULL),
            if(enumerate) '</ol>' else NULL)
        } else NULL,
        '</p>]]></text>',
        '</generalfeedback>'
      )
    }

    ## penalty and points
    xml <- c(xml,
      paste('<penalty>', penalty, '</penalty>', sep = ''),
      paste('<defaultgrade>', points, '</defaultgrade>', sep = '')
    )

    ## multiple choice processing
    if(type == "multichoice") {
      xml <- c(xml,
        paste('<shuffleanswers>', if(shuffle) 'true' else 'false', '</shuffleanswers>', sep = ''),
        paste('<single>', if(x$metainfo$type == "schoice") 'true' else 'false', '</single>', sep = ''),
        paste('<answernumbering>', answernumbering, '</answernumbering>', sep = '')
      )

      n <- length(x$solutionlist)
      frac <- rep(0, n)
      frac[x$metainfo$solution] <- 100 / sum(x$metainfo$solution)
      for(i in 1:n) {
        xml <- c(
          xml,
          paste('<answer fraction="', frac[i], '" format="html">', sep = ''),
          '<text><![CDATA[<p>', x$questionlist[i], '</p>]]></text>',
          if(!is.null(x$solutionlist[i])) {
            c('<feedback format="html">',
            '<text><![CDATA[<p>', x$solutionlist[i], '</p>]]></text>',
            '</feedback>')
          } else NULL,
          '</answer>'
        )
      }
    }

    ## numeric question processing
    if(type == "numerical") {
      xml <- c(xml,
        '<answer fraction="100" format="moodle_auto_format">',
        paste('<text>', x$metainfo$solution, '</text>', sep = ''),
        paste('<tolerance>', x$metainfo$tolerance[1], '</tolerance>', sep = ''),
        '</answer>'
      )
    }

    ## string questions
    if(type == "shortanswer") {
      xml <- c(xml,
        paste('<usecase>', usecase * 1, '</usecase>', sep = ''),
        '<answer fraction="100" format="moodle_auto_format">',
        '<text>', x$metainfo$solution, '</text>',
        '</answer>'
      )
    }

    ## cloze type questions
    if(type == "cloze") {
      ## how many questions
      solution <- if(!is.list(x$metainfo$solution)) {
        list(x$metainfo$solution)
      } else x$metainfo$solution
      n <- length(solution)

      questionlist <- if(!is.list(x$questionlist)) {
        if(x$metainfo$type == "cloze") as.list(x$questionlist) else list(x$questionlist)
      } else x$questionlist
      if(length(questionlist) < 1) questionlist <- NULL

      ## split id for the questionlist
      sid <- unlist(sapply(1:n, function(i) rep(i, length(solution[[i]]))))

      ## tolerance of numerical questions
      tol <- if(!is.list(x$metainfo$tolerance)) {
        if(x$metainfo$type == "cloze") as.list(x$metainfo$tolerance) else list(x$metainfo$tolerance)
      } else x$metainfo$tolerance
      tol <- rep(tol, length.out = n)

      ## cycle through all questions
      qtext <- NULL
      for(i in 1:n) {
        ql <- questionlist[sid == i]
        k <- length(ql)
        if(grepl("choice", x$metainfo$clozetype[i])) {
          tmp <- paste('{', 1, ':', cloze_mchoice_display, ':', sep = '')
          if(k < 2) {
            tmp <- paste(ql, tmp)
            ql <- paste(if(solution[[i]][1]) c('%0%', '~%100%') else c('%100%', '~%0%'),
              truefalse, sep = '', collapse = '')
          } else {
            ql2 <- NULL
            for(j in 1:k) {
              p <- sum(solution[[i]])
              p <- if(p == 0) 0 else 1 /  p * 100
              ql2 <- paste(ql2, if(j > 1) '~' else NULL,
                if(solution[[i]][j]) paste('%',  p, '%', sep = '') else '%0%',
                ql[j], sep = '')
            }
            ql <- ql2
          }
          tmp <- paste(tmp, ql, sep = '')
          tmp <- paste(tmp, '}', sep = '')
          qtext <- c(qtext, tmp)
        }
        if(x$metainfo$clozetype[i] == "num") {
          for(j in 1:k) {
            qtext <- c(qtext, paste(ql[j], ' {', 1, ':NUMERICAL:=', solution[[i]][j],
              ':', tol[[i]][1], '}', sep = ''))
          }
        }
        if(x$metainfo$clozetype[i] == "string") {
          for(j in 1:k) {
            qtext <- c(qtext, paste(ql[j], ' {', 1, ':SHORTANSWER:%100%', solution[[i]][j],
              if(!usecase) paste('~%100%', tolower(solution[[i]][j]), sep = '') else NULL,
              '}', sep = ''))
          }
        }
      }
      if(enumerate) qtext <- c('<ol type = "a">', paste('<li>', qtext, '</li>'), '</ol>')
      qtext <- c('<pre>', x$question, qtext, '</pre>')
      xml <- gsub('##QuestionText', paste(qtext, collapse = "\n"), xml)
    }

    ## end the question
    xml <- c(xml, '</question>\n')

    ## path replacements
    xml <- gsub(paste(attr(x$supplements, "dir"), .Platform$file.sep, sep = ""), "", xml)

    xml
  }
}

