## generate exams in Moodle XML format
## http://docs.moodle.org/en/Moodle_XML_format
exams2moodle <- function(file, n = 1L, nsamp = NULL, dir = ".",
  name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE, rds = FALSE,
  resolution = 100, width = 4, height = 4, svg = FALSE, encoding = "UTF-8", 
  iname = TRUE, stitle = NULL, testid = FALSE, zip = FALSE,
  num = NULL, mchoice = NULL, schoice = mchoice, string = NULL, cloze = NULL,
  points = NULL, rule = NULL, pluginfile = TRUE, forcedownload = FALSE,
  converter = "pandoc-mathjax", envir = NULL,
  table = NULL, css = NULL, ...)
{
  ## default converter is "ttm" if all exercises are Rnw, otherwise "pandoc"
  if(is.null(converter)) {
    converter <- if(any(tolower(tools::file_ext(unlist(file))) == "rmd")) "pandoc" else "ttm"
  }
  ## set up .html transformer
  htmltransform <- make_exercise_transform_html(converter = converter, ..., base64 = !pluginfile)

  ## encoding always assumed to be UTF-8 starting from R/exams 2.4-0
  if(!is.null(encoding) && !(tolower(encoding) %in% c("", "utf-8", "utf8"))) {
    warning("the only supported 'encoding' is UTF-8")
  }
  encoding <- "UTF-8"

  ## change <table> class for custom CSS in Moodle
  if(!is.null(table)) {
    if(isTRUE(table)) table <- "table_shade"
    .exams_set_internal(pandoc_table_class_fixup = table)
    on.exit(.exams_set_internal(pandoc_table_class_fixup = FALSE))

    if(is.null(css) && table %in% c("table_grid", "table_rule", "table_shade")) {
      css <- readLines(system.file(file.path("css", "table.css"), package = "exams"))
    }
  }

  ## create a name
  if(is.null(name)) name <- "moodlequiz"
  if(isTRUE(rds)) rds <- name

  ## generate the exam
  exm <- xexams(file, n = n, nsamp = nsamp,
   driver = list(
       sweave = list(quiet = quiet, pdf = FALSE, png = !svg, svg = svg,
         resolution = resolution, width = width, height = height,
         encoding = encoding, envir = envir),
       read = NULL, transform = htmltransform, write = NULL),
     dir = dir, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose, rds = rds, points = points)

  ## get the possible moodle question body functions and options
  moodlequestion = list(num = num, mchoice = mchoice, schoice = schoice, cloze = cloze, string = string)

  for(i in c("num", "mchoice", "schoice", "cloze", "string")) {
    if(is.null(moodlequestion[[i]])) moodlequestion[[i]] <- list()
    if(is.list(moodlequestion[[i]])) {
      if(is.null(moodlequestion[[i]]$eval))
        moodlequestion[[i]]$eval <- list("partial" = TRUE, "rule" = rule)
      if(is.list(moodlequestion[[i]]$eval)) {
        if(!moodlequestion[[i]]$eval$partial) stop("Moodle can only process partial credits!")
      }
      if(is.null(moodlequestion[[i]]$css)) moodlequestion[[i]]$css <- css
      moodlequestion[[i]] <- do.call("make_question_moodle", moodlequestion[[i]])
    }
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
  dir.create(test_dir <- file.path(file_path_as_absolute(tdir), name))

  exsecs <- rep(NA, length = nq)
  if(!is.null(stitle)) {
    if((ks <- length(stitle)) > nq) stop("more section titles than exercises specified!")
    exsecs[1:ks] <- stitle
  }

  ## encoding (actually only UTF-8 supported starting from 2.4-0)
  enc <- gsub("-", "", tolower(encoding), fixed = TRUE)
  if(enc %in% c("iso8859", "iso88591")) enc <- "latin1"
  if(enc == "iso885915") enc <- "latin9"
  charset <- encoding
  if(enc == "utf8")
    charset <- "UTF-8"
  if(enc == "latin1")
    charset <- "ISO-8859-1"
  if(enc == "latin9")
    charset <- "ISO-8859-15"

  ## start the quiz .xml
  xml <- c(paste('<?xml version="1.0" encoding="', charset, '"?>', sep = ''), '<quiz>\n')

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

      ## add sample and question number to name
      sqname <- ""
      if(nx>1L) sqname <- paste("R", formatC(i, flag = "0", width = nchar(nx)), " ", sep = "")
      exm[[i]][[j]]$metainfo$name <-
          paste(sqname,
                "Q", formatC(j, flag = "0", width = nchar(nq)),
                " : ",
                if(!is.null(exm[[i]][[j]]$metainfo$title)) {
                    exm[[i]][[j]]$metainfo$title
                } else exm[[i]][[j]]$metainfo$file,
                sep="")
      ## create the .xml
      question_xml <- moodlequestion[[type]](exm[[i]][[j]])

      ## include supplements using base64 encoding, with either moodle's
      ## pluginfile mechanism or data URIs
      if(length(exm[[i]][[j]]$supplements) > 0) {
        for(si in seq_along(exm[[i]][[j]]$supplements)) {
	  f <- basename(exm[[i]][[j]]$supplements[si])
	  href <- paste0("\"", f,"\"")
          if(any(grepl(href, question_xml, fixed = TRUE))) {
            if(isTRUE(pluginfile)) {
              newfn   <- paste0("@@PLUGINFILE@@/", f, if(isTRUE(forcedownload)) "?forcedownload=1" else "")
              newhref <- paste0("\"", newfn,"\"")
              filetag <- paste0("<file name=\"", f, "\" encoding=\"base64\">",
                                base64enc::base64encode(exm[[i]][[j]]$supplements[si]),
                                "</file>")

              # Prepend @@PLUGINFILE@@ to link target
              question_xml <- gsub(href, newhref, question_xml, fixed = TRUE)

              # Insert base64 encoded file at the end of <questiontext>
              idx <- which(grepl(newhref, question_xml, fixed = TRUE))
              for(k in idx) {
                textend <- which(grepl("</text>", question_xml, fixed = TRUE))
                textend <- head(textend[textend > k], 1)
                question_xml <- append(question_xml, filetag, after = textend)
              }
            } else {
              question_xml <- gsub(href,
                paste0('"', fileURI(exm[[i]][[j]]$supplements[si]), '"'),
                question_xml, fixed = TRUE)
            }
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


## Moodle question constructor function (originally for Moodle 2.3)
make_question_moodle <-
make_question_moodle23 <- function(name = NULL, solution = TRUE, shuffle = FALSE, penalty = 0,
  answernumbering = "abc", usecase = FALSE, cloze_mchoice_display = NULL, cloze_schoice_display = NULL,
  truefalse = c("True", "False"), enumerate = TRUE, abstention = NULL,
  eval = list(partial = TRUE, negative = FALSE, rule = "false2"),
  essay = NULL, numwidth = NULL, stringwidth = NULL, css = NULL)
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

    if(type == "cloze") {
      if(length(x$metainfo$clozetype) < 3L) {
        if(all(x$metainfo$clozetype %in% c("essay", "file"))) {
          x$metainfo$stringtype <- x$metainfo$clozetype
          type <- "shortanswer"
        }
      }
    }

    if(type == "shortanswer" && (isTRUE(x$metainfo$essay) || isTRUE(essay))) {
        type <- "essay"
    }
    if(type == "shortanswer") {
      if(!is.null(x$metainfo$stringtype)) {
        if(any(grepl("essay", x$metainfo$stringtype)) | any(grepl("file", x$metainfo$stringtype)))
          type <- "essay"
      }
    }

    ## question name
    if(is.null(name)) name <- x$metainfo$name

    ## extra abstention option
    if(is.null(abstention)) abstention <- x$metainfo$abstention
    if(is.null(abstention) || identical(abstention, FALSE)) abstention <- ""
    if(isTRUE(abstention)) abstention <- "Abstention"

    ## read CSS files
    if(!is.null(css) && all(tools::file_ext(css) == "css")) {
      css_inst <- system.file(file.path("css", css), package = "exams")
      css <- ifelse(!file.exists(css) & file.exists(css_inst), css_inst, css)
      if(!all(file.exists(css))) stop(paste("The following CSS files cannot be found: ",
        paste(css[!file.exists(css)], collapse = ", "), ".", sep = ""))
      css <- do.call("cbind", lapply(css, readLines))
    }
    ## turn into <style> tag
    if(!is.null(css) && !grepl("<style", css[1L], fixed = TRUE)) {
      css <- c('<style type="text/css" rel="stylesheet">', css, '</style>')
    }

    ## start the question xml
    xml <- c(
      paste('\n<question type="', type, '">', sep = ''),
      '<name>',
      paste('<text>', name, '</text>'),
      '</name>',
      '<questiontext format="html">',
      '<text><![CDATA[',
      css,
      '<p>',
      if(type != "cloze") x$question else '##QuestionText##',
      '</p>]]></text>',
      '</questiontext>'
    )

    ## insert the solution
    if((length(x$solution) | (nsol <- length(x$solutionlist))) && solution) {
      xml <- c(xml,
        '<generalfeedback format="html">',
        '<text><![CDATA[<p>', x$solution,
        if(!type %in% c("mchoice", "schoice") && nsol) {
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
    if((length(points) == 1) & (type == "cloze")) {
      #points <- points / length(x$metainfo$solution)
      points <- rep(points, length = length(x$metainfo$solution))
    }

    points2 <- points * as.integer(paste0(c(1, rep(0, max(sapply(points, dc)))), collapse = ""))

    xml <- c(xml,
      paste('<penalty>', penalty, '</penalty>', sep = ''),
      paste('<defaultgrade>', sum(points), '</defaultgrade>', sep = '')
    )

    ## multiple choice processing
    if(type == "multichoice") {
      xml <- c(xml,
        paste('<shuffleanswers>', if(shuffle) 'true' else 'false', '</shuffleanswers>', sep = ''),
        paste('<single>', if(x$metainfo$type == "schoice") 'true' else 'false', '</single>', sep = ''),
        paste('<answernumbering>', answernumbering, '</answernumbering>', sep = '')
      )

      ## evaluation policy
      if(!("pointvec" %in% names(eval))) eval <- do.call("exams_eval", eval)
      frac <- as.integer(x$metainfo$solution)
      pv <- eval$pointvec(paste(frac, sep = "", collapse = ""))
      pv[pv == -Inf] <- 0 ## FIXME: exams_eval() return -Inf when rule = "none"?

      frac[x$metainfo$solution] <- pv["pos"]
      frac[!x$metainfo$solution] <- pv["neg"]
      frac <- moodlePercent(frac)

      for(i in seq_along(x$questionlist)) {
        xml <- c(
          xml,
          paste('<answer fraction="', frac[i], '" format="html">', sep = ''),
          '<text><![CDATA[<p>', x$questionlist[i], '</p>]]></text>',
          if(length(x$solutionlist)) {
            c('<feedback format="html">',
            '<text><![CDATA[<p>', x$solutionlist[i], '</p>]]></text>',
            '</feedback>')
          } else NULL,
          '</answer>'
        )
      }

      ## add abstention option (if any)
      if(abstention != "") {
        xml <- c(xml,
          '<answer fraction="0" format="html">',
          '<text><![CDATA[<p>',
          abstention,
          '</p>]]></text>',
          '</answer>'
        )
      }
    }

    ## numeric question processing
    if(type == "numerical") {
      xml <- c(xml,
        '<answer fraction="100" format="moodle_auto_format">',
        paste('<text>', x$metainfo$solution, '</text>', sep = ''),
        paste('<tolerance>', max(x$metainfo$tolerance), '</tolerance>', sep = ''),
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

    ## string/essay questions
    if(type == "essay") {
        essay_opts <- list(format="plain", required=TRUE, fieldlines=5L,
            attachments=0L, attachmentsrequired=FALSE)

        if(!is.null(x$metainfo$stringtype)) {
          if(any(grepl("file", x$metainfo$stringtype))) {
            essay_opts$attachments <- 1L
            if(length(x$metainfo$stringtype) == 1L)
              essay_opts$fieldlines <- 0L
          }
        }

        if(!is.list(essay)) {
          essay <- list()
        }

        for(i in names(essay_opts)) {
          vn <- paste0("essay_", i)
          value <- x$metainfo[[vn]]
          if(!is.null(value)) {
            essay_opts[[i]] <- value
          }
        }

        i <- grep("essay_", names(x$metainfo), fixed = TRUE, value = TRUE)
        if(length(i)) {
          for(j in i) {
            jn <- gsub("essay_", "", j, fixed = TRUE)
            essay_opts[[jn]] <- x$metainfo[[j]]
          }
        }

        for(i in names(essay_opts)) {
          if(!is.null(essay[[i]])) essay_opts[[i]] <- essay[[i]]
        }

        if((essay_opts$fieldlines < 1L) | !essay_opts$required) {
          essay_opts$required <- FALSE
          essay_opts$format <- "noinline"
          essay_opts$attachmentsrequired <- TRUE
          if(essay_opts$attachments < 1L) {
            essay_opts$attachments <- 1L
          }
        }

        txt <- paste0(
          "<responseformat>", essay_opts$format, "</responseformat>\n",
          "<responserequired>", as.integer(essay_opts$required), "</responserequired>\n",
          "<responsefieldlines>", essay_opts$fieldlines, "</responsefieldlines>\n",
          "<attachments>", essay_opts$attachments, "</attachments>\n",
          "<attachmentsrequired>", as.integer(essay_opts$attachmentsrequired), "</attachmentsrequired>\n"
        )

        if(!is.null(essay_opts$wordlimit)) {
          if(length(essay_opts$wordlimit) < 2) {
            txt <- c(txt, paste0('<minwordlimit></minwordlimit>\n<maxwordlimit>',
              essay_opts$wordlimit[1L], '</maxwordlimit>'))
          } else {
            txt <- c(txt, paste0('<minwordlimit>', essay_opts$wordlimit[1L],
              '</minwordlimit>\n<maxwordlimit>', essay_opts$wordlimit[2L], '</maxwordlimit>'))
          }
        }

        xml <- c(xml, txt)
    }


    ## cloze type questions
    if(type == "cloze") {
      if(!all(points2 == points)) {
        if(any((points %% 1) > 0))
          warning("non-integer points, please check points in Moodle")
      }

      ## how many questions
      solution <- if(!is.list(x$metainfo$solution)) {
        list(x$metainfo$solution)
      } else x$metainfo$solution
      n <- length(solution)

      xml[grep('<defaultgrade>', xml, fixed = TRUE)] <- paste('<defaultgrade>', sum(points),
        '</defaultgrade>', sep = '')

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

      ## optionally fix the num answer field width
      ## by supplying an additional wrong answer
      if(is.null(numwidth)) numwidth <- x$metainfo$numwidth
      if(is.null(numwidth)) numwidth <- FALSE
      numcloze <- x$metainfo$clozetype == "num"
      if(!identical(numwidth, FALSE) && any(numcloze)) {
        ## all correct numeric solutions
        nums <- unlist(x$metainfo$solution[numcloze])
	## +/- corresponding tolerance
	nums <- cbind(nums - unlist(tol[numcloze]), nums + unlist(tol[numcloze]))

        ## formatted number (of a wrong solution to enforce the width)
        fnum <- if(is.logical(numwidth)) {
          gsub(" ", "", format(as.numeric(nums)), fixed = TRUE)
        } else if(is.character(numwidth)) {
	  numwidth
	} else {
          paste(rep.int("9", as.integer(numwidth)), collapse = "")
        }
	
	## make sure that the formatted number is not within tolerance of any correct solution
        fnum <- fnum[which.max(nchar(fnum))]
	num_w <- nchar(fnum)
	while(any(as.numeric(fnum) == nums) || any(as.numeric(fnum) >= nums[, 1] & as.numeric(fnum) <= nums[, 2])) {
	  fnum <- make_id(num_w)
	}
      }
      
      ## analogously fix the string width
      ## by supplying an additional wrong answer
      if(is.null(stringwidth)) stringwidth <- x$metainfo$stringwidth
      if(is.null(stringwidth)) stringwidth <- FALSE
      stringcloze <- x$metainfo$clozetype == "string"
      if(!identical(stringwidth, FALSE) && any(stringcloze)) {
        strings <- unlist(x$metainfo$solution[stringcloze])
	fstring <- if(is.logical(stringwidth)) {
	  strings[which.max(nchar(strings))]
	} else if(is.character(stringwidth)) {
	  stringwidth
	} else {
	  paste(rep.int("9", as.integer(stringwidth)), collapse = "")
	}	
	string_w <- nchar(fstring)
	while(any(tolower(fstring) == tolower(strings))) {
	  fstring <- paste(sample(base::LETTERS, string_w, replace = TRUE), collapse = "")
	}
      }

      if(any(x$metainfo$clozetype %in% c("essay", "file")))
        stop("essays or file uploads are not currently supported in Moodle cloze type exercises!")

      ## cycle through all questions
      qtext <- NULL; inum <- 1
      for(i in 1:n) {
        ql <- if(is.null(questionlist)) "" else questionlist[sid == i]
        k <- length(ql)
        tmp <- NULL
        if(grepl("choice", x$metainfo$clozetype[i])) {
          if(any(grepl("}", ql, fixed = TRUE))) {
            ## due to "{1:TYPE:...}" markup in cloze questions it is necessary
            ## to escape closing curly brackets in the questionlist
            ql <- gsub("}", "\\}", ql, fixed = TRUE)
          }

          ## Moodle multiple-choice and single-choice displays
          moodle_schoice_display <- c("MULTICHOICE", "MC", "MULTICHOICE_V", "MCV", "MULTICHOICE_H", "MCH",
	    "MULTICHOICE_S", "MCS", "MULTICHOICE_VS", "MCVS", "MULTICHOICE_HS", "MCHS")
          moodle_mchoice_display <- c("MULTIRESPONSE", "MR", "MULTIRESPONSE_H", "MRH", "MULTIRESPONSE_S", "MRS", "MULTIRESPONSE_HS", "MRHS")
          ## select display type, defaults:
	  ## - MULTIRESPONSE for mchoice items
	  ## - MULTICHOICE_V for schoice items with math markup \(...\) which isn't supported in drop-down menus
	  ## - MULTICHOICE for all other schoice items
          if(x$metainfo$clozetype[i] == "mchoice") {
	    cloze_mchoice_display_i <- if(is.null(cloze_mchoice_display)) "MULTIRESPONSE" else cloze_mchoice_display
	    if(cloze_mchoice_display_i %in% moodle_schoice_display) {
	      warning("MULTICHOICE-type displays should not be used for mchoice items, maybe it was intended to specify 'cloze_schoice_display'?")
	    }
	  } else {
	    ## try to catch old-style cloze_mchoice_display for schoice elements
	    if(x$metainfo$clozetype[i] == "schoice" &&
	       is.null(cloze_schoice_display) &&
	       !is.null(cloze_mchoice_display) &&
	       cloze_mchoice_display %in% moodle_schoice_display) {
	      warning("MULTICHOICE-type display was specified in 'cloze_mchoice_display' rather than 'cloze_schoice_display'")
	      cloze_schoice_display <- cloze_mchoice_display
	    }
	    cloze_mchoice_display_i <- if(!is.null(cloze_schoice_display)) {
	      cloze_schoice_display
	    } else if(any(grepl("\\(", ql, fixed = TRUE) & grepl("\\)", ql, fixed = TRUE))) {
	      "MULTICHOICE_V"
	    } else {
	      "MULTICHOICE"
	    }
	  }
	  ## FIXME: Warn if the selected display option cannot work? (e.g., mchoice or math?)
          tmp <- paste('{', points2[i], ':', cloze_mchoice_display_i, ':', sep = '')

          ## set up Moodle percent fractions for correct and incorrect items
	  ## -> use "=" instead of "%...%" for correct items if they sum up to 100%
          frac <- solution[[i]]
          if(length(frac) < 2) frac <- c(frac, !frac)
          frac2 <- frac
	  eval_i <- eval
          if(!("pointvec" %in% names(eval_i))) {
	    if(is.null(eval_i$rule)) eval_i$rule <- "none"
            eval_i <- do.call("exams_eval", eval_i)
          }
          pv <- eval_i$pointvec(frac) ## FIXME: this passes correct as a logical rather as a character, is this intended?
          frac[frac2] <- pv["pos"]
          frac[!frac2] <- pv["neg"]
          p <- moodlePercent(frac)
          if(isTRUE(all.equal(100, sum(as.numeric(p[frac2])), tol = 1e-5))) {
	    p <- paste0("%", p, "%")
            p[frac2] <- "="
	    if(all(p[!frac2] == "%0%")) p[!frac2] <- ""
	  } else {
	    p <- paste0("%", p, "%")
	  }

          if(k < 2) {
            tmp <- paste(ql, tmp)
            p[2] <- paste0('~', p[2])
            ql <- paste0(p, truefalse[rev(frac2 + 1)], collapse = '')
          } else {
            ql2 <- NULL
            for(j in 1:k)
              ql2 <- paste0(ql2, if(j > 1) '~' else NULL, p[j], ql[j])
            ql <- ql2
          }
          tmp <- paste0(tmp, ql)
          tmp <- paste0(tmp, '}')
        }
        if(x$metainfo$clozetype[i] == "num") {
          for(j in 1:k) {
            tmp <- c(tmp, paste0(ql[j], ' {', points2[i], ':NUMERICAL:=', solution[[i]][j],
              ':', tol[[i]][j],
	      if(!identical(numwidth, FALSE)) paste0('~%0%', fnum, ":0") else NULL,
              '}'))
          }
        }
        if(x$metainfo$clozetype[i] == "string") {
          for(j in 1:k) {
            tmp <- c(tmp, paste0(ql[j], ' {', points2[i], ':SHORTANSWER:%100%', gsub("}", "\\}", solution[[i]][j], fixed = TRUE),
              if(!usecase && tolower(solution[[i]][j]) != solution[[i]][j]) paste0('~%100%', tolower(gsub("}", "\\}", solution[[i]][j], fixed = TRUE))) else NULL,
	      if(!identical(stringwidth, FALSE)) paste0('~%0%', fstring) else NULL,
              '}'))
          }
        }
        if(x$metainfo$clozetype[i] == "verbatim") {
          for(j in 1:k) {
            tmp <- c(tmp, paste0(ql[j], ' {', points2[i], solution[[i]][j], '}'))
          }
        }

        ## FIXME, there is a NULL when using boxhist2?
        tmp <- gsub('NULL', '', tmp, fixed = TRUE)

        ## insert in ##ANSWERi## tag
        if(any(grepl(ai <- paste("##ANSWER", i, "##", sep = ""), x$question, fixed = TRUE))) {
          x$question <- gsub(ai, paste(tmp, collapse = ", "), x$question, fixed = TRUE)
        } else qtext <- c(qtext, tmp)
      }
      if(!is.null(qtext) & enumerate)
        qtext <- c('<ol type = "a">', paste('<li>', qtext, '</li>'), '</ol>')
      qtext <- c(x$question, qtext)
      ## add abstention option (if any)
#      if(abstention != "") {
#        qtext <- c(qtext,
#          paste0('<p>', abstention, ' {0:', cloze_mchoice_display_i, ':%100%', truefalse[1], '~%0%', truefalse[2], '} </p>')
#        )
#      }
      xml <- gsub('##QuestionText##', paste(qtext, collapse = "\n"), xml, fixed = TRUE)
    }

    ## end the question
    xml <- c(xml, '</question>\n')

    ## path replacements
    xml <- gsub(paste(attr(x$supplements, "dir"), .Platform$file.sep, sep = ""), "", xml, fixed = TRUE)

    xml
  }
}


## "Numbers" Moodle currently accepts as fraction value
## for mchoice items
moodleFractions <- c(100,90,83.33333,80,75,70,
                     66.66667,60,50,40,
                     33.33333,30,25,20,16.66667,
                     14.28571, 12.5,11.11111, 10,5)

## Convert a number in [0, 1] to one of the percentages
## above if the difference is less then 1
moodlePercent <- function(p)
{
  p <- 100 * p
  z <- abs(outer(abs(p), moodleFractions, "-"))
  mp <- moodleFractions[max.col(-z)] * sign(p)
  if(any(abs(mp - p) > 1))
    stop("Percentage not in list of moodle fractions")
  as.character(mp)
}

## Count decimal places.
dc <- function(x) {
  if((x %% 1) != 0) {
    strs <- strsplit(as.character(format(x, scientific = F)), "\\.")
    n <- nchar(strs[[1]][2])
  } else {
    n <- 0
  }
  return(n) 
}

