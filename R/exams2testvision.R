## create TestVision .xml files which are based on IMS QTI 2.1
## specifications and examples available at:
## http://www.imsglobal.org/question/#version2.0
## https://www.ibm.com/developerworks/library/x-qti/
## https://www.onyx-editor.de/
## http://membervalidator.imsglobal.org/qti/
## https://webapps.ph.ed.ac.uk/qtiworks/anonymous/validator
## http://www.imsglobal.org/question/qtiv2p1/imsqti_implv2p1.html
## http://www.imsglobal.org/question/index.html#version2.0
## https://www.w3.org/Math/XMLSchema/
exams2testvision <- function(file, n = 1L, nsamp = NULL, dir = ".",
  name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE,
  resolution = 100, width = 4, height = 4, svg = FALSE, encoding  = "UTF-8",
  envir = NULL, engine = NULL,
  num = NULL, mchoice = NULL, schoice = mchoice, string = NULL, cloze = NULL,
  template = "testvision",
  stitle = "Exercise", ititle = "Question",
  adescription = "Please solve the following exercises.",
  sdescription = "Please answer the following question.",
  maxattempts = 1,  solutionswitch = TRUE,
  zip = TRUE, points = NULL,
  eval = list(partial = TRUE, rule = "false2", negative = FALSE),
  converter = "pandoc", base64 = FALSE, mode = "hex", ...)
{
  ## default converter is "ttm" if all exercises are Rnw, otherwise "pandoc"
  if(is.null(converter)) {
    converter <- if(any(tolower(tools::file_ext(unlist(file))) == "rmd")) "pandoc" else "ttm"
  }
  ## set up .html transformer
  htmltransform <- if(converter %in% c("tth", "ttm")) {
    make_exercise_transform_html(converter = converter, ..., base64 = base64, mode = mode)
  } else {
    make_exercise_transform_html(converter = converter, ..., base64 = base64)
  }

  ## generate the exam
  is.xexam <- FALSE
  if(is.list(file)) {
    if(any(grepl("exam1", names(file))))
      is.xexam <- TRUE
  }
  if(!is.xexam) {
    exm <- xexams(file, n = n, nsamp = nsamp,
      driver = list(
        sweave = list(quiet = quiet, pdf = FALSE, png = !svg, svg = svg,
          resolution = resolution, width = width, height = height,
          encoding = encoding, envir = envir, engine = engine),
        read = NULL, transform = htmltransform, write = NULL),
      dir = dir, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose)
  } else {
    exm <- file
    rm(file)
  }

  ## start .xml assessement creation
  ## get the possible item body functions and options
  itembody <- list(num = num, mchoice = mchoice, schoice = schoice, cloze = cloze, string = string)

  for(i in c("num", "mchoice", "schoice", "cloze", "string")) {
    if(is.null(itembody[[i]])) itembody[[i]] <- list()
    if(is.list(itembody[[i]])) {
      if(is.null(itembody[[i]]$eval)) itembody[[i]]$eval <- eval
      ## if(i == "cloze" && is.null(itembody[[i]]$eval$rule)) itembody[[i]]$eval$rule <- "none"
      itembody[[i]]$solutionswitch <- solutionswitch
      itembody[[i]] <- do.call("make_itembody_testvision", itembody[[i]])
    }
    if(!is.function(itembody[[i]])) stop(sprintf("wrong specification of %s", sQuote(i)))
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

  ## the package directory
  pkg_dir <- find.package("exams")

  ## get the .xml template
  template <- path.expand(template)
  template <- ifelse(
    tolower(substr(template, nchar(template) - 3L, nchar(template))) != ".xml",
    paste(template, ".xml", sep = ""), template)
  template <- ifelse(file.exists(template),
    template, file.path(pkg_dir, "xml", basename(template)))
  if(!all(file.exists(template))) {
    stop(paste("The following files cannot be found: ",
      paste(basename(template)[!file.exists(template)], collapse = ", "), ".", sep = ""))
  }
  xml <- readLines(template[1L])

  ## check template for all necessary tags
  ## extract the template for the assessement, sections and items
#  if(length(start <- grep("<assessmentTest", xml, fixed = TRUE)) != 1L ||
#    length(end <- grep("</assessmentTest>", xml, fixed = TRUE)) != 1L) {
#    stop(paste("The XML template", template,
#      "must contain exactly one opening and closing <assessmentTest> tag!"))
#  }
#  assessment_xml <- xml[start:end]
#
#  if(length(start <- grep("<assessmentSection", xml, fixed = TRUE)) != 1L ||
#    length(end <- grep("</assessmentSection>", xml, fixed = TRUE)) != 1L) {
#    stop(paste("The XML template", template,
#      "must contain exactly one opening and closing <assessmentSection> tag!"))
#  }
#  section_xml <- xml[start:end]

  if(length(start <- grep("<imscp:manifest", xml, fixed = TRUE)) != 1L ||
    length(end <- grep("</imscp:manifest>", xml, fixed = TRUE)) != 1L) {
    stop(paste("The XML template", template,
      "must contain exactly one opening and closing <imscp:manifest> tag!"))
  }
  manifest_xml <- xml[(start - 1L):end]

  if(length(start <- grep("<imscp:resource ", xml, fixed = TRUE)) != 1L ||
    length(end <- grep("</imscp:resource>", xml, fixed = TRUE)) != 1L) {
    stop(paste("The XML template", template,
      "must contain exactly one opening and closing <imscp:resource> tag!"))
  }
  resource_xml <- xml[start:end]

  ## obtain the number of exams and questions
  nx <- length(exm)
  nq <- if(!is.xexam) length(exm[[1L]]) else length(exm)

  ## create a name
  if(is.null(name))
    name <- file_path_sans_ext(basename(template))
  name <- gsub("\\s", "_", name)
  name_base <- if(is_number1(name)) paste0("_", name) else name

  ## function for internal ids
  make_test_ids <- function(n, type = c("test", "section", "item"))
  {
    switch(type,
      "test" = paste(name_base, format(Sys.time(), "%Y%m%d%H%M"), sep = "_"),
      paste(type, formatC(1:n, flag = "0", width = nchar(n)), sep = "_")
    )
  }

  ## generate the test id
  test_id <- make_test_ids(type = "test")

  ## create section ids
  sec_ids <- paste(test_id, make_test_ids(nq, type = "section"), sep = "_")

  ## create section/item titles and section description
  if(is.null(stitle)) stitle <- ""
  stitle <- rep(stitle, length.out = nq)
  if(!is.null(ititle)) ititle <- rep(ititle, length.out = nq)
  if(is.null(adescription)) adescription <- ""
  if(is.null(sdescription) || identical(sdescription, FALSE)) sdescription <- ""
  sdescription <- rep(sdescription, length.out = nq)
  sdescription[sdescription != ""] <- sprintf(
    '<rubricBlock view="candidate"><p>%s</p></rubricBlock>',
    sdescription[sdescription != ""]
  )

  ## points setting
  if(!is.null(points))
    points <- rep(points, length.out = nq)

  ## create the directory where the test is stored
  dir.create(test_dir <- file.path(tdir, name))

  tvo_interactionType <- function(x, item = FALSE) {
    type <- switch(x,
      "mchoice" = "choiceInteraction",
      "schoice" = "choiceInteraction",
      "num" = "extendedTextinteraction",
      "cloze" = "div",
      "string" = "extendedTextinteraction"
    )
    type
  }

  ## cycle through all exams and questions
  ## similar questions are combined in a section,
  ## questions are then sampled from the sections




  items <- items_R <- NULL
  maxscore <- 0
  for(j in 1:nq) {
    ## first, create the section header
#    sec_xml <- c(sec_xml, gsub("##SectionId##", sec_ids[j], section_xml, fixed = TRUE))
#
#    ## insert a section title -> exm[[1]][[j]]$metainfo$name?
#    sec_xml <- gsub("##SectionTitle##", stitle[j], sec_xml, fixed = TRUE)
#
#    ## insert a section description -> exm[[1]][[j]]$metainfo$description?
#    sec_xml <- gsub("##SectionDescription##", sdescription[j], sec_xml, fixed = TRUE)

    ## special handler
    if(is.xexam) nx <- length(exm[[j]])

    ## create item ids
    if(nx == 1)
      item_ids <- paste(test_id, "item", formatC(j, flag = "0", width = nchar(nq)), sep = "_") else
        item_ids <- paste(sec_ids[j], make_test_ids(nx, type = "item"), sep = "_")

    ## now, insert the questions
    for(i in 1:nx) {
      ## special handling of indices
      if(is.xexam) {
        if(i < 2)
          jk <- j
        j <- i
        i <- jk
      }

      ## overule points
      if(!is.null(points)) exm[[i]][[j]]$metainfo$points <- points[[j]]
      if(i < 2) {
        tpts <- if(is.null(exm[[i]][[j]]$metainfo$points)) 1 else exm[[i]][[j]]$metainfo$points
        maxscore <- maxscore + sum(tpts)
      }

      ## get and insert the item body
      type <- exm[[i]][[j]]$metainfo$type

      ## create an id
      iname <- paste(item_ids[if(is.xexam) j else i], type, sep = "_")

      ## attach item id to metainfo
      exm[[i]][[j]]$metainfo$id <- iname

      ## overrule item name
#      if(!is.null(ititle))
#        exm[[i]][[j]]$metainfo$name <- ititle[j]
      ititle <- exm[[i]][[j]]$metainfo$name

      ## switch for debugging
      if(FALSE) {
        exm[[i]][[j]]$question <- "Here is the questiontext..."
        exm[[i]][[j]]$solution <- "This is the solutiontext..."
        exm[[i]][[j]]$solutionlist <- NA
      }

      exm[[i]][[j]]$converter <- converter

      ibody <- fix_tvo_img(itembody[[type]](exm[[i]][[j]]))
      ydobi <- fix_tvo_table(ibody)
      ibody <- ydobi[[1]]
      css <- ydobi[[2]]

      exm[[i]][[j]]$converter <- NULL

      ## copy supplements
      sec_items_R <- NULL
      if(length(exm[[i]][[j]]$supplements)) {
        if(!base64) {
          if(!file.exists(media_dir <- file.path(test_dir, "mediafiles")))
            dir.create(media_dir)
          sj <- 1
          while(file.exists(file.path(media_dir, sup_dir <- paste("supplements", sj, sep = "")))) {
            sj <- sj + 1
          }
          dir.create(ms_dir <- file.path(media_dir, sup_dir))
        }
        for(si in seq_along(exm[[i]][[j]]$supplements)) {
          f <- basename(exm[[i]][[j]]$supplements[si])
          if(base64) {
            replacement <- fileURI(exm[[i]][[j]]$supplements[si])

            if(any(grepl(dirname(exm[[i]][[j]]$supplements[si]), ibody))) {
              ibody <- gsub(dirname(exm[[i]][[j]]$supplements[si]),
                replacement, ibody, fixed = TRUE)
            } else {
              if(any(grepl(f, ibody))) {
                ibody <- gsub(paste(f, '"', sep = ''),
                  paste(replacement, '"', sep = ''), ibody, fixed = TRUE)
              }
            }
          } else {
            file.copy(exm[[i]][[j]]$supplements[si],
              file.path(ms_dir, f))

            fid <- gsub('\\', '', gsub('/', '_', file.path('mediafiles', sup_dir, f), fixed = TRUE), fixed = TRUE)
            fhref <- file.path('mediafiles', sup_dir, f)
            sec_items_R <- c(sec_items_R,
              paste('<imscp:file href="', fhref, '"/>', sep = '')
            )

            if(any(grepl(dirname(exm[[i]][[j]]$supplements[si]), ibody))) {
              ibody <- gsub(dirname(exm[[i]][[j]]$supplements[si]),
                file.path('mediafiles', sup_dir), ibody, fixed = TRUE)
            } else {
              if(any(grepl(f, ibody))) {
                ibody <- gsub(paste(f, '"', sep = ''),
                  paste('mediafiles/', sup_dir, '/', f, '"', sep = ''), ibody, fixed = TRUE)
              }
            }
          }
        }
      }

      ## copy css file
      if(!file.exists(css_dir <- file.path(test_dir, "css"))) dir.create(css_dir)
      writeLines(as.character(css), file.path(css_dir, paste0(iname, ".css")))
      ibody <- sub("007007007007007007007", iname, ibody)


      ## write the item xml to file
      writeLines(c('<?xml version="1.0" encoding="utf-8" standalone="yes"?>', ibody),
        file.path(test_dir, paste(iname, "xml", sep = ".")))

      ## include resource
      sec_items_R <- c(paste('<imscp:file href="', iname,'.xml" />', sep=""),
                       paste('<imscp:file href="css/', iname,'.css" />', sep=""), sec_items_R)

      res_xml <- resource_xml
      res_xml <- gsub('##ItemId##', iname, res_xml, fixed = TRUE)
      res_xml <- gsub('##ItemTitle##', paste(iname, ititle, sep ='_'), res_xml, fixed = TRUE)
      res_xml <- gsub('##QuestionType##', tvo_interactionType(type), res_xml, fixed = TRUE)
      res_xml <- gsub('##FileRefs##', paste(sec_items_R, collapse = '\n'), res_xml, fixed = TRUE)

      items_R <- c(items_R, res_xml)
    }
  }

  manifest_xml <- gsub('##AssessmentId##', test_id, manifest_xml, fixed = TRUE)
  manifest_xml <- gsub('##ManifestItemResources##', paste(items_R, collapse = '\n'), manifest_xml, fixed = TRUE)


  ## write xmls to dir
  writeLines(manifest_xml, file.path(test_dir, "imsmanifest.xml"))

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



make_itembody_testvision <- function(shuffle = FALSE,
  defaultval = NULL, minvalue = NULL, maxvalue = NULL, enumerate = FALSE,
  digits = NULL, tolerance = is.null(digits), maxchars = 12,
  eval = list(partial = TRUE, rule = "false2", negative = FALSE), solutionswitch = TRUE)
{
  function(x) {
    ## how many points?
    points <- if(is.null(x$metainfo$points)) 1 else x$metainfo$points

#    dopbl <- x$converter %in% c("ttm", "tth")

    ## how many questions
    solution <- if(!is.list(x$metainfo$solution)) {
      list(x$metainfo$solution)
    } else x$metainfo$solution
    n <- length(solution)

    questionlist <- if(!is.list(x$questionlist)) {
      if(x$metainfo$type == "cloze") {
        g <- rep(seq_along(x$metainfo$solution), sapply(x$metainfo$solution, length))
        split(x$questionlist, g)
      } else list(x$questionlist)
    } else x$questionlist
    if(length(questionlist) < 1) questionlist <- NULL
    for(i in 1:length(questionlist)) {
      if(length(questionlist[[i]]) < 1)
        questionlist[[i]] <- NA
    }

    tol <- if(!is.list(x$metainfo$tolerance)) {
      if(x$metainfo$type == "cloze") as.list(x$metainfo$tolerance) else list(x$metainfo$tolerance)
    } else x$metainfo$tolerance
    tol <- rep(tol, length.out = n)

    if((length(points) == 1) & (x$metainfo$type == "cloze"))
      points <- points / n

    q_points <- rep(points, length.out = n)
    if(x$metainfo$type == "cloze")
      points <- sum(q_points)

    ## set question type(s)
    type <- x$metainfo$type
    type <- if(type == "cloze") x$metainfo$clozetype else rep(type, length.out = n)

    ## evaluation policy
    if(is.null(eval) || length(eval) < 1L) eval <- exams_eval()
    if(!is.list(eval)) stop("'eval' needs to specify a list of partial/negative/rule")
    eval <- eval[match(c("partial", "negative", "rule"), names(eval), nomatch = 0)]
    if(x$metainfo$type %in% c("num", "string")) eval$partial <- FALSE
    ## if(x$metainfo$type == "cloze" && is.null(eval$rule)) eval$rule <- "none"
    eval <- do.call("exams_eval", eval) ## always re-call exams_eval

    ## character fields
    maxchars <- if(is.null(x$metainfo$maxchars)) {
        if(length(maxchars) < 2) {
           c(maxchars, NA, NA)
        } else maxchars[1:3]
    } else x$metainfo$maxchars
    if(!is.list(maxchars))
      maxchars <- list(maxchars)
    maxchars <- rep(maxchars, length.out = n)
    for(j in seq_along(maxchars)) {
      if(length(maxchars[[j]]) < 2)
        maxchars[[j]] <- c(maxchars[[j]], NA, NA)
    }

    ## start item presentation
    ## and insert question
    xml <- paste('<assessmentItem identifier="', x$metainfo$id, '" title="', paste(x$metainfo$id, x$metainfo$name, sep = '_'), '" adaptive="false" timeDependent="false" toolName="Testvision Online" toolVersion="39.0.9084" xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1">', sep = '')

    ## cycle trough all questions
    ids <- pv <- mv <- list()
    for(i in 1:n) {
      ## evaluate points for each question
      pv[[i]] <- eval$pointvec(solution[[i]])
      pv[[i]]["pos"] <- pv[[i]]["pos"] * q_points[i]
      pv[[i]]["neg"] <- pv[[i]]["neg"] * q_points[i]
      mv[[i]] <- pv[[i]]["neg"]
    }

    mmatrix <- if(length(i <- grep("matrix", names(x$metainfo)))) {
      x$metainfo[[i]]
    } else NULL

    ## extract solution.
    msol <- x$metainfo$solution
    if(!is.list(msol))
      msol <- list(msol)

    is_essay <- rep(FALSE, n)

    for(i in 1:n) {
      ## get item id
      iid <- x$metainfo$id

      ## generate ids
      if(is.null(mmatrix)) {
#       ids[[i]] <- list("response" = paste(iid, "RESPONSE", make_id(7), sep = "_"),
        ids[[i]] <- list("response" = "RESPONSE",
          "questions" = paste("alt", make_id(6, length(msol[[i]])), sep = "_"), "idcs" = paste("alt", make_id(4), sep = "_"))
      } else {
        qs <- strsplit(x$questionlist, mmatrix, fixed = TRUE)
        mrows <- unique(sapply(qs, function(x) { x[1] }))
        mcols <- unique(sapply(qs, function(x) { x[2] }))
#       ids[[i]] <- list("response" = paste(iid, "RESPONSE", make_id(7), sep = "_"),
        ids[[i]] <- list("response" = "RESPONSE",
          "questions" = paste("alt", make_id(6, length(msol[[i]])), sep = "_"), "idcs" = paste("alt", make_id(4), sep = "_"),
          "mmatrix_matches" = matrix(msol[[i]], nrow = length(mrows), byrow = TRUE)
        )
        ids[[i]]$mmatrix_questions <- list(
          "rows" = paste(iid, make_id(10, length(mrows)), sep = "_"),
          "cols" = paste(iid, make_id(10, length(mcols)), sep = "_")
        )
        rownames(ids[[i]]$mmatrix_matches) <- mrows
        colnames(ids[[i]]$mmatrix_matches) <- mcols
        for(j in seq_along(ids[[i]]$mmatrix_questions$rows)) {
          for(jj in seq_along(ids[[i]]$mmatrix_questions$cols)) {
            ids[[i]]$mmatrix_pairs <- c(ids[[i]]$mmatrix_pairs, paste(ids[[i]]$mmatrix_questions$rows[j], ids[[i]]$mmatrix_questions$cols[jj]))
          }
        }
      }

      ## first iterate through non-cloze items
      if(x$metainfo$type != "cloze"){
       ## insert choice type responses
       if(length(grep("choice", type[i]))) {
         xml <- c(xml,
           paste('<responseDeclaration identifier="', ids[[i]]$response,
             '" cardinality="', if(type[i] == "mchoice") "multiple" else "single",
             if(is.null(mmatrix)) '" baseType="identifier">' else '" baseType="directedPair">', sep = ''),
           '<correctResponse>'
         )
         for(j in seq_along(solution[[i]])) {
           if(solution[[i]][j]) {
             xml <- c(xml,
               paste('<value>', if(is.null(mmatrix)) ids[[i]]$questions[j] else ids[[i]]$mmatrix_pairs[j], '</value>', sep = '')
             )
           }
         }

         xml <- c(xml, if(!sum(solution[[i]])) '<value>all_options_incorrect</value>' else NULL, '</correctResponse>',
           paste('<mapping defaultValue="', if(is.null(defaultval)) 0 else defaultval,
             '" lowerBound="', mv[[i]] <- if(!eval$negative) "0.0" else {
               if(eval$partial) {
                 if(type[i] == "mchoice") pv[[i]]["neg"] * sum(!solution[[i]]) else pv[[i]]["neg"]
               } else pv[[i]]["neg"]
             }, '">', sep = '')
         )
         for(j in seq_along(solution[[i]])) {
           xml <- c(xml,
             paste('<mapEntry mapKey="', if(is.null(mmatrix)) ids[[i]]$questions[j] else ids[[i]]$mmatrix_pairs[j], '" mappedValue="',
               if(eval$partial) {
                 if(solution[[i]][j]) {
                   pv[[i]]["pos"]
                 } else {
                   pv[[i]]["neg"]
                 }
               } else {
                 if(solution[[i]][j]) {
                   if(type[i] == "mchoice") pv[[i]]["pos"] / sum(solution[[i]]) else pv[[i]]["pos"]
                 } else {
                   if(pv[[i]]["neg"] == 0) {
                     -1 * pv[[i]]["pos"]
                   } else {
                     if(type[i] == "mchoice") pv[[i]]["neg"] * length(solution[[i]]) else pv[[i]]["neg"]
                   }
                 }
               }, '"/>', sep = '')
           )
         }
         xml <- c(xml, '</mapping>', '</responseDeclaration>')
       }

       ## numeric responses
       if(type[i] == "num") {
         xml <- c(xml,
           paste('<responseDeclaration identifier="', ids[[i]]$response, '" cardinality="multiple" baseType="float">', sep = ''), #NS: TVO uses multiple here, possibly due to tolerances values
         '<correctResponse>',
           paste('<value>', solution[[i]], '</value>', sep = ''),
           paste('<value>', solution[[i]] - max(tol[[i]]), ';',  solution[[i]] + max(tol[[i]]), '</value>', sep = ''), #NS: TVO specifies lower and upper in responseDeclaration
           '</correctResponse>',
           '</responseDeclaration>'
         )
       }
       ## string responses
       if(type[i] == "string") {
         if((length(maxchars[[i]]) > 1) & sum(!is.na(maxchars[[i]])) == 1) {
           xml <- c(xml,
             paste('<responseDeclaration identifier="', ids[[i]]$response, '" cardinality="single" baseType="string">', sep = ''),
           '<correctResponse>',
             paste('<value>', solution[[i]], '</value>', sep = ''),
             '</correctResponse>',
             paste('<mapping defaultValue="', if(is.null(defaultval)) 0 else defaultval, '">', sep = ''),
             paste('<mapEntry mapKey="', solution[[i]], '" mappedValue="', pv[[i]]["pos"], '" />', sep = ''),
             '</mapping>',
             '</responseDeclaration>'
           )
         } else {
           is_essay[i] <- TRUE
           ## Essay type questions.
           xml <- c(xml,
             paste('<responseDeclaration identifier="', ids[[i]]$response, '" cardinality="single" baseType="string">', sep = ''),
               '<correctResponse>',
#               if(dopbl) process_html_pbl(x$solution) else x$solution,
               paste('<value>', solution[[i]], '</value>', sep = ''),
               '</correctResponse>',
               '</responseDeclaration>'
           )
         }
       }
     } else {
         xml <- c(xml,
           if(i==1){
           paste('<responseDeclaration identifier="', ids[[1]]$response,
             '" cardinality="multiple">', sep = '')} else NULL,
           if(i==1) '<correctResponse>' else NULL
           )

         ##choice responses
         if(length(grep("choice", type[i]))) {
           for(j in seq_along(solution[[i]])) {
             if(solution[[i]][j]) {
               xml <- c(xml,
                 paste('<value fieldIdentifier="', ids[[i]]$idcs, '" baseType="string">',
                 if(is.null(mmatrix)) ids[[i]]$questions[j] else ids[[i]]$mmatrix_pairs[j],
                 '</value>', sep = '')
                 )
             }
           }
         }

         ## numeric responses
         if(type[i] == "num") {
           xml <- c(xml,
             paste('<value fieldIdentifier="', ids[[i]]$idcs, '" baseType="float">',
                 solution[[i]], '</value>', sep = ''),
             paste('<value fieldIdentifier="', ids[[i]]$idcs, '" baseType="float">',
                 solution[[i]] - max(tol[[i]]), ';',  solution[[i]] + max(tol[[i]]), '</value>', sep = '')
           )
         }
         ## string responses
         if(type[i] == "string") {
           if((length(maxchars[[i]]) > 1) & sum(!is.na(maxchars[[i]])) == 1) {
             xml <- c(xml,
               paste('<value fieldIdentifier="', ids[[i]]$idcs, '" baseType="string">',
                solution[[i]], '</value>', sep = '')
             )
           } else {
             is_essay[i] <- TRUE
             ## Essay type questions.
             xml <- c(xml,
                 paste('<value fieldIdentifier="', ids[[i]]$idcs, '" baseType="string">',
                  solution[[i]], '</value>', sep = '')
             )
           }
         }

         xml <- c(xml, if(i==n) {c('</correctResponse>', '<mapping defaultValue="0">')} else NULL
           )
       }
    }

    for(i in 1:n) {
      if(x$metainfo$type == "cloze"){
          xml <- c(xml,
            paste('<mapEntry mapKey="', ids[[i]]$idcs, '" mappedValue="', 1/n, '" />', sep = '')
               )
          xml <- c(xml, if(i==n) {c('</mapping>', '</responseDeclaration>')} else NULL
             )
      }
    }

    if(is.null(minvalue))
      minvalue <- sum(as.numeric(unlist(mv)))

    xml <- c(xml,
      paste('<outcomeDeclaration identifier="SCORE" cardinality="single" baseType="float" ',
        'normalMaximum="', sum(q_points), '" normalMinimum="', minvalue, '" />', sep = ''),
      '<outcomeDeclaration identifier="FEEDBACK" cardinality="single" baseType="identifier" />',
      '<stylesheet href="css/007007007007007007007.css" type="text/css" />'
    )

#    if(n > 1){
#      for(i in 1:n) {
#      xml <- c(xml,
#      paste('<outcomeDeclaration identifier="SCORE', i, '" cardinality="single" baseType="float" ',
#        'normalMaximum="', q_points[i], '" normalMinimum="', minvalue, '" />', sep = ''))
#      }
#    }

    ## starting the itembody
#    xml <- c(xml,
#    '<itemBody>', paste('<div id="textBlockId_', make_id(4), '" class="textblock tvblock tvcss_1">', '<div class="textblock tvblock tvcss_1">', sep='')
#    )
#
#    xml <- c(xml, x$question, '</div>', '</div>', if(x$metainfo$type == "cloze") '<div class="interactieblok">' else NULL)
#
    if(ant <- any(grepl("##ANSWER[0-9]+##", x$question))) {
    patterns <- c(' align="left"', ' align="right"', ' align="center"') #necessary as long as TVO does not accept standard html
    for(i in seq_along(patterns))
    x$question <- gsub(patterns[i], " ", x$question)
    x$question <- c('<div class="interactieblok">', '<div class="textblock tvblock tvcss_1">',
                      '<div class="rte_zone tveditor1">', x$question, '</div>', '</div>', '</div>')
    }

    xml <- c(xml, '<itemBody>',  x$question, if(x$metainfo$type == "cloze" & !ant) '<div class="interactieblok">' else NULL)

    for(i in 1:n) {
      ans <- any(grepl(paste0("##ANSWER", i, "##"), xml))
      if(length(grep("choice", type[i]))) {
        if(is.null(mmatrix)) {
          if(x$metainfo$type != "cloze"){
            txml <- paste('<choiceInteraction responseIdentifier="', ids[[i]]$response,
                '" shuffle="', if(shuffle) 'true' else 'false','" maxChoices="',
                if(type[i] == "schoice") "1" else "0", '">', sep = '')
            for(j in seq_along(solution[[i]])) {
              txml <- c(txml, paste('<simpleChoice identifier="', ids[[i]]$questions[j], '">', sep = ''),
                paste('<div class="textblock tvblock tvcss_1">', '<div class="rte_zone tveditor1">', sep=''),
                paste(if(enumerate & !ans) {
                  paste(letters[if(x$metainfo$type == "cloze") i else j], ".",
                    if(x$metainfo$type == "cloze" && length(solution[[i]]) > 1) paste(j, ".", sep = "") else NULL,
                      sep = "")
                } else NULL, questionlist[[i]][j]),
                '</div>', '</div>',
                '</simpleChoice>'
              )
            }
            txml <- c(txml, '</choiceInteraction>')
            } else {
            txml <- c(
              if(!ant) paste('<div class="textblock tvblock tvcss_1">', '<div class="rte_zone tveditor1">', sep = '') else NULL,
                paste('<inlineChoiceInteraction class="multipleinput" id="', ids[[i]]$idcs,
               '" responseIdentifier="', ids[[1]]$response, '" shuffle="', if(shuffle) 'true' else 'false', '" required="true">', sep = '')
                )
            for(j in seq_along(solution[[i]])) {
              txml <- c(txml, paste('<inlineChoice identifier="', ids[[i]]$questions[j], '">',
                paste(if(enumerate & !ans) {
                  paste(if(length(solution[[i]]) > 1) paste(j, '.', sep = '') else NULL, sep = '')
                } else NULL, gsub("<[^>]+>","", questionlist[[i]][j])), ##TVO does not allow for any styling within the html-code of inlineChoice content
                '</inlineChoice>', sep ='')
              )
            }
            txml <- c(txml, '</inlineChoiceInteraction>', if(!ant) c('</div>', '</div>') else NULL)
              }
        } else {
          txml <- c(paste0('<matchInteraction class="match_matrix" responseIdentifier="', ids[[i]]$response,
            '" shuffle="', if(shuffle) 'true' else 'false','" maxAssociations="',
            if(type[i] == "schoice") "1" else "0", '">', sep = ''),
            '<simpleMatchSet>')
          for(j in seq_along(ids[[i]]$mmatrix_questions$rows)) {
            txml <- c(txml,
              paste0('<simpleAssociableChoice identifier="',
                ids[[i]]$mmatrix_questions$rows[j], '" matchMax="1" matchMin="0">'),
              '<p>',
              rownames(ids[[i]]$mmatrix_matches)[j],
              '</p>', '</simpleAssociableChoice>')
          }
          txml <- c(txml, '</simpleMatchSet>', '<simpleMatchSet>')
          for(j in seq_along(ids[[i]]$mmatrix_questions$cols)) {
            txml <- c(txml,
              paste0('<simpleAssociableChoice identifier="',
                ids[[i]]$mmatrix_questions$cols[j], '" matchMax="1" matchMin="0">'),
              '<p>',
              colnames(ids[[i]]$mmatrix_matches)[j],
              '</p>', '</simpleAssociableChoice>')
          }
          txml <- c(txml, '</simpleMatchSet>', '</matchInteraction>')
        }
      }
      if(type[i] == "num") {
        for(j in seq_along(solution[[i]])) {
          txml <- c(
            if(x$metainfo$type == "cloze" & !ant) paste('<div class="textblock tvblock tvcss_1">', '<div class="rte_zone tveditor1">', sep = '') else NULL,
              if(!is.null(questionlist[[i]][j])) {
                paste(if(enumerate & n > 1 ) {
                  paste(letters[if(x$metainfo$type == "cloze") i else j], ".",
                    if(x$metainfo$type == "cloze" && length(solution[[i]]) > 1) paste(j, ".", sep = "") else NULL,
                      sep = "")
                } else NULL, if(!is.na(questionlist[[i]][j])) questionlist[[i]][j] else NULL)
              },
            if(x$metainfo$type != "cloze") paste('<extendedTextInteraction responseIdentifier="', ids[[i]]$response, '"/>', sep = '') else
             paste('<textEntryInteraction class="multipleinput" id="', ids[[i]]$idcs, '" responseIdentifier="', ids[[1]]$response, '" expectedLength="12" />', sep = ''),
            if(x$metainfo$type == "cloze"& !ant) c('</div>', '</div>') else NULL
          )
        }
      }
      if(type[i] == "string") {
        if((length(maxchars[[i]]) > 1) & sum(is.na(maxchars[[i]])) < 1) {
          ## Essay type questions.
          txml <- c(
             if(x$metainfo$type == "cloze" & !ant) paste('<div class="textblock tvblock tvcss_1">', '<div class="rte_zone tveditor1">', sep = '') else NULL,
             if(!is.null(questionlist[[i]])) {
                paste(if(enumerate & n > 1) {
                  paste(letters[if(x$metainfo$type == "cloze") i else j], ".",
                    if(x$metainfo$type == "cloze" && length(solution[[i]]) > 1) paste(1, ".", sep = "") else NULL,
                      sep = "")
                } else NULL, if(!is.na(questionlist[[i]])) questionlist[[i]] else NULL)
             },
             paste(if(x$metainfo$type != "cloze") '<extendedTextInteraction responseIdentifier="' else '<textEntryInteraction responseIdentifier="', ids[[i]]$response,
              #'" minStrings="0" ', if(!is.na(maxchars[[i]][1])) {
             '" ', if(!is.na(maxchars[[i]][1])) {
                  paste0(' expectedLength="', maxchars[[i]][1], '"')
                } else NULL, if(!is.na(maxchars[[i]][2])) {
                  paste(' expectedLines="', maxchars[[i]][2], '" ', sep = '')
                } else NULL, if(x$metainfo$type == "cloze") {
                  paste0(' id="', ids[[i]]$idcs, '"')
                } else NULL,
                 '/>', sep = ''),
             if(x$metainfo$type == "cloze" & !ant) c('</div>', '</div>') else NULL
          )
        } else {
          for(j in seq_along(solution[[i]])) {
            txml <- c(
             if(x$metainfo$type == "cloze" & !ant) paste('<div class="textblock tvblock tvcss_1">', '<div class="rte_zone tveditor1">', sep = '') else NULL,
               if(!is.null(questionlist[[i]][j])) {
                  paste(if(enumerate & n > 1) {
                    paste(letters[if(x$metainfo$type == "cloze") i else j], ".",
                      if(x$metainfo$type == "cloze" && length(solution[[i]]) > 1) paste(j, ".", sep = "") else NULL,
                        sep = "")
                  } else NULL, if(!is.na(questionlist[[i]][j])) questionlist[[i]][j] else NULL)
               },
               paste(if(x$metainfo$type != "cloze") '<extendedTextInteraction responseIdentifier="' else '<textEntryInteraction responseIdentifier="', ids[[i]]$response,
                if(!is.na(maxchars[[i]][1])) {
                  paste0('" expectedLength="', maxchars[[i]][1], '"')
                } else NULL, if(!is.na(maxchars[[i]][2])) {
                  paste0('" expectedLines="', maxchars[[i]][2], '"')
                } else NULL,  if(x$metainfo$type == "cloze") {
                  paste0(' id="', ids[[i]]$idcs, '"')
                } else NULL, '/>', sep = ''),
              if(x$metainfo$type == "cloze" & !ant) c('</div>', '</div>') else NULL
            )
          }
        }
      }
      if(ans) {
        txml <- paste(txml, collapse = '\n')
        if(length(grep("choice", type[i])) & !any(grepl('<table>', xml, fixed = TRUE)))
          txml <- paste0('</p>', txml, '<p>')
        xml <- gsub(paste0("##ANSWER", i, "##"), txml, xml, fixed = TRUE)
      } else {
        xml <- c(xml, txml)
      }
    }

    ## close itembody
    xml <- c(xml, if(x$metainfo$type == "cloze" & !ant) '</div>' else NULL, '</itemBody>')

    ## response processing
    xml <- c(xml, '<responseProcessing>')

    ## all not answered
    xml <- c(xml,
      '<responseCondition>',
      '<responseIf>'
#      if(n > 1) '<and>' else NULL
    )
#    for(i in 1:n) {
     xml <- c(xml,
     '<isNull>',
     paste('<variable identifier="', ids[[1]]$response, '"/>', sep = ''),
     '</isNull>'
      )
#    }
    xml <- c(xml,
#      if(n > 1) '</and>' else NULL,
      '<setOutcomeValue identifier="SCORE">',
      '<baseValue baseType="float">0.0</baseValue>', ## FIXME: points when not answered?
      '</setOutcomeValue>',
      '<setOutcomeValue identifier="FEEDBACK">',
      '<baseValue baseType="identifier">FAILURE</baseValue>',
      '</setOutcomeValue>',
      '</responseIf>',
      '<responseElse>'
    )

#    ## not answered points single
#    for(i in 1:n) {
#      xml <- c(xml,
#        '<responseCondition>',
#        '<responseIf>',
#        '<isNull>',
#        paste('<variable identifier="', ids[[i]]$response, '"/>', sep = ''),
#        '</isNull>',
#        '<setOutcomeValue identifier="SCORE">',
#        '<sum>',
#        '<baseValue baseType="float">0.0</baseValue>', ## FIXME: points when not answered?
#        '</sum>',
#        '</setOutcomeValue>',
#        '</responseIf>',
#        '</responseCondition>'
#      )
#    }

    ## set the score
    for(i in 1:n) {
      if(x$metainfo$type != "cloze"){
        xml <- c(xml,
          if(type[i] == "num" ){
            c('<responseCondition>',
            '<responseIf>',
            '<match>',
            paste('<variable identifier="', ids[[i]]$response, '"/>', sep = ''),
            paste('<correct identifier="', ids[[i]]$response, '"/>', sep = ''),
            '</match>',
#           paste('<setOutcomeValue identifier="SCORE', ifelse(n > 1, i, ""), '">', sep = ''),
            '<setOutcomeValue identifier="SCORE">',
            paste('<baseValue baseType="float">', pv[[i]]["pos"], '</baseValue>', sep = ''),
            '</setOutcomeValue>',
            '</responseIf>',
            '<responseElse>',
#           paste('<setOutcomeValue identifier="SCORE', ifelse(n > 1, i, ""), '">', sep = ''),	
            '<setOutcomeValue identifier="SCORE">',									
            paste('<baseValue baseType="float">', pv[[i]]["neg"], '</baseValue>', sep = ''),
            '</setOutcomeValue>',
            '</responseElse>',
            '</responseCondition>')
          } else {
            c(
            #paste('<setOutcomeValue identifier="SCORE', ifelse(n > 1, i, ""), '">', sep = ''),
            '<setOutcomeValue identifier="SCORE">',
            switch(if(is_essay[i]) "essay" else type[i],
            "mchoice" =  paste('<mapResponse identifier="', ids[[i]]$response, '"/>', sep = ''),
            "schoice" =  paste('<mapResponse identifier="', ids[[i]]$response, '"/>', sep = ''),
            "string" =   paste('<mapResponse identifier="', ids[[i]]$response, '"/>', sep = ''),
            "essay" = paste('<baseValue baseType="float">0</baseValue>', sep = '')),
            '</setOutcomeValue>')
          }
        )

        ## Adapt points for mchoice.
        ## Case no correct answers.
        if(type[i] == "mchoice") {
          if(sum(solution[[i]]) < 1) {
            warning(sprintf("Exercise '%s' has all options incorrect, and can therefore not be handled by TestVision", x$metainfo$file))
            xml <- c(xml,
              '<responseCondition>',
              '<responseIf>',
              '<isNull>',
              paste('<variable identifier="', ids[[i]]$response, '"/>', sep = ''),
              '</isNull>',
              '<setOutcomeValue identifier="SCORE">',
              paste('<baseValue baseType="float">', q_points[i], '</baseValue>', sep = ''),
              '</setOutcomeValue>',
              '</responseIf>',
              '</responseCondition>'
            )
          }
        }
      }
        ## Deal with cloze items and case maximum points with rounding errors in cloze.
         else {
          xml <- c(xml,
            if(i==1){
              c('<setOutcomeValue identifier="SCORE">',
              paste('<mapResponse identifier="', ids[[i]]$response, '"/>', sep = ''),
              '</setOutcomeValue>',
              '<responseCondition>',
              '<responseIf>',
              '<equal toleranceMode="relative" tolerance="0.001">',
              '<variable identifier="SCORE"/>',
              '<variable identifier="MAXSCORE"/>',
              '</equal>',
              '<setOutcomeValue identifier="SCORE">',
              paste('<baseValue baseType="float">', sum(q_points), '</baseValue>', sep = ''),
              '</setOutcomeValue>',
              '</responseIf>',
              '</responseCondition>'
              )
            } else NULL
          )
        }
      }
#    if(x$metainfo$type == "cloze"){
#      xml <- c(xml,
#        '<setOutcomeValue identifier="SCORE">',
#        '<sum>',
#        paste('<variable identifier="SCORE', 1:n , '"/>', sep = ''),
#        '</sum>',
#        '</setOutcomeValue>'
#        )
#    }

    xml <- c(xml,
        '<responseCondition>',
        '<responseIf>',
        '<equal toleranceMode="relative" tolerance="0.001">',
        '<variable identifier="SCORE"/>',
        '<variable identifier="MAXSCORE"/>',
        '</equal>',
        '<setOutcomeValue identifier="FEEDBACK">',
        '<baseValue baseType="identifier">ANSWER_CORRECT</baseValue>',
        '</setOutcomeValue>',
        '</responseIf>',
        '<responseElse>',
        '<setOutcomeValue identifier="FEEDBACK">',
        '<baseValue baseType="identifier">FAILURE</baseValue>',
        '</setOutcomeValue>',
        '</responseElse>',
        '</responseCondition>',
        '</responseElse>',
        '</responseCondition>')

    ## show solution when answered and wrong
#    xml <- c(xml,
#      '<responseCondition>',
#      '<responseIf>',
#      if(type[i] != "num") {
#        c('<equal toleranceMode="relative" tolerance="0.001">',
#          '<variable identifier="SCORE"/>',
#          '<variable identifier="MAXSCORE"/>',
#          '</equal>')
#      } else {
#        c(
#          paste('<equal toleranceMode="absolute" tolerance="', max(tol[[i]]), ' ',
#            max(tol[[i]]),'" includeLowerBound="true" includeUpperBound="true">', sep = ''),
#          paste('<variable identifier="', ids[[i]]$response, '"/>', sep = ''),
#          paste('<correct identifier="', ids[[i]]$response, '"/>', sep = ''),
#          '</equal>'
#        )
#      },
#      '<setOutcomeValue identifier="FEEDBACK">',
#      '<baseValue baseType="identifier">correct</baseValue>',
#      '</setOutcomeValue>',
#      '</responseIf>',
#      '<responseElse>',
#      '<setOutcomeValue identifier="FEEDBACK">',
#      '<baseValue baseType="identifier">incorrect</baseValue>',
#      '</setOutcomeValue>',
#      if(!eval$partial) {
#        c('<setOutcomeValue identifier="SCORE">',
#          paste('<baseValue baseType="float">', minvalue, '</baseValue>', sep = ''),
#          '</setOutcomeValue>')
#      } else NULL,
#      '</responseElse>',
#      '</responseCondition>'
#    )

    ## set the minimum points
#    if(!is.null(minvalue)) {
#      xml <- c(xml,
#        '<responseCondition>',
#        '<responseIf>',
#        '<and>',
#        '<match>',
#        '<baseValue baseType="identifier">incorrect</baseValue>',
#        '<variable identifier="FEEDBACKBASIC"/>',
#        '</match>',
#        '<not>',
#        '<gte>',
#        '<variable identifier="SCORE"/>',
#        '<variable identifier="MINSCORE"/>',
#        '</gte>',
#        '</not>',
#        '</and>',
#        '<setOutcomeValue identifier="SCORE">',
#        paste('<baseValue baseType="float">', minvalue, '</baseValue>', sep = ''),
#        '</setOutcomeValue>',
#        '</responseIf>',
#        '</responseCondition>'
#      )
#    }

    if(solutionswitch) {
#      fid <- make_id(9, 1)
#      xml <- c(xml,
#        '<responseCondition>',
#        '<responseIf>',
#        '<and>',
#        '<match>',
#        '<baseValue baseType="identifier">incorrect</baseValue>',
#        '<variable identifier="FEEDBACKBASIC"/>',
#        '</match>',
#        '</and>',
#        '<setOutcomeValue identifier="FEEDBACKMODAL">',
#        '<multiple>',
#        '<variable identifier="FEEDBACKMODAL"/>',
#        paste('<baseValue baseType="identifier">Feedback', fid, '</baseValue>', sep = ''),
#        '</multiple>',
#        '</setOutcomeValue>',
#        '</responseIf>',
#        '</responseCondition>'
#      )

      ## create solution
      xsolution <- fix_tvo_img(x$solution)
      #noitulosx <- fix_tvo_table(x$solution, npcss = )
      #xsolution <- noitulosx[[1]]
      #css <- paste0(css, noitulosx[[2]])
      if(!is.null(x$solutionlist)) {
        if(!all(is.na(x$solutionlist))) {
          xsolution <- c(xsolution, if(length(xsolution)) "<br />" else NULL)
          xsolution <- c(xsolution, if(enumerate) '<ol>' else '<ul>')
          if(x$metainfo$type == "cloze") {
            g <- rep(seq_along(x$metainfo$solution), sapply(x$metainfo$solution, length))
            ql <- sapply(split(x$questionlist, g), paste, collapse = " / ")
            sl <- sapply(split(x$solutionlist, g), paste, collapse = " / ")
          } else {
            ql <- x$questionlist
            sl <- x$solutionlist
          }
          nsol <- length(ql)
          xsolution <- c(xsolution, paste(rep('<li>', nsol),
            ql, if(length(x$solutionlist)) "<br />" else NULL,
            sl, rep('</li>', nsol)),
            if(enumerate) '</ol>' else '</ul>')
        }
      }
    }

    xml <- c(xml, '</responseProcessing>')

    ## solution when wrong
    if(solutionswitch) {
      xml <- c(xml,
        paste('<modalFeedback identifier="FAILURE" outcomeIdentifier="FEEDBACK" showHide="show">', sep = ''),
        paste('<div class="textblock tvblock tvcss_1">', '<div class="rte_zone tveditor1">', sep=''),
        xsolution, '</div>', '</div>',
        '</modalFeedback>'
      )
    }

    ## solution when correct
    if(solutionswitch) {
      xml <- c(xml,
        paste('<modalFeedback identifier="ANSWER_CORRECT" outcomeIdentifier="FEEDBACK" showHide="show">', sep = ''),
        paste('<div class="textblock tvblock tvcss_1">', '<div class="rte_zone tveditor1">', sep=''),
        xsolution, '</div>', '</div>',
        '</modalFeedback>'
      )
    }

    ## solution when partially correct (cloze and mchoice)
    if(solutionswitch) {
      xml <- c(xml,
        paste('<modalFeedback identifier="PARTIAL_CORRECT" outcomeIdentifier="FEEDBACK" showHide="show">', sep = ''),
        paste('<div class="textblock tvblock tvcss_1">', '<div class="rte_zone tveditor1">', sep=''),
        xsolution, '</div>', '</div>',
        '</modalFeedback>'
      )
    }

    xml <- c(xml, '</assessmentItem>')

    xml
  }
}


## fix the issue of TVO not allowing for a missing alternate text 'alt=' in img specifations
fix_tvo_img <- function(x){
  img_start <- grep("<img[^>]+>", x)
    if(length(img_start) > 0L) {
      for(i in img_start) {
        if(!grepl("alt=", x[i])){
          x[i] <- gsub("/>", "alt=\"image\"/>", x[i])
        }
      }
    }
  return(x)
}

## fix the issue of TVO not allowing for allowing for including styles or alignments in tables
## now alignments and style defintions are placed in a css file for each exercise.
## FIXME: this function may need an update in the future if other table elements, such as width, appear in html table
fix_tvo_table <- function(x){
  table_att <- c("background", "background-color", "border", "border-color", "border-width", "color", "font-size", "font-weight",
                 "height", "letter-spacing", "line-height", "list-style-type", "margin", "margin-bottom", "margin-left", "margin-right",
                 "margin-top", "padding", "padding-bottom", "padding-left", "padding-right", "padding-top", "text-align", "text-decoration",
                 "vertical-align", "width")# as per e-mail Andries Bosma @teelen, 2022-09-28
  mtable_att <- c("align", "alignmentscope", "columnalign", "columnlines", "columnspacing",
                   "columnspan", "columnwidth", "displaystyle", "equalcolumns", "equalrows", "frame", "framespacing", "groupalign",
                   "groupalign", "groupalign", "minlabelspacing", "rowalign", "rowlines", "rowspacing",
                   "rowspan", "side", "width")# as per https://www.w3.org/Math/XMLSchema/

  css <- ".tvcss_1 {\nheight: auto;\n}"#to make sure the css-file is not empty, which would give errors uploading
  length_css <- 0
  if(any(grepl('(<table>)|(<table )', x))) {
    x <- paste(x, collapse = " ")
    from <- unique(regmatches(x, gregexpr('(?<=<table |<tr |<td |<th)(.|\n)*?(?=>)', x, perl = TRUE))[[1L]])
    length_css <- length(from)
    for(j in 1 : length_css){
      if(grepl("style=", from[j]) & grepl("class=", from[j])){#deal with situations in which both class and style are defined; rem class
          tmp <- paste0('class=\"', regmatches(from[j], gregexpr('(?<=class=\")(.|\n)*?(?=\")', from[j], perl = TRUE))[[1L]], '\"')
          tmp <- gsub(tmp, "", from[j])
          x <- gsub(from[j], tmp, x)
          from[j] <- gsub(from[j], tmp, from[j])
      }
      if(grepl("style=", from[j])){
          css <- c(css, paste0(paste0(".recss_", j, " {\n"), gsub('"', "", gsub("style=", "", from[j])), "\n}"))
          x <- gsub(from[j], paste0(" class=", '\"recss_', j, '\"'),  x, fixed = TRUE)
      } else if(grepl("^align=", from[j])){
          css <- c(css, paste0(paste0(".recss_", j, " {\n"), gsub('"', "", gsub("^align=", " text-align: ", from[j])), ";", "\n}"))
          x <- gsub(paste0("\\s+", from[j]), paste0(" class=", '\"recss_', j, '\"'),  x)
      }
    }
    for(i in 1 : length(css)) if(length(strsplit(css[i], ";" )[[1]]) > 2) css[i] <- gsub("; ", ";\n", css[i])
  }
  if(any(grepl('(<mtable>)|(<mtable )', x))) {
    x <- paste(x, collapse = " ")
    from <- unique(regmatches(x, gregexpr('(?<=<mtable |<mtr |<mtd )(.|\n)*?(?=>)', x, perl = TRUE))[[1L]])
    length_css <- length(from)
    for(j in 1 : length_css){
      if(grepl("style=", from[j]) & grepl("class=", from[j])){#deal with situations in which both class and style are defined; rem class
          tmp <- paste0('class=\"', regmatches(from[j], gregexpr('(?<=class=\")(.|\n)*?(?=\")', from[j], perl = TRUE))[[1L]], '\"')
          tmp <- gsub(tmp, "", from[j])
          x <- gsub(from[j], tmp, x)
          from[j] <- gsub(from[j], tmp, from[j])
      }
      if(grepl("style=", from[j])){
          css <- c(css, paste0(paste0(".recss_", j, " {\n"), gsub('"', "", gsub("style=", "", from[j])), "\n}"))
          x <- gsub(from[j], paste0(" class=", '\"recss_', j, '\"'),  x, fixed = TRUE)
      } else if(grepl("^align=", from[j])){
          css <- c(css, paste0(paste0(".recss_", j, " {\n"), gsub('"', "", gsub("^align=", " text-align: ", from[j])), ";", "\n}"))
          x <- gsub(paste0("\\s+", from[j]), paste0(" class=", '\"recss_', j, '\"'),  x)
      }
    }
    for(i in 1 : length(css)) if(length(strsplit(css[i], ";" )[[1]]) > 2) css[i] <- gsub("; ", ";\n", css[i])
  }
  if(any(grepl('<ol ', x))) {
    x <- paste(x, collapse = " ")
    oList <- regmatches(x, gregexpr('(?<=<ol )(.|\n)*?(?=/ol>)', x, perl = TRUE))[[1]]
    noList <- regmatches(x, gregexpr('(?<=<ol )(.|\n)*?(?=/ol>)', x, perl = TRUE), invert = TRUE)[[1]]
    oList <- lapply(oList, function(x) gsub("</p>", "", gsub("<p>", "", x)))
    nol <- length(oList)
    x <- NULL
    for(i in 1 : nol ){
        x <- paste0(x, noList[i], oList[[i]][1])
    }
    x <- paste0(x, noList[nol + 1])
    from <- unique(regmatches(x, gregexpr('(?<=<ol )(.|\n)*?(?=>)', x, perl = TRUE))[[1L]])
    length_css <- length(from)
    for(j in 1 : length_css){
      if(grepl("style=", from[j])){
          css <- c(css, paste0(paste0(".recss_", j, " {\n"), gsub('"', "", gsub("style=", "", from[j])), "\n}"))
          x <- gsub(from[j], paste0(" class=", '\"recss_', j, '\"'),  x, fixed = TRUE)
      }
    }
    for(i in 1 : length(css)) if(length(strsplit(css[i], ";" )[[1]]) > 2) css[i] <- gsub("; ", ";\n", css[i])
  }
  list(x, css)
}
