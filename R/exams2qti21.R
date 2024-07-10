## create IMS QTI 2.1 .xml files
## specifications and examples available at:
## http://www.imsglobal.org/question/#version2.1
## https://www.ibm.com/developerworks/library/x-qti/
## https://www.onyx-editor.de/
## http://membervalidator.imsglobal.org/qti/
## https://webapps.ph.ed.ac.uk/qtiworks/anonymous/validator
## http://www.imsglobal.org/question/qtiv2p1/imsqti_implv2p1.html
exams2qti21 <- function(file, n = 1L, nsamp = NULL, dir = ".",
  name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE, rds = FALSE,
  resolution = 100, width = 4, height = 4, svg = FALSE, encoding  = "UTF-8",
  num = NULL, mchoice = NULL, schoice = mchoice, string = NULL, cloze = NULL,
  template = "qti21",
  duration = NULL, stitle = NULL, ititle = NULL,
  adescription = "Please solve the following exercises.", sdescription = "", 
  maxattempts = 1, cutvalue = NULL, solutionswitch = TRUE, casesensitive = TRUE, cloze_schoice_display = "auto",
  navigation = "nonlinear", allowskipping = TRUE, allowreview = FALSE, allowcomment = FALSE,
  shufflesections = FALSE, zip = TRUE, points = NULL,
  eval = list(partial = TRUE, rule = "false2", negative = FALSE),
  converter = NULL, envir = NULL, engine = NULL, base64 = TRUE, mode = "hex",
  include = NULL,
  selection = c("pool", "exam"), flavor = "plain", ...)
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
  ## process solutionswitch
  solutionswitch <- sol_switch(solutionswitch)

  ## create a name (without spaces or periods)
  if(is.null(name)) name <- file_path_sans_ext(basename(template))
  name <- gsub("\\s|\\.", "_", name)
  name_base <- if(is_number1(name)) paste0("_", name) else name
  if(isTRUE(rds)) rds <- name

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
      dir = dir, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose, rds = rds, points = points)
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
      if(is.null(itembody[[i]]$solutionswitch)) itembody[[i]]$solutionswitch <- solutionswitch
      if(is.null(itembody[[i]]$casesensitive)) itembody[[i]]$casesensitive <- casesensitive
      if(i == "cloze" && is.null(itembody[[i]]$cloze_schoice_display)) itembody[[i]]$cloze_schoice_display <- cloze_schoice_display
      itembody[[i]] <- do.call("make_itembody_qti21", itembody[[i]])
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

  ## Pool or exam?
  is_exam <- match.arg(selection) == "exam"

  xml <- readLines(template[1L])

  ## check template for all necessary tags
  ## extract the template for the assessement, sections, and manifest
  if(length(start <- grep("<assessmentTest", xml, fixed = TRUE)) != 1L ||
     length(end <- grep("</assessmentTest>", xml, fixed = TRUE)) != 1L) {
    ## stop(paste("The XML template", template,
    ##   "must contain exactly one opening and closing <assessmentTest> tag!"))
    assessment_xml <- character(0L)
  } else {
    assessment_xml <- xml[start:end]
  }

  if(length(start <- grep("<assessmentSection", xml, fixed = TRUE)) != 1L ||
     length(end <- grep("</assessmentSection>", xml, fixed = TRUE)) != 1L) {
    ## stop(paste("The XML template", template,
    ##   "must contain exactly one opening and closing <assessmentSection> tag!"))
    section_xml <- character(0L)
  } else {
    section_xml <- xml[start:end]
  }

  if(length(start <- grep("<manifest", xml, fixed = TRUE)) != 1L ||
    length(end <- grep("</manifest>", xml, fixed = TRUE)) != 1L) {
    stop(paste("The XML template", template,
      "must contain exactly one opening and closing <manifest> tag!"))
  }
  manifest_xml <- xml[start:end]

  ## obtain the number of exams and questions
  nx <- length(exm)
  nq <- if(!is.xexam) length(exm[[1L]]) else length(exm)

  ## function for internal ids
  make_test_ids <- function(n, type = c("test", "section", "item"))
  {
    switch(type,
      "test" = paste(name_base, make_id(9), sep = "_"),
      paste(type, formatC(1:n, flag = "0", width = nchar(n)), sep = "_")
    )
  }

  ## generate the test id
  test_id <- make_test_ids(type = "test")

  ## create section ids
  sec_ids <- paste(test_id, make_test_ids(nq, type = "section"), sep = "_")

  ## create section/item titles and section description
  ## FIXME: quick & dirty workaround for stitle/ititle = NULL (i.e., the default) which was not properly handled
  stitle2 <- if(!is.null(stitle)) rep(stitle, length.out = nx) else stitle
  if(!is.null(stitle)) stitle <- rep(stitle, length.out = nq)
  if(!is.null(ititle)) ititle <- rep(ititle, length.out = nq)
  if(is.null(adescription)) adescription <- ""
  if(is.null(sdescription) || identical(sdescription, FALSE)) sdescription <- ""
  sdescription <- rep(sdescription, length.out = nq)
  sdescription[sdescription != ""] <- sprintf(
    '<rubricBlock view="candidate"><p>%s</p></rubricBlock>',
    sdescription[sdescription != ""]
  )

  ## enable different maxattempts per sections (simply added before section description)
  maxattempts[!is.finite(maxattempts) | maxattempts < 0] <- 0
  if(length(maxattempts) > 1L) {
    maxattempts <- rep(maxattempts, length.out = nq)
    sdescription <- paste0(
      sprintf('<itemSessionControl maxAttempts="%s"/>', round(as.numeric(maxattempts))),
      "\n",
      sdescription
    )
  }

  ## create the directory where the test is stored
  dir.create(test_dir <- file.path(file_path_as_absolute(tdir), name))

  ## cycle through all exams and questions
  ## similar questions are combined in a section,
  ## questions are then sampled from the sections
  items <- sec_xml <- sec_items_D <- sec_items_R <- sec_xml_mat <- NULL
  maxscore <- 0
  for(j in 1:nq) {
    ## first, create the section header
    sxmlj <- section_xml
    stj <- stitle[j]
    if(isTRUE(stj))
      stj <- as.character(j)
    if(is.null(stj) || isFALSE(stj)) stj <- ""

    if(stj == "")
      sxmlj <- gsub('visible="true"', 'visible="false"', sxmlj, fixed = TRUE)

    sec_xml <- c(sec_xml, gsub("##SectionId##", sec_ids[j], sxmlj, fixed = TRUE))

    ## insert a section title -> exm[[1]][[j]]$metainfo$name?
    sec_xml <- gsub("##SectionTitle##", stj, sec_xml, fixed = TRUE)

    ## insert a section description -> exm[[1]][[j]]$metainfo$description?
    sec_xml <- gsub("##SectionDescription##", sdescription[j], sec_xml, fixed = TRUE)

    ## special handler
    if(is.xexam) nx <- length(exm[[j]])

    ## create item ids
    item_ids <- paste(sec_ids[j], make_test_ids(nx, type = "item"), sep = "_")

    ## collect items for section linking
    sec_items_A <- NULL

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
      if(!is.null(ititle)) {
        if(is.logical(ititle[j])) {
          if(!ititle[j])
            exm[[i]][[j]]$metainfo$name <- ""
          else
            exm[[i]][[j]]$metainfo$name <- as.character(j)
        } else {
          exm[[i]][[j]]$metainfo$name <- ititle[j]
        }
      } else {
        if(!is.null(exm[[i]][[j]]$metainfo$title)) {
          exm[[i]][[j]]$metainfo$name <- exm[[i]][[j]]$metainfo$title
        } else {
          exm[[i]][[j]]$metainfo$name <- as.character(j)
        }
      }

      ## switch for debugging
      if(FALSE) {
        exm[[i]][[j]]$question <- "Here is the questiontext..."
        exm[[i]][[j]]$solution <- "This is the solutiontext..."
        exm[[i]][[j]]$solutionlist <- NA
      }

      exm[[i]][[j]]$converter <- converter
      exm[[i]][[j]]$flavor <- flavor

      ibody <- itembody[[type]](exm[[i]][[j]])

      exm[[i]][[j]]$converter <- NULL
      exm[[i]][[j]]$flavor <- NULL

      ## copy supplements
      if(length(exm[[i]][[j]]$supplements)) {
        if(!base64) {
          if(!file.exists(media_dir <- file.path(test_dir, "media")))
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

            fid <- gsub('\\', '', gsub('/', '_', file.path('media', sup_dir, f), fixed = TRUE), fixed = TRUE)
            fhref <- file.path('media', sup_dir, f)
            sec_items_R <- c(sec_items_R,
              paste('<resource identifier="', paste(fid, 'id', sep = '_'),
                '" type="imsqti_item_xmlv2p1" href="', fhref, '">', sep = ''),
              paste('<file href="', fhref, '"/>', sep = ''),
              '</resource>'
            )

            if(any(grepl(dirname(exm[[i]][[j]]$supplements[si]), ibody))) {
              ibody <- gsub(dirname(exm[[i]][[j]]$supplements[si]),
                file.path('media', sup_dir), ibody, fixed = TRUE)
            } else {
              if(any(grepl(f, ibody))) {
                ibody <- gsub(paste(f, '"', sep = ''),
                  paste('media/', sup_dir, '/', f, '"', sep = ''), ibody, fixed = TRUE)
              }
            }
          }
        }
      }

      ## write the item xml to file
      writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', ibody),
        file.path(test_dir, paste(iname, "xml", sep = ".")))

      ## include body in section
      sec_items_A <- c(sec_items_A,
        paste('<assessmentItemRef identifier="', iname, '" href="', iname, '.xml" fixed="false"/>', sep = '')
      )
      sec_items_D <- c(sec_items_D,
        paste('<dependency identifierref="', paste(iname, 'id', sep = '_'), '"/>', sep = '')
      )
      sec_items_R <- c(sec_items_R,
        paste('<resource identifier="', paste(iname, 'id', sep = '_'), '" type="imsqti_item_xmlv2p1" href="', iname, '.xml">', sep = ''),
        paste('<file href="', iname, '.xml"/>', sep = ''),
        '</resource>'
      )
    }

    if(is_exam) {
      sec_xml_mat <- rbind(sec_xml_mat, sec_items_A)
    } else {
      sec_xml <- gsub('##SectionItems##', paste(sec_items_A, collapse = '\n'), sec_xml, fixed = TRUE)
    }
  }

  if(is_exam) {
    ## hard coded
    select <- 1
    etitle <- NULL
    qtitle <- NULL

    if(is.null(etitle) || isFALSE(etitle)) etitle <- ""
    test_id_exam <- paste(test_id, 'Exam', sep = '_')
    sec_xml <- c(
      paste0('<assessmentSection identifier="', test_id_exam,
        '" fixed="false" title="', etitle,
        '" visible="', if(etitle != "") 'true' else 'false', '">'),
      paste0('<selection select="', select, '"/>'),
      paste0('<ordering shuffle="', if(TRUE) 'true' else 'false', '"/>')
    )
    for(j in 1:ncol(sec_xml_mat)) {
      test_id_exam_j <- paste(test_id_exam, j, sep = '_')
      stj2 <- stitle2[j]
      if(is.null(stj2) || isFALSE(stj2)) stj2 <- ""
      vis <- if(stj2 == "") 'false' else 'true'
      sec_xml <- c(sec_xml,
        paste0('<assessmentSection identifier="', test_id_exam_j,
          '" fixed="false" title="', stj2, '" visible="', vis, '">'),
        paste0('<ordering shuffle="', if(!identical(shufflesections, FALSE)) 'true' else 'false', '"/>')
      )
      for(i in 1:length(sec_xml_mat[, j])) {
        sec_xml <- c(sec_xml,
          paste0('<assessmentSection identifier="', paste0(test_id_exam_j, "_exercise_", i),
            '" fixed="false" title="', qtitle[i],
            '" visible="', if(is.null(qtitle[i]) || (qtitle[i] == "")) 'false' else 'true', '">'),
          sec_xml_mat[i, j],
          '</assessmentSection>'
        )
      }
      sec_xml <- c(sec_xml, '</assessmentSection>')
    }
    sec_xml <- c(sec_xml, '</assessmentSection>')
  }

  ## to shuffle sections an extra section layer must be inserted
  ## for now: use same specification for the outer section as for the inner sections
  if(!identical(shufflesections, FALSE) & !is_exam) {
     shufflesections <- if(identical(shufflesections, TRUE)) "" else as.character(shufflesections)
     sec_outer_xml <- section_xml[1L]
     sec_outer_xml <- gsub("##SectionId##", paste(test_id, 'part1', 'sections', sep = '_'), sec_outer_xml, fixed = TRUE)
     sec_outer_xml <- gsub("##SectionTitle##", shufflesections, sec_outer_xml, fixed = TRUE)
     sec_xml <- c(
       sec_outer_xml,
       '<ordering shuffle="true"/>',
       sec_xml,
       '</assessmentSection>'
     )
  }

  manifest_xml <- gsub('##AssessmentId##',
    test_id, manifest_xml, fixed = TRUE)
  manifest_xml <- gsub('##AssessmentTitle##',
    name, manifest_xml, fixed = TRUE)
  manifest_xml <- gsub('##ManifestItemDependencies##',
    paste(sec_items_D, collapse = '\n'), manifest_xml, fixed = TRUE)
  manifest_xml <- gsub('##ManifestItemRessources##',
    paste(sec_items_R, collapse = '\n'), manifest_xml, fixed = TRUE)
  manifest_xml <- gsub("##AssessmentDescription##", adescription, manifest_xml, fixed = TRUE)
  manifest_xml <- gsub("##Date##", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), manifest_xml, fixed = TRUE)

  ## warn if solutions could be copied by participants
  if(any(maxattempts != 1L) && any(c("incorrect", "partial") %in%  solutionswitch)) {
    warning("if solutionswitch is TRUE, maxattempts should typically be 1 so that the solution cannot be copied by participants")
  }

  assessment_xml <- gsub('##AssessmentId##', test_id, assessment_xml, fixed = TRUE)
  assessment_xml <- gsub('##TestpartId##', paste(test_id, 'part1', sep = '_'), assessment_xml, fixed = TRUE)
  assessment_xml <- gsub('##TestTitle##', name, assessment_xml, fixed = TRUE)
  assessment_xml <- gsub('##AssessmentSections##', paste(sec_xml, collapse = '\n'), assessment_xml, fixed = TRUE)
  assessment_xml <- gsub('##Score##', "0.0", assessment_xml, fixed = TRUE) ## FIXME: default score?
  assessment_xml <- gsub('##MaxScore##', maxscore, assessment_xml, fixed = TRUE)

  if(!is.null(cutvalue) && is.na(cutvalue)) cutvalue <- NULL
  if(!is.null(cutvalue) ) {
    j <- grep("</outcomeDeclaration>", assessment_xml, fixed = TRUE)
    j <- j[length(j)]
    assessment_xml[j] <- paste('</outcomeDeclaration>',
      '<outcomeDeclaration identifier="PASS" cardinality="single" baseType="boolean">',
      '<defaultValue>',
      '<value>false</value>',
      '</defaultValue>',
      '</outcomeDeclaration>',
      sep = '\n'
    )
    j <- grep("</setOutcomeValue>", assessment_xml, fixed = TRUE)
    j <- j[length(j)]
    assessment_xml[j] <- paste(
      '</setOutcomeValue>',
      '<outcomeCondition>',
      '<outcomeIf>',
      '<gte>',
      '<sum>',
      '<testVariables variableIdentifier="SCORE"/>',
      '</sum>',
      '<baseValue baseType="float">##CutValue##</baseValue>',
      '</gte>',
      '<setOutcomeValue identifier="PASS">',
      '<baseValue baseType="boolean">true</baseValue>',
      '</setOutcomeValue>',
      '</outcomeIf>',
      '<outcomeElse>',
      '<setOutcomeValue identifier="PASS">',
      '<baseValue baseType="boolean">false</baseValue>',
      '</setOutcomeValue>',
      '</outcomeElse>',
      '</outcomeCondition>',
      sep = '\n'
    )
    assessment_xml <- gsub('##CutValue##', round(as.numeric(cutvalue), digits = 8), assessment_xml, fixed = TRUE)
  }

  assessment_xml <- gsub('##MaxAttempts##', round(as.numeric(maxattempts[1L])), assessment_xml, fixed = TRUE)
  assessment_xml <- gsub('##ShowSolution##', if(any(c("correct", "incorrect", "partial", "summary") %in% solutionswitch)) 'true' else 'false', assessment_xml, fixed = TRUE)
  assessment_xml <- gsub('##NavigationMode##', match.arg(navigation, c("nonlinear", "linear")), assessment_xml, fixed = TRUE)
  assessment_xml <- gsub('##AllowComment##', if(allowcomment) 'true' else 'false', assessment_xml, fixed = TRUE)
  assessment_xml <- gsub('##AllowSkipping##', if(allowskipping) 'true' else 'false', assessment_xml, fixed = TRUE)
  assessment_xml <- gsub('##AllowReview##', if(allowreview) 'true' else 'false', assessment_xml, fixed = TRUE)

  ## assessment duration provided in minutes
  if(!is.null(duration)) {
    dursecs <- round(duration * 60)
    duration <- paste('<timeLimits maxTime="', dursecs, '"/>', sep = '')
  } else {
    duration <- ""
  }

  assessment_xml <- gsub('##TimeLimits##', duration, assessment_xml, fixed = TRUE)

  ## write xmls to dir
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', manifest_xml),
    file.path(test_dir, "imsmanifest.xml"))
  if(length(assessment_xml) > 0L) {
    writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', assessment_xml),
      file.path(test_dir, paste(test_id, "xml", sep = ".")))
  }

  ## include further files
  if(!is.null(include)) {
    if(is.list(include) && !is.null(names(include))) {
      for(i in names(include)) writeLines(include[[i]], file.path(test_dir, i))
    } else if(is.character(include) && all(file.exists(include))) {
      ## FIXME: not just absolute paths, but also support include in
      ## original working directory or edir
      ## N: should work like this
      if(any(!file.exists(include))) {
        if(all(file.exists(file.path(edir, include))))
          include <- file.path(edir, include)
      }
      if(any(!file.exists(include))) {
        owd <- getwd()
        if(all(file.exists(file.path(owd, include))))
          include <- file.path(owd, include)
      }
      file.copy(include, file.path(test_dir, basename(include)))
    } else {
      warning("ignoring 'include' argument due to unknown specification")
    }
  }

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


## QTI 2.1 item body constructor function
make_itembody_qti21 <- function(shuffle = FALSE,
  defaultval = NULL, minvalue = NULL, maxvalue = NULL, enumerate = FALSE,
  digits = NULL, tolerance = is.null(digits), maxchars = 12,
  eval = list(partial = TRUE, rule = "false2", negative = FALSE), solutionswitch = TRUE,
  casesensitive = TRUE, cloze_schoice_display = c("auto", "buttons", "dropdown"),
  copypaste = TRUE)
{
  cloze_schoice_display <- if(is.null(cloze_schoice_display)) "auto" else match.arg(cloze_schoice_display, c("auto", "buttons", "dropdown"))

  solutionswitch <- sol_switch(solutionswitch)

  function(x) {
    ## how many points?
    points <- if(is.null(x$metainfo$points)) 1 else x$metainfo$points

    ## process block-level elements? Needed for TtH/TtM output.
    pbl <- x$converter %in% c("ttm", "tth")

    ## QTI 2.1 flavor; especially whether it is for Ans output or not (e.g., OpenOlat)
    flavor <- x$flavor

    ## how many questions
    solution <- if(!is.list(x$metainfo$solution)) {
      list(x$metainfo$solution)
    } else {
      x$metainfo$solution
    }
    n <- length(solution)

    ## exercise (cloze)type
    type <- x$metainfo$type
    cloze <- type == "cloze"
    if(is.null(minvalue) & cloze) minvalue <- 0

    ## handle file/essay cloze types, use string to get evaluation policy right
    ## for strings with multiple file/essay fields, treat as cloze
    is_essay <- upfile <- rep.int(FALSE, n)
    upids <- rep.int(NA, n)
    if(!is.null(x$metainfo$essay_copypaste))
      copypaste <- x$metainfo$essay_copypaste
    if(!isFALSE(copypaste) && !isTRUE(copypaste)) {
      copypaste <- TRUE
    }
    if(cloze) {
      type <- x$metainfo$clozetype
      is_essay <- type == "essay"
      upfile <- type == "file"
      type[type %in% c("file", "essay")] <- "string"
    } else if(all(type == "string")) {
      if(!is.null(x$metainfo$stringtype)) {
        is_essay <- x$metainfo$stringtype == "essay"
        upfile <- x$metainfo$stringtype == "file"
        if(length(x$metainfo$stringtype) > 1L) {
          cloze <- TRUE
          n <- length(x$metainfo$stringtype)
          type <- rep.int("string", n)
          solution <- x$metainfo$solution <- rep(solution, length.out = n)
        }
      }
    }

    ## question list
    questionlist <- if(!is.list(x$questionlist)) {
      if(cloze) {
        g <- rep(seq_along(x$metainfo$solution), sapply(x$metainfo$solution, length))
        if(!is.null(x$questionlist)) {
          split(x$questionlist, g)
        } else {
          NULL
        }
      } else list(x$questionlist)
    } else x$questionlist
    if(length(questionlist) < 1) questionlist <- NULL
    if(!is.null(questionlist)) {
      for(i in 1:length(questionlist)) {
        if(length(questionlist[[i]]) < 1)
          questionlist[[i]] <- NA
      }
    }

    ## tolerance(s)
    tol <- if(!is.list(x$metainfo$tolerance)) {
      if(cloze) as.list(x$metainfo$tolerance) else list(x$metainfo$tolerance)
    } else x$metainfo$tolerance
    tol <- rep(tol, length.out = n)

    ## points
    if((length(points) == 1L) & cloze) points <- points / n
    q_points <- rep(points, length.out = n)
    if(cloze) points <- sum(q_points)

    ## evaluation policy
    if(is.null(eval) | length(eval) < 1L) {
      eval <- exams_eval()
      eval <- rep(list(eval), length.out = n)
    } else {
      if(!is.list(eval)) stop("'eval' needs to specify a list!")
      if(any(c("partial", "negative", "rule") %in% names(eval))) {
        eval <- rep(list(eval), length.out = n)
      } else {
        for(i in 1:n) {
          if(is.null(eval[[i]]))
            eval[[i]] <- exams_eval()
        }
      }
    }
    names(eval) <- paste0(type, ".", 1:n)

    for(i in 1:n) {
      if(type[i] %in% c("num", "string", "schoice"))
        eval[[i]]$partial <- FALSE
      others <- names(eval[[i]])[!names(eval[[i]]) %in% c("partial", "negative", "rule")]
      if(length(others))
        others <- eval[[i]][others]
      eval[[i]] <- eval[[i]][match(c("partial", "negative", "rule"), names(eval[[i]]), nomatch = 0)]
      eval[[i]] <- do.call("exams_eval", eval[[i]])
      if(length(others))
        eval[[i]][names(others)] <- others
    }

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

    letters2 <- c(letters,
      paste0(rep(letters, each = length(letters)), rep(letters, length(letters))))

    ## start item presentation
    ## and insert question
    xml <- paste('<assessmentItem xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1 http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_v2p1p1.xsd http://www.w3.org/1998/Math/MathML http://www.w3.org/Math/XMLSchema/mathml2/mathml2.xsd" identifier="', x$metainfo$id, '" title="', x$metainfo$name, '" adaptive="false" timeDependent="false" xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">', sep = '')

    ## cycle trough all questions
    ids <- el <- pv <- mv <- list()
    pdiff <- rep(NA, n)
    for(i in 1:n) {
      ## evaluate points for each question
      pv[[i]] <- eval[[i]]$pointvec(solution[[i]], type = type[i])
      pv[[i]]["pos"] <- pv[[i]]["pos"] * q_points[i]
      pv[[i]]["neg"] <- pv[[i]]["neg"] * q_points[i]

      ## setting minimum scores
      mv[[i]] <- if(eval[[i]]$negative) {
        -1 * q_points[i]
      } else "0.0"

      ## fix partial = FALSE for mchoice.
      if(type[i] == "mchoice" & !eval[[i]]$partial) {
        pv[[i]]["pos"] <- pv[[i]]["pos"] / sum(solution[[i]])
        pv[[i]]["neg"] <- -1 * q_points[i]
      }

      ## fix no correct solution, mchoice, partial = TRUE
      if(type[i] == "mchoice") {
        if(all(!solution[[i]])) {
          if(eval[[i]]$partial & !(eval[[i]]$rule == "all"))
            pv[[i]]["neg"] <- -1 * q_points[i] / length(solution[[i]])
        }
      }

      ## fix rounding problem.
      if(type[i] == "mchoice") {
        pv[[i]]["pos"] <- round(pv[[i]]["pos"], digits = 8)
        if(pv[[i]]["pos"] * sum(solution[[i]]) < q_points[i]) {
          pdiff[i] <- q_points[i] - pv[[i]]["pos"] * sum(solution[[i]])
        }
      }
    }

    mmatrix <- if(length(i <- grep("matrix", names(x$metainfo)))) {
      x$metainfo[[i]]
    } else NULL

    ## extract solution. (FIXME: again? what is the difference between "solution" and "msol"?)
    msol <- x$metainfo$solution
    if(!is.list(msol)) msol <- list(msol)

    ## small helper to remove too many ".
    cch <- function(x) {
      gsub("'", '&apos;', gsub('"', '&quot;', x))
    }

    for(i in 1:n) {
      ## get item id
      iid <- x$metainfo$id

      ## generate ids
      if(is.null(mmatrix)) {
        ids[[i]] <- list("response" = paste(iid, "RESPONSE", i, make_id(7), sep = "_"),
          "questions" = paste(iid, 1L:length(msol[[i]]), make_id(10, length(msol[[i]])), sep = "_"))
      } else {
        qs <- strsplit(x$questionlist, mmatrix, fixed = TRUE)
        mrows <- unique(sapply(qs, function(x) { x[1] }))
        mcols <- unique(sapply(qs, function(x) { x[2] }))
        ids[[i]] <- list("response" = paste(iid, "RESPONSE", i, make_id(7), sep = "_"),
          "questions" = paste(iid, 1L:length(msol[[i]]), make_id(10, length(msol[[i]])), sep = "_"),
          "mmatrix_matches" = matrix(msol[[i]], nrow = length(mrows), byrow = TRUE)
        )
        ids[[i]]$mmatrix_questions <- list(
          "rows" = paste(iid, 1L:length(mrows), make_id(10, length(mrows)), sep = "_"),
          "cols" = paste(iid, 1L:length(mcols), make_id(10, length(mcols)), sep = "_")
        )
        rownames(ids[[i]]$mmatrix_matches) <- mrows
        colnames(ids[[i]]$mmatrix_matches) <- mcols
        for(j in seq_along(ids[[i]]$mmatrix_questions$rows)) {
          for(jj in seq_along(ids[[i]]$mmatrix_questions$cols)) {
            ids[[i]]$mmatrix_pairs <- c(ids[[i]]$mmatrix_pairs, paste(ids[[i]]$mmatrix_questions$rows[j], ids[[i]]$mmatrix_questions$cols[jj]))
          }
        }
      }

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

        xml <- c(xml, '</correctResponse>',
          paste('<mapping defaultValue="', if(is.null(defaultval)) 0 else defaultval,
            '" lowerBound="', mv[[i]], '">', sep = '')
        )

        for(j in seq_along(solution[[i]])) {
          xml <- c(xml,
            paste('<mapEntry mapKey="', cch(if(is.null(mmatrix)) ids[[i]]$questions[j] else ids[[i]]$mmatrix_pairs[j]), '" mappedValue="',
              if(solution[[i]][j]) {
                if(!is.na(pdiff[i])) {
                  tpdiff <- pdiff[i]
                  pv[[i]]["pos"] + tpdiff
                } else pv[[i]]["pos"]
              } else {
                pv[[i]]["neg"]
              }, '"/>', sep = '')
          )
        }
        xml <- c(xml, '</mapping>', '</responseDeclaration>')
      }

      ## numeric responses
      if(type[i] == "num") {
        xml <- c(xml,
          sprintf('<responseDeclaration identifier="%s" cardinality="single" baseType="%s">',
            ids[[i]]$response, if(flavor == "ans") "string" else "float"),
          '<correctResponse>',
          sprintf('<value>%s</value>', solution[[i]]),
          '</correctResponse>',
          '</responseDeclaration>'
        )
      }

      ## string responses
      if(type[i] == "string") {
        if((length(maxchars[[i]]) > 1) & sum(!is.na(maxchars[[i]])) == 1 & !is_essay[i] & !upfile[i]) {
          xml <- c(xml,
            paste('<responseDeclaration identifier="', ids[[i]]$response, '" cardinality="single" baseType="string">', sep = ''),
          '<correctResponse>',
            paste('<value>', solution[[i]], '</value>', sep = ''),
            '</correctResponse>',
            paste('<mapping defaultValue="', if(is.null(defaultval)) 0 else defaultval, '">', sep = ''),
            paste('<mapEntry mapKey="', cch(solution[[i]]), '" mappedValue="', pv[[i]]["pos"], '" caseSensitive="', if(casesensitive) 'true' else 'false', '"/>', sep = ''),
            '</mapping>',
            '</responseDeclaration>'
          )
        } else {
          if(!upfile[i]) {
            is_essay[i] <- TRUE
            if(sum(!is.na(maxchars[[i]])) == 1) {
              maxchars[[i]] <- c(1000, 10, 50)
            }
            ## Essay type questions.
            xml <- c(xml,
              paste('<responseDeclaration identifier="', ids[[i]]$response,
                '" cardinality="single" baseType="string">', sep = ''),
              ## '<correctResponse>', ## N, correct response seems not to work?
              ## if(pbl) process_html_pbl(x$solution) else x$solution,
              ## paste('<value>', solution[[i]], '</value>', sep = ''),
              ## '</correctResponse>',
              '</responseDeclaration>'
            )
          }
        }
        if(upfile[i]) {
          xml <- c(xml, paste0('<responseDeclaration identifier="',
            ids[[i]]$response,'" cardinality="single" baseType="file">'), '</responseDeclaration>')
        }
      }
    }

    if("hint" %in% solutionswitch) {
      xml <- c(xml,
        '<responseDeclaration identifier="HINTREQUEST" cardinality="single" baseType="boolean"/>'
      )
    }

    if(is.null(minvalue)) ## FIXME: switch for minvalue for full question?
      minvalue <- sum(as.numeric(unlist(mv)))

    xml <- c(xml,
      paste('<outcomeDeclaration identifier="SCORE" cardinality="single" baseType="float" ',
        'normalMaximum="', sum(q_points), '" normalMinimum="', minvalue, '">', sep = ''),
      '<defaultValue>',
      '<value>0.0</value>',
      '</defaultValue>',
      '</outcomeDeclaration>',
      '<outcomeDeclaration identifier="MAXSCORE" cardinality="single" baseType="float">',
      '<defaultValue>',
      paste('<value>', if(is.null(maxvalue)) sum(q_points) else maxvalue, '</value>', sep = ''),
      '</defaultValue>',
      '</outcomeDeclaration>',
      '<outcomeDeclaration identifier="FEEDBACKBASIC" cardinality="single" baseType="identifier" view="testConstructor">',
      '<defaultValue>',
      '<value>empty</value>',
      '</defaultValue>',
      '</outcomeDeclaration>',
      '<outcomeDeclaration identifier="FEEDBACKMODAL" cardinality="multiple" baseType="identifier" view="testConstructor"/>',
      '<outcomeDeclaration identifier="SOLUTIONMODAL" cardinality="single" baseType="identifier" view="testConstructor"/>'
    )

    xml <- c(xml,
      '<outcomeDeclaration identifier="MINSCORE" cardinality="single" baseType="float">',
      '<defaultValue>',
      paste('<value baseType="float">', minvalue, '</value>', sep = ''),
      '</defaultValue>',
      '</outcomeDeclaration>'
    )

    for(i in 1:n) {
      ## score, minscore for each question.
      xml <- c(xml,
        paste0('<outcomeDeclaration identifier="SCORE_RESPONSE_', i, '" cardinality="single" baseType="float">'),
        '<defaultValue>',
        '<value>0.0</value>',
        '</defaultValue>',
        '</outcomeDeclaration>',
        paste0('<outcomeDeclaration identifier="MINSCORE_RESPONSE_', i, '" cardinality="single" baseType="float">'),
        '<defaultValue>',
        paste0('<value>', mv[[i]], '</value>'),
        '</defaultValue>',
        '</outcomeDeclaration>'
      )
    }

    if("hint" %in% solutionswitch) {
      xml <- c(xml,
        '<outcomeDeclaration identifier="HINTFEEDBACKMODAL" cardinality="single" baseType="identifier"/>'
      )
    }

    ## starting the itembody
    xml <- c(xml, '<itemBody>')
    if(!is.null(x$question))
      xml <- c(xml, if(pbl) process_html_pbl(x$question) else x$question)

    for(i in 1:n) {
      ans <- any(grepl(paste0("##ANSWER", i, "##"), xml))
      if(length(grep("choice", type[i]))) {
        if(is.null(mmatrix)) {
          txml <- paste('<choiceInteraction responseIdentifier="', ids[[i]]$response,
              '" shuffle="', if(shuffle) 'true' else 'false','" maxChoices="',
              if(type[i] == "schoice") "1" else "0", '">', sep = '')
          for(j in seq_along(solution[[i]])) {
            txml <- c(txml, paste('<simpleChoice identifier="', ids[[i]]$questions[j], '">', sep = ''),
              if(!ans) '<p>' else NULL,
              paste(if(enumerate & !ans) {
                paste(letters2[if(cloze) i else j], ".",
                  if(cloze && length(solution[[i]]) > 1) paste(j, ".", sep = "") else NULL,
                    sep = "")
              } else NULL, questionlist[[i]][j]),
              if(!ans) '</p>' else NULL,
              '</simpleChoice>'
            )
          }
          txml <- c(txml, '</choiceInteraction>')
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
            if(!ans) '<p>' else NULL,
              if(!is.null(questionlist[[i]][j])) {
                paste(if(enumerate & n > 1 & !ans) {
                  paste(letters2[if(cloze) i else j], ".",
                    if(cloze && length(solution[[i]]) > 1) paste(j, ".", sep = "") else NULL,
                      sep = "")
                } else NULL, if(!is.na(questionlist[[i]][j])) questionlist[[i]][j] else NULL)
              },
            sprintf('<textEntryInteraction responseIdentifier="%s"%s/>',
              ids[[i]]$response, 
              if(flavor == "inspera") sprintf(' expectedLength="%s"', pmax(ceiling(1.2 * nchar(format(x$metainfo$solution))), 20)) else ''),
            if(!ans) '</p>' else NULL
          )
        }
      }
      if(type[i] == "string") {
        if(((length(maxchars[[i]]) > 1) & sum(is.na(maxchars[[i]])) < 1) | upfile[i]) {
          ## Essay type questions.
          txml <- c(
            if(!ans) '<p>' else NULL,
             if(!is.null(questionlist[[i]])) {
                paste(if(enumerate & n > 1 & !ans) {
                  paste(letters2[if(cloze) i else j], ".",
                    if(cloze && length(solution[[i]]) > 1) paste(1, ".", sep = "") else NULL,
                      sep = "")
                } else NULL, if(!is.na(questionlist[[i]])) questionlist[[i]] else NULL)
             },
             if(!ans) '</p>' else NULL,
             if(!upfile[i]) {
               paste('<extendedTextInteraction', if(!copypaste) ' class="essay-nocopypaste" ' else ' ',
                if(flavor == "inspera") ' format="xhtml" ' else '',
                'responseIdentifier="', ids[[i]]$response,
                '" minStrings="0" ', if(!is.na(maxchars[[i]][1])) {
                    paste0(' expectedLength="', maxchars[[i]][1], '"')
                  } else NULL, if(!is.na(maxchars[[i]][2])) {
                    paste(' expectedLines="', maxchars[[i]][2], '" ', sep = '')
                  } else NULL, '/>', sep = '')
             } else {
               paste0('<uploadInteraction responseIdentifier="', ids[[i]]$response, '"/>')
             }
          )
        } else {
          for(j in seq_along(solution[[i]])) {
            txml <- c(
              if(!ans) '<p>' else NULL,
               if(!is.null(questionlist[[i]][j])) {
                  paste(if(enumerate & n > 1 & !ans) {
                    paste(letters2[if(cloze) i else j], ".",
                      if(cloze && length(solution[[i]]) > 1) paste(j, ".", sep = "") else NULL,
                        sep = "")
                  } else NULL, if(!is.na(questionlist[[i]][j])) questionlist[[i]][j] else NULL)
               },
               paste('<textEntryInteraction responseIdentifier="', ids[[i]]$response,
                if(!is.na(maxchars[[i]][1])) {
                  paste0('" expectedLength="', maxchars[[i]][1])
                } else NULL, if(!is.na(maxchars[[i]][2])) {
                  paste0('" expectedLines="', maxchars[[i]][2])
                } else NULL, '"/>', sep = ''),
              if(!ans) '</p>' else NULL
            )
          }
        }
      }
      if(ans) {
        txml <- paste(txml, collapse = '\n')
        ansi <- paste0("##ANSWER", i, "##")
        ii <- grep(ansi, xml)
        if(length(ii) > 1L)
          stop(paste0("multiple ##ANSWER", i, "## tags found!"))

        is_in_p <- grepl(paste0("<p>", ansi, "</p>"), xml[ii], fixed = TRUE)

        if(is_in_p && !grepl("choice", type[i])) {
          p_check <-
            any(grepl("<extendedTextInteraction", txml, fixed = TRUE)) |
            any(grepl("<uploadInteraction", txml, fixed = TRUE))
          if(p_check) {
            xml <- gsub(paste0("<p>##ANSWER", i, "##</p>"), txml, xml, fixed = TRUE)
          } else {
            xml <- gsub(paste0("##ANSWER", i, "##"), txml, xml, fixed = TRUE)
          }
         } else {
           if(grepl("choice", type[i])) {
             csd <- cloze_schoice_display
             if(type[i] == "schoice" && csd == "auto") {
               ## check for math tags
               ot <- c("\\(", "<math ", "<span class=\"math\"")
               ct <- c("\\)", "</math>", "</span>")
               has_math <- any(sapply(seq_along(ot), function(j) any(
                 grepl(ot[j], questionlist[[i]], fixed = TRUE) & grepl(ct[j], questionlist[[i]], fixed = TRUE))))
               csd <- if(is_in_p | has_math) "buttons" else "dropdown"
             }
             if((csd == "buttons") | (type[i] == "mchoice")) {
               xml <- if(!is_in_p) {
                 gsub(ansi, paste0("</p>", ansi, "<p>"), xml, fixed = TRUE)
               } else {
                 gsub(paste0("<p>", ansi, "</p>"), ansi, xml, fixed = TRUE)
               }
             } else {
               if(type[i] == "schoice") {
                 txml <- gsub('choiceInteraction', 'inlineChoiceInteraction', txml)
                 txml <- gsub('simpleChoice', 'inlineChoice', txml)
                 nch <- sapply(questionlist[[i]], nchar)
                 ql <- unique(questionlist[[i]][order(nch)])
                 for(ijj in ql) {
                   if(any(grepl("</", ijj, fixed = TRUE))) {
                     txml <- gsub(ijj, paste0("<![CDATA[", ijj, "]]>"), txml, fixed = TRUE)
                   }
                 }
               }
             }
           }
           xml <- gsub(ansi, txml, xml, fixed = TRUE)
         }
      } else {
        xml <- c(xml, txml)
      }
    }

    if("hint" %in% solutionswitch) {
      xml <- c(xml,
        '<p>',
        '<endAttemptInteraction responseIdentifier="HINTREQUEST" title="SolutionHint"/>',
        '</p>'
      )
    }

    ## close itembody
    xml <- c(xml, '</itemBody>')

    ## response processing
    xml <- c(xml, '<responseProcessing>')

    ## score each answer
    for(i in 1:n) {
      if(type[i] == "num") {
        if(flavor == "ans") {
          xml <- c(xml,
            '<responseCondition>',
            '<responseIf>',
            sprintf('<equal tolerance="%s" toleranceMode="absolute">', max(tol[[i]])),
            sprintf('<variable identifier="%s"/>', ids[[i]]$response),
            sprintf('<baseValue baseType="float">%s</baseValue>', solution[[i]]),
            '</equal>',
            '<setOutcomeValue identifier="SCORE">',
            sprintf('<baseValue baseType="float">%s</baseValue>', pv[[i]]["pos"]),
            '</setOutcomeValue>',
            '</responseIf>',
            '</responseCondition>'
          )
        } else {
          xml <- c(xml,
            '<responseCondition>',
            '<responseIf>',
            paste0('<equal toleranceMode="absolute" tolerance="', max(tol[[i]]), ' ',
              max(tol[[i]]),'" includeLowerBound="true" includeUpperBound="true">'),
            paste0('<variable identifier="', ids[[i]]$response, '"/>'),
            paste0('<correct identifier="', ids[[i]]$response, '"/>'),
            '</equal>',
            paste0('<setOutcomeValue identifier="SCORE_RESPONSE_', i, '">'),
            '<sum>',
            paste0('<variable identifier="SCORE_RESPONSE_', i, '"/>'),
            paste0('<baseValue baseType="float">', pv[[i]]["pos"], '</baseValue>'),
            '</sum>',
            '</setOutcomeValue>',
            '</responseIf>',
            '</responseCondition>'
          )
        }
      }

      if(type[i] %in% c("schoice", "mchoice", "string")) {
        if(!(is_essay[i] | upfile[i])) {
          xml <- c(xml,
            paste0('<setOutcomeValue identifier="SCORE_RESPONSE_', i, '">'),
            '<sum>',
            paste0('<variable identifier="SCORE_RESPONSE_', i, '"/>'),
            paste0('<mapResponse identifier="', ids[[i]]$response, '"/>'),
            '</sum>',
            '</setOutcomeValue>'
          )

          ## Adapt points for mchoice.
          ## Case no correct answers.
          if(type[i] == "mchoice") {
            if(sum(solution[[i]]) < 1) {
              xml <- c(xml,
                '<responseCondition>',
                '<responseIf>',
                '<isNull>',
                paste0('<variable identifier="', ids[[i]]$response, '"/>'),
                '</isNull>',
                paste0('<setOutcomeValue identifier="SCORE_RESPONSE_', i, '">'),
                paste0('<baseValue baseType="float">', q_points[i], '</baseValue>'),
                '</setOutcomeValue>',
                '</responseIf>',
                '</responseCondition>'
              )
            }
          }
        }
      }
    }

    ## negative points
    for(i in 1:n) {
      n_points <- if(eval[[i]]$negative) pv[[i]]["neg"] else 0.0

      if(!grepl("choice", type[i])) {
        xml <- c(xml,
          '<responseCondition>',
          '<responseIf>',
          '<and>',
          '<not>',
          '<isNull>',
          paste0('<variable identifier="', ids[[i]]$response, '"/>'),
          '</isNull>',
          '</not>',
          '<lt>',
          paste0('<variable identifier="SCORE_RESPONSE_', i, '"/>'),
          paste0('<baseValue baseType="float">', pv[[i]]["pos"], '</baseValue>'),
          '</lt>',
          '</and>',
          paste0('<setOutcomeValue identifier="SCORE_RESPONSE_', i, '">'),
          '<sum>',
          paste0('<variable identifier="SCORE_RESPONSE_', i, '"/>'),
          paste0('<baseValue baseType="float">', n_points, '</baseValue>'),
          '</sum>',
          '</setOutcomeValue>',
          '</responseIf>',  
          '</responseCondition>'
        )
      }

      if((type[i] == "mchoice") & !eval[[i]]$partial) {
        xml <- c(xml,
          '<responseCondition>',
          '<responseIf>',
          '<and>',
          '<not>',
          '<isNull>',
          paste0('<variable identifier="', ids[[i]]$response, '"/>'),
          '</isNull>',
          '</not>',
          '<lt>',
          paste0('<variable identifier="SCORE_RESPONSE_', i, '"/>'),
          paste0('<baseValue baseType="float">', q_points[i], '</baseValue>'),
          '</lt>',
          '</and>',
          paste0('<setOutcomeValue identifier="SCORE_RESPONSE_', i, '">'),
          '<sum>',
          paste0('<variable identifier="SCORE_RESPONSE_', i, '"/>'),
          paste0('<baseValue baseType="float">', -1 * q_points[i], '</baseValue>'),
          '</sum>',
          '</setOutcomeValue>',
          '</responseIf>',  
          '</responseCondition>'
        )
      }
    }

    ## check minvalues for each question
    for(i in 1:n) {
      xml <- c(xml,
        '<responseCondition>',
        '<responseIf>',
        '<lt>',
        paste0('<variable identifier="SCORE_RESPONSE_', i, '"/>'),
        paste0('<variable identifier="MINSCORE_RESPONSE_', i, '"/>'),
        '</lt>',
        paste0('<setOutcomeValue identifier="SCORE_RESPONSE_', i, '">'),
        paste0('<variable identifier="MINSCORE_RESPONSE_', i, '"/>'),
        '</setOutcomeValue>',
        '</responseIf>',
        '</responseCondition>'
      )
    }

    ## FIXME: create a switch?
    not_answered_points <- if(is.null(eval[[i]]$not_answered)) {
      0.0
    } else {
      as.numeric(eval[[i]]$not_answered)
    }

    ## not answered points single
    for(i in 1:n) {
      xml <- c(xml,
        '<responseCondition>',
        '<responseIf>',
        '<isNull>',
        paste('<variable identifier="', ids[[i]]$response, '"/>', sep = ''),
        '</isNull>',
        paste0('<setOutcomeValue identifier="SCORE_RESPONSE_', i, '">'),
        '<sum>',
        paste0('<variable identifier="SCORE_RESPONSE_', i, '"/>'),
        paste0('<baseValue baseType="float">', not_answered_points, '</baseValue>'),
        '</sum>',
        '</setOutcomeValue>',
        '</responseIf>',
        '</responseCondition>'
      )
    }

    ## sum all points.
    xml <- c(xml, '<setOutcomeValue identifier="SCORE">', '<sum>')
    for(i in 1:n)
      xml <- c(xml, paste0('<variable identifier="SCORE_RESPONSE_', i, '"/>'))
    xml <- c(xml, '</sum>', '</setOutcomeValue>')

    ## check minscore
    xml <- c(xml, 
      '<responseCondition>',
      '<responseIf>',
      '<lt>',
      '<variable identifier="SCORE"/>',
      '<variable identifier="MINSCORE"/>',
      '</lt>',
      '<setOutcomeValue identifier="SCORE">',
      '<variable identifier="MINSCORE"/>',
      '</setOutcomeValue>',
      '</responseIf>',
      '</responseCondition>'
    )

    if(any(c("incorrect", "summary", "partial", "hint") %in% solutionswitch)) {

      if("incorrect" %in% solutionswitch) {
        fid <- make_id(9, 1)
        xml <- c(xml,
          '<responseCondition>',
          '<responseIf>',
          '<lt>',
          '<variable identifier="SCORE"/>',
          paste('<baseValue baseType="float">', points, '</baseValue>', sep = ''),
          '</lt>',
          '<setOutcomeValue identifier="FEEDBACKMODAL">',
          '<multiple>',
          '<variable identifier="FEEDBACKMODAL"/>',
          paste('<baseValue baseType="identifier">Feedback', fid, '</baseValue>', sep = ''),
          '</multiple>',
          '</setOutcomeValue>',
          '</responseIf>',
          '</responseCondition>'
        )
      }

      ## create solution
      xsolution <- x$solution
      if(!is.null(x$solutionlist)) {
        if(!all(is.na(x$solutionlist))) {
          xsolution <- c(xsolution, if(length(xsolution)) "<br />" else NULL)
          xsolution <- c(xsolution, if(enumerate) '<ol type = "a">' else '<ul>')
          if(cloze) {
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
      if(pbl) xsolution <- process_html_pbl(xsolution)
      if(flavor == "ans") xsolution <- fix_pre_lines(xsolution, sep = "</code><br/><code>")
    }

    if("summary" %in% solutionswitch) {
      fid_m <- make_id(9, 1)
      xml <- c(xml,
        '<responseCondition>',
        '<responseIf>',
        '<lt>',
        '<variable identifier="SCORE"/>',
        paste('<baseValue baseType="float">', points, '</baseValue>', sep = ''),
        '</lt>',
        '<setOutcomeValue identifier="SOLUTIONMODAL">',
        paste('<baseValue baseType="identifier">Feedback', fid_m, '</baseValue>', sep = ''),
        '</setOutcomeValue>',
        '</responseIf>',
        '</responseCondition>'
      )
    }

    if("partial" %in% solutionswitch) {
      fid_p <- make_id(9, 1)
      xml <- c(xml,
        '<responseCondition>',
        '<responseIf>',
        '<and>',
        '<gt>',
        '<variable identifier="SCORE"/>',
        paste('<baseValue baseType="float">', 0, '</baseValue>', sep = ''),
        '</gt>',
        '<lt>',
        '<variable identifier="SCORE"/>',
        paste('<baseValue baseType="float">', points, '</baseValue>', sep = ''),
        '</lt>',
        '</and>',
        '<setOutcomeValue identifier="FEEDBACKMODAL">',
        '<multiple>',
        '<variable identifier="FEEDBACKMODAL"/>',
        paste('<baseValue baseType="identifier">Feedback', fid_p, '</baseValue>', sep = ''),
        '/<multiple>',
        '</setOutcomeValue>',
        '</responseIf>',
        '</responseCondition>'
      )
    }

    if("hint" %in% solutionswitch) {
      xml <- c(xml,
        '<responseCondition>',
        '<responseIf>',
        '<variable identifier="HINTREQUEST"/>',
        '<setOutcomeValue identifier="HINTFEEDBACKMODAL">',
        '<baseValue baseType="identifier">HINT</baseValue>',
        '</setOutcomeValue>',
        '</responseIf>',
        '</responseCondition>'
      )
    }

    xml <- c(xml, '</responseProcessing>')

    ## solution when wrong
    if("incorrect" %in% solutionswitch) {
      xml <- c(xml,
        if(flavor == "ans") {
          '<modalFeedback outcomeIdentifier="FEEDBACK" showHide="show" identifier="incorrect">'
        } else {
          paste('<modalFeedback identifier="Feedback', fid, '" outcomeIdentifier="FEEDBACKMODAL" showHide="show">', sep = '')        
        },
        xsolution,
        '</modalFeedback>'
      )
    }

    if("partial" %in% solutionswitch) {
      xml <- c(xml,
        if(flavor == "ans") {
          '<modalFeedback outcomeIdentifier="FEEDBACK" showHide="show" identifier="incorrect">'
        } else {
          paste('<modalFeedback identifier="Feedback', fid_p, '" outcomeIdentifier="FEEDBACKMODAL" showHide="show">', sep = '')        
        },
        xsolution,
        '</modalFeedback>'
      )
    }

    if(("summary" %in% solutionswitch) & (flavor != "ans")) {
      xml <- c(xml,
        paste('<modalFeedback identifier="Feedback', fid_m, '" outcomeIdentifier="SOLUTIONMODAL" showHide="show">', sep = ''),       
        xsolution,
        '</modalFeedback>'
      )
    }

    if(("hint" %in% solutionswitch) & (flavor != "ans")) {
      if(is.null(x$metainfo$hint)) {
        hint <- xsolution
      } else {
        hint <- x$metainfo$hint
        if(pbl) hint <- process_html_pbl(hint)
      }
      xml <- c(xml,
        '<modalFeedback identifier="HINT" outcomeIdentifier="HINTFEEDBACKMODAL" showHide="show">',       
        hint,
        '</modalFeedback>'
      )
    }
    
    xml <- c(xml, '</assessmentItem>')
    xml
  }
}


## process solutionswitch
sol_switch <- function(x = TRUE)
{
  if(is.null(x))
    x <- FALSE
  if(is.logical(x)) {
    x <- if(x) "incorrect" else character(0)
  }
  if(!is.character(x))
    x <- as.character(x)
  x <- x[x != ""]
  if(length(x) > 0L) {
    x <- vapply(tolower(x),
      FUN = match.arg, FUN.VALUE = "",
      choices = c("correct", "incorrect", "partial", "summary", "hint"))
  }
  x <- unname(x)
  return(x)
}


## Function to check for block-level elements and <p> tags.
process_html_pbl <- function(x)
{
  ## List of block-level elements from
  ## https://www.w3schools.com/html/html_blocks.asp
  ble <- c(
    "address",
    "article",
    "aside",
    "blockquote",
    "canvas",
    "dd",
    "div",
    "dl",
    "dt",
    "fieldset",
    "figcaption",
    "figure",
    "footer",
    "form",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "header",
    "hr",
    ## "li",
    "main",
    "nav",
    "noscript",
    "ol",
    "output",
    ## "p",
    "pre",
    "section",
    "table",
    "tfoot",
    "ul",
    "video"
  )
  ble <- paste0("<", ble)

  x <- x[x != '<div class="p"><!----></div>']
  x <- gsub('<div class="p"><!----></div>', '', x, fixed = TRUE)

  if(any(grepl("table>", x))) {
    if(!any(grepl("tbody>", x))) {
      patterns <- c('<\\s*table[^>]*>', '<\\s*/\\s*table>')
      replacements <- c('<table><tbody>', '</tbody></table>')
      for(i in seq_along(patterns))
        x <- gsub(patterns[i], replacements[i], x)
    }
  }

  tags <- NULL
  for(b in ble) {
    if(any(grepl(b, x, fixed = TRUE)))
      tags <- c(tags, b)
  }

  if(is.null(tags)) {
    x <- c("<p>", x, "</p>")
  } else {
    x <- paste(x, collapse = "\n")
    for(p in tags) {
      p <- gsub("<", "", p, fixed = TRUE)
      pat <- paste0('(<\\s*', p, '[^>]*>)')
      x <- gsub(pat, '</p>\\1', x, perl = TRUE)
      pat <- paste0('(<\\s*/\\s*', p, '>)')
      x <- gsub(pat, '\\1<p>', x, perl = TRUE)
    }
    x <- paste0('<p>', x, '</p>')
    x <- gsub('<p></p>', '', x, fixed = TRUE)
  }

  return(x)
}

## Check if first element of string is a number.
is_number1 <- function(x)
{
  x <- strsplit(x, "")
  x <- sapply(x, function(z) {
    suppressWarnings(!is.na(as.numeric(z[1])))
  })
  return(x)
}

