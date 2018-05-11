
exams2blackboard <- function(file, n = 1L, nsamp = NULL, dir = ".",
  name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE,
  resolution = 100, width = 4, height = 4, encoding  = "",
  num = NULL, mchoice = NULL, schoice = mchoice, string = NULL, cloze = NULL,
  template = "blackboard",
  pdescription = "This is an item from an item pool.", tdescription = "This is today's test.",
  pinstruction = "Please answer the following question.", tinstruction = "Give an answer to each question.",
  maxattempts = 1, zip = TRUE,
  points = NULL, eval = list(partial = TRUE, negative = FALSE), base64 = FALSE, converter = NULL, ...)
{
  ## default converter is "ttm" if all exercises are Rnw, otherwise "pandoc"
  if(is.null(converter)) {
    converter <- if(any(tolower(tools::file_ext(unlist(file))) == "rmd")) "pandoc" else "ttm"
  }
  ## set up .html transformer
  htmltransform <- make_exercise_transform_html(converter = converter, base64 = base64, ...)

  ## generate the exam
  exm <- xexams(file, n = n, nsamp = nsamp,
    driver = list(
      sweave = list(quiet = quiet, pdf = FALSE, png = TRUE,
        resolution = resolution, width = width, height = height,
        encoding = encoding),
      read = NULL, transform = htmltransform, write = NULL),
    dir = dir, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose)

  ## start .xml assessement creation
  ## get the possible item body functions and options
  itembody <- list(num = num, mchoice = mchoice, schoice = schoice, cloze = cloze, string = string)

  for(i in c("num", "mchoice", "schoice", "cloze", "string")) {
    if(is.null(itembody[[i]])) itembody[[i]] <- list()
    if(is.list(itembody[[i]])) {
      if(is.null(itembody[[i]]$eval))
        itembody[[i]]$eval <- eval
      if(i == "cloze" & is.null(itembody[[i]]$eval$rule))
        itembody[[i]]$eval$rule <- "none"
      itembody[[i]] <- do.call("make_itembody_blackboard", itembody[[i]])
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
  if(length(start <- grep("<questestinterop", xml, fixed = TRUE)) != 1L ||
    length(end <- grep("</questestinterop>", xml, fixed = TRUE)) != 1L) {
    stop(paste("The XML template", template,
      "must contain exactly one opening and closing <questestinterop> tag!"))
  }
  assessment_xml <- xml[start:end]

  if(length(start <- grep("<section>", xml, fixed = TRUE)) != 1L ||
    length(end <- grep("</section>", xml, fixed = TRUE)) != 1L) {
    stop(paste("The XML template", template,
      "must contain exactly one opening and closing <section> tag!"))
  }
  section_xml <- xml[start:end]

  if(length(start <- grep("<item ", xml, fixed = TRUE)) != 1L ||
    length(end <- grep("</item>", xml, fixed = TRUE)) != 1L) {
    stop(paste("The XML template", template,
      "must contain exactly one opening and closing <item> tag!"))
  }
  item_xml <- xml[start:end]

  if(length(start <- grep("<selection_ordering>", xml, fixed = TRUE)) != 1L ||
    length(end <- grep("</selection_ordering>", xml, fixed = TRUE)) != 1L) {
    stop(paste("The XML template", template,
      "must contain exactly one opening and closing <selection_ordering> tag!"))
  }
  ordering_xml <- xml[start:end]

  ## obtain the number of exams and questions
  nx <- length(exm)
  nq <- length(exm[[1L]])

  ## create a name
  if(is.null(name)) name <- file_path_sans_ext(basename(template))

  ## function for internal ids
  make_test_ids <- function(n, type = c("test", "section", "item"))
  {
    switch(type,
      "test" = paste(name, make_id(9), sep = "_"),
      paste(type, formatC(1:n, flag = "0", width = nchar(n)), sep = "_")
    )
  }

  ## generate the test id
  test_id <- make_test_ids(type = "test")

  ## create section ids
  sec_ids <- paste(test_id, make_test_ids(2*nq + 1, type = "section"), sep = "_")

  ## create section/item titles and section description
  if(is.null(pdescription)) pdescription <- ""
  if(is.null(tdescription)) tdescription <- ""
  if(is.null(pinstruction)) pinstruction <- ""
  if(is.null(tinstruction)) tinstruction <- ""

  ## points setting
  if(!is.null(points))
    points <- rep(points, length.out = nq)

  ## create the directory where the test is stored
  dir.create(test_dir <- file.path(tdir, name))

  ## get correct question type spec for blackboard
  ## FIXME: cloze items are not yet available for blackboard
  bb_questiontype <- function(x, item = FALSE) {
    type <- switch(x,
      "mchoice" = "Multiple Answer",
      "schoice" = "Multiple Choice",
      "num" = "Numeric",
      "cloze" = "Cloze",
      "string" = "Fill in the Blank"
    )
    type
  }

  ## function to give right option for <bbmd_negative_points_ind>

  set_negative_points <- function(eval = eval, type = type) {
      if(!eval$negative){
          x <- "N"
      } else if(grepl("choice", type)){
          x <- "Y"
      } else {
          x <- "N"
      }
      x
  }

  ## process maximal number of attempts
  make_integer_tag <- function(x, type, default = 1) {
    if(is.null(x)) x <- Inf
    x <- round(as.numeric(x))
    if(x < default) {
      warning(paste("invalid ", type, " specification, ", type, "=", default, " used", sep = ""))
      x <- default
    }
    if(is.finite(x)) sprintf("%s=\"%i\"", type, x) else ""
  }
  maxattempts <- make_integer_tag(maxattempts, type = "maxattempts", default = 1)

  ## write fixed part of test description file
  test_xml <- assessment_xml
  test_xml <- gsub('##TestTitle##', paste0(name, "-test"), test_xml, fixed = TRUE)
  test_xml <- gsub('##TestIdent##', test_id, test_xml, fixed = TRUE)
  test_xml <- gsub('##MaxTestScore##', sprintf("%d.0", nq), test_xml, fixed = TRUE)
  test_xml <- gsub('##AssessmentDescription##', tinstruction, test_xml, fixed = TRUE)
  test_xml <- gsub('##AssessmentTitle##', tdescription, test_xml, fixed = TRUE)
  test_xml <- gsub('##SectionContent##', paste(section_xml, collapse = "\n"), test_xml, fixed = TRUE)
  test_xml <- gsub('##SectionId##', sec_ids[2*nq + 1], test_xml, fixed = TRUE)
  test_xml <- gsub('##AssessmentType##', "Test", test_xml, fixed = TRUE)
  test_xml <- gsub('##SectionType##', "Subsection", test_xml, fixed = TRUE)
  test_xml <- gsub('##QuestionType##', "Multiple Choice", test_xml, fixed = TRUE)
  test_xml <- gsub('##AbsoluteScoreMax##', sprintf("%d.0", nq), test_xml, fixed = TRUE)
  test_xml <- gsub('##Weighting##', "0.0", test_xml, fixed = TRUE)

  ## write heading of manifest_xml
  manifest_xml  <- c(
      '<manifest identifier="man00001" xmlns:bb="http://www.blackboard.com/content-packaging/">',
      '<organizations/>',
      '<resources>')

  bank_xml <- NULL

  ## cycle through all exams and questions
  ## similar questions are combined in a section,
  ## questions are then sampled from the sections
  items <- sec_xml <- NULL; all_points <- rep(0, length = nq)
  for(j in 1:nq) {

    ## create item ids
    item_ids <- paste(sec_ids[j], make_test_ids(nx, type = "item"), sep = "_")

    ## create variable part of test description file
    bank_xml <- c(bank_xml, section_xml)
    bank_xml <- gsub('##SectionId##', sec_ids[nq + j], bank_xml, fixed = TRUE)
    bank_xml <- gsub('##AssessmentType##', "Test", bank_xml, fixed = TRUE)
    bank_xml <- gsub('##SectionType##', "Random Block", bank_xml, fixed = TRUE)
    bank_xml <- gsub('##QuestionType##', "Multiple Choice", bank_xml, fixed = TRUE)
    bank_xml <- gsub('##AbsoluteScoreMax##', "1.0", bank_xml, fixed = TRUE)
    bank_xml <- gsub('##Weighting##', "1.0", bank_xml, fixed = TRUE)
    bank_xml <- gsub('##SectionItems##', paste(ordering_xml, collapse = "\n"), bank_xml, fixed = TRUE)
    bank_xml <- gsub('##QuestionType##', bb_questiontype(exm[[1]][[j]]$metainfo$type), bank_xml, fixed = TRUE)
    bank_xml <- gsub('##SourceBankRef##', sprintf("res%05d", j), bank_xml, fixed = TRUE)

    ## create structure of pool j
    pool_xml <- assessment_xml
    pool_xml <- gsub('##TestTitle##', exm[[1]][[j]]$metainfo$name, pool_xml, fixed = TRUE)
    pool_xml <- gsub('##TestIdent##', paste(test_id, j, sep="_"), pool_xml, fixed = TRUE)
    pool_xml <- gsub('##MaxTestScore##', "0.0", pool_xml, fixed = TRUE)
    pool_xml <- gsub('##AssessmentDescription##', pdescription, pool_xml, fixed = TRUE)
    pool_xml <- gsub('##AssessmentTitle##', pinstruction, pool_xml, fixed = TRUE)
    pool_xml <- gsub('##SectionContent##', paste(section_xml, collapse = "\n"), pool_xml, fixed = TRUE)
    pool_xml <- gsub('##SectionId##', sec_ids[j], pool_xml, fixed = TRUE)
    pool_xml <- gsub('##AssessmentType##', "Pool", pool_xml, fixed = TRUE)
    pool_xml <- gsub('##SectionType##', "Subsection", pool_xml, fixed = TRUE)
    pool_xml <- gsub('##QuestionType##', bb_questiontype(exm[[1]][[j]]$metainfo$type), pool_xml, fixed = TRUE)
    pool_xml <- gsub('##AbsoluteScoreMax##', "0.0", pool_xml, fixed = TRUE)
    pool_xml <- gsub('##Weighting##', "0.0", pool_xml, fixed = TRUE)

    ## include info of each pool j into manifest_xml
    manifest_xml <- c(manifest_xml,sprintf('<resource bb:file="res%05d.dat" bb:title="%s" identifier="res%05d" type="assessment/x-bb-qti-pool" xml:base="res%05d"/>', j, exm[[1]][[j]]$metainfo$name, j, j))

    ibody <- NULL

    ## now, insert the questions
    for(i in 1:nx) {
      ## overule points
      if(!is.null(points)) exm[[i]][[j]]$metainfo$points <- points[[j]]
      if(i < 2)
        all_points[j] <- if(is.null(exm[[i]][[j]]$metainfo$points)) 1 else exm[[i]][[j]]$metainfo$points

      ## get and insert the item body
      type <- exm[[i]][[j]]$metainfo$type

      ## create an id
      iname <- paste(item_ids[i], type, sep = "_")

      ## attach item id to metainfo
      exm[[i]][[j]]$metainfo$id <- iname
      ibody <- c(ibody, gsub("##ItemBody##",
        paste(thebody <- fix_bb_pre(itembody[[type]](exm[[i]][[j]])), collapse = "\n"),
        item_xml, fixed = TRUE))

      ## insert possible solution
      enumerate <- attr(thebody, "enumerate")
      if(is.null(enumerate)) enumerate <- FALSE
      xsolution <- fix_bb_pre(exm[[i]][[j]]$solution)
      if(!is.null(exm[[i]][[j]]$solutionlist)) {
        if(!all(is.na(exm[[i]][[j]]$solutionlist))) {
          xsolution <- c(xsolution, if(length(xsolution)) "<br />" else NULL)
          if(enumerate) xsolution <- c(xsolution, '<ol type = "a">')
          if(exm[[i]][[j]]$metainfo$type == "cloze") {
            g <- rep(seq_along(exm[[i]][[j]]$metainfo$solution), sapply(exm[[i]][[j]]$metainfo$solution, length))
            ql <- sapply(split(exm[[i]][[j]]$questionlist, g), paste, collapse = " / ")
            sl <- sapply(split(exm[[i]][[j]]$solutionlist, g), paste, collapse = " / ")
          } else {
            ql <- exm[[i]][[j]]$questionlist
            sl <- exm[[i]][[j]]$solutionlist
          }
          nsol <- length(ql)
          xsolution <- c(xsolution, paste(if(enumerate) rep('<li>', nsol) else NULL,
            ql, if(length(exm[[i]][[j]]$solutionlist)) "<br />" else NULL,
            sl, if(enumerate) rep('</li>', nsol) else NULL))
          if(enumerate) xsolution <- c(xsolution, '</ol>')
        }
      }

      ibody <- gsub("##ItemSolution##", paste(xsolution, collapse = "\n"), ibody, fixed = TRUE)

      ## insert an item id
      ibody <- gsub("##ItemId##", iname, ibody)

      ## copy supplements
      if(length(exm[[i]][[j]]$supplements)) {
        if(!base64) {
          if(!file.exists(media_dir <- file.path(test_dir, "csfiles", "home_dir")))
            dir.create(media_dir, recursive = TRUE)
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
            xid <- sprintf("xid-1%03d%03d_1", si, sj)
            file.copy(exm[[i]][[j]]$supplements[si],
              file.path(ms_dir, gsub(".", paste0("__", xid, "."), f, fixed = TRUE)))
            if(any(grepl(dirname(exm[[i]][[j]]$supplements[si]), ibody))) {
              ibody <- gsub(dirname(exm[[i]][[j]]$supplements[si]),
                file.path(sup_dir), ibody, fixed = TRUE)
            } else {
              if(any(grepl(f, ibody))) {
                ibody <- gsub(paste(f, '"', sep = ''),
                  paste("@X@EmbeddedFile.requestUrlStub@X@bbcswebdav", sup_dir, paste0(xid, "\""), sep = "/"), ibody, fixed = TRUE)
              }
            }
          }
        }
      }


      ## insert question type
      ibody <- gsub("##QuestionType##", bb_questiontype(type, item = FALSE), ibody, fixed = TRUE)
      ibody <- gsub('##MaxAttempts##', 'maxattempts="1"', ibody, fixed = TRUE)
      ibody <- gsub('##NegativePoints##', set_negative_points(eval = eval, type = type) , ibody, fixed = TRUE)
      ibody <- gsub('##PartialCredit##', ifelse(grepl("choice", type) & eval$partial, "true", "false"), ibody, fixed = TRUE)
    }

    ## fill pool j with item content and write to file
    pool_xml <- gsub('##SectionItems##', paste(ibody, collapse = "\n"), pool_xml, fixed = TRUE)
    writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', pool_xml), file.path(test_dir, sprintf("res%05d.dat", j)))
   }

  ## join fixed and variable parts of test description file and write to this file
  test_xml <- gsub('##SectionItems##', paste(bank_xml, collapse = "\n"), test_xml, fixed = TRUE)
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', test_xml), file.path(test_dir, sprintf("res%05d.dat", nq + 1)))

  ## finish manifest_xml and write to file
  manifest_xml <- c(manifest_xml,
                  sprintf('<resource bb:file="res%05d.dat" bb:title="%s-test" identifier="res%05d" type="assessment/x-bb-qti-test" xml:base="res%05d"/>', nq + 1, name, nq + 1, nq + 1),
  	            '</resources>',
                  '</manifest>')
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', manifest_xml), file.path(test_dir, "imsmanifest.xml"))

  ## write .bb-package-info file, needs just single line
  bb.inf <- 'cx.package.info.version=6.0'
  writeLines(bb.inf, file.path(test_dir, ".bb-package-info"))

  ## compress; added all.files=T for including bb info file
  if(zip) {
    owd <- getwd()
    setwd(test_dir)
    zip(zipfile = zipname <- paste(name, "zip", sep = "."), files = list.files(test_dir, all.files = T))
    setwd(owd)
  } else zipname <- list.files(test_dir)

  ## copy the final .zip file
  file.copy(file.path(test_dir, zipname), dir, recursive = TRUE)

  ## assign test id as an attribute
  attr(exm, "test_id") <- test_id

  invisible(exm)
}


## Blackboard item body constructor function
make_itembody_blackboard <- function(rtiming = FALSE, shuffle = FALSE, rshuffle = shuffle,
  minnumber = NULL, maxnumber = NULL, defaultval = NULL, minvalue = NULL,
  maxvalue = NULL, cutvalue = NULL, enumerate = TRUE, digits = NULL, tolerance = is.null(digits),
  maxchars = 12, eval = list(partial = TRUE, negative = FALSE),
  qti12 = FALSE)
{
  function(x) {
    ## how many points?
    points <- if(is.null(x$metainfo$points)) 1 else x$metainfo$points

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

    tol <- if(!is.list(x$metainfo$tolerance)) {
      if(x$metainfo$type == "cloze") as.list(x$metainfo$tolerance) else list(x$metainfo$tolerance)
    } else x$metainfo$tolerance
    tol <- rep(tol, length.out = n)

    q_points <- rep(points, length.out = n)
    if(x$metainfo$type == "cloze")
      points <- sum(q_points)

    ## set question type(s)
    type <- x$metainfo$type
    if(type == "cloze") stop('"cloze" type questions not supported in exams2blackboard() yet!')
    type <- if(type == "cloze") x$metainfo$clozetype else rep(type, length.out = n)

    ## evaluation policy
    if(is.null(eval) || length(eval) < 1L) eval <- exams_eval()
    if(!is.list(eval)) stop("'eval' needs to specify a list of partial/negative/rule")
    eval <- eval[match(c("partial", "negative", "rule"), names(eval), nomatch = 0)]
    if(x$metainfo$type %in% c("num", "string")) eval$partial <- FALSE
    if(x$metainfo$type == "cloze" & is.null(eval$rule)) eval$rule <- "none"
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
    if(qti12) {
      xml <- c(
        '<presentation>',
        '<flow>',
        if(!is.null(x$question)) {
          c(
            '<material>',
            '<mat_extension><mat_formattedtext type="HTML"><![CDATA[',
            x$question,
            ']]></mat_formattedtext></mat_extension>',
            '</material>'
          )
        } else NULL
      )
    } else {
      xml <- c(
        '<presentation>',
        '<flow class="Block">',
        '<flow class="QUESTION_BLOCK">',
        '<flow class="FORMATTED_TEXT_BLOCK">',
        if(!is.null(x$question)) {
          c(
            '<material>',
            '<mat_extension><mat_formattedtext type="HTML"><![CDATA[',
            x$question,
            ']]></mat_formattedtext></mat_extension>',
            '</material>'
          )
        } else NULL,
        rep('</flow>', 2)
      )
    }

    ## insert responses
    ids <- el <- pv <- list()
    for(i in 1:n) {
      if(!qti12)
        xml <- c(xml, '<flow class="RESPONSE_BLOCK">')

      ## get item id
      iid <- x$metainfo$id

      ## generate ids
      ids[[i]] <- list("response" = paste(iid, "RESPONSE", make_id(7), sep = "_"),
        "questions" = paste(iid, make_id(10, length(x$metainfo$solution)), sep = "_"))

      ## evaluate points for each question
      pv[[i]] <- eval$pointvec(solution[[i]])
      pv[[i]]["pos"] <- pv[[i]]["pos"] * q_points[i]
      if(length(grep("choice", type[i])))
        pv[[i]]["neg"] <- pv[[i]]["neg"] * q_points[i]

      ## insert choice type responses
      if(length(grep("choice", type[i]))) {
        xml <- c(xml,
          paste('<response_lid ident="', ids[[i]]$response, '" rcardinality="',
            if(type[i] == "mchoice") "Multiple" else "Single", '" rtiming=',
            if(rtiming) '"Yes"' else '"No"', '>', sep = ''),
          paste('<render_choice shuffle=', if(shuffle) '"Yes"' else '"No">', sep = '')
        )
        for(j in seq_along(solution[[i]])) {
          if(qti12) {
            xml <- c(xml,
              '<flow_label class="List">',
              paste('<response_label ident="', ids[[i]]$questions[j], '" rshuffle="',
                if(rshuffle) 'Yes' else 'No', '">', sep = ''),
              '<material>',
              '<mat_extension><mat_formattedtext type="HTML"><![CDATA[',
               paste(if(enumerate) {
                 paste(letters[if(x$metainfo$type == "cloze") i else j], ".",
                   if(x$metainfo$type == "cloze" && length(solution[[i]]) > 1) paste(j, ".", sep = "") else NULL,
                 sep = "")
               } else NULL, questionlist[[i]][j]),
              ']]></mat_formattedtext></mat_extension>',
              '</material>',
              '</response_label>',
              '</flow_label>'
            )
          } else {
            xml <- c(xml,
              '<flow_label class="Block">',
              paste('<response_label ident="', ids[[i]]$questions[j], '" rshuffle="',
                if(rshuffle) 'Yes' else 'No', '" rarea="Ellipse" rrange="Exact">', sep = ''),
              '<flow_mat class="FORMATTED_TEXT_BLOCK">',
              '<material>',
              '<mat_extension>',
              '<mat_formattedtext type="HTML">',
              '<![CDATA[',
               paste(if(enumerate) {
                 paste(letters[if(x$metainfo$type == "cloze") i else j], ".", ## Blackboard's enumeration turned off by using <bbmd_numbertype>none
                   if(x$metainfo$type == "cloze" && length(solution[[i]]) > 1) paste(j, ".", sep = "") else NULL,
                 sep = "")
               } else NULL, questionlist[[i]][j]),
              ']]>',
              '</mat_formattedtext>',
              '</mat_extension>',
              '</material>',
              '</flow_mat>',
              '</response_label>',
              '</flow_label>'
            )
          }
        }

        ## finish response tag
        xml <- c(xml,
          '</render_choice>',
          '</response_lid>'
        )
      }
      if(type[i] == "string" || type[i] == "num") {
        for(j in seq_along(solution[[i]])) {
          soltext <- if(type[i] == "num") {
             if(!is.null(digits)) format(round(solution[[i]][j], digits), nsmall = digits) else solution[[i]][j]
          } else {
            if(!is.character(solution[[i]][j])) format(solution[[i]][j]) else solution[[i]][j]
          }
          xml <- c(xml,
            if(!is.null(questionlist[[i]][j])) {
              c('<material>',
                paste('<mat_extension><mat_formattedtext type="HTML"><![CDATA[', paste(if(enumerate) {
                  paste(letters[i], ".", sep = '')
                } else NULL, questionlist[[i]][j]), ']]></mat_formattedtext></mat_extension>', sep = ""),
                '</material>')
            } else NULL,
            paste(if(type[i] == "string") '<response_str ident="' else '<response_num ident="',
               ids[[i]]$response, '" rcardinality="Single">', sep = ''),
            paste('<render_fib',
              if(!is.na(maxchars[[i]][1])) {
                paste(' maxchars="', max(c(nchar(soltext), maxchars[[i]][1])), '"', sep = '')
              } else NULL,
              if(!is.na(maxchars[[i]][2])) {
                paste(' rows="', maxchars[[i]][2], '"', sep = '')
              } else NULL,
              if(!is.na(maxchars[[i]][3])) {
                paste(' columns="', maxchars[[i]][3], '"', sep = '')
              } else NULL, '>', sep = ''),
              '</render_fib>',
            if(type[i] == "string") '</response_str>' else '</response_num>')
        }
      }
      if(!qti12)
        xml <- c(xml, '</flow>')
    }

    ## finish presentation
    xml <- c(xml, '</flow>', '</presentation>')

    if(is.null(minvalue)) {  ## FIXME: add additional switch, so negative points don't carry over?!
      if(eval$negative) {
        minvalue <- sum(sapply(pv, function(x) { x["neg"] }))
      } else minvalue <- 0
    }

    ## start resprocessing
    xml <- c(xml,
      '<resprocessing>',
      '<outcomes>',
      paste('<decvar varname="SCORE" vartype="Decimal" defaultval="',
        if(is.null(defaultval)) 0 else defaultval, '" minvalue="',
        if(is.null(minvalue)) 0 else minvalue, '" maxvalue="',
        if(is.null(maxvalue)) points else maxvalue, '" cutvalue="',
        if(is.null(cutvalue)) points else cutvalue, '"/>', sep = ''),
      '</outcomes>')

    ## most is left as with qti12, but some stuff, such as wrong_answers is never used.
    ## in many cases Blackboard does not check respident, but simply uses the order in <respconditions>
    ## to deal with it just_answers is introduced
    correct_answers <- wrong_answers <- correct_num <- just_answers <- vector(mode = "list", length = n)
    for(i in 1:n) {
      if(length(grep("choice", type[i]))) {
        for(j in seq_along(solution[[i]])) {
          if(solution[[i]][j]) {
            correct_answers[[i]] <- c(correct_answers[[i]],
              paste('<varequal respident="', ids[[i]]$response,
                '" case="Yes">', ids[[i]]$questions[j], '</varequal>', sep = '')
            )
            just_answers[[i]] <- c(just_answers[[i]],
              paste('<varequal respident="', ids[[i]]$response,
                '" case="Yes">', ids[[i]]$questions[j], '</varequal>', sep = '')

            )
          } else {
            wrong_answers[[i]] <- c(wrong_answers[[i]],
              paste('<varequal respident="', ids[[i]]$response,
                '" case="Yes">', ids[[i]]$questions[j], '</varequal>', sep = '')
            )
            just_answers[[i]] <- c(just_answers[[i]],
              paste('<not>\n<varequal respident="', ids[[i]]$response,
                '" case="Yes">', ids[[i]]$questions[j], '</varequal>\n</not>', sep = '')
            )
          }
        }
      }
      if(type[i] == "string" || type[i] == "num") {
        for(j in seq_along(solution[[i]])) {
          if(type[i] == "string") {
            soltext <- if(!is.character(solution[[i]][j])) {
              format(round(solution[[i]][j], digits), nsmall = digits)
            } else solution[[i]][j]
            correct_answers[[i]] <- c(correct_answers[[i]], paste('<varequal respident="', ids[[i]]$response,
              '" case="No">', soltext, '</varequal>', sep = "")
            )
          } else {
            correct_answers[[i]] <- c(correct_answers[[i]],
              if(!tolerance) {
                paste('<varequal respident="', ids[[i]]$response,
                  '" case="No"><![CDATA[', if(!is.null(digits)) {
                    format(round(solution[[i]][j], digits), nsmall = digits)
                  } else solution[[i]][j],
                  ']]></varequal>', sep = "")
              } else {
                paste(
                  paste('<vargte respident="', ids[[i]]$response, '">',
                    solution[[i]][j] - max(tol[[i]]),
                    '</vargte>\n', sep = ""),
                  paste('<varlte respident="', ids[[i]]$response, '">',
                    solution[[i]][j] + max(tol[[i]]),
                    '</varlte>\n', sep = ""),
                  paste('<varequal respident="', ids[[i]]$response, '">',
                    solution[[i]][j],
                    '</varequal>\n', sep = ""),
                   collapse = '\n', sep = ''
                )
              }
            )
          }
        }
      }
      if(!is.null(correct_answers[[i]])) {
        attr(correct_answers[[i]], "points") <- pv[[i]]
        attr(correct_answers[[i]], "type") <- type[i]
      }
      if(!is.null(wrong_answers[[i]]))
        attr(wrong_answers[[i]], "points") <- pv[[i]]
      if(!is.null(just_answers[[i]]))
        attr(just_answers[[i]], "points") <- pv[[i]]
    }

    ## delete NULL list elements
    correct_answers <- delete.NULLs(correct_answers)
    wrong_answers <- delete.NULLs(wrong_answers)
    just_answers <- delete.NULLs(just_answers)
    correct_num <- unlist(delete.NULLs(correct_num))

    ## partial cloze incorrect num string answers
    if(eval$partial & x$metainfo$type == "cloze") {
      if(length(correct_answers)) {
        for(i in seq_along(correct_answers)) {
          ctype <- attr(correct_answers[[i]], "type")
          if(ctype == "string" || ctype == "num") {
            xml <- c(xml,
              '<respcondition title="incorrect">',
              '<conditionvar>',
              '<not>',
              correct_answers[[i]],
              '</not>',
              '</conditionvar>',
              paste('<setvar varname="SCORE" action="Add">',
                attr(correct_answers[[i]], "points")["neg"], '</setvar>', sep = ''),
              '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
              '</respcondition>'
            )
          }
        }
      }
    }

    ## scoring/solution display for the correct answers
    xml <- c(xml,
      if(x$metainfo$type != "string")'<respcondition title="correct">' else '<respcondition title="right">',## string fails with "correct"
      '<conditionvar>',
      if(!is.null(correct_answers) & (length(correct_answers) > 1 | x$metainfo$type == "mchoice")) '<and>' else NULL
    )

    xml <- c(xml,
      if(x$metainfo$type == "mchoice") unlist(just_answers) else unlist(correct_answers),
      if(!is.null(just_answers) & (length(just_answers) > 1 | x$metainfo$type == "mchoice")) '</and>' else NULL,
      '</conditionvar>',
      if(!eval$partial & grepl("choice", x$metainfo$type)) {
        paste('<setvar varname="SCORE" action="Set">', points, '</setvar>', sep = '') ## note that Blackboard never uses "Add" (as in qti12) but "Set"
      } else NULL,
      if(!eval$partial & x$metainfo$type == "string") {
        paste('<setvar varname="EvaluationType" action="Set">', "CONTAINS", '</setvar>', sep = '')
      } else NULL,
      '<displayfeedback feedbacktype="Response" linkrefid="correct"/>',
      '</respcondition>'
    )

    ## earlier stuff concerning forcing display of correct answers deleted

    ## handling incorrect answers deleted

    ## handle all other cases
    xml <- c(xml,
      '<respcondition title="incorrect">',
      '<conditionvar>',
      '<other/>',
      '</conditionvar>',
      paste('<setvar varname="SCORE" action="Set">', if(!eval$partial) minvalue else 0, '</setvar>', sep = ''),
      #'<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>', NS: changed
      '<displayfeedback feedbacktype="Response" linkrefid="incorrect"/>',
      '</respcondition>'
    )

    ## handle unanswered cases deleted (Blackboard does not know <unanswered>)

    ## partial points (Blackboard presents credit bookkeeping  at the end of <resprocessing>)
    if(eval$partial) {
      if(length(unlist(just_answers))) {
        for(i in 1:n) {
          for(j in seq_along(solution[[i]])) {
            if(solution[[i]][j]) {
              xml <- c(xml,
                '<respcondition>',
                '<conditionvar>',
                paste('<varequal respident="', ids[[i]]$response,
                '" case="No">', ids[[i]]$questions[j], '</varequal>', sep = ''),
                '</conditionvar>',
                paste('<setvar varname="SCORE" action="Set">',
                  eval$pointvec(solution[[i]])["pos"]*100, '</setvar>', sep = ''),
                '</respcondition>'
              )
            } else {
              xml <- c(xml,
                '<respcondition>',
                '<conditionvar>',
                paste('<varequal respident="', ids[[i]]$response,
                '" case="No">', ids[[i]]$questions[j], '</varequal>', sep = ''),
                '</conditionvar>',
                paste('<setvar varname="SCORE" action="Set">',
                  eval$pointvec(solution[[i]])["neg"]*100, '</setvar>', sep = ''),
                '</respcondition>'
              )
            }
          }
        }
      }
    }

    ## end of response processing
    xml <- c(xml, '</resprocessing>')

    attr(xml, "enumerate") <- enumerate

    xml
  }
}



## function to create identfier ids
make_id <- function(size, n = 1L) {
  if(is.null(n)) n <- 1L
  rval <- matrix(sample(0:9, size * n, replace = TRUE), ncol = n, nrow = size)
  rval[1L, ] <- pmax(1L, rval[1L, ])
  colSums(rval * 10^((size - 1L):0L))
}

## delete NULL list elements
delete.NULLs <- function(x.list) {
  rval <- x.list[unlist(lapply(x.list, length) != 0)]
  rval <- if(length(rval)) rval else NULL
  rval
}

## fix Blackboard's failure to render html-<pre> environment properly
fix_bb_pre <- function(x) {
  pre_start <- grep("<pre>", x, fixed = TRUE)
  pre_end <- grep("</pre>", x, fixed = TRUE)
  if(length(pre_start) > 0L) {
    x[pre_start] <- gsub("<pre>", "<p><span style=\"font-family: courier new,courier;\">", x[pre_start], fixed = TRUE)
    x[pre_end] <- gsub("</pre>", "</span></p>", x[pre_end], fixed = TRUE)
    for(i in seq_along(pre_start)) {
      x[(pre_start[i] + 1L):(pre_end[i] - 1L)] <- paste(x[(pre_start[i] + 1L):(pre_end[i] - 1L)], "<br/>", sep = "")
    }
  }
  return(x)
}
