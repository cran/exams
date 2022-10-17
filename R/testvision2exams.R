testvision2exams <- function(x, markup = c("markdown", "latex"), rawHTML = FALSE,
  dir = ".", exshuffle = TRUE, name = NULL, shareStats = FALSE)
{
  ## read Moodle XML file (if necessary)
  stopifnot(requireNamespace("xml2"))
  if(!inherits(x, "xml_node") && length(x) == 1L) x <- xml2::read_xml(x)

  ## set up template in indicated markup
  markup <- match.arg(markup)
  if(markup == "markdown") markup <- "markdown_strict"
  if(markup == "latex" & rawHTML) rawHTML <- FALSE #raw html is only relevant for markdown markup
  if(!(markup %in% c("markdown_strict", "latex"))) stop("'markup' must be either markdown/markdown_strict or latex")
  fext <- if(markup == "markdown_strict") "Rmd" else "Rnw"
  tmpl <- if(markup == "markdown_strict") {

'Question
========
%s
%s

%s

Meta-information
================
exname: %s
extype: %s
exsolution: %s
%s
'

  } else {

'\\begin{question}
%s
%s
\\end{question}

%s

%%%% Meta-information
\\exname{%s}
\\extype{%s}
\\exsolution{%s}
%s
'

  }

  ## convenience functions
  answerlist_env <- function(x) {
    if(is.list(x)) {
      x <- lapply(x, paste, collapse = "\n  ")
      x <- unlist(x)
    }
    if(markup == "markdown_strict") {
      c("Answerlist", "----------", paste("*", x))
    } else {
      c("\\begin{answerlist}", paste("  \\item", x), "\\end{answerlist}")
    }
  }

  solution_env <- function(solutions, feedback) {
    if(length(solutions) > 0L) solutions <- answerlist_env(solutions)
    if(any(grepl("answerlist", tolower(feedback)))) {
      solutions <- feedback
      feedback <- NULL
    }
    solutions <- c(feedback, solutions)
    solutions <- solutions[!grepl("tightlist", solutions)]
    solutions <- if(markup == "markdown_strict") {
      c("Solution", "========", solutions)
    } else {
      c("\\begin{solution}", solutions, "\\end{solution}")
    }
  }

  name_to_file <- function(name) {
    name <- gsub(":|,|!", "", name)
    name <- gsub(" ", "_", name)
    for(i in c("...", "|", "/", "(", ")", "[", "]", "{", "}", "<", ">")) {
      name <- gsub(i, "", name, fixed = TRUE)
    }
    return(name)
  }

  stripdiv <- function(xml) { #remove the <div> that TVO puts everywhere
    xml <- gsub("</div>", "", xml)
    xml <- gsub("<div [^>]*>", "", xml)
    xml <- gsub("\\n", " ", xml)
    xml <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", xml, perl = TRUE)
    xml
  }

  stripmodf <- function(xml) { #remove the modalFeedback environment
    xml <- stripdiv(xml)
    xml <- gsub("</modalFeedback>", "", xml)
    xml <- gsub("<modalFeedback [^>]*>", "", xml)
    xml <- gsub("\\n", "", xml)
    xml <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", xml, perl = TRUE)
  }

  exfile <- function(x) {
    if(any(grepl('<a href', x))) {
      fls <- regmatches(x, gregexpr('(?<=href=")(.|\n)*?(?=")', x, perl = TRUE))[[1L]]
      tgs <- regmatches(x,  gregexpr('<a href="[^>]*>((.|\n)*)</a>', x))
      for(i in 1 : length(fls)) {
      x <- gsub(tgs[i], paste0("[", fls[i], "](", fls[i], ")"), x)
      }
    }
    return(x)
  }

  equation <- function(x) {
    if(any(grepl('application/x-tex', x))) {
      eqs <- regmatches(x,gregexpr('(?<=<math)(.|\n)*?(?=</math>)', x, perl = TRUE))[[1L]]
      eqs <- as.vector(apply(as.matrix(eqs), 1, function(x) paste0("<math", x, "</math>")))
      inline <- grepl('display="inline', eqs)
      aligned <- grepl('(\\{aligned|\\{eqnarray)', eqs)
      for(i in 1:length(eqs)) {
        tmp <- regmatches(eqs[i], gregexpr('<math[^>]*>(.*)x-tex">', eqs[i]))[[1]]
        x <- sub(tmp, if(inline[i]) "\\(" else if(!aligned[i]) "\\[" else "", x, fixed = TRUE)
        x <- sub("</annotation></semantics></math>", if(inline[i]) "\\)" else if(!aligned[i]) "\\]" else "", x, fixed = TRUE)
      }
      x <- gsub("#38;", "", x)
      x <- gsub("\\\\begin\\{aligned\\}", "<br>\\\\begin\\{aligned\\}<br>", x)
      x <- gsub("\\\\end\\{aligned\\}", ",<br>\\\\end\\{aligned\\}<br>", x)
      x <- gsub("\\\\begin\\{eqnarray\\*\\}", "<br>\\\\begin\\{eqnarray\\*\\}<br>", x)
      x <- gsub("\\\\end\\{eqnarray\\*\\}", "<br>\\\\end\\{eqnarray\\*\\}<br>", x)
      x <- gsub("\\\\\\\\", "\\\\\\\\<br>", x)
    }
    return(x)
  }

  mediafile <- function(x) {
    supps <- NULL
    if(length(i <- grep("img src=", x, fixed = TRUE))) {
      for(j in i) {
        img <- x[j]
        fpath <- regmatches(img, gregexpr('(?<=img src=").*?(?=")', img, perl = TRUE))[[1L]]
        for(k in 1 : length(fpath)){
          fname <- name_to_file(basename(fpath[k]))
          file.copy(paste0("./", fpath[k]), file.path(dir, fname))
          x <- gsub(fpath[k], fname, x, fixed = TRUE)
          supps <- c(supps, fname)
        }
      }
    }
    if(length(i <- grep('href=\"mediafiles', x, fixed = TRUE))) {
      for(j in i) {
        df <- x[j]
        dfl <- regmatches(df,  gregexpr('<a href="[^>]*>(.*)</a>', df))
        fpath <- regmatches(dfl, gregexpr('(?<=href=").*?(?=")', dfl, perl = TRUE))[[1L]]
        for(k in 1 : length(fpath)){
          fname <- name_to_file(basename(fpath[k]))
          file.copy(paste0("./", fpath[k]), file.path(dir, fname))
          x <- gsub(fpath[k], fname, x, fixed = TRUE)
          x <- gsub(paste0("<code>", fname, "</code>"), "", x, fixed = TRUE)
          supps <- c(supps, fname)
        }
      }
    }
    return(list("txt" = x, "supplements" = supps))
  }

  ## Collect supplements.
  supps <- NULL

  ## get basic parts from question
  x <- xml2::xml_ns_strip(x)
  exname <- if(is.null(name)) xml2::xml_attr(x, "title") else name
  response  <- xml2::xml_find_all(x, "responseDeclaration")
  cardinality <- xml2::xml_attr(response,"cardinality")
  baseType <- xml2::xml_attr(response,"baseType")
  if(is.na(baseType)){
    extype <- "cloze"
    } else if(baseType == "identifier") {
        if(cardinality == "single") extype <- "schoice"
        else extype <- "mchoice"
    } else if(baseType == "float") {
      extype <- "num"
      } else {
      extype <- "string"
    }
  if(is.na(baseType)) stop("The xml-file contains no question of supported extype")

  ibody <- xml2::xml_find_all(x, "itemBody")
  #ibody <- xml2::xml_contents(xml2::xml_find_all(x, ".//itemBody"))
  interact <- xml2::xml_find_all(ibody, switch(extype, schoice = "choiceInteraction", mchoice = "choiceInteraction",
                                                   num = "extendedTextInteraction", string = "extendedTextInteraction"))
  rblock <- xml2::xml_find_all(ibody, "rubricBlock")
  exshuffle <- if(!rawHTML) as.character(exshuffle) else tolower(exshuffle)
  choices <- xml2::xml_children(xml2::xml_find_all(interact, "simpleChoice"))#Naar schoice en mchoice
  choiceid <- lapply(xml2::xml_find_all(interact, "simpleChoice"), function(x) xml2::xml_attr(x, "identifier"))

  fdbck <- xml2::xml_find_all(x, "modalFeedback")


  ## Remove superfluous stuff from ibody
  xml2::xml_remove(interact)#
  xml2::xml_remove(rblock)

  cresp <- xml2::xml_text(xml2::xml_children(xml2::xml_children(response)))

  ## set up variables for question
  exrc <- vector(mode = "list", length = 1L)

  ## cycle through question
  ## cloze not fully supported yet
  if(extype == "cloze") warning("cloze conversion not fully supported yet!")
  if(grep("itemBody", ibody)) {
    qu <- stripdiv(xml2::xml_contents(ibody))
    pff <- mediafile(qu)
    supps <- c(supps, pff$supplements)
    qtext <- paste0(as.character(pff$txt), collapse = "")
    if(!rawHTML){
      qtext <- pandoc(equation(qtext),
        from = "html+tex_math_dollars+tex_math_single_backslash",
        to = markup)
    }
    exsol <- extol <- feedback <- taxstr <- exother <- NULL
    answers <- solutions <- list()

    ## num
    if(extype == "num") {
      exsol <- as.numeric(cresp[1])
      if(is.na(cresp[2])) extol <- 0 else {
        interval <- as.numeric(unlist(strsplit(cresp[2], ";", 2)))
        extol <- abs(interval[1] - interval[2])/2
      }
    }

    ## schoice/mchoice
    if(grepl("choice", extype)) {
      pff <- mediafile(choices)
      supps <- c(supps, pff$supplements)
      ans <- lapply(pff$txt, function(x) paste0(stripdiv(x), collapse = ""))
      sol <- choiceid %in% cresp
      exsol <- paste0(sol*1, collapse = "")
      for(j in 1L:length(ans)) {
        answers[[j]] <- if(!rawHTML) pandoc(equation(ans[j]),
          from = "html+tex_math_dollars+tex_math_single_backslash",
            to = markup) else ans[j]
        }
    }

    ## string
    if(extype == "string") {
      ans <- "answer"
      exsol <- "solution"
    }

    ## feedback
    if(length(fdbck)) {
      xml <- fdbck
      fbnames <-xml2:: xml_attr(xml, "identifier")
      fbanswers <- xml2::xml_contents(xml[grep("alt_", fbnames)])
      tmp <- mediafile(xml)
      fdbck <- lapply(tmp$txt, stripmodf)
      if(!rawHTML) fdbck <- lapply(fdbck, equation)
      supps <- c(supps, tmp$supplements)
      fbcorrect <- fdbck[fbnames == "ANSWER_CORRECT"]
      fbfailure <- fdbck[fbnames == "FAILURE"]
      fbquestion <- fdbck[fbnames == "QUESTION_FEEDBACK"]
      fbanswers <- fdbck[grep("alt_", fbnames)]
      ## remove feedback duplicates
      ufb <- unique(c(fbcorrect, fbfailure, fbquestion))
      if(length(ufb) < 3) {
        if(length(ufb) == 1){
            fbquestion <- ufb
            fbfailure <- fbcorrect <- NULL
        } else {
            if(identical(fbfailure, fbcorrect)) fbfailure <- NULL else {
                if(identical(fbquestion, fbcorrect)) fbcorrect <- NULL else {
                    if(identical(fbquestion, fbfailure)) fbfailure <- NULL
              }
            }
          }
        }
      if(shareStats) {
        fbsubject <- NULL
        taxstr <- stripdiv(unlist(fdbck[fbnames == "SUBJECT_MATTER"]))
      } else fbsubject <- fdbck[fbnames == "SUBJECT_MATTER"]
      fdbck <- list(fbcorrect = fbcorrect, fbfailure = fbfailure, fbquestion = fbquestion,
         fbanswers = fbanswers, fbsubject = fbsubject)
      fdbck <- lapply(fdbck, function(x) if(length(x) == 0) x <- NULL else x)

      if(!is.null(unlist(fbanswers))){
        solutions <- fbanswers
        fdbck$fbanswers <- NULL
      }

      for(i in 1 : length(fdbck)) {
        fbtmp <- fdbck[[i]]
        if(!rawHTML){
          fbtmp <- pandoc(fbtmp,
            from = "html+tex_math_dollars+tex_math_single_backslash",
            to = markup)
        }
        feedback <- c(feedback, "", fbtmp)
      }
    }

    ## rubrics (information for teacher), add to feedback collection
    if(length(rblock)){
      rblock <- paste0("<p>", stripdiv(xml2::xml_contents(rblock)), "</p>", collapse = "")
      if(!rawHTML){
        rblock <- equation(rblock)
        rblock <- pandoc(rblock,
            from = "html+tex_math_dollars+tex_math_single_backslash",
            to = markup)
      }
      feedback <- c(feedback, "", rblock)
    }

    ## further meta-information tags
    exother <- if(extype == "num" && !is.null(extol)) {
      sprintf(if(markup == "markdown_strict") "extol: %s\n" else "\\extol{%s}\n", extol)
    } else if(grepl("choice", extype)) {
      sprintf(if(markup == "markdown_strict") "exshuffle: %s\n" else "\\exshuffle{%s}\n", exshuffle)
    } else {
      ""
    }

    if(shareStats) {
      splitinfo <- unlist(strsplit(taxstr, ";"))
      splitinfo[splitinfo == "na"] <- NA
      splitinfo <- sprintf(if(markup == "markdown_strict")
        "exsection: %s\nexextra[Type]: %s\nexextra[Program]: %s\nexextra[Language]: %s\nexextra[Level]: %s" else
        "\\exsection{%s}\n\\exextra[Type]{%s}\n\\exextra[Program]{%s}\n\\exextra[Language]{%s}\n\\exextra[Level]{%s}",
        splitinfo[1], splitinfo[2], splitinfo[3], splitinfo[4], splitinfo[5])
      exother <- paste0(exother, splitinfo, collapse = "\n")
    }


    if(extype %in% c("schoice", "mchoice") & (length(solutions) < 1)) {
      solutions <- as.list(ifelse(sol, "True", "False"))
    }

    ## insert information into template
    exrc <- sprintf(tmpl,
        paste(qtext, collapse = "\n"),
	if(extype %in% c("schoice", "mchoice"))
    paste(c("", answerlist_env(answers)), collapse = "\n") else "",
      if(length(solutions) >= 1L || !is.null(feedback))
        paste(solution_env(solutions, feedback), collapse = "\n") else "",
	exname,
	extype,
	exsol,
	exother)

    exrc <- gsub("#38;", "", exrc)
    exrc <- gsub("&amp;", "&", exrc)
    exrc <- gsub("\\\\href", "\\\\url", exrc)
    exrc <- gsub("\\\\textbackslash\\s*", "\\\\", exrc)
    exrc <- exfile(exrc)

    ## supplements.
    if(length(supps)) {
      scode <- if(markup == "markdown_strict") {
        '```{r, echo = FALSE, results = "hide"}'
      } else {
        '<<echo=FALSE, results=hide>>='
      }
      for(f in supps)
        scode <- c(scode, paste0('include_supplement("', f, '")'))
      scode <- c(scode, if(markup == "markdown_strict") {
        '```'
      } else {
        '@'
      })
      exrc <- c(scode, "", exrc)
    }

    ## default file name
    exname <- name_to_file(exname)
  }


  ## write/return resulting exercises
  names(exrc) <- exname
  if(!is.null(dir)) {
    dir.create(tdir <- tempfile())
    writeLines(exrc, file.path(tdir, paste(exname, fext, sep = ".")))
    fn <- dir(tdir)
    file.copy(file.path(tdir, fn), file.path(dir, fn), overwrite = TRUE)
    unlink(tdir)
    invisible(exrc)
  } else {
    return(exrc)
  }
}
