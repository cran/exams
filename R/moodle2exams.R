moodle2exams <- function(x, markup = c("markdown", "latex"),
  dir = ".", exshuffle = TRUE, names = NULL)
{
  ## read Moodle XML file (if necessary)
  stopifnot(requireNamespace("xml2"))
  if(!inherits(x, "xml_node") && length(x) == 1L) x <- xml2::read_xml(x)

  ## set up template in indicated markup
  markup <- match.arg(markup)
  if(markup == "markdown") markup <- "markdown_strict"
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

  pluginfile <- function(x, xml) {
    supps <- NULL
    pif <- grep("@PLUGINFILE@", x, fixed = TRUE)
    if(length(pif)) {
      pif <- x[pif]
      pif <- regmatches(pif, gregexpr('(?<=@@).*?(?=")', pif, perl = TRUE))
      for(j in seq_along(pif)) {
        pfn <- gsub("PLUGINFILE@@/", "", pif[j], fixed = TRUE)
        pfnn <- xml2::xml_find_all(xml, ".//file")
        if(length(pfnn)) {
          for(f in 1:length(pfnn)) {
            fname <- xml2::xml_attr(pfnn[f], "name")
            bnf <- basename(fname)
            if(bnf == pfn) {
              ftext <- xml2::xml_text(pfnn[f])
              b64f <- base64enc::base64decode(ftext)
              writeBin(b64f, file.path(dir, bnf))
              x <- gsub(paste0("@@PLUGINFILE@@/", bnf), bnf, x, fixed = TRUE)
              xml2::xml_remove(pfnn[f])
              x <- xml2::xml_text(xml)
              supps <- c(supps, bnf)
            }
          }
        }
      }
      if(any(grepl("@@PLUGINFILE@@/", x, fixed = TRUE))) {
        x <- gsub("@@PLUGINFILE@@/", "", x, fixed = TRUE)
      }
    }
    if(length(i <- grep('href="data:text', x, fixed = TRUE))) {
      for(j in i) {
        df <- x[j]
        dfl <- regmatches(df, gregexpr("(?<=<a).*?(?=</a>)", df, perl = TRUE))
        b64f <- regmatches(dfl, gregexpr('(?<=href=").*?(?=")', dfl, perl = TRUE))[[1L]]
        fname <- regmatches(dfl, gregexpr('(?<=download=").*?(?=")', dfl, perl = TRUE))[[1L]]
        b64f <- paste0(b64f, "'")
        b64f <- regmatches(b64f, gregexpr("(?<=,).*?(?=')", b64f, perl = TRUE))[[1L]]
        b64fd <- base64enc::base64decode(b64f)
        writeBin(b64fd, file.path(dir, fname))
        x <- gsub(paste0('href="', paste0("data:text/csv;base64,", b64f), '"'),
          paste0('href="', fname, '"'), x, fixed = TRUE)
        supps <- c(supps, fname)
      }
    }
    return(list("text" = x, "xml" = xml, "supplements" = supps))
  }

  ## extract questions
  ## FIXME: restrict question types some more?
  qu <- xml2::xml_find_all(x, "question")
  type <- xml2::xml_attr(qu, "type")
  qu <- qu[type != "category"]
  type <- type[type != "category"]
  n <- length(qu)
  if(n < 1L) stop("no <question> tags (of supported type)")
  
  ## set up variables for each question
  exrc <- vector(mode = "list", length = n)
  if(is.null(names)) {
    names <- rep.int("", n)
  } else {
    names <- rep_len(as.character(names), n)
    names[is.na(names) | duplicated(names)] <- ""
  }
  exshuffle <- rep_len(as.character(exshuffle), n)

  ## Feedback tags.
  fbtags <- c("generalfeedback", "partiallycorrectfeedback", "incorrectfeedback")

  ## Collect supplements.
  supps <- NULL
  
  ## cycle through questions
  for(i in 1L:n) {
    ## cloze not fully supported yet
    if(type[i] == "cloze")
      warning("cloze conversion not fully supported yet!")
    qui <- xml2::xml_children(qu[[i]])
    qn <- xml2::xml_name(qui)
    if("questiontext" %in% qn) {
      qtext <- xml2::xml_text(qui[qn == "questiontext"])
      pff <- pluginfile(qtext, qui)
      supps <- c(supps, pff$supplements)
      qtext <- pff$text
      qui <- pff$xml
      qtext <- pandoc(qtext,
        from = "html+tex_math_dollars+tex_math_single_backslash",
        to = markup)
      exsol <- extol <- feedback <- NULL
      answers <- solutions <- list()

      ## num
      if(type[i] == "numerical") {
        exsol <- xml2::xml_text(xml2::xml_children(qui[qn == "answer"])[1L])
        extol <- xml2::xml_children(qui[qn == "answer"])
        if("tolerance" %in% xml2::xml_name(extol))
          extol <- xml2::xml_text(extol[xml2::xml_name(extol) == "tolerance"])
      }
      
      ## schoice/mchoice
      if(type[i] == "multichoice") {
        single <- qui[qn == "single"]
        if(!is.null(single)) {
          single <- xml2::xml_text(single)
          single <- single == "true"
        } else {
          single <- FALSE
        }
        if(single)
          type[i] <- "singlechoice"
        ans <- qui[qn == "answer"]
        frac <- xml2::xml_attr(ans, "fraction")
        frac <- as.numeric(frac)
        sol <- frac > 0
        exsol <- paste0(sol * 1, collapse = "")
        for(j in 1L:length(ans)) {
          ac <- xml2::xml_children(ans[j])
          ac <- ac[xml2::xml_name(ac) == "text"]
          ac <- xml2::xml_text(ac)
          answers[[j]] <- pandoc(ac[1],
            from = "html+tex_math_dollars+tex_math_single_backslash",
            to = markup)
          if(length(ac) > 1L) {
            solutions[[j]] <- pandoc(ac[2],
              from = "html+tex_math_dollars+tex_math_single_backslash",
              to = markup)
          }
        }
      }
      
      ## string
      if(type[i] == "shortanswer") {
        ans <- xml2::xml_children(qui[qn == "answer"])
        exsol <- xml2::xml_text(ans[xml2::xml_name(ans) == "text"][1])
      }

      ## general feedback
      if(any(k <- fbtags %in% qn)) {
        for(l in seq_along(fbtags[k])) {
          fbtmp <- qui[qn == fbtags[k][l]]
          pff <- pluginfile(xml2::xml_text(fbtmp), fbtmp)
          fbtmp <- pff$text
          supps <- c(supps, pff$supplements)
          slist <- grepl("<ul>", fbtmp, fixed = TRUE)
          fbtmp <- pandoc(fbtmp,
            from = "html+tex_math_dollars+tex_math_single_backslash",
            to = markup)
          if(slist) {
            if(markup == "latex") {
              fbtmp <- c("", gsub("{itemize}", "{answerlist}", fbtmp, fixed = TRUE))
            } else {
              fbtmp <- c("", "Answerlist", "----------", fbtmp)
            }
          }
          feedback <- c(feedback, fbtmp)
        }
      }

      ## name/label and type
      exname <- xml2::xml_text(qui[qn == "name"])
      extype <- switch(type[i],
        "numerical" = "num",
        "essay" = "string",
        "cloze" = "cloze",
        "multichoice" = "mchoice",
        "singlechoice" = "schoice",
        "shortanswer" = "string"
      )

      ## further meta-information tags
      exother <- if(type[i] == "numerical" && !is.null(extol)) {
        sprintf(if(markup == "markdown_strict") "extol: %s" else "\\extol{%s}", extol)
      } else if(type[i] == "multichoice") {
        sprintf(if(markup == "markdown_strict") "exshuffle: %s" else "\\exshuffle{%s}", exshuffle[i])
      } else {
        ""
      }

      if(type[i] %in% c("singlechoice", "multichoice") & (length(solutions) < 1)) {
        solutions <- as.list(ifelse(sol, "True", "False"))
      }

      ## insert information into template
      exrc[[i]] <- sprintf(tmpl,
        paste(qtext, collapse = "\n"),
	if(type[i] %in% c("singlechoice", "multichoice"))
    paste(c("", answerlist_env(answers)), collapse = "\n") else "",
      if(length(solutions) >= 1L || !is.null(feedback))
        paste(solution_env(solutions, feedback), collapse = "\n") else "",
	if(names[i] == "") exname else names[i],
	extype,
	exsol,
	exother)

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
        exrc[[i]] <- c(scode, "", exrc[[i]])
      }
      
      ## default file name
      if(names[i] == "") names[i] <- name_to_file(exname)
    }
  }

  ## write/return resulting exercises
  names(exrc) <- names
  if(!is.null(dir)) {
    dir.create(tdir <- tempfile())
    for(i in 1L:length(exrc)) {
      writeLines(exrc[[i]], file.path(tdir, paste(names[i], fext, sep = ".")))
    }
    fn <- dir(tdir)
    file.copy(file.path(tdir, fn), file.path(dir, fn), overwrite = TRUE)
    unlink(tdir)
    invisible(exrc)
  } else {
    return(exrc)
  }
}
