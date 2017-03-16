exams2pandoc <- function(file, n = 1L, nsamp = NULL, dir = ".",
  name = "pandoc", type = "docx", template = "plain.tex",
  question = "Question", solution = "Solution",
  header = list(Date = Sys.Date()), inputs = NULL, options = NULL,
  quiet = TRUE, resolution = 100, width = 4, height = 4, svg = FALSE, encoding = "",
  edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE, points = NULL, ...)
{
  ## determine intermediate output format from template
  via <- switch(tolower(file_ext(template)),
    "tex" = "latex",
    "html" = "html",
    "md" = "markdown_github+tex_math_single_backslash"
  )

  ## output name processing 
  if(is.null(name)) name <- file_path_sans_ext(basename(template))

  ## pandoc (if necessary) as default transformer
  transform <- make_exercise_transform_pandoc(to = via,
    base64 = if(via == "html") TRUE else FALSE,
    options = if(via == "html") "--mathml" else NULL)

  ## create PDF write with custom options
  pandocwrite <- make_exams_write_pandoc(name = name, type = type, template = template,
    question = question, solution = solution, header = header,
    inputs = inputs, options = options)

  ## generate xexams
  rval <- xexams(file, n = n, nsamp = nsamp,
    driver = list(sweave = list(quiet = quiet, pdf = FALSE, png = !svg, svg = svg,
      resolution = resolution, width = width, height = height, encoding = encoding),
      read = NULL, transform = transform, write = pandocwrite),
    dir = dir, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose,
    points = points)

  ## return xexams object invisibly
  invisible(rval)
}

## writes the final .html site
make_exams_write_pandoc <- function(name = "pandoc", type = "docx", template = "plain.tex",
  question = "Question", solution = "Solution", header = list(Date = Sys.Date()),
  inputs = NULL, options = NULL)
{
  ## determine intermediate output format from template
  via1 <- tolower(file_ext(template))
  via2 <- switch(via1,
    "tex" = "latex",
    "html" = "html",
    "md" = "markdown"
  )

  ## output name processing
  if(is.null(name)) name <- file_path_sans_ext(basename(template))

  ## find and read template file
  template <- if(is.null(template)) {
    file.path(find.package("exams"), "pandoc", "plain.tex")
  } else path.expand(template)
  template <- ifelse(file.exists(template),
    template, file.path(find.package("exams"), "pandoc", basename(template)))
  if(!all(file.exists(template))) {
    stop(paste("The following files cannot be found: ",
      paste(template[!file.exists(template)], collapse = ", "), ".", sep = ""))
  }
  template <- readLines(template)
  
  ## extract item body
  pd_exercises <- which(template == "#-")
  if(length(pd_exercises) != 9L) stop(sprintf("invalid template: exactly 9 '#-' lines required (and %s found)", length(pd_exercises)))
  pd_item <- split(template, rep.int(1L:10L, diff(c(1L, pd_exercises, length(template) + 1L))))[2L:9L]
  pd_item <- lapply(pd_item, "[", -1L)
  names(pd_item) <- c("start", "questionheader", "question", "questionlist", "solutionheader", "solution", "solutionlist", "end")
  pd_after <- pd_exercises[1L] - 1L
  template <- template[-(pd_exercises[1L]:pd_exercises[9L])]
  if(identical(question, FALSE)) {
    pd_item$questionheader <- pd_item$question <- pd_item$questionlist <- NULL
  } else {
    header$Questionheader <- question
  }
  if(identical(solution, FALSE)) {
    pd_item$solutionheader <- pd_item$solution <- pd_item$solutionlist <- NULL
  } else {
    header$Solutionheader <- solution  
  }
  
  ## set up item with specified number of choices on questionlist/solutionlist
  make_pd_item_list <- function(k) {
    pd <- pd_item
    k <- rep_len(k, 2L)
    if(k[1L] == 0L) pd$questionlist <- NULL
    if(k[2L] == 0L) pd$solutionlist <- NULL
    if(!is.null(li <- pd$questionlist)) {
      ix <- grep("##Questionlist##", li)
      qli <- li[ix]
      qli <- sapply(1L:k[1L],
        function(i) gsub("##Questionlist##", sprintf("##Questionlist%s##", i),
	qli, fixed = TRUE))
      pd$questionlist <- append(li[-ix], qli, after = ix - 1L)
    }
    if(!is.null(li <- pd$solutionlist)) {
      ix <- grep("##Solutionlist##", li)
      sli <- li[ix]
      sli <- sapply(1L:k[2L],
        function(i) gsub("##Solutionlist##", sprintf("##Solutionlist%s##", i),
	sli, fixed = TRUE))
      pd$solutionlist <- append(li[-ix], sli, after = ix - 1L)
    }
    unlist(pd)
  }

  ## check further inputs (if any)
  if(!is.null(inputs)) {
    if(!all(file.exists(inputs))) stop(paste("The following inputs cannot be found: ",
      paste(inputs[!file.exists(inputs)], collapse = ", "), ".", sep = ""))
  }

  function(exm, dir, info)
  {
    ## basic indexes
    id <- info$id
    n <- info$n
    m <- length(exm)

    ## current directory
    dir_orig <- getwd()
    on.exit(setwd(dir_orig))

    ## temporary directory and collection of extra inputs
    dir_temp <- tempfile()
    if(!file.exists(dir_temp) && !dir.create(dir_temp))
      stop(gettextf("Cannot create temporary work directory '%s'.", dir_temp))
    if(!is.null(inputs)) file.copy(inputs, dir_temp)
    setwd(dir_temp) 
    on.exit(unlink(dir_temp), add = TRUE)

    ## collapse answer groups of clozes (if necessary)
    for(j in seq_along(exm)) {
      if(exm[[j]]$metainfo$type == "cloze") {
        g <- rep(seq_along(exm[[j]]$metainfo$solution), sapply(exm[[j]]$metainfo$solution, length))
        if(!is.list(exm[[j]]$questionlist)) exm[[j]]$questionlist <- as.list(exm[[j]]$questionlist)
        exm[[j]]$questionlist <- sapply(split(exm[[j]]$questionlist, g), paste, collapse = " / ")
        if(!is.null(exm[[j]]$solutionlist)) exm[[j]]$solutionlist <- sapply(split(exm[[j]]$solutionlist, g), paste, collapse = " / ")
        for(qj in seq_along(exm[[j]]$questionlist)) {
          if(any(grepl(paste("##ANSWER", qj, "##", sep = ""), exm[[j]]$question, fixed = TRUE))) {
            ans <- exm[[j]]$questionlist[qj]
            exm[[j]]$question <- gsub(paste("##ANSWER", qj, "##", sep = ""),
              ans, exm[[j]]$question, fixed = TRUE)
            exm[[j]]$questionlist[qj] <- NA
          }
        }
      }
    }

    pd_body <- ""

    ## question and solution insertion
    for(j in seq_along(exm)) {
      nli <- c(length(exm[[j]]$questionlist), length(exm[[j]]$solutionlist))
      pd_body_j <- make_pd_item_list(nli)
      pd_body_j <- gsub("##Question##", paste(exm[[j]]$question, collapse = "\n"), pd_body_j, fixed = TRUE)
      pd_body_j <- gsub("##Solution##", paste(exm[[j]]$solution, collapse = "\n"), pd_body_j, fixed = TRUE)
      if(nli[1L] > 0L) for(i in 1L:nli[1L]) {
        pd_body_j <- gsub(sprintf("##Questionlist%s##", i),
	  exm[[j]]$questionlist[i], pd_body_j, fixed = TRUE)
      }
      if(nli[2L] > 0L) for(i in 1L:nli[2L]) {
        pd_body_j <- gsub(sprintf("##Solutionlist%s##", i),
	  exm[[j]]$solutionlist[i], pd_body_j, fixed = TRUE)
      }

      ## handle and copy possible supplements
      if(length(exm[[j]]$supplements)) {
        if(!file.exists(media_dir <- file.path(dir_temp, "media")))
          dir.create(media_dir)
        if(!file.exists(exm_dir <- file.path(media_dir, exi <- paste("supplements", id, sep = ""))))
          dir.create(exm_dir)
        if(!file.exists(ex_dir <- file.path(exm_dir, exj <- paste("exercise", j, sep = ""))))
          dir.create(ex_dir)
        for(sup in exm[[j]]$supplements) {
          file.copy(sup, file.path(ex_dir, basename(sup)))
          if(any(grep(dirname(sup), pd_body_j, fixed = TRUE))) {
            pd_body_j <- gsub(dirname(sup), file.path("media", exi, exj),
              pd_body_j, fixed = TRUE)
          }
	  sprefix <- switch(via1,
	    "tex" = '{',
	    "html" = '="',
	    "md" = ']('
	  )
          src <- paste0(sprefix, basename(sup))
          if(any(grep(src, pd_body_j, fixed = TRUE))) {
            pd_body_j <- gsub(src,
	      paste0(sprefix, file.path("media", exi, exj, basename(sup))),
	      pd_body_j, fixed = TRUE)
          }
        }
      }
      
      pd_body <- c(pd_body, pd_body_j)
    }

    ## insert exercises
    pd_body <- append(template, pd_body, after = pd_after)

    ## insert header info
    if(is.null(header$ID)) header$ID <- id
    for(n in names(header)) {
      pd_body <- gsub(paste0("##", n, "##"),
        if(is.function(header[[n]])) header[[n]](id) else header[[n]],
	pd_body, fixed = TRUE)
    }

    ## fixup LaTeX code (not in pandoc transformer because latex->latex)
    if(via2 == "latex") {
      tab <- rbind(
        c("\\\\begin\\{Sinput}",  "\\\\begin{verbatim}"),
        c("\\\\end\\{Sinput}",    "\\\\end{verbatim}"),
        c("\\\\begin\\{Soutput}", "\\\\begin{verbatim}"),
        c("\\\\end\\{Soutput}",   "\\\\end{verbatim}"),
        c("\\\\begin\\{Schunk}",  ""),
        c("\\\\end\\{Schunk}",    ""),
        c("\\\\textit\\{",	  "\\\\emph{"),
        c("\\\\textnormal\\{",    "\\\\text{"),
        c("\\\\not",		  "\\\\not "),
        c("\\\\not +=", 	  "\\\\neq"),
        c("\\\\not +<", 	  "\\\\nless"),
        c("\\\\not +>", 	  "\\\\ngtr"),
        c("\\\\not +\\\\le",	  "\\\\nleq"),
        c("\\\\not +\\\\ge",	  "\\\\ngeq")
      )
      for(i in 1:nrow(tab)) pd_body <- gsub(tab[i,1L], tab[i,2L], pd_body)
    }
    
    ## write intermediate file, pandoc, and copy result
    viafile <- paste0(name, id, ".", via1)
    outfile <- paste0(name, id, ".", type)
    writeLines(pd_body, viafile)
    rmarkdown::pandoc_convert(input = viafile, output = outfile,
      from = via2, to = if(type == "pdf") "latex" else type, options = options)
    file.copy(outfile, dir, recursive = TRUE)
    invisible(NULL)
  }
}
