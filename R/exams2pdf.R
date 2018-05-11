exams2pdf <- function(file, n = 1L, nsamp = NULL, dir = ".",
  template = NULL, inputs = NULL, header = list(Date = Sys.Date()),
  name = NULL, control = NULL, encoding = "", quiet = TRUE,
  transform = NULL, edir = NULL, tdir = NULL, sdir = NULL, texdir = NULL,
  verbose = FALSE, points = NULL, ...)
{
  ## for Rnw exercises use "plain" template, for Rmd "plain8"
  if(is.null(template)) template <- if(any(tolower(tools::file_ext(unlist(file))) == "rmd")) "plain8" else "plain"

  ## output directory or display on the fly
  display <- missing(dir)
  if(missing(dir) & n == 1L & length(template) == 1L) {
    display <- TRUE
    dir.create(dir <- tempfile())
  } else {
    display <- FALSE
    if(is.null(dir)) stop("Please specify an output 'dir'.")
  }

  ## output name processing 
  if(is.null(name)) name <- file_path_sans_ext(basename(template))

  ## pandoc (if necessary) as default transformer
  if(is.null(transform)) transform <- make_exercise_transform_pandoc(to = "latex", base64 = FALSE)

  ## create PDF write with custom options
  if(!is.null(texdir)) {
    if(!file.exists(texdir) && !dir.create(texdir))
      stop(gettextf("Cannot create temporary work directory '%s'.", texdir))
    texdir <- tools::file_path_as_absolute(texdir)
  }
  pdfwrite <- make_exams_write_pdf(template = template, inputs = inputs, header = header,
    name = name, encoding = encoding, quiet = quiet, control = control, texdir = texdir)

  ## generate xexams
  rval <- xexams(file, n = n, nsamp = nsamp,
    driver = list(sweave = list(quiet = quiet, encoding = encoding, ...),
                  read = NULL, transform = transform, write = pdfwrite),
    dir = dir, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose,
    points = points)

  ## display single .pdf on the fly
  if(display) {
    out <- normalizePath(file.path(dir, paste(name, "1.pdf", sep = "")))
    if(.Platform$OS.type == "windows") shell.exec(out)
      else system(paste(shQuote(getOption("pdfviewer")), shQuote(out)), wait = FALSE)
  }
  
  ## return xexams object invisibly
  invisible(rval)
}

make_exams_write_pdf <- function(template = "plain", inputs = NULL,
  header = list(Date = Sys.Date()), name = NULL, encoding = "", quiet = TRUE,
  control = NULL, texdir = NULL)
{
  ## template pre-processing
  template_raw <- template
  template_tex <- template_path <- ifelse(
    tolower(substr(template, nchar(template) - 3L, nchar(template))) != ".tex",
    paste(template, ".tex", sep = ""), template)
  template_base <- file_path_sans_ext(template_tex)
  template_path <- ifelse(file.exists(template_tex),
    template_tex, file.path(find.package("exams"), "tex", template_tex))
  if(!all(file.exists(template_path))) stop(paste("The following files cannot be found: ",
    paste(template_raw[!file.exists(template_path)], collapse = ", "), ".", sep = ""))  

  ## read template
  template <- lapply(template_path, readLines)
  ## which input types in template?
  input_types <- function(x) {
    x <- x[grep("\\exinput", x, fixed = TRUE)]
    if(length(x) < 1L) return(NULL) #was# stop("templates must specify at least one \\exinput{}")
    as.vector(sapply(strsplit(sapply(strsplit(x,
      paste("\\exinput{", sep = ""), fixed = TRUE), tail, 1L), "}"), head, 1L))
  }
  template_it <- lapply(template, input_types)
  template_has_header <- sapply(template_it, function(x) "header" %in% x)
  template_has_questionnaire <- sapply(template_it, function(x) "questionnaire" %in% x)
  template_has_exercises <- sapply(template_it, function(x) "exercises" %in% x)

  ## output name processing
  if(is.null(name)) name <- file_path_sans_ext(basename(template_base))

  ## check further inputs (if any)
  if(!is.null(inputs)) {
    if(!all(file.exists(inputs))) stop(paste("The following inputs cannot be found: ",
      paste(inputs[!file.exists(inputs)], collapse = ", "), ".", sep = ""))
  }

  ## convenience functions for writing LaTeX  
  mchoice.symbol <- if(!is.null(control) && !is.null(control$mchoice.symbol)) { ## FIXME: further control options?
    control$mchoice.symbol
  } else {
    c(True = "X", False = " ")
  }
  cloze.collapse <- if(!is.null(control) && !is.null(control$cloze.collapse)) {
    control$cloze.collapse
  } else {
    " / "
  }
  mchoice2quest <- function(x, cmd = "exmchoice") {
    rval <- ifelse(x, mchoice.symbol[["True"]], mchoice.symbol[["False"]])
    rval <- if(length(rval) == 1L) paste("{", rval, "}", sep = "") else {
      paste("{", rval[1L], "}[", paste(rval[-1L], collapse = "]["), "]", sep = "")
    }
    paste("  \\item \\", cmd, rval, sep = "")
  }
  num2quest <- function(x) {
    rval <-  paste("  \\item \\exnum{", 
      paste(strsplit(format(c(100000.000, x), nsmall = 3, scientific = FALSE)[-1], "")[[1]][-7],
      collapse = "}{"), "}", sep = "")
    if(length(x) > 1) rval <- paste(rval, " \\\\\n        \\exnum{",
      paste(strsplit(format(c(100000.000, x), nsmall = 3, scientific = FALSE)[-1], "")[[2]][-7],
      collapse = "}{"), "}", sep = "")
    rval 
  }
  string2quest <- function(x) paste("  \\item \\exstring{", x, "}", sep = "")  
  cloze2quest <- function(x, type) paste(
      "  \\item \n",
      "  \\begin{enumerate}\n   ",
      paste(sapply(seq_along(x), function(i) switch(type[i],
        "schoice" = mchoice2quest(x[[i]], cmd = if(cloze.collapse == "enumerate") "exclozechoice" else "exmchoice"),
        "mchoice" = mchoice2quest(x[[i]], cmd = if(cloze.collapse == "enumerate") "exclozechoice" else "exmchoice"),
        "num" = num2quest(x[[i]]),
        "string" = string2quest(x[[i]]),
        "verbatim" = stop("Question type 'verbatim' is not supported by exams2pdf")
      )), collapse = "\\\\\n    "),
      "\n  \\end{enumerate}",
      collapse = "\n"
    )

  ## set up actual write function
  function(exm, dir, info)
  {
    ## basic indexes
    id <- info$id
    n <- info$n
    m <- length(exm)
  
    ## current directory
    dir_orig <- getwd()
    on.exit(setwd(dir_orig))

    ## (temporary) directory in which LaTeX is compiled
    dir_temp <- if(is.null(texdir)) tempfile() else texdir
    if(!file.exists(dir_temp) && !dir.create(dir_temp))
      stop(gettextf("Cannot create temporary work directory '%s'.", dir_temp))
    setwd(dir_temp) 
    if(is.null(texdir)) on.exit(unlink(dir_temp), add = TRUE)

    ## collect extra inputs
    if(!is.null(inputs)) file.copy(inputs, dir_temp, overwrite = TRUE)
    
    ## collect supplementary files
    supps <- unlist(lapply(exm, "[[", "supplements")) ## FIXME: restrict in some way? omit .csv and .rda?
    if(!is.null(supps)) {
      bn <- basename(supps)
      dups <- which(duplicated(bn))
      if(length(dups)) {
        bnd <- paste(file_path_sans_ext(bn[dups]), "-", 1L:length(dups) + 1L, ".", file_ext(bn[dups]), sep = "")
        dn <- dirname(supps[dups])
        nfn <- file.path(dn, bnd)
        file.rename(supps[dups], nfn)
        supps[dups] <- nfn

        dups_graphics_gsub <- function(pattern, replacement, x) {
          for(i in c("question", "questionlist", "solution", "solutionlist")) {
            if(length(x[[i]])) {
              if(any(ix <- grepl("includegraphics{", x[[i]], fixed = TRUE))) {
                x[[i]][ix] <- gsub("(includegraphics\\{[[:graph:]]+\\})", "\\1.image", x[[i]][ix])
                pn <- paste(file_path_sans_ext(pattern), "}.image", sep = "")
                rn <- paste(file_path_sans_ext(replacement), "}", sep = "")
                if(length(j <- grep(pn, x[[i]], fixed = TRUE)))
                  x[[i]][j] <- gsub(pn, rn, x[[i]][j], fixed = TRUE)
                pn <- paste(pattern, "}.image", sep = "")
                rn <- paste(replacement, "}", sep = "")
                if(length(j <- grep(pn, x[[i]], fixed = TRUE))) {
                  x[[i]][j] <- gsub(pn, rn, x[[i]][j], fixed = TRUE)
                }
              }
            }
          }
          return(x)
        }

        for(j in seq_along(dups))
          exm[[dups[j]]] <- dups_graphics_gsub(bn[dups[j]], bnd[j], exm[[dups[j]]])
      }

      file.copy(supps, dir_temp, overwrite = TRUE)
    }
    
    ## extract required metainfo
    fil <- names(exm) #to assure different file names# sapply(exm, function(x) x$metainfo$file)
    typ <- sapply(exm, function(x) x$metainfo$type)
    sol <- lapply(exm, function(x) x$metainfo$solution)
    clz <- lapply(exm, function(x) x$metainfo$clozetype)

    collapse <- function(x) {
      if(length(x) == 1L) return(x)
      if(cloze.collapse != "enumerate") return(paste(x, collapse = cloze.collapse))
      paste("\\begin{enumerate}\n",
        paste("  \\item ", x, collapse = "\n"),
	"\\end{enumerate}", sep = "")
    }

    ## write out LaTeX code
    for(j in 1L:m) {
    
      ## collapse answer groups of clozes (if necessary)
      if(exm[[j]]$metainfo$type == "cloze") {
        g <- rep(seq_along(exm[[j]]$metainfo$solution), sapply(exm[[j]]$metainfo$solution, length))
        if(!is.list(exm[[j]]$questionlist)) exm[[j]]$questionlist <- as.list(exm[[j]]$questionlist)
        exm[[j]]$questionlist <- sapply(split(exm[[j]]$questionlist, g), collapse)
        if(!is.null(exm[[j]]$solutionlist)) exm[[j]]$solutionlist <- sapply(split(exm[[j]]$solutionlist, g), collapse)
        for(qj in seq_along(exm[[j]]$questionlist)) {
          if(any(grepl(paste("##ANSWER", qj, "##", sep = ""), exm[[j]]$question, fixed = TRUE))) {
            ans <- exm[[j]]$questionlist[qj]
            exm[[j]]$question <- gsub(paste("##ANSWER", qj, "##", sep = ""),
              ans, exm[[j]]$question, fixed = TRUE)
            exm[[j]]$questionlist[qj] <- NA
          }
        }
      }
      
      ## combine question+questionlist and solution+solutionlist
      writeLines(c(
        "",
	"\\begin{question}",
        exm[[j]]$question,
	if(is.null(exm[[j]]$questionlist) || length(ql <- na.omit(exm[[j]]$questionlist)) == 0) NULL else c(
        "\\begin{answerlist}",
        paste("  \\item", ql),
        "\\end{answerlist}"),
	"\\end{question}",
	"",
        if(length(exm[[j]]$solution) | length(exm[[j]]$solutionlist)) {
	  c("\\begin{solution}",
            if(length(exm[[j]]$solution)) exm[[j]]$solution else NULL,
	    if(is.null(exm[[j]]$solutionlist)) NULL else c(
	      "\\begin{answerlist}",
              paste("  \\item", exm[[j]]$solutionlist),
	      "\\end{answerlist}"),
	    "\\end{solution}")
        } else NULL,
	""), paste(fil[j], ".tex", sep = ""))
    }

    ## assign names for output files
    make_full_name <- function(name, id, type = "")
      paste(name, formatC(id, width = floor(log10(n)) + 1L, flag = "0"), ifelse(type == "", "", "."), type, sep = "")
    out_tex <- make_full_name(name, id, type = "tex")
    out_pdf <- make_full_name(name, id, type = "pdf")

    ## compile output files for all templates
    for(j in seq_along(template)) {
      tmpl <- template[[j]]

      ## input header
      if(template_has_header[j]) {
        wi <-  grep("\\exinput{header}", tmpl, fixed = TRUE)
        tmpl[wi] <- if(length(header) < 1) "" else paste("\\", names(header), "{",
 	  sapply(header, function(x) if(is.function(x)) x(id) else paste(as.character(x), collapse = "}{")),
	  "}", collapse = "\n", sep = "")
      }

      ## input questionnaire
      if(template_has_questionnaire[j]) {
        wi <-  grep("\\exinput{questionnaire}", tmpl, fixed = TRUE)
        tmpl[wi] <- paste(c("\\begin{enumerate}", sapply(seq_along(typ), function(i)
	  switch(typ[i],
	    "schoice" = mchoice2quest(sol[[i]]),
	    "mchoice" = mchoice2quest(sol[[i]]),
            "num" =  num2quest(sol[[i]]),
            "cloze" =  cloze2quest(sol[[i]], clz[[i]]),
            "string" = string2quest(sol[[i]]))),
 	  "\\end{enumerate}", ""), collapse = "\n")
      }

      ## input exercise tex
      if(template_has_exercises[j]) {
        wi <-  grep("\\exinput{exercises}", tmpl, fixed = TRUE)
        tmpl[wi] <- paste("\\input{", fil, "}", sep = "", collapse = "\n")
      }

      ## create and compile output tex
      con <- base::file(out_tex[j], open = "w+", encoding = encoding)
      if(encoding != "") tmpl <- base::iconv(tmpl, to = encoding)
      writeLines(tmpl, con = con)
      base::close(con)
      texi2dvi(out_tex[j], pdf = TRUE, clean = TRUE, quiet = quiet)
    }

    ## check output PDF files and copy to output directory
    out_ok <- file.exists(out_pdf)
    if(any(!out_ok)) {
      warning(paste("could not generate the following files:", paste(out_pdf[!out_ok], collapse = ", ")))
      out_pdf <- out_pdf[out_ok]
    }
    if(!is.null(out_pdf)) file.copy(out_pdf, dir, overwrite = TRUE)
    invisible(out_pdf)
  }
}

