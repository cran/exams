nops_fix <- function(
  scans = dir(pattern = "^nops_scan_[[:digit:]]*\\.zip$"),
  exam = NULL,    ## integer. default: all lines with invalid type/id/registration
  id = NULL,      ## integer or character. Last digits of the exam id.
  field = NULL,   ## character vector from "type", "id", "registration", "answers". default: all that need fixing
  answer = NULL,  ## numeric. Number of answers to check (default: all answers)
  check = NULL,   ## character. Fix only answers the fulfill a certain check ("missing", "schoice", "mchoice")
  display = NULL, ## character vector from "plot", "browser", "interactive". default: "plot" if package "png" is available
  string = NULL,  ## logical. String scans (as opposed to multiple-choice scans)?
  language = "en" ## language of the annotation in the interactive HTML form
) {

  ## any scans?
  if(length(scans) < 1L) stop("no 'scans' specified")

  ## directories
  odir <- getwd()
  dir.create(tdir <- tempfile())
  on.exit(unlink(tdir))

  ## copy scans 
  oscans <- file_path_as_absolute(scans)
  file.copy(scans, file.path(tdir, scans <- basename(scans)))
  setwd(tdir)
  on.exit(setwd(odir), add = TRUE)

  ## string scans?
  if(is.null(string)) string <- identical(substr(scans, 1L, 17L), "nops_string_scan_")

  ## unzip scan results (and clean up file names)
  scan_zip <- scans
  scan_fil <- unzip(scan_zip)
  scan_fil <- gsub(normalizePath(file.path(tdir, ""), winslash = "/"), "", normalizePath(scan_fil, winslash = "/"), fixed = TRUE)
  scan_fil <- gsub("/", "", scan_fil, fixed = TRUE)
  data_txt <- if(string) "Daten2.txt" else "Daten.txt"
  if(!(data_txt %in% scan_fil)) {
    file.remove(scan_fil)
    stop(sprintf("'%s' does not contain a '%s' file", scan_zip, data_txt))
  }
  rezip <- FALSE

  ## defaults: how to display scanned png files
  display_orig <- display
  if(is.null(display)) {
    display <- if(requireNamespace("png")) "plot" else "browser"
  }
  display <- sapply(tolower(display), match.arg, choices = c("plot", "browser", "interactive"))
  if("plot" %in% display && !requireNamespace("png")) {
    warning('display = "plot" requires package "png" which is not available, using "browser" instead')
    display <- setdiff(display, "plot")
  }

  ## defaults: fields to check (NULL = fix all that need fixing)
  all_fields <- c("type", "id", "registration", "answers")
  if(string) all_fields <- all_fields[-3L]
  if(!is.null(field)) field <- sapply(tolower(field), match.arg, choices = all_fields)

  ## additional check condition for answers to be checked (NULL = no additional conditions)
  if(!is.null(check)) check <- match.arg(check, c("schoice", "mchoice", "missing"))

  ## convenience functions for checking validty of fields
  valid_digits <- function(x, n = NULL) !is.null(x) && (is.null(n) || (n == nchar(x))) && grepl("^[0-9]*$", x) && !grepl("^[0]*$", x)
  valid_answer <- function(x) !is.null(x) && (nchar(x) == 5L) && grepl("^[0-1]*$", x)

  ## interactive HTML form and JSON strings for display = "interactive"
  html <- readLines(file.path(system.file(package = "exams"), "xml", "nops_fix.html"))
  lang <- if(is.character(language)) nops_language(language) else language
  if(!all(c("RegistrationNumber", "Replacement", "DocumentID", "DocumentType", "Answers") %in% names(lang))) {
    warning("invalid language specification, using 'en' instead")
    lang <- nops_language("en")
  }
  lang <- sprintf("  let inputFieldLabels = '%s';", list2json(lang[c("RegistrationNumber", "Replacement", "DocumentID", "DocumentType", "Answers")]))
  dd_register <- "  let dd_registrations = '[]';" ## FIXME: could infer this from register csv
  dd_ids <- "  let dd_examIds = '[]';"            ## FIXME: could infer this from solutions rds
  scan_path <- "  let scanDataFilePath = './';"
  if(requireNamespace("clipr", quietly = TRUE)) {
    read_clipboard <- clipr::read_clip
    try_clipboard <- TRUE
  } else {
    read_clipboard <- function () readLines("clipboard", warn = FALSE)
    try_clipboard <- .Platform$OS.type == "windows"
  }
  if("interactive" %in% display) {
    message(paste(
      "With interactive display, scan results can be edited in the browser.",
      "Clicking OK closes the form and copies the results to the clipboard.",
      if(try_clipboard) "R can then read the results from the clipboard." else "The results can then be pasted into R.",
      "",
    sep = "\n"))
  }

  ## data formatting: check boxes vs. string
  if(!string) {

    ## coordinates for answers 1-15 / 16-30 / 31-45
    acoord1 <- cbind(0.5532 + rep(0L:2L, each = 5L) * 0.148 + (0L:4L) * 0.027, 0.04125 + 2 * 0.047)
    acoord2 <- acoord1 + cbind(rep(0, 15), 0.376)
    acoord3 <- acoord2 + cbind(rep(0, 15), 0.376 * 60/64)
    acoord <- rbind(acoord1, acoord2, acoord3)

    ## adjustment for id / type digits
    digit_adjust <- c(0, 0)

    ## replace ERROR lines with placeholders
    d <- readLines(data_txt)
    fixme <- grep("ERROR", d, fixed = TRUE)
    if(length(fixme) > 0L) {
      d[fixme] <- paste(sapply(strsplit(d[fixme], " ", fixed = TRUE), "[", 1L), 
        paste(c("XXXXXXXXXXX 00 XXX 0 XXXXXXX", rep.int("00000", 45L)), collapse = " "))
      writeLines(d, data_txt)
      field <- NULL
      rezip <- TRUE
      if(is.null(display_orig)) display <- "interactive"
    }

    ## read Daten.txt:
    ## 1: file / 2: id / 3: scrambling (00) / 4: type / 5: replacement / 6: registration / 7-51: answers
    d <- read.table(data_txt, colClasses = "character")
    nam <- c("file", "id", "scrambling", "type", "replacement", "registration", 1:45)

  } else {

    ## coordinates for answers 1-3
    acoord <- cbind(0.5532 + (0L:2L) * 0.027 - 0.4243, 0.04125 + 2 * 0.047 + 0.50025)

    ## adjustment for id / type digits
    digit_adjust <- c(0.2065, 0)

    ## replace ERROR lines with placeholders
    d <- readLines(data_txt)
    fixme <- grep("ERROR", d, fixed = TRUE)
    if(length(fixme) > 0L) {
      d[fixme] <- paste(sapply(strsplit(d[fixme], " ", fixed = TRUE), "[", 1L), 
        paste(c("XXXXXXXXXXX XXX", rep.int("00000", 3L)), collapse = " "))
      writeLines(d, data_txt)
      field <- NULL
      rezip <- TRUE
      if(is.null(display_orig)) display <- "interactive"
    }

    ## read Daten2.txt:
    ## 1: file / 2: id / 3: type / 4-6: answers
    d <- read.table(data_txt, colClasses = "character")
    nam <- c("file", "id", "type", 1:3)

  }

  ## iterate through rows of Daten(2).txt
  if(!is.null(id)) {
    if(!is.null(exam)) warning("only one of 'exam' and 'id' should be specified, using 'id'")
    all_id <- unique(d[[2L]])
    pre_id <- unique(substr(all_id, 1L, 6L))
    if(is.numeric(id)) {
      if(all(id < 100000) && length(pre_id) == 1L) {
        id <- paste0(pre_id, formatC(id, flag = "0", width = 5L, digits = 5L, format = "fg"))
      } else {
        id <- as.character(id)
      }
    }
    if(!all(id %in% all_id)) {
      warning(paste("the following 'id' could not be found:", paste(id[!(id %in% all_id)], collapse = ", ")))
      id <- id[id %in% all_id]
    }
    exam <- which(d[[2L]] %in% id)
  }
  if(is.null(exam)) exam <- 1L:nrow(d)
  for(i in exam) {

    ## which fields to update?
    if(!is.null(field)) {
      field_i <- field
    } else {
      field_i <- c(
        if(!valid_digits(d[i, if(string) 3L else 4L], 3L)) "type",
        if(!valid_digits(d[i, 2L], 11L)) "id",
        if(!string && !valid_digits(d[i, 6L])) "registration"
      )
      if(length(field_i) == if(string) 2L else 3L) field_i <- c(field_i, "answers")
    }
    if((!is.null(answer) | !is.null(check)) && !("answers" %in% field_i)) field_i <- c(field_i, "answers")

    ## browse/read trimmed pixel matrix
    if(length(field_i) > 0L) {
      if("browser" %in% display) browseURL(d[i, 1L])
      png_i <- if("plot" %in% display) d[i, 1L] else NULL
    }

    ## if interactive display is used, only use that and do not iterate through the individual fields
    if(length(field_i) > 0L && "interactive" %in% display) {
      ## get current scan data and set up JSON string    
      d_i <- d[i, , drop = FALSE]
      names(d_i) <- nam
      if(!string) {
        nex <- if(valid_digits(d_i$type)) as.integer(substr(d_i$type, 2L, 3L)) else 45L
        nex <- pmax(1L, nex)
        d_i <- d_i[, 1L:(6L + nex), drop = FALSE]
      } else {
        nex <- 3L
        d_i$scrambling <- "00"
        d_i$replacement <- "0"
        d_i$registration <- "-"
      }
      d_i$numExercises <- nex
      d_i$numChoices <- "5" ## FIXME: could infer this from solutions rds
      d_i <- sprintf("  let scanData = '%s';", list2json(d_i))
    
      ## insert JSON into HTML nops_fix template and display
      html_i <- html
      html_i[html_i == "</body>"] <- paste(c("</body>", "", "<script>", dd_register, dd_ids, scan_path, lang, d_i, "</script>"), collapse ="\n")
      writeLines(html_i, "nops_fix.html")
      browseURL("nops_fix.html")
    
      ## interaction for updating scan data
      if(try_clipboard) {
        r <- readline(prompt = "After clicking OK in the browser press <Enter> here: ")
        if(length(r) == 0L || nchar(r) == 0L) r <- try(read_clipboard(), silent = TRUE)
        if(inherits(r, "try-error") || length(r) == 0L || nchar(r) == 0L) r <- readline(prompt = "Reading the clipboard from R failed, please paste clipboard here: ")   
      } else {
        r <- readline(prompt = "After clicking OK in the browser, please paste clipboard here: ")
      }
      if(length(r) == 0L || nchar(r) == 0L) stop("Invalid output, maybe the clipboard was modified after clicking OK in the browser?")

      ## update scan data
      d_i <- json2list(r)
      if(d_i$file != d[i, 1L]) stop("Invalid output, possibly the data in the clipboard was not copied/updated correctly?")
      d_i <- d_i[, intersect(nam, names(d_i)), drop = FALSE]
      d[i, match(names(d_i), nam)] <- d_i
    
      ## proceed to next iteration in loop
      rezip <- TRUE
      next
    }

    ## update desired fields
    if("type" %in% field_i) {
      if(is.character(png_i)) {
        png_i <- try(trim_nops_scan(png_i), silent = TRUE)
        if(inherits(png_i, "try-error")) {
          png_i <- NULL
          browseURL(d[i, 1L])
        }
      }
      maskplot(png_i, center = c(0.3925, 0.074) - digit_adjust, prop = c(0.035, 0.078), main = d[i, 1L])
      j <- if(string) 3L else 4L
      p <- sprintf("Correct type (for %s, %s): ", d[i, j], d[i, 1L])
      r <- readline(prompt = p)
      if(r == "") r <- d[i, j]
      while(!valid_digits(r, 3L)) r <- readline(prompt = paste("!Type must be a 3-digit number!", p, sep = "\n"))
      d[i, j] <- r
      if(!string && as.numeric(substr(d[i, j], 1L, 1L)) > 3L) d[i, 5L] <- "1"
      rezip <- TRUE
    }
    
    if("id" %in% field_i) {
      if(is.character(png_i)) {
        png_i <- try(trim_nops_scan(png_i), silent = TRUE)
        if(inherits(png_i, "try-error")) {
          png_i <- NULL
          browseURL(d[i, 1L])
        }
      }
      maskplot(png_i, center = c(0.3925, 0.275) - digit_adjust, prop = c(0.035, 0.19), main = d[i, 1L])
      p <- sprintf("Correct exam ID (for %s, %s): ", d[i, 2L], d[i, 1L])
      r <- readline(prompt = p)
      if(r == "") r <- d[i, 2L]
      while(!valid_digits(r, 11L)) r <- readline(prompt = paste("!ID must be a 11-digit number!", p, sep = "\n"))
      d[i, 2L] <- r
      rezip <- TRUE
    }

    if("registration" %in% field_i) {
      regextra <- as.numeric(substr(d[i, 4L], 1L, 1L)) # 0=regular; 1/2/3=regextra; 4/5/6=regextra+backup
      if(regextra > 3L) regextra <- regextra - 3L
      reglength <- 7L + regextra

      if(is.character(png_i)) {
        png_i <- try(trim_nops_scan(png_i), silent = TRUE)
        if(inherits(png_i, "try-error")) {
          png_i <- NULL
          browseURL(d[i, 1L])
        }
      }
      maskplot(png_i, center = c(0.25, 0.87 - 0.04 * as.numeric(substr(d[i, 4L], 1L, 1L))), prop = 0.35, main = d[i, 1L])
      p <- sprintf("Correct registration number (for %s, %s): ", d[i, 6L], d[i, 1L])
      r <- readline(prompt = p)
      if(r == "") r <- d[i, 6L]
      while(!valid_digits(r, reglength)) r <- readline(prompt = sprintf("!Registration must be a %s-digit number!\n%s", reglength, p))
      d[i, 6L] <- r
      rezip <- TRUE
    }

    if("answers" %in% field_i) {
      k <- if(string) 3L else 6L
      answer_i <- answer
      nx <- if(!string) 1L:as.numeric(substr(d[i, 4L], 2L, 3L)) else 1L:3L
      if(is.null(answer_i)) answer_i <- nx
      if(!is.null(check)) {
        check_i <- switch(check,
          "schoice" = which(!(as.character(d[i, k + nx]) %in% c("00000", "10000", "01000", "00100", "00010", "00001"))),
          "mchoice" = which(as.character(d[i, k + nx]) == "11111"), ## FIXME: what about sheets with fewer alternatives?
          "missing" = which(as.character(d[i, k + nx]) == "00000"))
        answer_i <- intersect(answer_i, check_i)
      }
      for(j in answer_i) {
        if(is.character(png_i)) {
          png_i <- try(trim_nops_scan(png_i), silent = TRUE)
          if(inherits(png_i, "try-error")) {
            png_i <- NULL
            browseURL(d[i, 1L])
          }
        }
        maskplot(png_i, center = acoord[j,], prop = c(0.03, 0.18), main = d[i, 1L])
        p <- sprintf("Correct answer %s (for %s, %s): ", j, d[i, k + j], d[i, 1L])
        r <- readline(prompt = p)
        if(r == "") r <- d[i, k + j]
        r <- answer2digits(r)
        while(!valid_answer(r)) {
          r <- readline(prompt = paste("!Answer must be either one of: 0/1 indicator of length 5 / letters in a-e / single number in 1-5!", p, sep = "\n"))
          r <- answer2digits(r)
        }
        d[i, k + j] <- r
        rezip <- TRUE
      }
    }

  }

  ## update files and copy back
  write.table(d, file = data_txt, quote = FALSE, row.names = FALSE, col.names = FALSE)
  if(rezip) {
    file.remove(scan_zip)
    zip(scan_zip, scan_fil)
    file.copy(scan_zip, oscans, overwrite = TRUE)
  }
  invisible(d)
}

maskplot <- function(x, center = c(0.5, 0.5), prop = 1, ...) {

  ## do nothing if no input
  if(is.null(x)) return(invisible(NULL))

  ## read PNG file if file name provided
  if(is.character(x)) x <- trim_nops_scan(x)

  ## center in pixels
  if(center[1L] < 1) center[1L] <- floor(center[1L] * nrow(x))
  if(center[2L] < 1) center[2L] <- floor(center[2L] * ncol(x))

  ## dimensions in pixels
  d <- ceiling(rep(prop, length.out = 2L) * nrow(x))
  d2 <- c(max(d), ceiling(max(d)/2))

  ## select maximum possible subset of x
  j <- seq(from = -d2[2L] + 1L, by = 1L, length.out = d2[1L])
  i <- center[1L] + j
  j <- center[2L] + j
  if(min(i) < 1L) {
    pad <- abs(min(i)) + 1L
    x <- rbind(matrix(0, nrow = pad, ncol = ncol(x)), x)
    i <- i + pad
  }
  if(max(i) > nrow(x)) {
    pad <- max(i) - nrow(x)
    x <- rbind(x, matrix(0, nrow = pad, ncol = ncol(x)))
  }
  if(min(j) < 1L) {
    pad <- abs(min(j)) + 1L
    x <- cbind(matrix(0, ncol = pad, nrow = nrow(x)), x)
    j <- j + pad
  }
  if(max(j) > ncol(x)) {
    pad <- max(j) - ncol(x)
    x <- cbind(x, matrix(0, ncol = pad, nrow = nrow(x)))
  }  
  x <- x[i, j]

  ## mask extended range (if any)
  m <- nrow(x) - d[1L]
  if(m > 1L) {
    m <- c(1L:floor(m/2), ceiling(nrow(x) - m/2):nrow(x))
    x[m, ] <- x[m, ] * 0.1
  }
  m <- ncol(x) - d[2L]
  if(m > 1L) {
    m <- c(1L:floor(m/2), ceiling(ncol(x) - m/2):ncol(x))
    x[, m] <- x[, m] * 0.1
  }

  ## set up square matrix for visualization
  y <- matrix(0, nrow = d2[1L], ncol = d2[1L])
  m <- if(nrow(x) == nrow(y)) 0L else floor((nrow(y) - nrow(x))/2)
  n <- if(ncol(x) == ncol(y)) 0L else floor((ncol(y) - ncol(x))/2)
  y[m + 1L:nrow(x), n + 1L:ncol(x)] <- x

  ycoord <- which(y > 0.5, arr.ind = TRUE)
  n <- nrow(ycoord)
  ycoord <- rbind(ycoord, which(y <= 0.5 & y > 0.05, arr.ind = TRUE))
  n <- c(n, nrow(ycoord) - n)
  ycoord <- ycoord/d2[1L]

  mar0 <- getOption("mar")  
  par(mar = rep(1, 4))
  on.exit(par(mar = mar0))
  plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, xlim = c(0, 1), ylim = c(0, 1), ...)
  if(prod(dim(ycoord)) > 0L) rect(ycoord[,2L] - 1/d2[1L], 1 - (ycoord[,1L] - 1/d2[1L]),
    ycoord[,2L], 1 - ycoord[,1L], col = rep(gray(c(0, 0.95)), n), border = "transparent") 
}

answer2digits <- function(a) {
  ## split up input answer
  a <- tolower(strsplit(as.character(a), "", fixed = TRUE)[[1L]])
  n <- length(a)
  if(n == 0L) return("00000")
  if(!(n %in% 1L:5L)) return(NULL)
  
  ## set up output digits (always length 5)
  abcde <- c("a", "b", "c", "d", "e")
  d <- structure(rep.int(0L, 5L), .Names = abcde)

  if((n == 1L) && (a %in% c("0", " ", "-"))) {
  ## nothing ticked
  } else if((n == 1L) && (a %in% as.character(1L:5L))) {
  ## single integer
    d[as.integer(a)] <- 1L
  } else if(all(a %in% abcde)) {
  ## letters
    d[a] <- 1L
  } else if(all(a %in% c("0", "1"))) {
  ## 0/1 indicators
    d[seq_along(a)] <- a
  } else {
  ## unknown
    return(NULL)
  }
  
  ## return digits as string
  d <- paste(d, collapse = "")
  return(d)
}

## naive JSON encoder and decoder that is sufficient for
## the JSON strings needed in nops_fix.html (could switch
## to RJSONIO or jsonlite in the future if needed)
list2json <- function(x) {
  x <- lapply(x, as.character)
  x <- unlist(x)
  x <- paste(sprintf('"%s":"%s"', names(x), x), collapse = ", ")
  x <- paste0("{", x, "}")
  return(x)
}

json2list <- function(x, data.frame = TRUE) {
  x <- gsub(' |\\{|\\}|"', "", x)
  x <- matrix(strsplit(x, ",|:")[[1]], nrow = 2L)
  x <- structure(x[2L, ], names = x[1L, ])
  x <- as.list(x)
  if(data.frame) x <- as.data.frame(x, check.names = FALSE)
  return(x)
}
