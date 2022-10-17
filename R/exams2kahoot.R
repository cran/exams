exams2kahoot <- function(file, n = 1L, dir = ".", name = "kahootquiz",
  quiet = TRUE, time = NULL, ...)
{
  ## export to .xlsx required
  stopifnot(requireNamespace("openxlsx"))

  ## unify time limit
  timeopts <- c(5, 10, 20, 30, 60, 90, 120, 240)
  adjusttime <- function(t) timeopts[which.min(abs(timeopts - as.numeric(t)[1L]))]
  if(!is.null(time)) time <- sapply(time, adjusttime)

  ## use pandoc to convert to plain text
  plaintransform <- make_exercise_transform_pandoc(to = "plain", options = "--wrap=none", base64 = FALSE)

  ## create xlsx
  kahootwrite <- function(exm, dir, info) {
    ## basic indexes
    id <- info$id
    n <- info$n
    m <- length(exm)

    ## output file name
    fil <- gsub("/", " ", name, fixed = TRUE)
    fil <- gsub(" ", "-", fil, fixed = TRUE)
    fil <- paste0(fil, "-", formatC(id, width = floor(log10(n)) + 1L, flag = "0"), ".xlsx")
   
    ## time limits
    if(!is.null(time)) time <- rep_len(time, m)

    ## check whether all exercises are:
    ## - schoice/mchoice
    ## - without supplements
    ## - without overlong question
    wrong_type <- sapply(1L:m, function(n) exm[[n]]$metainfo$file)[
      !sapply(1L:m, function(n) exm[[n]]$metainfo$type %in% c("schoice", "mchoice")) |
      sapply(1L:m, function(n) length(exm[[n]]$supplements) > 0L) |
      sapply(1L:m, function(n) nchar(paste(exm[[n]]$question, collapse = " ")) > 120L) |
      sapply(1L:m, function(n) max(nchar(exm[[n]]$questionlist)) > 75L) ]
    if(length(wrong_type) > 0) {
      stop(paste("all exercises must be schoice/mchoice with plain short questions (<= 120 chars) and answers (<= 75 chars), the following are not supported:",
        paste(wrong_type, collapse = ", ")))
    }

    nanswer <- max(sapply(1L:m, function(n) length(exm[[n]]$questionlist)))
    temp <- matrix("", nrow = m, ncol = 3L + nanswer)
    colnames(temp) <- c("Question", paste("Answer", 1:nanswer), "Time limit", "Correct answer(s)")

    for(j in 1L:m) {
      temp[j, 1L] <- paste(exm[[j]]$question, collapse = " ")
      temp[j, 1L + 1L:nanswer] <- exm[[j]]$questionlist
      temp[j, 2L + nanswer] <- if(!is.null(exm[[j]]$metainfo$time)) {
        adjusttime(exm[[j]]$metainfo$time)
      } else if(!is.null(time)) {
        time[j]
      } else {
        60
      }
      temp[j, 3L + nanswer] <- paste(which(exm[[j]]$metainfo$solution), collapse = ", ")
    }
    
    openxlsx::write.xlsx(as.data.frame(temp, stringsAsFactors = FALSE), fil)
    file.copy(fil, dir, overwrite = TRUE)
  }

  ## generate xexams
  rval <- xexams(file, n = n, dir = dir,
    driver = list(
      sweave = list(quiet = quiet, encoding = "UTF-8"),
      read = NULL,
      transform = plaintransform,
      write = kahootwrite),
    ...)

  ## return xexams object invisibly
  invisible(rval)
}
