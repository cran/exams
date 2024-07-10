exams2evaexam <- function(file, n = 1L, dir = ".", name = "evaexam",
  quiet = TRUE, resolution = 100, width = 4, height = 4,
  points = NULL, fix_choice = TRUE, sep = ";", converter = "pandoc-mathjax", base64 = TRUE, ...)
{
  ## HTML transformer:
  htmltransform <- make_exercise_transform_html(converter = converter, base64 = base64, ...)

  ## create CSV writer with custom options
  evaexamwrite <- make_exams_write_evaexam(name = name, fix_choice = fix_choice, sep = sep)

  ## generate xexams
  rval <- xexams(file, n = n, dir = dir,
    driver = list(
      sweave = list(quiet = quiet, pdf = FALSE, png = TRUE, resolution = resolution, width = width, height = height),
      read = NULL,
      transform = htmltransform,
      write = evaexamwrite),
    points = points, ...)

  ## return xexams object invisibly
  invisible(rval)
}

make_exams_write_evaexam <- function(name = "evaexam", fix_choice = TRUE, sep = ";")
{
  ## file name
  name <- gsub("/", "", name, fixed = TRUE)
  name <- gsub(" ", "-", name, fixed = TRUE)

  ## questionlist processing
  fix_choice <- if(fix_choice) {
    function(x) {
      x <- unlist(x)
      x <- gsub("$", "", x, fixed = TRUE)
      x <- gsub("$", "", x, fixed = TRUE)
      x <- gsub("\\%", "%", x, fixed = TRUE)
      x
    }
  } else {
    function(x) unlist(x)
  }
  
  ## set up actual write function
  function(exm, dir, info)
  {
    ## basic indexes
    id <- info$id
    n <- info$n
    m <- length(exm)

    ## file name
    fil <- paste0(name, "-", formatC(id, width = floor(log10(n)) + 1L, flag = "0"), ".csv")

    ## check whether all exercises are supported
    wrong_type <- sapply(1L:m, function(n) exm[[n]]$metainfo$file)[
      !sapply(1L:m, function(n) exm[[n]]$metainfo$type %in% c("schoice", "mchoice", "num", "string"))]
    if(length(wrong_type) > 0) {
      stop(paste("the following exercises are not supported:",
        paste(wrong_type, collapse = ", ")))
    }

    ## set up character matrix for all exercises
    ## https://help.evasys.de/evaexam/en/user/Help/Help_Text/Help_Text-117.htm
    df <- matrix("", nrow = m, ncol = 11L, dimnames = list(NULL,
      c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")))
    ## A: Title of exercise group
    ## B: Exercise type (2: string, 8: mchoice, 10: schoice)
    ## C: Length of question list
    ## D: Question text
    ## E: Question list Answer1|Answer2|Answer3 (or number of lines in string questions)
    ## F: not used
    ## G: not used
    ## H: Number of points for each list, overall points, negative points (e.g., 0|1|0|4|1)
    ## I: Exam question? (1/0 for yes/no)
    ## J: Assigned difficulty level
    ## K: maximum number of characters for string exercises

    ## currently all questions from R/exams are treated as exam questions
    df[, "I"] <- "1"

    ## add information for all exercises
    for(i in 1L:m) {
      meta <- exm[[i]]$metainfo
      df[i, "A"] <- meta$file ## FIXME: iname?
      df[i, "B"] <- switch(meta$type, "schoice" = "10", "mchoice" = "8", "2")
      df[i, "D"] <- paste(exm[[i]]$question, collapse = "\n")

      if(meta$type %in% c("schoice", "mchoice")) {
        df[i, "C"] <- meta$length
        
        ql <- fix_choice(exm[[i]]$questionlist)
        ql <- gsub("|", "&vert;", ql, fixed = TRUE)
        df[i, "E"] <- paste(ql, collapse = "|")
        
        df[i, "H"] <- paste(c(
          as.numeric(meta$solution),                   ## question list
          if(is.null(meta$points)) 1 else meta$points, ## overall points
          0                                            ## negative points
        ), collapse = "|")
      } else {
        df[i, "H"] <- if(is.null(meta$points)) "1" else meta$points
        df[i, "E"] <- "10" ## 10 lines for input
        df[i, "K"] <- "0"  ## unlimited number of characters      
      }
    }
    
    ## turn to data frame and export to CSV
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    write.table(df, fil, quote = TRUE, col.names = FALSE, row.names = FALSE, sep = sep)
    file.copy(fil, dir, overwrite = TRUE)
  }
}
