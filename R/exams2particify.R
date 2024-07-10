exams2particify <- function(file, n = 1L, dir = ".", name = "particify",
  quiet = TRUE, resolution = 100, width = 4, height = 4, svg = FALSE,
  abstention = FALSE, fix_choice = FALSE, exshuffle = NULL, ...)
{
  ## Markdown transformer:
  mdtransform <- make_exercise_transform_pandoc(to = "markdown", options = "--wrap=none")

  ## create CSV writer with custom options
  particifywrite <- make_exams_write_particify(name = name, abstention = abstention, fix_choice = fix_choice)

  ## generate xexams
  rval <- xexams(file, n = n, dir = dir,
    driver = list(
      sweave = list(quiet = quiet, pdf = FALSE, png = !svg, svg = svg, resolution = resolution, width = width, height = height),
      read = list(exshuffle = exshuffle),
      transform = mdtransform,
      write = particifywrite),
    ...)

  ## return xexams object invisibly
  invisible(rval)
}

make_exams_write_particify <- function(name = "particify", abstention = FALSE, fix_choice = TRUE)
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
    df <- matrix("", nrow = m, ncol = 7L, dimnames = list(NULL,
      c("format", "body", "additionalText", "options", "correctOptions", "multiple", "abstentionsAllowed")))
    df[, "format"] <- "TEXT"
    df[, "multiple"] <- "false"
    df[, "abstentionsAllowed"] <- "true"

    ## add information for all exercises
    for(i in 1L:m) {
      ## question type
      typ <- exm[[i]]$metainfo$type
      
      ## collapse question text
      df[i, "body"] <- paste(exm[[i]]$question, collapse = "\n")
      if(typ %in% c("schoice", "mchoice")) {
        ql <- fix_choice(exm[[i]]$questionlist)
        df[i, "format"] <- "CHOICE"
        df[i, "options"] <- paste(ql, collapse = "\n")
        df[i, "correctOptions"] <- paste(ql[exm[[i]]$metainfo$solution], collapse = "\n")
        if(typ == "mchoice") df[i, "multiple"] <- "true"
        df[i, "abstentionsAllowed"] <- tolower(as.character(abstention))
      }
      
      ## FIXME for non-CHOICE questions: Is it possible to store correct answer somewhere?
    }
    
    ## turn to data frame and export to CSV
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    filwb <- file(fil, "wb")
    write.table(df, filwb, quote = TRUE, col.names = TRUE, row.names = FALSE, sep = ",")
    close(filwb)
    file.copy(fil, dir, overwrite = TRUE)
  }
}
