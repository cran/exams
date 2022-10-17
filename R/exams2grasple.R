exams2grasple <- function(file, n = 1L, dir = ".",
  name = NULL, quiet = TRUE, resolution = 100, width = 4, height = 4, svg = FALSE, encoding = "UTF-8", converter = "pandoc-mathjax",
  zip = TRUE, use_solutionlist = TRUE, license_name = NULL, license_description = NULL, license_value = NULL, license_link = NULL, ...)
{

  ## create a name
  if(is.null(name)) name <- "grasple"

  ## Markdown transformer:
  ## - math mode with \( rather than $

  mdtransform <- make_exercise_transform_html(converter = converter)

  ## create JSON write with custom options
  grasplewrite <- make_exams_write_grasple(name = name, license_name = license_name, license_description = license_description,
                                           license_value = license_value, license_link = license_link, zip = zip, use_solutionlist = use_solutionlist)

  ## generate xexams
  rval <- xexams(file, n = n,  dir = dir,
    driver = list(
      sweave = list(quiet = quiet, pdf = FALSE, png = !svg, svg = svg, resolution = resolution, width = width, height = height, encoding = encoding),
      read = NULL,
      transform = mdtransform,
      write = grasplewrite),
    ...
  )

  ## return xexams object invisibly
  invisible(rval)
}

make_exams_write_grasple <- function(name = NULL, license_name = NULL, license_description = NULL, license_value = NULL, license_link = NULL, zip = TRUE, use_solutionlist = TRUE)
{
  ## open_answer block of question template

  number_answer <- list(
    list(
      id = 1,
      percentage_of_full_points = 100,
      correct = TRUE,
      rule_definition = list(
        and = list(
          list(
            inverse = FALSE,
            lhs = "student.answer1",
            relation = "greater_than_or_equal",
            rhs = "L"
          ),
          list(
            inverse = FALSE,
            lhs = "student.answer1",
            relation = "less_than_or_equal",
            rhs = "U"
          )
        )
      )
    )
  )
  ## question template

  qtemp <- list(
    id = 0,
    name = "1",
    mode = "at_once",
    license = c(name = if(is.null(license_name)) "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License" else license_name,
                  description = if(is.null(license_description)) "The ShareStats item bank is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License" else license_description,
                  value = if(is.null(license_value)) "cc-by-nc-sa" else license_value,
                  link = if(is.null(license_link)) "https:\\/\\/creativecommons.org\\/licenses\\/by-nc-sa\\/4.0" else gsub("/", "\\\\/", license_link)),
    questions = list(list(
      id = 2,
      type = "open-math",
      question = list(
        list(en = ""),
        list(nl = "")
      ),
    answers = list(),
      feedback_correct = list(
        list(en = ""),
        list(nl = "")
      ),
      feedback_incorrect = list(
        list(en = ""),
        list(nl = "")
      )
    )
    )
  )

  ## set up actual write function
  function(exm, dir, info)
  {
    ## basic indexes
    id <- info$id
    n <- info$n
    m <- length(exm)

    ## check whether all exercises are schoice/num
    wrong_type <- sapply(1L:m, function(n) exm[[n]]$metainfo$file)[
      !sapply(1L:m, function(n) exm[[n]]$metainfo$type %in% c("schoice", "num"))]
    if(length(wrong_type) > 0) {
      stop(paste("the following exercises are not supported:",
        paste(wrong_type, collapse = ", ")))
    }

    for(j in 1L:m) {
      ## fix square brackets issue
      exm[[j]]$solution <- fix_pandoc_env(exm[[j]]$solution)
      exm[[j]]$question <- fix_pandoc_env(exm[[j]]$question)
      exm[[j]]$questionlist <- fix_pandoc_list(exm[[j]]$questionlist)
      exm[[j]]$solutionlist <- fix_pandoc_list(exm[[j]]$solutionlist)

      ## if solutionlist does not contain valuable feedback pass general
      ## feedback to each of the options
      if(!use_solutionlist & exm[[j]]$metainfo$type == "schoice"){
        for(o in 1 : exm[[j]]$metainfo$length){
          exm[[j]]$solutionlist[o] <- paste0(if(any(nchar(exm[[j]]$solution))) exm[[j]]$solution else "", collapse = " ")
        }
      }

      ## copy question template
      json <- qtemp
      if(is.null(exm[[j]]$metainfo$Language)) exm[[j]]$metainfo$Language <- "English"
      json$id <- json$questions[[1]]$id <- as.numeric(format(Sys.time(), "%H%M%S")) + j
      json$name <- exm[[j]]$metainfo$name
      if(exm[[j]]$metainfo$type == "schoice") json$questions[[1]]$type <- "mc"
      if(exm[[j]]$metainfo$type == "num") json$questions[[1]]$answers <- number_answer

      if(grepl("nglish", exm[[j]]$metainfo$Language)) {
        json$questions[[1]]$question[[1]]$en <- paste(exm[[j]]$question, collapse = " ")
        json$questions[[1]]$feedback_correct[[1]]$en  <- paste(exm[[j]]$solution, collapse = " ")
        json$questions[[1]]$feedback_incorrect[[1]]$en  <- paste(exm[[j]]$solution, collapse = " ")
      }

      if(grepl("utch", exm[[j]]$metainfo$Language)) {
        json$questions[[1]]$question[[2]]$nl <- paste(exm[[j]]$question, collapse = " ")
        json$questions[[1]]$feedback_correct[[2]]$nl  <- paste(exm[[j]]$solution, collapse = " ")
        json$questions[[1]]$feedback_incorrect[[2]]$nl  <- paste(exm[[j]]$solution, collapse = " ")
      }

      if(exm[[j]]$metainfo$type == "schoice") json$questions[[1]]$answers <- lapply(seq_along(exm[[j]]$questionlist),
                function(i) list(id = json$id + 1000 + i,  display_answer = list(
                  list(en = if(exm[[j]]$metainfo$Language == "English") as.vector(exm[[j]]$questionlist[i]) else ""),
                  list(nl = if(exm[[j]]$metainfo$Language == "Dutch") as.vector(exm[[j]]$questionlist[i]) else "")),
                  percentage_of_full_points = exm[[j]]$metainfo$solution[i]*100, correct = exm[[j]]$metainfo$solution[i],
                  feedback = list(list(en = if(exm[[j]]$metainfo$Language == "English") as.vector(exm[[j]]$solutionlist[i]) else ""),
                                  list(nl = if(exm[[j]]$metainfo$Language == "Dutch") as.vector(exm[[j]]$solutionlist[i]) else ""))))


      if(exm[[j]]$metainfo$type == "num") {
        json$questions[[1]]$answers[[1]]$id <- json$id + 1000 + 1
        json$questions[[1]]$answers[[1]]$rule_definition$and[[1]]$rhs <- paste0(exm[[j]]$metainfo$solution - exm[[j]]$metainfo$tolerance)
        json$questions[[1]]$answers[[1]]$rule_definition$and[[2]]$rhs <- paste0(exm[[j]]$metainfo$solution + exm[[j]]$metainfo$tolerance)
      }
      ## create and copy output json
	  fil <- paste0("exercise_", json$id, ".json")
      writeLines(RJSONIO::toJSON(json), fil)
    }
    ## compress?
    if(zip) {
      zip(zipfile = zipname <- paste(name, "zip", sep = "."), files = list.files(pattern = "\\.json$"))
    } else zipname <- list.files(pattern = "\\.json$")

    ## copy the final .zip file
    file.copy(file.path(zipname), dir, recursive = TRUE)
  }
}

  ## fix to remove the square brackets that pandoc produces for math display (not inline!).
  ## Grasple fails when these brackets surround the aligned environment
  ## Also remove CSS ref math inline and display within <span>
fix_pandoc_env <- function(x) {
  if(any(grepl('class=\\"math', x))) {
    x <- gsub("\\{eqnarray\\}|\\{eqnarray\\*\\}", "\\{aligned\\}", x)
    x <- paste(x, collapse = " ")
    mds <- regmatches(x, gregexpr('(?<=<span class="math display">)(.|\n)*?(?=</span>)', x, perl = TRUE))[[1]]
    if(length(mds) > 0){
      for(i in 1 : length(mds)){
        x <- gsub(mds[i], gsub('\\\\\\[|\\\\\\]', '', mds[i]), x, fixed = TRUE)
        mds[i] <- gsub('\\\\\\[|\\\\\\]', '', mds[i])
        if(!grepl("\\{aligned\\}", mds[i])) x <- gsub(mds[i], paste0('\\begin{aligned}', mds[i], '\\end{aligned}'), x, fixed = TRUE)
      }
      x <- gsub('\\begin{aligned}', '\\(\\begin{aligned}', x, fixed = TRUE)
      x <- gsub('\\end{aligned}', '\\end{aligned}\\)', x, fixed = TRUE)
    }
    x <- gsub('\\s+class=\\"math inline\\"', "", x)
    x <- gsub('\\s+class=\\"math display\\"', "", x)
  }
  return(x)
}

  ## same for questionlist and solutionlist, only contain inline math which needs to be adjusted
fix_pandoc_list <- function(x) {
  z <- paste(x, collapse = " ")
  z <- regmatches(z, gregexpr('(?<=<span class="math inline">)(.|\n)*?(?=</span>)', z, perl = TRUE))[[1]]
  if(length(z) > 0){
    for(i in 1 : length(z)){
      x <- sub(paste0('<span class="math inline">', z[i], "</span>"), z[i], x, fixed = TRUE)
    }
  }
  return(x)
}
