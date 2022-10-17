browse_exercise <- function(file, dir = ".", template = "plain.html", name = "browse",
  solution = TRUE, exshuffle = FALSE,
  quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE,
  resolution = 100, width = 4, height = 4, svg = FALSE,
  mathjax = TRUE, envir = NULL, converter = "pandoc", seed = NULL, ...)
{
  file <- as.character(unlist(file))

  ## FIXME: exshuffle

  if(isTRUE(solution)) solution <- ""

  ## output directory or display on the fly
  if(missing(dir)) {
    display <- TRUE
    dir.create(dir <- tempfile())
  } else {
    display <- FALSE
  }

  ## output name processing 
  if(is.null(name)) name <- file_path_sans_ext(basename(template))
  
  ## HTML transformer
  to_html <- make_exercise_transform_html(converter = converter, ...)

  ## metainfo extractor
  clps <- function(x) if(is.null(x)) return(NULL) else paste(x, collapse = " | ")
  metainfo <- function(x) {

    ## general metainformation
    x <- x$metainfo
    mi <- c(
      file = x$file,
      name = x$name,
      title = x$title,
      section = x$section,
      version = x$version,
      type = x$type,
      solution = paste(unlist(x$solution), collapse = ", "),
      tolerance = if(x$type %in% c("schoice", "mchoice", "string")) NULL else clps(x$tolerance),
      points = clps(x$points)
    )
    
    ## special cloze handling
    if(x$type == "cloze") {
      mi["type"] <- paste0(mi["type"], " (", clps(x$clozetype), ")")
      mi["solution"] <- clps(sapply(lapply(x$solution, as.character), paste, collapse = ", "))
    }

    ## advanced/extra tags
    mix <- x[(which(names(x) == "string") + 1):length(x)]
    mix <- lapply(mix, function(y) clps(unlist(y)))
    mix <- do.call("c", mix)
    mi <- c(mi, mix)
    
    ## HTML formatting
    mi <- sprintf('<tr class="%s"><td align="left"><tt>%s</tt></td><td align="left">%s</td></tr>',
      rep_len(c("odd", "even"), length(mi)), names(mi), mi)
    return(mi)
  }

  ## combined trafo
  trafo <- function(x) {
    x <- to_html(x)
    
    x$question <- c(
      '<h4>Metainformation</h4>',
      '',
      '<table>',
      '<thead>',
      '<tr class="header">',
      '<th align="left">Tag</th>',
      '<th align="left">Value</th>',
      '</tr>',
      '</thead>',
      '<tbody>',
      metainfo(x),
      '</tbody>',
      '</table>',
      '',
      '<h4>Question</h4>',
      '',
      x$question
    )
    
    if(!is.null(x$solution) && length(x$solution) > 0) x$solution <- c('<h4>Solution</h4>', '', x$solution)
    
    if(!is.null(x$questionlist) && length(x$questionlist) > 0) {
      x$questionlist <- paste0(
        "<b>",
        unlist(lapply(x$metainfo$solution, as.character)),
        ":</b> ",
        x$questionlist
      )
    }
    
    return(x)
  }

  ## HTML writer function
  html_write <- make_exams_write_html(template = template, name = name,
    question = "", solution = solution, mathjax = mathjax)


  ## create final .html exam
  rval <- xexams(file,
    driver = list(sweave = list(quiet = quiet, pdf = FALSE, png = !svg, svg = svg,
      resolution = resolution, width = width, height = height, encoding = "UTF-8", envir = envir),
      read = list(exshuffle = exshuffle),
      transform = trafo,
      write = html_write),
    dir = dir, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose, seed = seed)

  ## display single .html on the fly
  if(display) {
    out <- file.path(dir, paste(name, "1.html", sep = ""))
    out <- normalizePath(out)
    browse_file(out)
  }
  
  ## return xexams object invisibly
  invisible(rval)
}

