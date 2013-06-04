xexams <- function(file, n = 1L, nsamp = NULL,
  driver = list(sweave = NULL, read = NULL, transform = NULL, write = NULL),
  dir = ".", edir = NULL, tdir = NULL, sdir = NULL)
{
  ## process driver
  if(is.null(driver$sweave)) {
    driver$sweave <- function(f) Sweave(f, quiet = TRUE)
  } else if(is.list(driver$sweave)) {
    driver_sweave_args <- driver$sweave
    driver$sweave <- function(f) do.call("Sweave", c(list(file = f), driver_sweave_args))
  }
  if(is.null(driver$read)) driver$read <- read_exercise
  stopifnot(is.function(driver$sweave), is.function(driver$read),
    is.null(driver$transform) || is.function(driver$transform),
    is.null(driver$write) || is.function(driver$write))

  ## manage directories: 
  ##   - for producing several files an output directory is required
  if(is.null(dir) & !is.null(driver$write)) stop("Please specify an output 'dir'.")
  if(!is.null(dir) && !file.exists(dir) && !dir.create(dir))
    stop(gettextf("Cannot create output directory '%s'.", dir))
  ##   - further: dir (output), dir_orig (original), dir_temp (temp), dir_pkg (package), dir_supp (supplements)
  if(!is.null(dir)) dir <- file_path_as_absolute(dir)
  dir_orig <- getwd()
  on.exit(setwd(dir_orig))
  dir_temp <- if(is.null(tdir)) tempfile() else tdir
  if(!file.exists(dir_temp) && !dir.create(dir_temp))
    stop(gettextf("Cannot create temporary work directory '%s'.", dir_temp))
  dir_pkg <- find.package("exams")
  dir_supp <- if(is.null(sdir)) tempfile() else sdir
  if(!file.exists(dir_supp) && !dir.create(dir_supp))
    stop(gettextf("Cannot create temporary work directory '%s'.", dir_supp))
  
  ## number of available exercises in each element of 'file'
  ## and number of selected samples per element
  nfile <- length(file)
  if(is.null(nsamp)) nsamp <- 1L
  if(length(nsamp) < nfile) nsamp <- rep(nsamp, length.out = nfile)
  navail <- sapply(file, length)  
  if(any(navail < nsamp)) {
    ix <- which(navail < nsamp)
    warning(paste("Only", navail[ix], "exercise(s) available in element", ix,
      "of the 'file' argument. Sampling with replacement will be used in order to obtain",
      nsamp[ix], "replications."))
  }
  
  ## file pre-processing:
  ##   - transform to vector (remember grouping IDs)
  ##   - add paths (generate "foo", "foo.Rnw", "foo.tex", and "path/to/foo.Rnw")
  ##   - check existence (use local files if they exist, otherwise take from package)
  ##   - setup sampling (draw random configuration)
  file_id <- rep(seq_along(file), navail)
  file_raw <- unlist(file)
  file_Rnw <- ifelse(
    tolower(substr(file_raw, nchar(file_raw)-3L, nchar(file_raw))) != ".rnw",
    paste(file_raw, ".Rnw", sep = ""), file_raw)
  file_base <- file_path_sans_ext(file_Rnw)
  file_tex <- paste(file_base, ".tex", sep = "")
  file_path <- search_files(file_Rnw, edir, recursive = !is.null(edir))
  file_path <- ifelse(!is.na(file_path), file_path, file.path(dir_pkg, "exercises", file_Rnw))
  if(!all(file.exists(file_path))) stop(paste("The following files cannot be found: ",
    paste(file_raw[!file.exists(file_path)], collapse = ", "), ".", sep = ""))

  sample_id <- function() unlist(lapply(unique(file_id), function(i) {
    wi <- file_id == i
    if(sum(wi) > 1L)
      sample(which(wi), nsamp[i], replace = navail[i] < nsamp[i])
    else
      rep(which(wi), length.out = nsamp[i])
  }))
  

  ## take everything to temp dir
  file.copy(file_path, dir_temp)
  setwd(dir_temp) 
  on.exit(unlink(dir_temp), add = TRUE)
  
  ## set up list of exams (length n) with list of exercises (length m = sum(nsamp))
  m <- sum(nsamp)
  exm <- rep(list(vector(mode = "list", length = m)), n)
  names(exm) <- paste("exam", formatC(1L:n, width = floor(log10(n)) + 1L, flag = "0"), sep = "")
  
  ## cycle through exams: call Sweave, read LaTeX, store supplementary files
  for(i in 1L:n) {
  
    ## sub-directory for supplementary files
    dir_supp_i <- file.path(dir_supp, names(exm)[i])
    if(!dir.create(dir_supp_i)) stop("could not create directory for supplementary files")
  
    ## select exercise files
    id <- sample_id()
    stopifnot(length(id) == m)
    names(exm[[i]]) <- paste("exercise", formatC(1L:m, width = floor(log10(m)) + 1L, flag = "0"), sep = "")
    
    ## cycle through exercises within exam
    for(j in 1L:m) {

      ## id of exercise within full list of files
      idj <- id[j]

      ## sub-directory for supplementary files
      dir_supp_ij <- file.path(dir_supp_i, names(exm[[i]])[j])
      if(!dir.create(dir_supp_ij)) stop("could not create directory for supplementary files")

      ## dirver: Sweave
      driver$sweave(file_Rnw[idj])

      ## driver: read LaTeX file
      exm[[i]][[j]] <- driver$read(file_tex[idj])

      ## infer and save supplements
      sfile <- dir(pattern = "[.]")
      sfile <- sfile[!(sfile %in% c(file_tex, file_Rnw))]
      if(length(sfile) > 0L) {
        file.copy(sfile, dir_supp_ij)
	file.remove(sfile)
      }
      exm[[i]][[j]]$supplements <- structure(file.path(dir_supp_ij, sfile), names = sfile, dir = dir_supp_ij)

      ## driver: transform exercise (e.g., LaTeX -> HTML)
      if(!is.null(driver$transform)) exm[[i]][[j]] <- driver$transform(exm[[i]][[j]])
    }

    ## driver: write output for each exam
    if(!is.null(driver$write)) driver$write(exm[[i]], dir = dir, info = list(id = i, n = n)) ## FIXME: do we need further information?
  }

  invisible(exm)
}

exams_metainfo <- function(x, ...) {
  if(inherits(x, "exams_metainfo")) return(x)
  structure(lapply(x, function(xi) lapply(xi, "[[", "metainfo")),
    class = "exams_metainfo")
}
