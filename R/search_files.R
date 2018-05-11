search_files <- function(file, dir = ".", recursive = TRUE)
{
  if(is.null(dir)) dir <- "."
  lf <- list.files(dir, full.names = TRUE, recursive = recursive)
  bn <- basename(lf)
  gr <- lapply(file, function(f) which(bn == f))
  if(any((gl <- sapply(gr, length)) > 1L)) {
    warning(paste("The following files are found more than once, first match used:",
      paste(file[gl > 1L], collapse = ", ")))
  }
  ix <- rep.int(seq_along(file), gl)
  gr <- unlist(gr)
  fp <- file.path(dirname(lf[gr]), file[ix])
  fe <- file.exists(fp)
  fp[match(seq_along(file), ix[fe])]
}

include_supplement <- function(file, dir = NULL, recursive = FALSE) {
  if(is.null(dir)) {
    dir <- try(.exams_get_internal("xexams_dir_exercises"), silent = TRUE)
    if(inherits(dir, "try-error") | is.null(dir)) dir <- getwd()
  } else {
    if(!file.exists(dir)) {
      odir <- dir
      dir <- try(file.path(.exams_get_internal("xexams_dir_exercises"), dir), silent = TRUE)
    }
    if(inherits(dir, "try-error") | !file.exists(dir)) {
      warning(sprintf("The 'dir' could not be found: %s", odir))
      dir <- getwd()
    }
  }
  sfile <- search_files(file, dir = dir, recursive = recursive)
  if(any(is.na(sfile))) {
    warning(sprintf("The following supplement files could not be found: %s.",
      paste(file[is.na(sfile)], collapse = ", ")))
    file <- file[!is.na(sfile)]
    sfile <- sfile[!is.na(sfile)]
  }
  if(length(file) > 0L) file.copy(sfile, file)
}

match_exams_call <- function(which = 1L, deparse = TRUE) {
  if(getRversion() < "3.2.0") return("")
  rval <- .exams_get_internal("xexams_call")
  if(deparse) rval <- sapply(rval, function(x) deparse(x[[1L]], width.cutoff = 500))
  if(!is.null(which)) rval <- rval[[which]]
  rval[rval == "NULL"] <- ""
  return(rval)
}

match_exams_device <- function() .exams_get_internal("xweave_device")
