expar <- function(file, ...)
{
  ## parameters to keep fixed
  par <- list(...)
  if(length(par) < 1L) stop("no parameters specified")
  if(length(par) == 1L && is.list(par[[1L]])) par <- par[[1L]]
  par <- sapply(par, deparse)

  ## find file
  file <- file[1L]
  file <- ifelse(
    tolower(substr(file, nchar(file) - 3L, nchar(file))) %in% c(".rnw", ".rmd"),
    file, paste(file, ".Rnw", sep = ""))
  file_ext <- tools::file_ext(file)
  if(!file.exists(file)) {
    if(file.exists(file.path(find.package("exams"), "exercises", file))) {
      file <- file.path(find.package("exams"), "exercises", file)
    } else {
      stop(sprintf("exercise file '%s' cannot be found", file))
    }
  }  

  ## new temporary file name
  hex <- function(x) format(as.hexmode(as.integer(x)), upper.case = TRUE)
  tfile <- unclass(Sys.time())
  tfile <- c(hex(tfile), hex(1000000 * (tfile - floor(tfile))))
  tfile <- paste0(tools::file_path_sans_ext(basename(file)), "_", tfile[1], "_", tfile[2], ".", file_ext)
  tfile <- file.path(tempdir(), tfile) ## FIXME: tempdir() currently hard-coded
  tfile <- normalizePath(tfile, mustWork = FALSE)

  ## read file and replace
  txt <- readLines(file)
  sta <- grep(if(tolower(file_ext) == "rmd") "^```\\{(.+)\\}" else "^<<(.+)>>=", txt)
  if(length(sta) < 1L) stop("no code chunk found")
  sta <- sta[1L]
  end <- grep(if(tolower(file_ext) == "rmd") "^```" else "^@", txt[-(1L:sta)])
  if(length(end) < 1L) stop("no complete code chunk found")
  end <- sta + end[1L]
  for(i in seq_along(par)) {
    patterni <- sprintf("^(%s)\\s*(\\=|<-)", names(par)[i])
    j <- grep(patterni, txt[(sta + 1L):(end - 1L)])
    if(length(j) < 1L) {
      warning(sprintf("parameter '%s' not defined in first code chunk", names(par)[i]))
    } else {
      j <- sta + j[1L]
      txt[j] <- gsub(patterni, sprintf("%s <- %s #", names(par)[i], par[i]), txt[j])
    }
  }

  ## temporary file path
  writeLines(txt, tfile)
  return(tfile)
}
