## see mime types at
## https://www.iana.org/assignments/media-types/media-types.xhtml
.fileURI_mime_types <- matrix(c(

  "bmp", "image/bmp",
  "png", "image/png",
  "jpg", "image/jpeg",
  "jpeg","image/jpeg",
  "gif", "image/gif",
  "heic","image/heic",
  "svg", "image/svg+xml",
  "wmf", "image/wmf",

  "doc", "application/msword",
  "docx","application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  "css", "text/css",
  "htm", "text/html",
  "html","text/html",
  "pdf", "application/pdf",
  "rtf", "application/rtf",
  "tex", "application/x-tex",
  "txt", "text/plain",
  "xml", "application/xml",

  "c",   "text/plain",
  "js",  "text/javascript",
  "py",  "text/plain",
  "r",   "text/plain",
  "rmd", "text/markdown",
  "md",  "text/markdown",

  "csv", "text/csv",
  "dta", "application/octet-stream",
  "ods", "application/vnd.oasis.opendocument.spreadsheet",
  "raw", "text/plain",
  "rda", "application/octet-stream",
  "rds", "application/octet-stream",
  "sav", "application/octet-stream",
  "tsv", "text/tab-separated-values",
  "xls", "application/vnd.ms-excel",
  "xlsx","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  "zip", "application/zip",

  "mp3", "audio/mpeg",
  "m4a", "audio/mp4",
  "ogg", "audio/ogg",
  "flac","audio/flac",
  "wav", "audio/wav",
  "avi", "video/x-msvideo",
  "mp4", "video/mp4",
  "mkv", "video/x-matroska",

  "gh",  "application/octet-stream",
  "3dm", "model/vnd.3dm"
), ncol = 2L, byrow = TRUE, dimnames = list(NULL, c("ext", "mime")))


fileURI <- function(file, mime = NULL, guess = getOption("exams_guess_mime_type", FALSE)) {
  ## file extension
  ext <- tolower(file_ext(file))

  ## determine mime type
  if(is.null(mime)) mime <- c(structure(.fileURI_mime_types[, "mime"], names = .fileURI_mime_types[, "ext"])[ext])
  if(is.na(mime) && guess) mime <- if(is_binary(file)) "text/plain" else "application/octet-stream"

  ## for unknown mime type zip file
  if(!is.na(mime)) {
    rval <- base64enc::dataURI(file = file, mime = mime)
  } else {
    owd <- getwd()
    setwd(dirname(file))
    on.exit(setwd(owd))
    zipname <- paste(file_path_sans_ext(basename(file)), "zip", sep = ".")
    zip(zipfile = zipname, files = basename(file))
    rval <- base64enc::dataURI(file = zipname, mime = "application/zip")
  }
  return(rval)
}

is_binary <- function(filepath, n = 1000L){
  f <- file(filepath, open = "rb", raw = TRUE)
  on.exit(close(f))
  b <- readBin(f, what = "int", n = n, size = 1L, signed = FALSE)
  return(max(b) > 128L)
}
