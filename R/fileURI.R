## see mime types at e.g.
## http://www.freeformatter.com/mime-types-list.html
.fileURI_mime_types <- matrix(c(

  "bmp", "image/bmp",
  "png", "image/png",
  "jpg", "image/jpeg",
  "jpeg","image/jpeg",
  "gif", "image/gif",
  "svg", "image/svg+xml",

  "doc", "application/msword",
  "docx","application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  "htm", "text/html",
  "html","text/html",
  "pdf", "application/pdf",
  "tex", "application/x-tex",
  "txt", "text/plain",
  "xml", "application/xml",

  "csv", "text/csv",
  "dta", "application/octet-stream",
  "ods", "application/vnd.oasis.opendocument.spreadsheet",
  "raw", "text/plain",
  "rda", "application/octet-stream",
  "sav", "application/octet-stream",
  "tsv", "text/tab-separated-values",
  "xls", "application/vnd.ms-excel",
  "xlsx","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  "zip", "application/zip"

), ncol = 2L, byrow = TRUE, dimnames = list(NULL, c("ext", "mime")))


fileURI <- function(file) {
  f_ext <- tolower(file_ext(file))
  if(any(ix <- f_ext == .fileURI_mime_types[, "ext"])) {
    rval <- base64enc::dataURI(file = file, mime = .fileURI_mime_types[ix, "mime"])
  } else {
    owd <- getwd()
    setwd(dirname(file))
    zip(zipfile = zipname <- paste(file_path_sans_ext(basename(file)), "zip", sep = "."),
      files = basename(file))
    rval <- base64enc::dataURI(file = zipname, mime = "application/zip")
    setwd(owd)
  }
  rval
}
