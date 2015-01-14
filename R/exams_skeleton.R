exams_skeleton <- exams.skeleton <- function(dir = ".",
  type = c("num", "schoice", "mchoice", "cloze", "string"),
  writer = c("exams2html", "exams2pdf", "exams2moodle", "exams2qti12"),
  absolute = FALSE, encoding = "")
{
  ## match available types/writers
  type <- as.vector(sapply(type, match.arg,
    c("num", "schoice", "mchoice", "cloze", "string")))
  writer <- as.vector(sapply(writer, match.arg,
    c("exams2html", "exams2pdf", "exams2moodle", "exams2qti12")))

  ## create output directory (if necessary)
  create_dir <- function(path) {
    isdir <- file.info(path)$isdir
    if(identical(isdir, FALSE)) stop(sprintf("File (rather than directory) %s exists.", path))
    if(is.na(isdir)) dir.create(path)
  }
  create_dir(dir)
  create_dir(edir <- file.path(dir, "exercises"))
  if(!identical(writer, "exams2moodle")) create_dir(templ <- file.path(dir, "templates"))
  pdir <- find.package("exams")
  if(absolute) dir <- file_path_as_absolute(dir)
  
  ## select exercises fro demo script and all available exercises
  exrc <- c(
    "num"     = "tstat.Rnw",
    "schoice" = "tstat2.Rnw",
    "mchoice" = "boxplots.Rnw",
    "cloze"   = "boxhist.Rnw",
    "string"  = "function.Rnw"
  )
  exrc <- exrc[type]
  axrc <- list.files(path = file.path(pdir, "exercises"), pattern = "Rnw$")
  axrc <- axrc[axrc != "confint.Rnw"]
  file.copy(file.path(pdir, "exercises", axrc), edir)

  ## encoding
  enc <- gsub("-", "", tolower(encoding), fixed = TRUE)
  if(enc %in% c("iso8859", "iso88591")) enc <- "latin1"
  if(enc == "iso885915") enc <- "latin9"
  charset <- encoding
  if(enc == "utf8") {
    exrc <- c(exrc, "currency8.Rnw")
    charset <- "UTF-8"
  }
  if(enc == "latin1") {
    exrc <- c(exrc, "currency1.Rnw")
    charset <- "ISO-8859-1"
  }
  if(enc == "latin9") {
    exrc <- c(exrc, "currency9.Rnw")
    charset <- "ISO-8859-15"
  }
  
  ## copy templates
  if("exams2pdf" %in% writer) {
    file.copy(file.path(pdir, "tex", c("exam.tex", "solution.tex")), templ)
    if(encoding != "") {
      x <- readLines(file.path(pdir, "tex", "exam.tex"))
      i <- grep("documentclass", x, fixed = TRUE)[1L]
      x <- c(x[1L:i], "", sprintf('\\usepackage[%s]{inputenc}', enc), "", x[-(1L:i)])
      writeLines(x, file.path(templ, "exam.tex"))
      x <- readLines(file.path(pdir, "tex", "solution.tex"))
      i <- grep("documentclass", x, fixed = TRUE)[1L]
      x <- c(x[1L:i], "", sprintf('\\usepackage[%s]{inputenc}', enc), "", x[-(1L:i)])
      writeLines(x, file.path(templ, "solution.tex"))      
    }
  }
  if("exams2html" %in% writer) {
    file.copy(file.path(pdir, "xml", "plain.html"), templ)
    if(encoding != "") {
      x <- readLines(file.path(pdir, "xml", "plain.html"))
      i <- grep("</head>", x, fixed = TRUE)[1L] - 1L
      x <- c(x[1L:i], "", sprintf('<meta charset="%s">', charset), "", x[-(1L:i)])
      writeLines(x, file.path(templ, "plain.html"))
    }
  }
  if("exams2qti12" %in% writer) {
    file.copy(file.path(pdir, "xml", "qti12.xml"), templ)
    if(encoding != "") {
      x <- readLines(file.path(pdir, "xml", "qti12.xml"))
      x[1L] <- gsub("UTF-8", charset, x[1L], fixed = TRUE)
      writeLines(x, file.path(templ, "qti12.xml"))
    }
  }
  
  ## start script
  script <- c(
    '## load package',
    'library("exams")',
    '',
    '## exam with a simple vector of exercises',
    '## (alternatively try a list of vectors of more exercises)',
    sprintf('myexam <- c("%s")', paste(exrc, collapse = '", "')),
    if(enc %in% c("utf8", "latin1", "latin9")) sprintf(
    '## note that the currency exercise is in %s encoding',
      paste(unique(c(encoding, charset)), collapse = "/")) else NULL,
    '',
    ''
  )

  if("exams2html" %in% writer) script <- c(script,
    '## generate a single HTML exam (shown in browser)',
    'exams2html(myexam, n = 1,',
    if(encoding != "") sprintf('  encoding = "%s",', encoding) else NULL,
    sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
    sprintf('  template = "%s")', if(absolute) file.path(dir, "templates", "plain.html") else file.path("templates", "plain.html")),
    '',
    '## generate three HTML exams without solutions in output directory',
    'exams2html(myexam, n = 3, name = "html-demo", solution = FALSE,',
    if(encoding != "") sprintf('  encoding = "%s",', encoding) else NULL,
    sprintf('  dir = "%s",', if(absolute) file.path(dir, "output") else "output"),
    sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
    sprintf('  template = "%s")', if(absolute) file.path(dir, "templates", "plain.html") else file.path("templates", "plain.html")),
    '',
    ''
  )
  
  if("exams2pdf" %in% writer) script <- c(script,
    '## generate a single PDF exam (shown in PDF viewer)',
    'exams2pdf(myexam, n = 1,',
    if(encoding != "") sprintf('  encoding = "%s",', encoding) else NULL,
    sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
    sprintf('  template = "%s")', if(absolute) file.path(dir, "templates", "exam.tex") else file.path("templates", "exam.tex")),
    '',
    '## generate three PDF exams and corresponding solutions in output directory',
    '## (with the header used to set a custom Date and ID for the exam)',
    'exams2pdf(myexam, n = 3, name = c("pdf-exam", "pdf-solution"),',
    if(encoding != "") sprintf('  encoding = "%s",', encoding) else NULL,
    sprintf('  dir = "%s",', if(absolute) file.path(dir, "output") else "output"),
    sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
    sprintf('  template = c("%s", "%s"),',
      if(absolute) file.path(dir, "templates", "exam.tex") else file.path("templates", "exam.tex"),
      if(absolute) file.path(dir, "templates", "exam.tex") else file.path("templates", "solution.tex")),
    '  header = list(',
    '    Date = "2015-01-01",',
    '    ID = function(i) formatC(i, width = 5, flag = "0")',
    '  ))',
    '',
    ''
  )

  if("exams2moodle" %in% writer) script <- c(script,
    '## generate Moodle exam with three replications per question',
    'exams2moodle(myexam, n = 3, name = "moodle-demo",',
    if(encoding != "") sprintf('  encoding = "%s",', encoding) else NULL,
    sprintf('  dir = "%s",', if(absolute) file.path(dir, "output") else "output"),    
    sprintf('  edir = "%s")', if(absolute) file.path(dir, "exercises") else "exercises"),
    '',
    ''
  )
  
  if("exams2qti12" %in% writer) script <- c(script,
    '## generate QTI 1.2 exam for OLAT/OpenOLAT with three replications per question',
    '## (showing correct solutions after failed attempts and passing only if solving',
    '## all items)',
    'exams2qti12(myexam, n = 3, name = "qti12-demo",',
    if(encoding != "") sprintf('  encoding = "%s",', encoding) else NULL,
    sprintf('  dir = "%s",', if(absolute) file.path(dir, "output") else "output"),    
    sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
    sprintf('  template = "%s",', if(absolute) file.path(dir, "templates", "qti12.xml") else file.path("templates", "qti12.xml")),
    sprintf('  solutionswitch = TRUE, maxattempts = 1, cutvalue = %i)', length(exrc)),
    '',
    ''
  )
  
  writeLines(script, file.path(dir, "demo.R"))
  invisible(script)
}
