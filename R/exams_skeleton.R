exams_skeleton <- exams.skeleton <- function(dir = ".",
  type = c("num", "schoice", "mchoice", "string", "cloze"),
  writer = c("exams2html", "exams2pdf", "exams2moodle", "exams2qti12", "exams2qti21", "exams2arsnova", "exams2nops"),
  markup = "markdown", absolute = FALSE, encoding = "UTF-8")
{
  ## match available types/writers
  type <- as.vector(sapply(type, match.arg,
    c("num", "schoice", "mchoice", "cloze", "string")))
  writer <- as.vector(sapply(writer, match.arg,
    c("exams2html", "exams2pdf", "exams2moodle", "exams2qti12", "exams2qti21", "exams2arsnova", "exams2nops")))
  markup <- match.arg(markup, c("latex", "markdown"))

  ## create output directory (if necessary)
  create_dir <- function(path) {
    isdir <- file.info(path)$isdir
    if(identical(isdir, FALSE)) stop(sprintf("file (rather than directory) %s exists", path))
    if(is.na(isdir)) dir.create(path)
  }
  create_dir(dir)
  create_dir(edir <- file.path(dir, "exercises"))
  if(any(c("exams2html", "exams2pdf", "exams2qti12", "exams2qti21") %in% writer)) create_dir(templ <- file.path(dir, "templates"))
  if("exams2nops" %in% writer) create_dir(file.path(dir, "nops"))
  pdir <- find.package("exams")
  if(absolute) dir <- file_path_as_absolute(dir)
  
  ## select exercises for demo script and all available exercises
  exrc <- c(
    "num"     = "deriv",
    "schoice" = "swisscapital",
    "mchoice" = "boxplots",
    "cloze"   = "lm",
    "string"  = "function"
  )
  exrc <- exrc[type]
  exrc <- paste0(exrc, if(markup == "latex") ".Rnw" else ".Rmd")
  axrc <- list.files(path = file.path(pdir, "exercises"), pattern = if(markup == "latex") "Rnw$" else "Rmd$")
  axrc <- axrc[axrc != "confint.Rnw"]
  file.copy(file.path(pdir, "exercises", axrc), file.path(edir, axrc))

  ## encoding always assumed to be UTF-8 starting from R/exams 2.4-0
  if(!is.null(encoding) && !(tolower(encoding) %in% c("", "utf-8", "utf8"))) {
    warning("the only supported 'encoding' is UTF-8")
  }
  encoding <- "UTF-8"

  ## copy templates
  if("exams2pdf" %in% writer) {
    file.copy(file.path(pdir, "tex", c("exam.tex", "solution.tex", "plain.tex")), templ)
  }
  if("exams2html" %in% writer) {
    file.copy(file.path(pdir, "xml", "plain.html"), templ)
  }
  if("exams2qti12" %in% writer) {
    file.copy(file.path(pdir, "xml", "qti12.xml"), templ)
  }

  demo_all <- c(
    '## exams ----------------------------------------------------------------------------',
    '',
    '## load package',
    'library("exams")',
    '',
    '## this script gives an overview of the example exercises provided',
    '## and basic usage of exams2html/exams2pdf - for more advanced usage',
    '## and further interfaces, see the other demo-*.R scripts',
    '',
    '## to get an overview of the available exercises in this demo,',
    '## switch to the "exercises" folder',
    sprintf('setwd("%s")', if(absolute) file.path(dir, "exercises") else "exercises"),
    'dir()',
    '',
    sprintf('## in the following the exercises in %s (.Rnw) format are discussed',
      if(markup == "latex") "R/LaTeX" else "R/Markdown"),
    '## for some more details see also http://www.R-exams.org/tutorials/first_steps/',
    '',
    '',
    '## inspect individual exercises -----------------------------------------------------',
    '',
    '## simply turn a single exercise into a HTML file (shown in browser, using MathML)',
    sprintf('exams2html("%s")', exrc[1L]),
    '## simply turn a single exercise into a HTML file (shown in browser, using MathJax)',
    sprintf('exams2html("%s", converter = "pandoc-mathjax")', exrc[1L]),
    '## or a PDF file (shown in PDF viewer)',
    sprintf('exams2pdf("%s")', exrc[1L]),
    '',
    '## extract the meta-information to check whether it is processed correctly',
    sprintf('exams_metainfo(exams2html("%s"))', exrc[1L]),
    '',
    '',
    '## types of exercises ---------------------------------------------------------------',
    '',
    '## for an overview see also http://www.R-exams.org/templates/',
    '',
    if(!("num" %in% type)) NULL else c(
    '## numeric exercises',
    'exams2html(c(',
    '  "deriv.Rnw",        ## product rule for derivatives',
    '  "tstat.Rnw",        ## 1-sample t-test statistic',
    '  "dist.Rnw",         ## distances and the Pythagorean theorem',
    '  "regression.Rnw",   ## simple linear regression (by hand)',
    '  "fruit.Rnw",        ## image-based systems of linear equations (numeric)',
    '  "lagrange.Rnw",     ## method of Lagrange multipliers',
    '  "currency8.Rnw"     ## convert currencies (UTF-8 encoding)',
    '))',
    ''),
    if(!("schoice" %in% type)) NULL else c(
    '## single-choice exercises',
    'exams2html(c(',
    '  "swisscapital.Rnw", ## knowledge quiz question about the Swiss capital',
    '  "Rlogo.Rnw",        ## knowledge quiz question about the R logo',
    '  "deriv2.Rnw",       ## product rule for derivatives (single-choice, via num_to_schoice)',
    '  "tstat2.Rnw",       ## 1-sample t-test statistic (single-choice, by hand)',
    '  "dist3.Rnw",        ## distances and the Pythagorean theorem (single-choice, via num_to_schoice)',
    '  "fruit2.Rnw",       ## image-based systems of linear equations (single-choice, via num_to_schoice)',
    '  "hessian.Rnw",      ## 2x2 Hessian matrix (single-choice)',
    '  "logic.Rnw"         ## interpretation of logic gates (using TikZ)',
    '))',
    ''),
    if(!("mchoice" %in% type)) NULL else c(
    '## multiple-choice exercises',
    'exams2html(c(',
    '  "switzerland.Rnw",  ## knowledge quiz question about Switzerland',
    '  "gaussmarkov.Rnw",  ## knowledge quiz question about Gauss-Markov assumptions',
    '  "anova.Rnw",        ## 1-way analysis of variance',
    '  "boxplots.Rnw",     ## interpretation of 2-sample boxplots',
    '  "scatterplot.Rnw",  ## interpretation of a scatterplot',
    '  "ttest.Rnw",        ## interpretation of 2-sample t test',
    '  "relfreq.Rnw",      ## interpretation of relative frequency tables',
    '  "cholesky.Rnw",     ## Cholesky decomposition',
    '  "automaton.Rnw"     ## interpretation of automaton diagrams (using TikZ)',
    '))',
    ''),
    if(!("string" %in% type)) NULL else c(
    '## string exercises',
    'exams2html(c(',
    '  "function.Rnw",     ## string question about R functions',
    '  "countrycodes.Rnw"  ## string question about ISO country codes',
    '))',
    ''),
    if(!("cloze" %in% type)) NULL else c(
    '## cloze exercises (combining several num/schoice/mchoice/string parts)',
    'exams2html(c(',
    '  "lm.Rnw",           ## simple linear regression (with CSV data, schoice/num)',
    '  "boxhist.Rnw",      ## univariate exploration of a CSV file (schoice/num)',
    '  "confint2.Rnw",     ## 2-sided confidence interval (num)',
    '  "dist2.Rnw",        ## three distances in a Cartesian coordinate system (num)',
    '  "fourfold.Rnw"      ## fourfold table (num)',
    '))',
    '',
    ''),
    '## other interfaces -----------------------------------------------------------------',
    '',
    '## switch back to the original folder',
    sprintf('setwd("%s")', if(absolute) dir else ".."),
    '',
    '## other interfaces include:',
    '## - exams2pdf for customizable PDF output',
    '## - exams2nops for a fixed PDF format that can be automatically scanned and evaluated',
    '##',
    '## - exams2html for customizable HTML output',
    '## - exams2moodle for Moodle XML that can be imported into Moodle quizzes',
    '## - exams2blackboard for QTI 1.2 that can be imported into Blackboard and compatible systems',
    '## - exams2qti12/exams2qti21 for QTI XML (version 1.2 or 2.1) that can be imported',
    '##   into various learning management systems (e.g., OLAT or OpenOlat among others)',
    '## - exams2canvas for QTI 1.2 for Canvas (via exams2qti12/exams2qti21)',
    '## - exams2openolat for QTI 2.1 (or 1.2) for OpenOlat (via exams2qti12/exams2qti21)',
    '##',
    '## - exams2arsnova for a JSON format that can be imported into ARSnova live quizzes',
    '## - exams2pandoc for customizable outputs in various formats (Docx, ODF, PDF, ...)',
    '',
    '## see the demo-*.R scripts in this directory for more examples',
    'dir()',
    '',
    '',
    '## ----------------------------------------------------------------------------------'
  )
  if(markup == "markdown") demo_all <- gsub(".Rnw", ".Rmd", demo_all, fixed = TRUE)

  demo_exams <- c(
    '## exams ----------------------------------------------------------------------------',
    '',
    '## load package',
    'library("exams")',
    '',
    sprintf('## exam with a simple vector of exercises in %s format',
      if(markup == "latex") "R/LaTeX (.Rnw)" else "R/Markdown (.Rmd)"),
    '## -> alternatively try a list of vectors of more exercises',
    sprintf('myexam <- c("%s")', paste(exrc, collapse = '", "')),
    '',
    ''
  )

  demo_html <- c(demo_exams,
    '## exams2html -----------------------------------------------------------------------',
    '## HTML output (1 file per exam)',
    '## -> typically used for quickly checking if an exercise can be converted to HTML',
    '## -> or customized via suitable templates',
    '',
    '## generate the HTML version of a single exercise (shown in browser)',
    '## with default settings, using MathML',
    sprintf('exams2html("%s")', if(absolute) file.path(dir, "exercises", exrc[1L]) else file.path("exercises", exrc[1L])),
    '',
    '## using MathJax (works in all browsers, including Chrome)',
    sprintf('exams2html("%s", converter = "pandoc-mathjax")', if(absolute) file.path(dir, "exercises", exrc[1L]) else file.path("exercises", exrc[1L])),
    '',
    '## generate a single HTML exam (shown in browser)',
    '## with specification of a template (the default)',
    'exams2html(myexam, n = 1,',
    sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
    sprintf('  template = "%s")', if(absolute) file.path(dir, "templates", "plain.html") else file.path("templates", "plain.html")),
    '',
    '## generate three HTML exams without solutions in output directory',
    'exams2html(myexam, n = 3, name = "html-demo", solution = FALSE,',
    sprintf('  dir = "%s",', if(absolute) file.path(dir, "output") else "output"),
    sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
    sprintf('  template = "%s")', if(absolute) file.path(dir, "templates", "plain.html") else file.path("templates", "plain.html")),
    '',
    '',
    '## ----------------------------------------------------------------------------------'
  )

  demo_pdf <- c(demo_exams,
    '## exams2pdf ------------------------------------------------------------------------',
    '## PDF output (1 file per exam)',
    '## -> typically used for quickly checking if an exercise can be converted to PDF',
    '## -> or customized via suitable templates',
    '',
    '## generate the PDF version of a single exercise (shown in PDF viewer)',
    '## with default settings',
    sprintf('exams2pdf("%s")', if(absolute) file.path(dir, "exercises", exrc[1L]) else file.path("exercises", exrc[1L])),
    '',
    '## generate a single PDF exam (shown in PDF viewer)',
    '## with specification of a template (for an exam)',
    'exams2pdf(myexam, n = 1,',
    sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
    sprintf('  template = "%s")', if(absolute) file.path(dir, "templates", "exam.tex") else file.path("templates", "solution.tex")),
    '',
    '## generate three PDF exams and corresponding solutions in output directory',
    '## (with the header used to set a custom Date and ID for the exam)',
    'exams2pdf(myexam, n = 3, name = c("pdf-exam", "pdf-solution"),',
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
    '',
    '## ----------------------------------------------------------------------------------'
  )

  demo_moodle <- c(demo_exams,
    '## exams2moodle ---------------------------------------------------------------------',
    '## Moodle XML output (1 file containing all exams)',
    '## -> for import into a Moodle system',
    '',
    '## generate Moodle exam with three replications per question',
    'exams2moodle(myexam, n = 3, name = "moodle-demo",',
    sprintf('  dir = "%s",', if(absolute) file.path(dir, "output") else "output"),    
    sprintf('  edir = "%s")', if(absolute) file.path(dir, "exercises") else "exercises"),
    '',
    '## hint: to quickly check (prior to Moodle export) whether each exercise can be',
    '## converted to HTML, exams2html() can be used',
    sprintf('exams2html("%s")', if(absolute) file.path(dir, "exercises", exrc[1L]) else file.path("exercises", exrc[1L])),
    '',
    '',
    '## ----------------------------------------------------------------------------------'
  )

  demo_qti <- c(demo_exams,
    if(!("exams2qti12" %in% writer)) NULL else c(
    '## exams2qti12 ----------------------------------------------------------------------',
    '## XML output in QTI 1.2 format (1 file containing all exams, zipped by default)',
    '## -> for import into QTI-based learning management systems (e.g., OLAT/OpenOlat)',
    '',
    '## generate QTI 1.2 exam with three replications per question',
    '## (showing correct solutions after failed attempts and passing if solving',
    '## at least two items)',
    'exams2qti12(myexam, n = 3, name = "qti12-demo",',
    sprintf('  dir = "%s",', if(absolute) file.path(dir, "output") else "output"),    
    sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
            '  solutionswitch = TRUE, maxattempts = 1, cutvalue = 2)',
    '',
    '## hint: if customization of the QTI 1.2 template is necessary, modify',
    '## the file templates/qti12.xml and then set the argument:',
    sprintf('## template = "%s"', if(absolute) file.path(dir, "templates", "qti12.xml") else file.path("templates", "qti12.xml")),
    '',
    '## hint: to quickly check (prior to QTI export) whether each exercise can be',
    '## converted to HTML, exams2html() can be used',
    sprintf('exams2html("%s")', if(absolute) file.path(dir, "exercises", exrc[1L]) else file.path("exercises", exrc[1L])),
    '',
    ''),
    if(!("exams2qti21" %in% writer)) NULL else c(
    '## exams2qti21 ----------------------------------------------------------------------',
    '## XML output in QTI 2.1 format (1 file containing all exams, zipped by default)',
    '## -> for import into QTI-based learning management systems',
    '',
    '## generate QTI 2.1 exam with three replications per question',
    '## (showing correct solutions after failed attempts and passing if solving',
    '## at least two items)',
    'exams2qti21(myexam, n = 3, name = "qti21-demo",',
    sprintf('  dir = "%s",', if(absolute) file.path(dir, "output") else "output"),    
    sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
            '  solutionswitch = TRUE, maxattempts = 1, cutvalue = 2)',
    '',
    '## hint: if customization of the QTI 2.1 template is necessary, modify',
    '## the file templates/qti21.xml and then set the argument:',
    sprintf('## template = "%s"', if(absolute) file.path(dir, "templates", "qti21.xml") else file.path("templates", "qti21.xml")),
    '',
    '## hint: to quickly check (prior to QTI export) whether each exercise can be',
    '## converted to HTML, exams2html() can be used',
    sprintf('exams2html("%s")', if(absolute) file.path(dir, "exercises", exrc[1L]) else file.path("exercises", exrc[1L])),
    '',
    ''),
    '## ----------------------------------------------------------------------------------'
  )

  ## omit cloze exercises for exams2arsnova
  exrc <- exrc[substr(exrc, 1, 2) != "lm"]
  demo_exams[8L] <- sprintf('myexam <- c("%s")', paste(exrc, collapse = '", "'))
  
  demo_arsnova <- c(demo_exams,
    '## exams2arsnova --------------------------------------------------------------------',
    '## JSON output in ARSnova format (1 file per exam)',
    '## -> for import into an ARSnova session',
    '',
    '## generate ARSnova exam/quiz with one replication per question',
    '## (without allowing abstentions)',
    'exams2arsnova(myexam, n = 1, name = "arsnova-demo",',
      sprintf('  dir = "%s",', if(absolute) file.path(dir, "output") else "output"),    
      sprintf('  edir = "%s",', if(absolute) file.path(dir, "exercises") else "exercises"),
    '  abstention = FALSE)',
    '',
    '## hint: if the arguments url/sessionkey/jsessionid are specified the JSON output',
    '## can be directly sent to the running ARSnova session (without manually saving and',
    '## importing it)',
    '',
    '',
    '## ----------------------------------------------------------------------------------'
  )  

  demo_nops <- c(
    '## exams ----------------------------------------------------------------------------',
    '',
    '## load package',
    'library("exams")',
    '',
    '## switch to dedicated directory for nops output',
    sprintf('setwd("%s")', if(absolute) file.path(dir, "nops") else "nops"),
    '',
    '',
    '## exams2nops -----------------------------------------------------------------------',
    '## PDF output in NOPS format (1 file per exam)',
    '## -> for exams that can be printed, scanned, and automatically evaluated in R',
    '',
    '',
    '## --- Step 1 ---',
    '## exam generation',
    '',
    '## define an exam (= list of exercises)',
    'myexam <- list(',
    '  "tstat2.Rnw",',
    '  "ttest.Rnw",',
    '  "relfreq.Rnw",',
    '  "anova.Rnw",',
    '  c("boxplots.Rnw", "scatterplot.Rnw"),',
    '  "cholesky.Rnw"',
    ')',
    '',
    '## create multiple exams on the disk with different numbers of points',
    '## per exercise (see ?exams2nops for more examples)',
    'set.seed(403)',
    'ex1 <- exams2nops(myexam, n = 2, dir = ".", date = "2015-07-29",',
    '  points = c(1, 1, 1, 2, 2, 3), showpoints = TRUE)',
    'dir()',
    '',
    '## assume the PDF exams were already printed (and possibly backed up',
    '## in a different directory) so that they are not needed anymore',
    'file.remove(dir(pattern = "pdf$"))',
    '',
    '',
    '## --- Step 2 ---',
    '## scan results',
    '',
    '## assume two participants filled out the printed exam sheets',
    '## and the corresponding scans are in two PNG files,',
    'img <- dir(system.file("nops", package = "exams"), pattern = "nops_scan",',
    '  full.names = TRUE)',
    '',
    '## copy the PNG files to the working directory',
    'file.copy(img, to = ".")',
    '',
    '## read the scanned images (all locally available .png files) and collect',
    '## results in a ZIP archive (see ?nops_scan for more details)',
    'nops_scan()',
    'dir()',
    '',
    '## the ZIP archive contains copies of the PNG images so that these are',
    '## can be deleted here (possibly after backup in a different directory)',
    'file.remove(dir(pattern = "png$"))',
    '',
    '',
    '## -- Step 3 ---',
    '## evaluate results',
    '',
    '## three files are required: (a) an RDS file with the exam meta-information',
    '## (see Step 1), (b) a ZIP file with the scanned sheets (see Step 2), (c) a',
    '## CSV file with the student infomation (registration number, name, and some',
    '## for of ID/username)',
    '',
    '## here we create the CSV file on the fly but in practice this will typically',
    '## be processed from some registration service or learning management system etc',
    'write.table(data.frame(',
    '  registration = c("1501090", "9901071"),',
    '  name = c("Jane Doe", "Ambi Dexter"),',
    '  id = c("jane_doe", "ambi_dexter")',
    '), file = "Exam-2015-07-29.csv", sep = ";", quote = FALSE, row.names = FALSE)',
    'dir()',
    '## now the exam can be evaluated creating an output data frame (also stored',
    '## as CSV file) and individual HTML reports (stored in a ZIP file),',
    '',
    '## as there is only exactly on CSV/RDS/ZIP file in the current directory,',
    '## these are found automatically - furthermore an evaluation scheme without',
    '## partial points and differing points per exercise are used',
    'ev1 <- nops_eval(eval = exams_eval(partial = FALSE, negative = FALSE))',
    'dir()',
    '',
    '## inspect evaluated data',
    'ev1',
    '',
    '## inspect corresponding HTML reports',
    'unzip("nops_eval.zip")',
    'browseURL(file.path("jane_doe",    "Exam-2015-07-29.html"))',
    'browseURL(file.path("ambi_dexter", "Exam-2015-07-29.html"))',
    '',
    '',
    '## --- Options ---',
    '## below three typically needed options are discussed:',
    '## (a) using a different evaluation strategy (here with partial credits),',
    '## (b) using a different language (here de/German),',
    '## (c) an error of the participant when filling in the registration number.',
    '',
    '## as for (a): partial credits should only be used for multiple-choice questions',
    '## where at least one alternative is correct and at least one is false',
    '## [note that in this example this is not the case for the first question',
    '## (single-choice) and the third question for Jane Doe (no alternative correct)]',
    '',
    '## as for (c): for Ambi Dexter such an error was included in the PNG example',
    '## image, the actual number is "9911071" but the crosses indicate "9901071"',
    '',
    '## clean up previous evaluation',
    'file.remove(c("nops_eval.csv", "nops_eval.zip"))',
    '',
    '## write correct registration information',
    'write.table(data.frame(',
    '  registration = c("1501090", "9911071"),',
    '  name = c("Jane Doe", "Ambi Dexter"),',
    '  id = c("jane_doe", "ambi_dexter")',
    '), file = "Exam-2015-07-29.csv", sep = ";", quote = FALSE, row.names = FALSE)',
    '',
    '## call nops_eval() with modified options, where the error in the registration',
    '## number of Ambi Dexter will trigger an interactive prompt',
    'ev2 <- nops_eval(eval = exams_eval(partial = TRUE, rule = "false2"),',
    '  language = "de")',
    '',
    '## inspect evaluated data',
    'ev2',
    'cbind(ev1$points, ev2$points)',
    '',
    '## inspect corresponding HTML reports',
    'unzip("nops_eval.zip")',
    'browseURL(file.path("jane_doe",    "Exam-2015-07-29.html"))',
    'browseURL(file.path("ambi_dexter", "Exam-2015-07-29.html"))',
    '',
    '',
    '## ----------------------------------------------------------------------------------'
  )
  if(markup == "markdown") demo_nops <- gsub(".Rnw", ".Rmd", demo_nops, fixed = TRUE)

  writeLines(demo_all, file.path(dir, "demo-all.R"))
  if("exams2html" %in% writer)    writeLines(demo_html,    file.path(dir, "demo-html.R"))
  if("exams2pdf" %in% writer)     writeLines(demo_pdf,     file.path(dir, "demo-pdf.R"))
  if("exams2moodle" %in% writer)  writeLines(demo_moodle,  file.path(dir, "demo-moodle.R"))
  if(any(c("exams2qti12", "exams2qti21") %in% writer)) writeLines(demo_qti, file.path(dir, "demo-qti.R"))
  if("exams2arsnova" %in% writer) writeLines(demo_arsnova, file.path(dir, "demo-arsnova.R"))
  if("exams2nops" %in% writer)    writeLines(demo_nops,    file.path(dir, "demo-nops.R"))

  ## return all scripts invisibly
  invisible(list(
    all = demo_all,
    html = demo_html,
    pdf = demo_pdf,
    moodle = demo_moodle,
    qti = demo_qti,
    arsnova = demo_arsnova,
    nops = demo_nops
  ))
}
