\name{nops_eval}
\alias{nops_eval}

\title{Evaluate NOPS Exams}

\description{
  Evaluate NOPS exams produced with \code{\link[exams]{exams2nops}},
  and scanned by \code{\link[exams]{nops_scan}}.
}

\usage{
  nops_eval(register = dir(pattern = "\\\\.csv$"), solutions = dir(pattern = "\\\\.rds$"),
    scans = dir(pattern = "^nops_scan_[[:digit:]]*\\\\.zip$"),
    points = NULL, eval = exams_eval(partial = TRUE, negative = FALSE, rule = "false2"),
    mark = c(0.5, 0.6, 0.75, 0.85), labels = NULL,
    dir = ".", results = "nops_eval",
    html = NULL, col = hcl(c(0, 0, 60, 120), c(70, 0, 70, 70), 90),
    encoding = "UTF-8", language = "en", converter = NULL, interactive = TRUE,
    string_scans = dir(pattern = "^nops_string_scan_[[:digit:]]*\\\\.zip$"),
    string_points = seq(0, 1, 0.25))
}

\arguments{
  \item{register}{character. File name of a CSV file (semicolon-separated)
    of the registered students. Must contain columns \code{"registration"}
    (registration number), \code{"name"} (student name), \code{"id"} (some user name
    or other string unique for each student). The file name should not contain spaces,
    umlaut or other special characters (e.g., something like
    \code{"exam-2015-07-01.csv"} is recommended).}
  \item{solutions}{character. File name of the RDS exam file
    produced by \code{\link[exams]{exams2nops}}.}
  \item{scans}{character. File name of the ZIP file with scanning results
    (containing Daten.txt and PNG files) as produced by
    \code{\link[exams]{nops_scan}}.}
  \item{points}{numeric. Vector of points per exercise. By default read from
    \code{solutions}.}
  \item{eval}{list specification of evaluation policy as computed by
    \code{\link[exams]{exams_eval}}.}
  \item{mark}{logical or numeric. If \code{mark = FALSE}, no marks are computed.
    Otherwise \code{mark} needs to be a numeric vector with threshold values to
    compute marks. The thresholds can either be relative (all lower than 1)
    or absolute. In case results exactly matching a threshold, the better mark is used.}
  \item{labels}{character. Vector of labels for the marks with
    \code{length(mark) + 1} elements. Default is \code{(length(mark) + 1):1}.}
  \item{dir}{character. File path to the output directory (the default being the
    current working directory).}
  \item{results}{character. Prefix for output files.}
  \item{html}{character. File name for individual HTML report files, by default
    the same as \code{register} with suffix \code{.html}.}
  \item{col}{character. Hex color codes used for exercises with
    negative, neutral, positive, full solution.}
  \item{encoding}{character. Encoding of \code{register}, e.g.,
    \code{"latin1"} or \code{"UTF-8"} (default).}
  \item{language}{character. Path to a DCF file with a language specification.
    Currently, the package ships: English (\code{"en"}), Dutch (\code{"nl"}),
    French (\code{"fr"}), German (\code{"de"}), Italian (\code{"it"}),
    Romanian (\code{"ro"}), Portuguese (\code{"pt"}), Spanish (\code{"es"}),
    Turkish (\code{"tr"}).}
  \item{converter}{character. The HTML converter to be used for the \code{language}
    text elements. Can be \code{"none"}, \code{"tth"}, or \code{"pandoc"}.}
  \item{interactive}{logical. Should possible errors in the Daten.txt file
    by corrected interactively? Requires the \pkg{png} package for full
    interactivity.}
  \item{string_scans}{character. Optional file name of the ZIP file with scanning results
    of string exercise sheets (if any) containing Daten2.txt and PNG files as produced
    by \code{\link[exams]{nops_scan}}.}
  \item{string_points}{numeric. Vector of length 5 with points assigned
    to string results.}
}

\details{
  \code{nops_eval} is a companion function for \code{\link[exams]{exams2nops}}
  and \code{\link[exams]{nops_scan}}. It evaluates the scanned exams by computing
  the sums of the points achived and (if desired) maps them to marks. Furthermore
  a HTML report for each individual student is generated (e.g., for upload into
  a learning management system).
}

\value{
  A \code{data.frame} with the detailed exam results is returned invisibly.
  It is also written to a CSV file in the current directory, along with a ZIP
  file containing the HTML reports.
}

\seealso{\code{\link[exams]{exams2nops}}, \code{\link[exams]{nops_scan}}}

\examples{
## --- Preliminaries ---

## load package and enforce par(ask = FALSE)
library("exams")
options(device.ask.default = FALSE)

## set up a temporary working directory in which all files are managed
odir <- getwd()
dir.create(mydir <- tempfile())
setwd(mydir)


## --- Step 1 ---
## exam generation

## define an exam (= list of exercises)
myexam <- list(
  "tstat2.Rnw",
  "ttest.Rnw",
  "relfreq.Rnw",
  "anova.Rnw",
  c("boxplots.Rnw", "scatterplot.Rnw"),
  "cholesky.Rnw"
)

## create multiple exams on the disk with different numbers of points
## per exercise (see ?exams2nops for more examples)
set.seed(403)
ex1 <- exams2nops(myexam, n = 2, dir = ".", date = "2015-07-29",
  points = c(1, 1, 1, 2, 2, 3), showpoints = TRUE)
dir()

## assume the PDF exams were already printed (and possibly backed up
## in a different directory) so that they are not needed anymore
file.remove(dir(pattern = "pdf$"))


## --- Step 2 ---
## scan results

## assume two participants filled out the printed exam sheets
## and the corresponding scans are in two PNG files,
img <- dir(system.file("nops", package = "exams"), pattern = "nops_scan",
  full.names = TRUE)

## copy the PNG files to the working directory
file.copy(img, to = ".")

## read the scanned images (all locally available .png files) and collect
## results in a ZIP archive (see ?nops_scan for more details)
nops_scan()
dir()

## the ZIP archive contains copies of the PNG images so that these are
## can be deleted here (possibly after backup in a different directory)
file.remove(dir(pattern = "png$"))


## -- Step 3 ---
## evaluate results

## three files are required: (a) an RDS file with the exam meta-information
## (see Step 1), (b) a ZIP file with the scanned sheets (see Step 2), (c) a
## CSV file with the student infomation (registration number, name, and some
## for of ID/username)

## here we create the CSV file on the fly but in practice this will typically
## be processed from some registration service or learning management system etc
write.table(data.frame(
  registration = c("1501090", "9901071"),
  name = c("Jane Doe", "Ambi Dexter"),
  id = c("jane_doe", "ambi_dexter")
), file = "Exam-2015-07-29.csv", sep = ";", quote = FALSE, row.names = FALSE)
dir()
## now the exam can be evaluated creating an output data frame (also stored
## as CSV file) and individual HTML reports (stored in a ZIP file),

## as there is only exactly on CSV/RDS/ZIP file in the current directory,
## these are found automatically - furthermore an evaluation scheme without
## partial points and differing points per exercise are used
ev1 <- nops_eval(eval = exams_eval(partial = FALSE, negative = FALSE))
dir()

## inspect evaluated data
ev1

## inspect corresponding HTML reports
if(interactive()) {
unzip("nops_eval.zip")
browseURL(file.path(mydir, "jane_doe",    "Exam-2015-07-29.html"))
browseURL(file.path(mydir, "ambi_dexter", "Exam-2015-07-29.html"))
}


## --- Options ---
if(interactive()) {
## below three typically needed options are discussed:
## (a) using a different evaluation strategy (here with partial credits),
## (b) using a different language (here de/German),
## (c) an error of the participant when filling in the registration number.

## as for (a): partial credits should only be used for multiple-choice questions
## where at least one alternative is correct and at least one is false
## [note that in this example this is not the case for the first question
## (single-choice) and the third question for Jane Doe (no alternative correct)]

## as for (c): for Ambi Dexter such an error was included in the PNG example
## image, the actual number is "9911071" but the crosses indicate "9901071"

## clean up previous evaluation
file.remove(c("nops_eval.csv", "nops_eval.zip"))

## write correct registration information
write.table(data.frame(
  registration = c("1501090", "9911071"),
  name = c("Jane Doe", "Ambi Dexter"),
  id = c("jane_doe", "ambi_dexter")
), file = "Exam-2015-07-29.csv", sep = ";", quote = FALSE, row.names = FALSE)

## call nops_eval() with modified options, where the error in the registration
## number of Ambi Dexter will trigger an interactive prompt
ev2 <- nops_eval(eval = exams_eval(partial = TRUE, rule = "false2"),
  language = "de")

## inspect evaluated data
ev2
cbind(ev1$points, ev2$points)

## inspect corresponding HTML reports
unzip("nops_eval.zip")
browseURL(file.path(mydir, "jane_doe",    "Exam-2015-07-29.html"))
browseURL(file.path(mydir, "ambi_dexter", "Exam-2015-07-29.html"))
}

## switch back to original working directory
setwd(odir)
}

\keyword{utilities}
