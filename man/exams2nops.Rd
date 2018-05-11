\name{exams2nops}
\alias{exams2nops}
\alias{make_nops_template}

\title{Generation of Written Exams for Automatic Evaluation}

\description{
  Generation of exams in PDF format that can be printed, scanned,
  and evaluated automatically.
}

\usage{
  exams2nops(file, n = 1L, nsamp = NULL, dir = NULL, name = NULL,
    language = "en", title = "Exam", course = "",
    institution = "R University", logo = "Rlogo.png", date = Sys.Date(), 
    replacement = FALSE, intro = NULL, blank = NULL, duplex = TRUE, pages = NULL,
    usepackage = NULL, header = NULL, encoding = "", startid = 1L,
    points = NULL, showpoints = FALSE, samepage = FALSE,
    twocolumn = FALSE, reglength = 7L, \dots)

  make_nops_template(n, replacement = FALSE, intro = NULL, blank = NULL,
    duplex = TRUE, pages = NULL, file = NULL, nchoice = 5, encoding = "",
    samepage = FALSE, twocolumn = FALSE, reglength = 7L)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files.}
  \item{n}{integer. The number of copies to be compiled from \code{file}
    (in \code{exams2nops}) and the number of exercises per exam
    (in \code{make_nops_template}), respectively.}
  \item{nsamp}{integer. The number(s) of exercise files sampled from each
    list element of \code{file}. Sampling without replacement is used
    if possible. (Only if some element of \code{nsamp} is larger than
    the length of the corresponding element in \code{file}, sampling
    with replacement is used.)}
  \item{dir}{character. The default is either display on the screen or
    the current working directory.}
  \item{name}{character. A name prefix for resulting exams and RDS file.}
  \item{language}{character. Path to a DCF file with a language specification.
    Currently, the package ships: English (\code{"en"}), Croatian (\code{"hr"}),
    Danish (\code{"da"}), Dutch (\code{"nl"}), French (\code{"fr"}),
    German (\code{"de"}), Hungarian (\code{"hu"}), Italian (\code{"it"}),
    Romanian (\code{"ro"}), Portuguese (\code{"pt-PT"} or \code{"pt-BR"} or \code{"pt"}
    which is synonymous with \code{"pt-PT"}), Spanish (\code{"es"}),
    Swiss German (\code{"gsw"}), Turkish (\code{"tr"}).}
  \item{title}{character. Title of the exam, e.g., \code{"Introduction to Statistics"}.}
  \item{course}{character. Optional course number, e.g., \code{"101"}.}
  \item{institution}{character. Name of the institution at which the exam is conducted.}
  \item{logo}{character. Path to a logo image. If the logo is not found, it is
    simply omitted.}
  \item{date}{character or \code{"Date"} object specifying the date of the exam.}
  \item{replacement}{logical. Should a replacement exam sheet be included?}
  \item{intro}{character. Either a single string with the path to a .tex file or a vector
    with with LaTeX code for optional introduction text on the first page of the exam.}
  \item{blank}{integer. Number of blank pages to be added at the end.
    (Default is chosen to be half of the number of exercises.)
    If \code{pages} is specified, \code{blank} can also be a vector of
    length two with blank pages before and after the extra \code{pages},
    respectively.}
  \item{duplex}{logical. Should blank pages be added after the title page
    (for duplex printing)?}
  \item{pages}{character. Path(s) to additional PDF pages to be included
    at the end of the exam (e.g., formulary or distribution tables).}
  \item{usepackage}{character. Names of additional LaTeX packages to be
    included.}
  \item{header}{list. A list of further options to be passed to
    the LaTeX files.}
  \item{encoding}{character, passed to \code{\link[exams]{xweave}}.}
  \item{startid}{integer. Starting ID for the exam numbers (defaults to 1).}
  \item{points}{integer. How many points should be assigned to each exercise? Note that this
    argument overules any exercise points that are provided within the \code{expoints} tags
    of the exercise files (if any). The vector of points supplied should either have
    length 1 or the number of exercises in the exam.}
  \item{showpoints}{logical. Should the PDF show the number of points associated with
    each exercise (if specified in the Rnw/Rmd exercise or in \code{points})?}
  \item{samepage}{logical. Should the itemized question lists be forced to be on
    the same page?}
  \item{twocolumn}{logical. Should a two-column layout be used?}
  \item{reglength}{integer. Number of digits in the registration ID. The
    default is 7 and it can be increased up to 10.}
  \item{\dots}{arguments passed on to \code{\link[exams]{exams2pdf}}.}
  \item{nchoice}{character. The number of choice alternatives per exercise.}
}

\details{
  \code{exams2nops} is a convenience interface for \code{\link[exams]{exams2pdf}}
  with a dynamically generated title page which can be printed,
  scanned with \code{\link[exams]{nops_scan}} and evaluated automatically
  by \code{\link[exams]{nops_eval}}. It is originally intended for
  single- and multiple choice (schoice/mchoice) questions only but has also
  some limited support for open-ended (string) questions.
  
  The exam sheet consists of various sections where information is either
  printed our filled in by the students. The section with personal data is
  just for human readers, it is not read automatically. The registration number
  has to be filled in in digits and also marked with corresponding crosses
  where only the latter is read automatically. The exam ID/type/scrambling
  are printed directly into the PDF and read automatically after scanning.
  Note that the font in the PDF must not be modified for the reading step
  to work reliably. (A sans-serif font is used and hence the \code{sfmath}
  LaTeX package is also used - if it is installed.) The questions can have
  up to five alternatives which have to answered by the students. The crosses
  are read automatically where both empty and completely filled boxes are
  regarded as not crossed.
  
  Limitations: (a) Only up to five answer alternatives per question are supported.
  (b) Currently, only up to 45 questions are supported. If you have
  more questions, consider splitting the entire exam up into two NOPS exams.
  (c) Only up to 3 open-ended questions can be included. (d) Each question
  must have the same number of answer alternatives and the same number of
  points across random replications. For example, the \code{n} replications
  drawn for the first exercise all need, say, five alternatives and two points.
  Then, the second exercise may have, say, four alternatives and five points
  and so on. But this may not be mixed within the same exercise number.
  
  The examples below show how PDF exams can be generated along with an RDS
  file with (serialized) R data containing all meta-information about the exam.
  The PDFs can be printed out for conducting the exam and the exam sheet
  from the first page then needs to be scanned into PDF or PNG images. Then
  the information from these scanned images can be read by \code{nops_scan},
  extracting information about the exam, the participants, and the corresponding
  answers (as described above). The ZIP file produced by \code{nops_scan}
  along with the RDS of the exam meta-information and a CSV file with participant
  information can then be used by \code{nops_eval} to automatically evaluate
  the whole exam and producing HTML reports for each participant. See
  \code{\link[exams]{nops_eval}} for a worked example.
  
  Currently, up to three open-ended string questions can also be included.
  These do not generate boxes on the first exam sheet but instead a second
  exam sheet is produced for these open-ended questions. It is assumed that
  a human reader reads these open-ended questions and then assigns points
  by marking boxes on this separate sheet. Subsequently, this sheet can
  also be read by \code{nops_scan}.
  
  The \code{language} elements can be specified through a relatively simple
  text file in DCF format, see \url{http://www.R-exams.org/tutorials/nops_language/}
  for more details. The package ships with a variety of languages, the names
  of which correspond to the ISO 639 codes (\url{https://www.loc.gov/standards/iso639-2/php/code_list.php})
  or IETF language tags (\url{https://en.wikipedia.org/wiki/IETF_language_tag}) if
  no ISO 639 codes exists (as for Brazilian Portuguese).
}

\value{
  A list of exams as generated by \code{\link[exams]{xexams}} is
  returned invisibly.
}

\examples{
## load package and enforce par(ask = FALSE)
library("exams")
options(device.ask.default = FALSE)

## define an exam (= list of exercises)
myexam <- list(
  "tstat2.Rnw",
  "ttest.Rnw",
  "relfreq.Rnw",
  "anova.Rnw",
  c("boxplots.Rnw", "scatterplot.Rnw"),
  "cholesky.Rnw"
)

if(interactive()) {
## compile a single random exam (displayed on screen)
exams2nops(myexam, duplex = FALSE, language = "de")
}

## create multiple exams on the disk (in a
## temporary directory)
mydir <- tempdir()

## generate NOPS exam in temporary directory
set.seed(403)
ex1 <- exams2nops(myexam, n = 2, dir = mydir)
dir(mydir)

## use a few customization options: different
## university/logo and language/title
## with a replacement sheet but for non-duplex printing
set.seed(403)
ex2 <- exams2nops(myexam, n = 2,
  institution = "Universit\\\"at Innsbruck",
  name = "uibk", logo = "uibk-logo-bw.png",
  title = "Klausur", language = "de",
  replacement = TRUE, duplex = FALSE)
dir(mydir)
}

\keyword{utilities}
