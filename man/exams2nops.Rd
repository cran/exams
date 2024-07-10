\name{exams2nops}
\alias{exams2nops}
\alias{make_nops_template}
\encoding{UTF-8}

\title{Generation of Written Exams for Automatic Evaluation}

\description{
  Generation of exams in PDF format that can be printed, scanned,
  and evaluated automatically.
}

\usage{
  exams2nops(file, n = 1L, nsamp = NULL, dir = ".", name = NULL,
    language = "en", title = "Exam", course = "",
    institution = "R University", logo = "Rlogo.png", date = Sys.Date(), 
    replacement = FALSE, intro = NULL, blank = NULL, duplex = TRUE, pages = NULL,
    usepackage = NULL, header = NULL, encoding = "UTF-8", startid = 1L,
    points = NULL, showpoints = FALSE, samepage = FALSE, newpage = FALSE,
    twocolumn = FALSE, reglength = 7L, seed = NULL, \dots)

  make_nops_template(n, replacement = FALSE, intro = NULL, blank = NULL,
    duplex = TRUE, pages = NULL, file = NULL, nchoice = 5, encoding = "UTF-8",
    samepage = FALSE, newpage = FALSE, twocolumn = FALSE, reglength = 7L)
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
  \item{dir}{character. The default is either display on the screen
    (if \code{n = 1L}) or the current working directory.}
  \item{name}{character. A name prefix for resulting exams and RDS file.}
  \item{language}{character. Path to a DCF file with a language specification.
    See below for the list of supported languages.}
  \item{title}{character. Title of the exam, e.g., \code{"Introduction to Statistics"}.}
  \item{course}{character. Optional course number, e.g., \code{"101"}.}
  \item{institution}{character. Name of the institution at which the exam is conducted.}
  \item{logo}{character. Path to a logo image (in a file format supported by pdfLaTeX).
    If set to \code{NULL}, the logo is omitted.}
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
  \item{header}{character vector or list. Either a character vector with LaTeX
    code to include in the header or a named list with further options to
    be passed to the LaTeX files.}
  \item{encoding}{character, ignored. The encoding is always assumed to be UTF-8.}
  \item{startid}{integer. Starting ID for the exam numbers (defaults to 1).}
  \item{points}{integer. How many points should be assigned to each exercise? Note that this
    argument overules any exercise points that are provided within the \code{expoints} tags
    of the exercise files (if any). The vector of points supplied should either have
    length 1 or the number of exercises in the exam.}
  \item{showpoints}{logical. Should the PDF show the number of points associated with
    each exercise (if specified in the Rnw/Rmd exercise or in \code{points})?}
  \item{samepage}{logical. Should the itemized question lists be forced to be on
    the same page?}
  \item{newpage}{logical. Should each exercise start on a new page? (Technically,
    a page break is added after each exercise.)}
  \item{twocolumn}{logical. Should a two-column layout be used?}
  \item{reglength}{integer. Number of digits in the registration ID. The
    default is 7 and it can be increased up to 10. In case of \code{reglength < 7},
    internally \code{reglength = 7} is enforced (and thus necessary in the registration
    CSV file) but the initial ID digits are fixed to 0 in the exam sheet and
    corresponding boxes ticked already.}
  \item{seed}{integer matrix or logical. Either \code{NULL} (default), logical, or a
    matrix of random seeds for each possible exercise to be set prior to calling
    \code{driver@sweave}. If \code{NULL} no random seeds are set. If a matrix,
    the number of rows must be \code{n} and the number of columns must
    correspond to \code{unlist(file)}. If \code{TRUE} a suitable matrix of seeds
    is sampled.}
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
  
  Tutorial for NOPS workflow: \url{https://www.R-exams.org/tutorials/exams2nops/}.
  
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
  text file and the package already ships with:
  English (\code{"en"}),
  Croatian (\code{"hr"}),
  Danish (\code{"da"}),
  Dutch (\code{"nl"}),
  Finnish (\code{"fi"}),
  French (\code{"fr"}),
  Galician (\code{"gl"}),
  German (\code{"de"}),
  Hungarian (\code{"hu"}),
  Italian (\code{"it"}),
  Japanese (\code{"ja"}),
  Korean (\code{"ko"}),
  Norwegian (Bokmål, \code{"no"}),
  Portuguese (\code{"pt-PT"} or \code{"pt-BR"}; also \code{"pt"} is synonymous with \code{"pt-PT"}),
  Romanian (\code{"ro"}),
  Russian (\code{"ru"}),
  Serbian (\code{"sr"}),
  Slovak (\code{"sk"}),
  Slovenian (\code{"sl"}),
  Spanish (\code{"es"}),
  Swiss German (\code{"gsw"}),
  Turkish (\code{"tr"}),
  Vietnamese (\code{"vi"}).
  Note that the language names correspond to the ISO 639 codes
  (\url{https://www.loc.gov/standards/iso639-2/php/code_list.php})
  or IETF language tags (\url{https://en.wikipedia.org/wiki/IETF_language_tag}) if
  no ISO 639 codes exists (as for Brazilian Portuguese).
  For more details about the underlying text file in DCF format, see
  \url{https://www.R-exams.org/tutorials/nops_language/}

}

\value{
  A list of exams as generated by \code{\link[exams]{xexams}} is
  returned invisibly.
}

\examples{
## load package and enforce par(ask = FALSE)
## additionally, for simplicity, enforce using the basic
## tools::texi2dvi() LaTeX interface instead of the more
## flexible/robust tinytex::latexmk()
library("exams")
oopt <- options(device.ask.default = FALSE, exams_tex = "tools")

## define an exam (= list of exercises)
myexam <- list(
  "tstat2.Rmd",
  "ttest.Rmd",
  "relfreq.Rmd",
  "anova.Rmd",
  c("boxplots.Rmd", "scatterplot.Rmd"),
  "cholesky.Rmd"
)

if(interactive()) {
## compile a single random exam (displayed on screen)
exams2nops(myexam, duplex = FALSE, language = "de")
}

## create multiple exams on the disk (in a
## temporary directory)
dir.create(mydir <- tempfile())

## generate NOPS exam in temporary directory
set.seed(403)
ex1 <- exams2nops(myexam, n = 2, dir = mydir)
dir(mydir)

\donttest{
## use a few customization options: different
## university/logo and language/title
## with a replacement sheet but for non-duplex printing
set.seed(403)
ex2 <- exams2nops(myexam, n = 2, dir = mydir,
  institution = "Universit\\\\\"at Innsbruck",
  name = "uibk", logo = "uibk-logo-bw.png",
  title = "Klausur", language = "de",
  replacement = TRUE, duplex = FALSE)
dir(mydir)
}

options(exams_tex = oopt$exams_tex)
}

\keyword{utilities}
