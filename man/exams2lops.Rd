\name{exams2lops}
\alias{exams2lops}
\alias{make_exams_write_lops}

\title{Generation of Exams in LOPS Exam Server Format (WU Wien)}

\description{
  Automatic generation of exams in LOPS exam server format (WU Wien).
}

\usage{
  exams2lops(file, n = 1L, nsamp = NULL, dir = ".", name = NULL, 
    quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE,
    solution = TRUE, doctype = NULL, head = NULL, resolution = 100, width = 4, 
    height = 4, svg = FALSE,  encoding = "UTF-8", envir = NULL, engine = NULL,
    converter = "tex2image", base64 = FALSE,
    auto_scramble = TRUE, \dots)

  make_exams_write_lops(name = NULL, auto_scramble = TRUE, \dots)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files.}
  \item{n}{integer. The number of copies to be compiled from \code{file}.}
  \item{nsamp}{integer. The number(s) of exercise files sampled from each
    list element of \code{file}. Sampling without replacement is used
    if possible. (Only if some element of \code{nsamp} is larger than
    the length of the corresponding element in \code{file}, sampling
    with replacement is used.)}
  \item{dir}{character. The default is the current working directory.}
  \item{name}{character. A name prefix for resulting exercises.}
  \item{quiet}{logical. Should output be suppressed when calling
    \code{\link[exams]{xweave}}?}
  \item{edir}{character specifying the path of the directory (along with its
    sub-directories) in which the files in \code{file} are stored
    (see also \code{\link{xexams}}).}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempfile}}. Note that this
    is cleaned up (i.e., existing files are deleted) and only certain
    temporary files are preserved.}
  \item{sdir}{character specifying a directory for storing supplements, by
    default this is chosen via \code{\link[base]{tempfile}}.}
  \item{verbose}{logical. Should information on progress of exam generation
    be reported?}
  \item{solution}{logical. Should the solution be included in the HTML
    output?}
  \item{doctype}{character vector with a DOCTYPE tag for the HTML page.
    By default HTML4 is employed.}
  \item{head}{character vector for the head tag. By default a simple
    header is employed, setting the font to Arial.}
  \item{resolution, width, height}{numeric. Options for rendering PNG (or SVG)
    graphics passed to \code{\link[exams]{xweave}}.}
  \item{svg}{logical. Should graphics be rendered in SVG or PNG (default)?}
  \item{encoding}{character, ignored. The encoding is always assumed to be UTF-8.}
  \item{envir}{argument passed to \code{\link[exams]{xweave}} (which passes
    it to \code{\link[knitr]{knit}}).}
  \item{engine}{argument passed to \code{\link[exams]{xweave}} indicating whether
    \code{"Sweave"} (default) or \code{"knitr"} should be used for rendering Rnw exercises.}
  \item{converter}{character. Workhorse function for transforming LaTeX
    code to HTML.}
  \item{base64}{logical. Should supplementary files be embedded using Base 64 coding?
    Argument \code{base64} may also be a character vector of file suffixes that should be encoded, e.g.
    \code{base64 = c("png", "rda")} will only encode PNG images and binary \code{.rda} files.
    If set to \code{NULL} only image files will be encoded.}
  \item{auto_scramble}{logical. Should answers be scrambled automaticall?}  
  \item{\dots}{arguments passed on to \code{make_exercise_transform_html}.}
}

\details{
  \code{exams2lops} will produce a \code{.zip} file that may be uploaded. It proceeds by (1)
  calling \code{\link[exams]{xweave}} on each exercise, (2) reading the resulting LaTeX code, (3)
  transforming the LaTeX code to HTML, and (4) embedding the HTML code in a XML file using the
  LOPS exam server XML format (WU Wien).

  For steps (1) and (2) the standard drivers in \code{xexams} are used. In step (3), a suitable
  transformation function is set up on the fly using \code{make_exercise_transform_html}, see also
  the details section in \code{\link{exams2html}}.

  For step (4) a simple writer function is set up on the fly that embeds the transformed HTML code
  into the final XML files for each question and the exam.

  Note that in \code{make_exams_write_lops} only multiple and single choice questions are supported at
  the moment, since the LOPS exam server XML format (WU Wien) is used to generate printed versions for large scale
  multiple choice exams. In addition, only images of the question/questionlist/solution/solutionlist
  should be generated, since the server has only minimum support for e.g. MathML markup used to
  produce mathematical formulas.
}

\references{
Zeileis A, Umlauf N, Leisch F (2014).
  Flexible Generation of E-Learning Exams in R: Moodle Quizzes, OLAT Assessments, and Beyond.
  \emph{Journal of Statistical Software}, \bold{58}(1), 1--36.
  \doi{10.18637/jss.v058.i01}.
}

\value{
  \code{exams2lops} returns a list of exams as generated by \code{\link[exams]{xexams}}.
  
  \code{make_exams_write_lops} eturns a function that generates the XML code
  for the question in LOPS exam server format (WU Wien).
}

\seealso{
  \code{\link[exams]{xexams}},
  \code{\link[tth]{ttm}},
  \code{\link[tth]{tth}},
  \code{\link[exams]{tex2image}},
  \code{\link[exams]{make_exercise_transform_html}},
}

\examples{
\dontrun{
## output directory
dir.create(mydir <- tempfile())

## generate the exam
exams2lops(c("scatterplot.Rmd", "boxplots.Rmd"), dir = mydir)
dir(mydir)
}
}

\keyword{utilities}
