\name{exams2pdf}
\alias{exams2pdf}
\alias{make_exams_write_pdf}

\title{Generation of Exams in PDF Format}

\description{
  Automatic generation of exams in PDF format.
}

\usage{
  exams2pdf(file, n = 1L, nsamp = NULL, dir = ".", template = NULL, 
    inputs = NULL, header = list(Date = Sys.Date()), name = NULL, 
    control = NULL, encoding = "", quiet = TRUE, transform = NULL,
    edir = NULL, tdir = NULL, sdir = NULL, texdir = NULL,
    verbose = FALSE, points = NULL, \dots)
  
  make_exams_write_pdf(template = "plain", inputs = NULL,
    header = list(Date = Sys.Date()), name = NULL, encoding = "",
    quiet = TRUE, control = NULL, texdir = NULL)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files.}
  \item{n}{integer. The number of copies to be compiled from \code{file}.}
  \item{nsamp}{integer. The number(s) of exercise files sampled from each
    list element of \code{file}. Sampling without replacement is used
    if possible. (Only if some element of \code{nsamp} is larger than
    the length of the corresponding element in \code{file}, sampling
    with replacement is used.)}
  \item{dir}{character specifying the output directory (default:
    current working directory). If only a single PDF file is produced
    and no \code{dir} is explicitly specified, the file is displayed on the
    screen rather than saved in \code{dir}.}
  \item{template}{character. A specification of a LaTeX template. The package
    currently provides \code{"exam"}, \code{"solution"}, \code{"plain"},
    among others. The default is to use the \code{"plain.tex"} file unless
    there are Rmd exercises in \code{file} for which \code{"plain8.tex"}
    is used. For further details see below.}
  \item{inputs}{character. Names of files that are needed as inputs during
    LaTeX compilation (e.g., style files, headers). Either the full path
    must be given or the file needs to be in \code{edir}.}
  \item{header}{list. A list of further options to be passed to
    the LaTeX files.}
  \item{name}{character. A name prefix for resulting exercises, by default
    chosen based on \code{template}.}
  \item{control}{A list of control arguments for the appearance of
    multiple choice results (see details).}
  \item{encoding}{character, passed to \code{\link[exams]{xweave}}.}
  \item{quiet}{logical. Should output be suppressed when calling
    \code{\link[exams]{xweave}} and \code{\link[tools]{texi2dvi}}.}
  \item{transform}{function. An optional transform driver passed to
    \code{xexams} (by default no transformation is used).}
  \item{edir}{character specifying the path of the directory in which
    the files in \code{file} are stored (see also below).}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempdir}}. Note that this
    is cleaned up and potentially temporary files are deleted.}
  \item{sdir}{character specifying a directory for storing supplements, by
    default this is chosen via \code{\link[base]{tempdir}}.}
  \item{texdir}{character specifying a directory for running \code{\link[tools]{texi2dvi}}
    in. By default this is chosen via \code{\link[base]{tempdir}} (and deleted
    again) but, if specified by the user, the temporary LaTeX files are preserved and not
    deleted. This is intended especially for debugging purposes.}
  \item{verbose}{logical. Should information on progress of exam generation
    be reported?}
  \item{points}{integer. How many points should be assigned to each exercise? Note that this
    argument overules any exercise points that are provided within the \code{expoints} tags
    of the exercise files (if any). The vector of points supplied should either have
    length 1 or the number of exercises in the exam.}
  \item{\dots}{further arguments passed on to \code{\link[exams]{xweave}}.}
}

\details{
  \code{exams2pdf} is a more flexible re-implementation of the old (version 1)
  \code{\link[exams]{exams}} function (Gruen and Zeileis 2009), using the new extensible
  \code{\link[exams]{xexams}} framework (Zeileis et al. 2014). A detailed
  introduction is provided in \code{vignette("exams", package = "exams")}, also
  pointing out relative advantages of the new interface.
  
  \code{exams2pdf} proceeds by using \code{make_exams_write_pdf} to set up a custom
  \code{driver$write} function on the fly before calling \code{xexams}.
  This custom driver combines each exams with the desired \code{template}
  (and \code{inputs} etc.) and then calls \code{\link[tools]{texi2dvi}}
  on the resulting LaTeX file to produce PDF output.  
  For a single exam (\code{n = 1}) the resulting PDF is displayed on
  screen (unless \code{dir} is explicitly specified) while for \code{n > 1} 
  the PDF files are stored in the output directory \code{dir}.

  The argument \code{control} is specified by a named list, currently with elements
  \code{mchoice.symbol} and \code{cloze.collapse}. \code{mchoice.symbol}
  has to be a character vector with elements \code{True} and \code{False},
  specifying the symbol used for the questionnaire output in the final PDF file.
  \code{cloze.collapse} specifies the character used for collapsing mchoice/schoice
  alternatives within a cloze exercise. By default, these are separated by \code{" / "}
  but with \code{cloze.collapse = "\\\\\\\\"} each alternative would be in a new line.
  Finally, \code{cloze.collapse = "enumerate"} can also be used which employs a nested
  enumerate environment. In the latter case, the questionnaire uses \code{exclozechoice}
  rather than \code{exmchoice} (see \code{exam.tex} or \code{solution.tex} for an
  illustration.
}

\references{
Gruen B, Zeileis A (2009). Automatic Generation of Exams in R.
  \emph{Journal of Statistical Software}, \bold{29}(10), 1--14.
  \url{http://www.jstatsoft.org/v29/i10/}.

Zeileis A, Umlauf N, Leisch F (2014).
  Flexible Generation of E-Learning Exams in R: Moodle Quizzes, OLAT Assessments, and Beyond.
  \emph{Journal of Statistical Software}, \bold{58}(1), 1--36.
  \url{http://www.jstatsoft.org/v58/i01/}.
}

\value{
  \code{exams2pdf} returns a list of exams as generated by \code{\link[exams]{xexams}}.
  
  \code{make_exams_write_pdf} returns a function that is suitable for being
  supplied as \code{driver$write} to \code{\link[exams]{xexams}}.
}

\seealso{
  \code{\link[exams]{xexams}},
  \code{\link[exams]{exams}},
  \code{\link[tools]{texi2dvi}}
}

\examples{
## load package and enforce par(ask = FALSE)
options(device.ask.default = FALSE)

if(interactive()) {
## compile a single random exam (displayed on screen)
exams2pdf(list(
  "boxplots",
  c("tstat", "ttest", "confint"),
  c("regression", "anova"),
  "scatterplot",
  "relfreq"
))
}
}

\keyword{utilities}
