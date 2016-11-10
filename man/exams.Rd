\name{exams}
\alias{exams}
\alias{print.exams_metainfo}

\title{Generation of Simple Exams}

\description{
  Old (version 1) interface for Sweave-based automatic generation
  of exams including multiple choice questions and arithmetic problems.
  Now it is recommended to use the (version 2) interface
  \code{\link[exams]{exams2pdf}}.
}

\usage{
  exams(file, n = 1, nsamp = NULL, dir = NULL, template = "plain",
    inputs = NULL, header = list(Date = Sys.Date()), name = NULL,
    quiet = TRUE, edir = NULL, tdir = NULL, control = NULL)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files,
    for details see below.}
  \item{n}{integer. The number of copies to be compiled from
    \code{file}.}
  \item{nsamp}{integer. The number(s) of exercise files sampled from each
    list element of \code{file}. Sampling without replacement is used
    if possible. (Only if some element of \code{nsamp} is larger than
    the length of the corresponding element in \code{file}, sampling
    with replacement is used.)}
  \item{dir}{character. The output directory, this has to be set if
    \code{n} is greater than 1 (or \code{template} is longer than 1).}
  \item{template}{character. A specification of a LaTeX template. The package
    currently provides \code{"exam"}, \code{"solution"}, \code{"plain"}.
    For details see below.}
  \item{inputs}{character. Names of files that are needed as inputs during
    LaTeX compilation (e.g., style files, headers). Either the full path
    must be given or the file needs to be in \code{edir}.}
  \item{header}{list. A list of further options to be passed to
    the LaTeX files.}
  \item{name}{character. A name prefix for resulting exercises, by default
    chosen based on \code{template}.}
  \item{quiet}{logical. Should output be suppressed when calling
    \code{\link[utils]{Sweave}} and \code{\link[tools]{texi2dvi}}.}
  \item{edir}{character specifying the path of the directory in which
    the files in \code{file} are stored (see also below).}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempdir}}. Note that this
    is cleaned up and potentially temporary files are deleted.}
  \item{control}{A list of control arguments for the appearance of
    multiple choice results (see \sQuote{Details}).}
}

\details{
  \code{exams} is the old (version 1) interface for Sweave-based
  generation of PDF exams. It is only provided for backward compatibility
  and is superseded by the far more flexible function
  \code{\link[exams]{exams2pdf}}.

  \code{exams} generates exams from lists of \code{\link[utils]{Sweave}}
  source files by:
  (1) running \code{Sweave} on each exercise,
  (2) including the resulting LaTeX files in a \code{template},
  (3) running \code{\link[tools]{texi2dvi}} on the template, and
  (4) storing the resulting PDF file in an output \code{dir}
  (or displaying it interactively).
  
  Each exercise in an exam is essentially a standalone Sweave source file
  that \code{exams} knows (almost) nothing about, it just calls \code{Sweave}
  (\code{n} times). The only exception is some meta-information which
  is passed by means of four commands back to \code{exams}. The commands
  are \samp{\\extype} (which may be \samp{mchoice} or \samp{num}),
  \samp{\\exsolution} (e.g., \code{3.124} for a numeric solution and
  \code{10010} for a multiple choice solution), \samp{\\exstring}
  (containing a human-readable string with the solution), and
  \samp{\\extol} (a tolerance for numeric solutions).
  
  The specification in \code{file} should be either of form \code{"foo"}
  or equivalently \code{"foo.Rnw"}, where the file \code{"foo.Rnw"} should
  either be in the local directory, the \code{edir} directory or in
  the \code{exercises} directory of the package. \code{file} can either
  be a simple vector or a list of vectors. In the latter case, exercises
  are chosen randomly within each list element. For example, the specification
  \code{file = list(c("a", "b"), "xyz")} will result in an exam with two
  exercises: the first exercise is chosen randomly between \code{"a"} and
  \code{"b"} while \code{"xyz"} is always included as the second exercise.

  The \code{template} is a (vector of) specification(s) of LaTeX templates.
  It can be \code{"foo"} or equivalently \code{"foo.tex"} where \code{"foo.tex"}
  should either be in the local directory (or provided with the full path)
  or in the \code{tex} directory
  of the package. It should specify where in the template the exercises
  are included, using the markup \samp{\\exinput\{exercises\}}. Additionally,
  it may contain \samp{\\exinput\{questionnaire\}} and \samp{\\exinput\{header\}}.
  \code{template} can also be a vector, then for each of the \code{n} runs
  several output files (one for each template) are created.
  
  The name prefix for each file is by default the base name of the 
  corresponding \code{template} but can also be changed via \code{name}.
  
  \code{exams} creates the PDF files and stores them in an output
  directory together with the solution meta information as \samp{metainfo.rda}
  (see also below). If only a single PDF is created (currently
  the default), \code{dir} may be \code{NULL} and it is only displayed
  on the screen.

  The argument \code{control} is specified by a named list with elements
  \code{mchoice.print} and \code{mchoice.symbol}. The element
  \code{mchoice.print} is used for specifying the characters used for
  printing. It is again a named list where element \code{True} gives the
  (five) characters used for printing when the answer is correct and
  \code{False} if the answer is wrong. The symbol used for the
  questionnaire output in the final PDF file is defined by
  \code{mchoice.symbol} which is vector with elements \code{True} and
  \code{False}.
}

\references{
Gruen B, Zeileis A (2009). Automatic Generation of Exams in R.
  \emph{Journal of Statistical Software}, \bold{29}(10), 1--14.
  \url{http://www.jstatsoft.org/v29/i10/}.
}

\value{
An object of class \code{"exams_metainfo"} is returned invisibly. It is a
list of length \code{n}, containing a list of meta informations for each
exercise:
  \item{mchoice}{logical. Is the exercise a multiple choice exercise?}
  \item{length}{integer. Length of solution.}
  \item{solution}{either a logical vector (for multiple choice) or numeric
    vector (for arithmetic problems).}
  \item{string}{character. A human-readable version of the solution.}
}

\seealso{\code{\link[exams]{exams2pdf}},
  \code{\link[utils]{Sweave}},
  \code{\link[tools]{texi2dvi}},
  \code{\link[exams]{mchoice2string}}}

\examples{
## load package and enforce par(ask = FALSE)
library("exams")
options(device.ask.default = FALSE)

## define an exams (= list of exercises)
myexam <- list(
  "boxplots",
  c("tstat", "ttest", "confint"),
  c("regression", "anova"),
  "scatterplot",
  "relfreq"
)

if(interactive()) {
## compile a single random exam (displayed on screen)
sol <- exams(myexam)
sol
}

## generate multiple exams (stored in output directory)
odir <- tempfile()
sol <- exams(myexam, n = 3, dir = odir, template = c("exam", "solution"))
sol

## inspect solution for a particular exam
print(sol, 3)

if(interactive()) {
## modify control argument for printing
mymchoice.control <- list(mchoice.print = list(True = LETTERS[1:5], False = "_"))
sol <- exams("boxplots", template = "solution",
  control = mymchoice.control)
sol
}
}

\keyword{utilities}
