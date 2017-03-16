\name{exams2pandoc}
\alias{exams2pandoc}

\title{Generation of Exams via Pandoc}

\description{
  Automatic generation of exams via pandoc, by default in docx format.
}

\usage{
  exams2pandoc(file, n = 1L, nsamp = NULL, dir = ".",
    name = "pandoc", type = "docx", template = "plain.tex",
    question = "Question", solution = "Solution",
    header = list(Date = Sys.Date()), inputs = NULL, options = NULL,
    quiet = TRUE, resolution = 100, width = 4, height = 4, svg = FALSE, encoding = "",
    edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE, points = NULL, \dots)
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
    current working directory). If only a single HTML file is produced
    and no \code{dir} is explicitly specified, the file is displayed in the
    browser rather than saved in \code{dir}.}
  \item{name}{character. A name prefix for resulting exercises.}
  \item{type}{character. The file type to convert to using pandoc. The default is
    \code{"docx"} (but other choices are also supported, e.g., \code{"odt"},
    \code{"html"}, \code{"markdown"} etc.).}
  \item{template}{character. A specification of a template in either
    LaTeX, HTML, or Markdown format. The default is to use the \code{"plain.tex"}
    file provided but an alternative \code{"plain.html"} is also available.}
  \item{question}{character or logical. Should the question be included in the
    output? If \code{question} is a character it will be used as a header for
    resulting questions.}
  \item{solution}{character or logical, see argument \code{question}.}
  \item{header}{list. A list of named character strings (or functions generating such)
    to be substituted in the \code{template}.}
  \item{inputs}{character. Names of files that are needed as inputs for the
    \code{template} (e.g., images, headers). Either the full path
    must be given or the file needs to be in \code{edir}.}
  \item{options}{character. A string of options to be passed on to
    \code{\link[rmarkdown]{pandoc_convert}}.}
  \item{quiet}{logical. Should output be suppressed when calling
    \code{\link[exams]{xweave}}?}
  \item{resolution, width, height}{numeric. Options for rendering PNG (or SVG)
    graphics passed to \code{\link[exams]{xweave}}.}
  \item{svg}{logical. Should graphics be rendered in SVG or PNG (default)?}
  \item{encoding}{character, passed to \code{\link[exams]{xweave}}.}    
  \item{edir}{character specifying the path of the directory in which
    the files in \code{file} are stored (see also below).}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempdir}}. Note that this
    is cleaned up and potentially temporary files are deleted.}
  \item{sdir}{character specifying a directory for storing supplements, by
    default this is chosen via \code{\link[base]{tempdir}}.}
  \item{verbose}{logical. Should information on progress of exam generation
    be reported?}
  \item{points}{integer. How many points should be assigned to each exercise? Note that this
    argument overules any exercise points that are provided within the \code{expoints} tags
    of the exercise files (if any). The vector of points supplied should either have
    length 1 or the number of exercises in the exam.}
  \item{\dots}{currently not used.}
}

\details{
  \code{exams2pandoc} can generate exams in various output formats
  (by default docx) using \code{\link[exams]{xexams}} and
  \code{\link[rmarkdown]{pandoc_convert}}. It proceeds by (1) calling
  \code{\link[exams]{xweave}} on each exercise, (2) reading the resulting
  LaTeX or Markdown code, (3) transforming the code to the markup of
  some exam template (either LaTeX, HTML, or Markdown), (4) embedding the
  code in a template and converting it to the desired output format
  using pandoc.

  For steps (1) and (2) the standard drivers in \code{xexams} are used.
  
  For step (3) a suitable transformation function is set up on the fly
  using \code{\link[exams]{make_exercise_transform_pandoc}}. Depending
  on which format the template uses (LaTeX or HTML or Markdown) the
  transformation may or may not be trivial.
  
  For step (4) all exercises are inserted into the template (and also
  replacing certain additional tags from \code{header}) and then
  \code{\link[rmarkdown]{pandoc_convert}} is used to convert to the
  desired output format (one file for each exam). In principle, all output
  types of pandoc are supported, but most of them have not been tested.
  (The main motivation for \code{exams2pandoc} was the generation of
  \code{"docx"} or \code{"odt"} files.)
}

\value{
  \code{exams2pandoc} returns a list of exams as generated by \code{\link[exams]{xexams}}.
}

\seealso{
  \code{\link[exams]{xexams}},
  \code{\link[rmarkdown]{pandoc_convert}}
}

\examples{
## load package and enforce par(ask = FALSE)
options(device.ask.default = FALSE)

## define an exams (= list of exercises)
myexam <- list(
  "boxplots",
  c("tstat", "ttest", "confint"),
  c("regression", "anova"),
  c("scatterplot", "boxhist"),
  "relfreq"
)

## output directory
mydir <- tempdir()

\donttest{
## compile two docx and odt versions each
set.seed(1090)
exams2pandoc(myexam, n = 2, dir = mydir, type = "docx")
set.seed(1090)
exams2pandoc(myexam, n = 2, dir = mydir, type = "odt")
}
}

\keyword{utilities}
