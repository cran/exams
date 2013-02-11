\name{exams2html}
\alias{exams2html}
\alias{make_exams_write_html}
\alias{make_exercise_transform_html}

\title{Generation of Exams in HTML Format}

\description{
  Automatic generation of exams in HTML format.
}

\usage{
  exams2html(file, n = 1L, nsamp = NULL, dir = ".", template = "plain",
  name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL,
  question = "<h4>Question</h4>", solution = "<h4>Solution</h4>",
  mathjax = FALSE, resolution = 100, width = 4, height = 4, \dots)

  make_exercise_transform_html(converter = c("ttm", "tth", "tex2image"),
    base64 = TRUE, \dots)
    
  make_exams_write_html(template = "plain", name = NULL,
    question = "<h4>Question</h4>", solution = "<h4>Solution</h4>",
    mathjax = FALSE)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files.}
  \item{n}{integer. The number of copies to be compiled from \code{file}.}
  \item{nsamp}{integer. The number of exercise files sampled from each
    list element of \code{file}. Sampling without replacement is used
    if possible. (Only if some element of \code{nsamp} is larger than
    the length of the corresponding element in \code{file}, sampling
    with replacement is used.)}
  \item{dir}{character. The output directory, this has to be set if
    \code{n} is greater than 1. Current working directory is used by default.}
  \item{template}{character. A specification of a HTML template. The package
    currently provides \code{"plain.html"}.}
  \item{name}{character. A name prefix for resulting exercises.}
  \item{quiet}{logical. Should output be suppressed when calling
    \code{\link[utils]{Sweave}}?}
  \item{edir}{character specifying the path of the directory in which
    the files in \code{file} are stored (see also below).}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempdir}}.}
  \item{sdir}{character specifying a directory for storing supplements, by
    default this is chosen via \code{\link[base]{tempdir}}.}
  \item{question}{character or logical. Should the question be included in the HTML
    output? If \code{question} is a character it will be used as a header for resulting questions.
    Argument \code{question} may also be a vector that controls the output for the templates.}
  \item{solution}{character or logical, see to argument \code{question}.}
  \item{mathjax}{logical. Should the JavaScript from \url{http://www.MathJax.org/}
    be included for rendering mathematical formulas?}
  \item{resolution, width, height}{numeric. Options for rendering PNG
    graphics passed to \code{\link[utils]{Sweave}}.}
  \item{converter}{character. Workhorse function for transforming LaTeX
    code to HTML. For details see below.}
  \item{base64}{logical. Should images be embedded using Base 64 coding? Argument \code{base64} may
    also be a character vector of file endings that should be Base 64 encoded, e.g.
    \code{base64 = c("png", "rda")} will only encode PNG images and binary \code{.rda} files.}
  \item{\dots}{arguments passed on to \code{make_exercise_transform_html}.}
}

\details{
  \code{exams2html} generates exams in a very simple HTML format
  using \code{\link[exams]{xexams}}. It proceeds by (1) calling \code{\link[utils]{Sweave}}
  on each exercise, (2) reading the resulting LaTeX code, (3) transforming
  the LaTeX code to HTML, and (4) embedding the HTML code in a very
  simple exercise template (which is currently hard-coded).
  
  For steps (1) and (2) the standard drivers in \code{xexams} are used.
  
  For step (3) a suitable transformation function is set up on the fly
  using \code{make_exercise_transform_html}. This transforms the
  LaTeX code in \code{question}/\code{questionlist} and
  \code{solution}/\code{solutionlist} by leveraging one of three
  functions: \code{\link[tth]{ttm}} produces HTML with MathML
  for mathematical formulas, \code{\link[tth]{tth}} produces
  plain HTML that aims to emulate mathematical formulas, and
  \code{\link[exams]{tex2image}} runs LaTeX and turns the result into
  a single image. In all cases, images can either be stored in
  supplementary files or embedded directly in Base 64 coding.
  
  For step (4) a simple writer function is set up on the fly that
  embeds the transformed HTML code into a hard-coded template and
  writes a single HTML file for each exam.

  When generating only a single exam (as in the examples below),
  \code{exams2html} tries to display this directly in a browser
  using \code{\link[utils]{browseURL}}. This may not work properly
  in RStudio versions older than 0.97.133. To avoid these problems,
  one can either upgrade to a more recent version of RStudio or set
  R's \code{browser} option manually to a suitable value, e.g.,
  to \code{options(browser = NULL)} on Windows,
  \code{options(browser = "/usr/bin/firefox")} on Linux, or
  \code{options(browser = "/Applications/Firefox")} on OS X.
}

\value{
  \code{exams2html} returns a list of exams as generated by \code{\link[exams]{xexams}}.
  
  \code{make_exercise_transform_html} returns a function that is suitable for being
  supplied as \code{driver$transform} to \code{\link[exams]{xexams}}.

  \code{make_exams_write_html} returns a function that is suitable for being
  supplied as \code{driver$write} to \code{\link[exams]{xexams}}.
}

\seealso{
  \code{\link[exams]{xexams}},
  \code{\link[tth]{ttm}},
  \code{\link[tth]{tth}},
  \code{\link[exams]{tex2image}},
  \code{\link[utils]{browseURL}}
}

\examples{
## load package and enforce par(ask = FALSE)
options(device.ask.default = FALSE)

if(interactive()) {
## compile a single random exam (displayed in the browser)
exams2html(list(
  "boxplots",
  c("tstat", "ttest", "confint"),
  c("regression", "anova"),
  "scatterplot",
  "relfreq"
))

## show various versions of displaying mathematical formulae

## via MathML (displayed correctly in MathML-aware browsers, e.g. Firefox)
exams2html("tstat")

## via MathML + MathJax (should work in all major browsers,
## note the display options you get when right-clicking on the formulas
## in the browser)
exams2html("tstat", mathjax = TRUE)

## via plain HTML (works in all browsers but with inferior formatting)
exams2html("tstat", converter = "tth")

## via HTML with embedded picture (works in all browsers but
## is slow and requires LaTeX and ImageMagick)
\dontrun{
exams2html("tstat", converter = "tth")
}
}
}

\keyword{utilities}