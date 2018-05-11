\name{exams2html}
\alias{exams2html}
\alias{make_exams_write_html}
\alias{make_exercise_transform_html}
\alias{make_exercise_transform_pandoc}

\title{Generation of Exams in HTML Format}

\description{
  Automatic generation of exams in HTML format.
}

\usage{
  exams2html(file, n = 1L, nsamp = NULL, dir = ".", template = NULL,
    name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE,
    question = "<h4>Question</h4>", solution = "<h4>Solution</h4>",
    mathjax = FALSE, resolution = 100, width = 4, height = 4, svg = FALSE,
    encoding = "", envir = NULL, converter = NULL, \dots)

  make_exercise_transform_html(converter = c("ttm", "tth", "pandoc", "tex2image"),
    base64 = TRUE, \dots)
    
  make_exams_write_html(template = "plain", name = NULL,
    question = "<h4>Question</h4>", solution = "<h4>Solution</h4>",
    mathjax = FALSE)
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
  \item{template}{character. A specification of a HTML template. The default
    is to use the \code{"plain.html"} file provided in the package unless
    there are Rmd exercises in \code{file}. Then, \code{"plain8.html"}
    is used (which expects UTF-8 encoding as used by pandoc).}
  \item{name}{character. A name prefix for resulting exercises.}
  \item{quiet}{logical. Should output be suppressed when calling
    \code{\link[exams]{xweave}}?}
  \item{edir}{character specifying the path of the directory in which
    the files in \code{file} are stored (see also below).}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempdir}}. Note that this
    is cleaned up and potentially temporary files are deleted.}
  \item{sdir}{character specifying a directory for storing supplements, by
    default this is chosen via \code{\link[base]{tempdir}}.}
  \item{verbose}{logical. Should information on progress of exam generation
    be reported?}
  \item{question}{character or logical. Should the question be included in the HTML
    output? If \code{question} is a character it will be used as a header for resulting questions.
    Argument \code{question} may also be a vector that controls the output for the templates.}
  \item{solution}{character or logical, see argument \code{question}.}
  \item{mathjax}{logical. Should the JavaScript from \url{http://www.MathJax.org/}
    be included for rendering mathematical formulas?}
  \item{resolution, width, height}{numeric. Options for rendering PNG (or SVG)
    graphics passed to \code{\link[exams]{xweave}}.}
  \item{svg}{logical. Should graphics be rendered in SVG or PNG (default)?}
  \item{encoding}{character, passed to \code{\link[exams]{xweave}}.}
  \item{envir}{argument passed to \code{\link[exams]{xweave}} (which passes
    it to \code{\link[knitr]{knit}}).}
  \item{base64}{logical. Should supplementary files be embedded using Base 64 coding?
    Argument \code{base64} may also be a character vector of file suffixes that should be encoded, e.g.
    \code{base64 = c("png", "rda")} will only encode PNG images and binary \code{.rda} files.
    If set to \code{NULL} only image files will be encoded.}
  \item{converter, \dots}{arguments passed on to \code{make_exercise_transform_html}.
    The default for \code{converter} is set to \code{"ttm"} unless there are Rmd
    exercises in \code{file} where \code{"pandoc"} is used.}
}

\details{
  \code{exams2html} generates exams in a very simple HTML format
  using \code{\link[exams]{xexams}}. It proceeds by (1) calling \code{\link[exams]{xweave}}
  on each exercise, (2) reading the resulting LaTeX code, (3) transforming
  the LaTeX code to HTML, and (4) embedding the HTML code in a template
  (a simple and plain template is used by default).
  
  For steps (1) and (2) the standard drivers in \code{xexams} are used.
  
  For step (3) a suitable transformation function is set up on the fly
  using \code{make_exercise_transform_html}. This transforms the
  LaTeX code in \code{question}/\code{questionlist} and
  \code{solution}/\code{solutionlist} by leveraging one of four
  functions: \code{\link[tth]{ttm}} produces HTML with MathML
  for mathematical formulas, \code{\link[tth]{tth}} produces
  plain HTML that aims to emulate mathematical formulas,
  \code{\link[rmarkdown]{pandoc_convert}} employs pandoc offering different
  options for handling formulas, and \code{\link[exams]{tex2image}} runs
  LaTeX and turns the result into a single image. In all cases, images
  can either be stored in supplementary files or embedded directly in Base 64 coding.
  
  For step (4) a simple writer function is set up on the fly that
  embeds the transformed HTML code into a template and
  writes a single HTML file for each exam.
}

\references{
Zeileis A, Umlauf N, Leisch F (2014).
  Flexible Generation of E-Learning Exams in R: Moodle Quizzes, OLAT Assessments, and Beyond.
  \emph{Journal of Statistical Software}, \bold{58}(1), 1--36.
  \url{http://www.jstatsoft.org/v58/i01/}.
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
  \code{\link[rmarkdown]{pandoc_convert}},
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

## examples with different locales (UTF-8, ISO-8859-15)
## using special characters (Euro and Pound symbol, German umlaut)
if(!identical(Sys.getlocale(), "C")) {
## UTF-8
exams2html("currency8", encoding = "utf8", template = "plain8")

## ISO Latin 9 (aka ISO-8859-15)
exams2html("currency9", encoding = "latin9", template = "plain9")
}


## various versions of displaying mathematical formulae

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
exams2html("tstat", converter = "tex2image")
}
}
}

\keyword{utilities}
