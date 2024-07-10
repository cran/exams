\name{exams2grasple}
\alias{exams2grasple}
\alias{make_exams_write_grasple}

\title{Generation of Exercises in Grasple Format}

\description{
  Automatic generation of exercises in JSON format for the e-learning platform Grasple.
}


\usage{
exams2grasple(file, n = 1L, dir = ".", name = NULL, quiet = TRUE,
  resolution = 100, width = 4, height = 4, svg = FALSE, encoding = "UTF-8",
  envir = NULL, engine = NULL,
  converter = "pandoc-mathjax", zip = TRUE, use_solutionlist = TRUE,
  license_name = NULL, license_description = NULL, license_value = NULL,
  license_link = NULL, \dots)

make_exams_write_grasple(name = NULL, license_name = NULL,
  license_description = NULL, license_value = NULL, license_link = NULL,
  zip = TRUE, use_solutionlist = TRUE)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files.}
  \item{n}{integer. The number of copies to be compiled from \code{file}.}
  \item{dir}{character. The default is the current working directory.}
  \item{name}{character. A name prefix for resulting ZIP file.}
  \item{quiet}{logical. Should output be suppressed when calling
    \code{\link[exams]{xweave}} and \code{\link[tools]{texi2dvi}}.}
  \item{resolution, width, height}{numeric, passed to \code{\link[exams]{xweave}}.}
  \item{svg}{logical. Should graphics be rendered in SVG or PNG (default)?}
  \item{encoding}{character, ignored. The encoding is always assumed to be UTF-8.}
  \item{envir}{argument passed to \code{\link[exams]{xweave}} (which passes
    it to \code{\link[knitr]{knit}}).}
  \item{engine}{argument passed to \code{\link[exams]{xweave}} indicating whether
    \code{"Sweave"} (default) or \code{"knitr"} should be used for rendering Rnw exercises.}
  \item{converter}{character passed on to \code{\link[exams]{make_exercise_transform_html}},
    indicating the type of converter from LaTeX/Markdown to the specific requirements in
    Grasple. To accomplish this the converter must be set to \code{"pandoc-mathjax"}.}
  \item{zip}{logical. Should the resulting JSON file(s) be zipped?}
  \item{use_solutionlist}{logical. By default it is assumed that for schoice items
    the solutionlist contains separate feedback for each option. If set to
    \code{FALSE} the general feedback is placed in the solutionlist for each option
    separately.}
  \item{license_name, license_description, license_value, license_link}{character,
    arguments for specifying the copyright license for the exercise(s).
    Defaults to "Non-explicit license".}
  \item{\dots}{arguments passed on to \code{\link[exams]{xexams}}.}
}

\details{
  \code{exams2grasple} generates exercises in the JSON format for Grasple
  using \code{\link[exams]{xexams}}. It proceeds by (1) calling \code{\link[exams]{xweave}}
  on each exercise, (2) reading the resulting Markdown or LaTeX text, (3) transforming
  the text to Markdown, and (4) embedding the Markdown text into the
  JSON format for Grasple.

  For steps (1) and (2) the standard drivers in \code{xexams} are used.
  For step (3) a suitable transformation function is set up on the fly
  using \code{\link[exams]{make_exercise_transform_html}}.
  For step (4) a simple writer function is set up on the fly that
  embeds the transformed Markdown code into a hard-coded JSON template
  using \code{\link[RJSONIO]{toJSON}} and writes a JSON
  file for each exercise and by default bundles the collection in a ZIP file.

  Note that Grasple only officially supports \code{schoice} and \code{num}
  items, and hence other item types are not supported in the current version of
  \code{exams2grasple} either. If the function is used for other types the execution
  of the function is stopped and a warning is issued.

  For \code{num} exercises the content as specified under the solution environment
  in the Rmd/Rnw files is presented as feedback in Grasple. By contrast, for \code{schoice}
  items Grasple requires separate feedback for each option. This can be accomplished in two ways.
  First (the default), by putting unique feedback for each option in the items of the
  solutionlist in the Rmd/Rnw file. Second by putting a general feedback in the solution
  environment and setting \code{use_solutionlist} to \code{FALSE}, which creates a
  solutionlist with this general feedback for each option.

  If the meta-information of an exercise contains an \code{exsection} element, its content will be
  transferred to the name element of the JSON-file. This name element is searchable in Grasple. In
  the ShareStats project the \code{exsection} element is used for classifying items in terms of its
  taxonomy of topics in statistics education.  

  Within the HTML of Grasple exercises, LaTeX elements must be embedded within \\\\(\\\\) blocks.
  After conversion using \code{"pandoc-mathjax"} the function employs further tweaks to meet
  Grasple's requirements. Also, in case of displaying multiline equations
  Grasple only allows for using the \code{align} environment. Environments \code{eqnarray}
  and \code{eqnarray*} are automatically converted into \code{align}. It is uncertain how
  pandoc deals with other multiline environments.
}

\references{Grasple - Open Education (2022). \emph{Format for Open Interactive Math Exercises}. \url{https://github.com/grasple/open-format}}

\value{
  A list of exercises as generated by \code{\link[exams]{xexams}} is
  returned invisibly.
}

\examples{
## load package and enforce par(ask = FALSE)
library("exams")
options(device.ask.default = FALSE)

## define a list of exercises
myexam <- list(
  "fruit.Rmd",
  "tstat.Rmd",
  "regression.Rmd",
  "swisscapital.Rmd",
  "tstat2.Rmd",
  "dist3.Rmd"
)

## output directory
dir.create(mydir <- tempfile())

\dontrun{
## generate .zip with Grasple exercises and use general feedback
exams2grasple(myexam, dir = mydir, use_solutionlist = FALSE)
dir(mydir)
}
}

\keyword{utilities}
