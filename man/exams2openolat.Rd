\name{exams2openolat}
\alias{exams2openolat}
\alias{openolat_config}

\title{Generation of Exams for OpenOlat}

\description{
  Automatic generation of exams in QTI 2.1 (or 1.2) with some
  tweaks optimized for OpenOlat.
}

\usage{
  exams2openolat(file, n = 1L, dir = ".", name = "olattest",
    qti = "2.1", config = TRUE, converter = "pandoc-mathjax", table = TRUE,
    maxattempts = 1, cutvalue = NULL, \dots)

  openolat_config(cancel = FALSE, suspend = FALSE, scoreprogress = FALSE,
    questionprogress = FALSE, maxscoreitem = TRUE, menu = TRUE,
    titles = TRUE, notes = FALSE, hidelms = TRUE, hidefeedbacks = FALSE,
    blockaftersuccess = FALSE, attempts = 1, anonym = FALSE,
    manualcorrect = FALSE)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files.}
  \item{n}{integer. The number of copies to be compiled from \code{file}.}
  \item{dir}{character. The default is the current working directory.}
  \item{name}{character. A name prefix for resulting exercises and ZIP file.}
  \item{qti}{character indicating whether QTI \code{"1.2"} or \code{"2.1"}
    (default) should be generated.}
  \item{config}{logical or named list with arguments to be passed to
    function \code{openolat_config}, for adding an OpenOlat-specifc configuration
    file \code{QTI21PackageConfig.xml} to the exam. If set to \code{config = FALSE},
    no configuration file is added (which was the default behavior up to R/exams 2.3.6).
    Setting \code{config = TRUE} is equivalent to \code{config = list()} or
    \code{config = openolat_config()} (which is the current default, starting from R/exams 2.4.0).
    Custom configurations can be specified using the syntax \code{config = list(...)},
    see the details on the possible arguments below.}
  \item{converter}{character passed on to \code{\link[exams]{make_exercise_transform_html}},
    indicating the type of converter from LaTeX/Markdown to HTML. Defaults
    to HTML with MathJax (and OpenOlat-specific fixups).}
  \item{table}{logical or character. Should a dedicated table class be
    used in the HTML for OpenOlat? For details see below.}
  \item{maxattempts}{integer. The maximum attempts for one question within an exam.
    Set to \code{Inf} or \code{0} for unlimited attempts. For a finite number of attempts this
    must be smaller than \code{100000}. See also \code{attempts} below for allowing more
    than one attempt for the exam as a whole.}
  \item{cutvalue}{numeric. The number of points at which the exam is passed. If set to
    \code{NULL} (default) or equivalently \code{NA}, no cutvalue is set and the result
    of the exam is just the number of points.}
  \item{\dots}{arguments passed on to \code{\link[exams]{exams2qti21}} (or
    \code{\link[exams]{exams2qti12}}, respectively). See the corresponding
    manual pages for further important arguments such as
    \code{solutionswitch}, \code{casesensitive}, \code{cloze_schoice_display}
    \code{navigation}, \code{shufflesections}, \code{eval}, \code{selection},
    among many others.}
  \item{cancel}{logical. Are participants allowed to cancel an exam after starting it?
    (Default: \code{FALSE}.)}
  \item{suspend}{logical. Are participants allowed to suspend an exam after starting it
    (in order to continue it later)? (Default: \code{FALSE}.)}
  \item{scoreprogress}{logical. Should the progress of the score/points achieved so far
    be displayed during the exam? (Default: \code{FALSE}.)}
  \item{questionprogress}{logical. Should the number of questions solved so far be
    displayed during the exam? (Default: \code{FALSE}.)}
  \item{maxscoreitem}{logical. Should the maximum score of a question/item be displayed?
    (Default: \code{TRUE}.)}
  \item{menu}{logical. Should the menu be displayed during the exam? (Default: \code{TRUE}.)}
  \item{titles}{logical. Should question titles be displayed during the exam?
    (Default: \code{TRUE}.)}
  \item{notes}{logical. Should participants be enabled to take notes in OpenOlat during
    the exam? (Default: \code{FALSE}.)}
  \item{hidelms}{logical. Should access to the OpenOlat learning management system be
    hidden during the exam? (Default: \code{TRUE}.)}
  \item{hidefeedbacks}{logical. Should feedbacks be hidden? (Default: \code{FALSE}.)}
  \item{blockaftersuccess}{logical. Should the exam be blocked after successful completion?
    (Default: \code{FALSE}.)}
  \item{attempts}{integer. How many attempts are allowed for the exam as a whole?
    Note that this should not be confused with the \code{maxattempts} for a question
    within the exam (see above).}
  \item{anonym}{logical. Should anonymous users be allowed to take the exam? (Default: \code{FALSE}.)}
  \item{manualcorrect}{logical. Should the points and pass/fail status from the exam
    be evaluated manually? (Default: \code{FALSE}.)}
}

\details{
  \code{exams2openolat} is a convenience interface to \code{\link[exams]{exams2qti12}}
  and \code{\link[exams]{exams2qti21}} for generating either QTI 1.2 or 2.1
  (default) output with some small tweaks for OpenOlat. Specifically, the MathJax
  and table output from pandoc is post-processed as expected by OpenOlat. See the
  corresponding manual page for many more arguments that are supported by
  \code{exams2openolat}.
  
  Dedicated \code{table} classes currently include: Full-width tables with borders
  (\code{"b_grid"}, \code{"b_border"}) or without (\code{"b_full"}, \code{"b_borderless"}).
  Or regular tables without background color (\code{"b_middle"}) or with white grid
  and color background (\code{"b_gray"}, \code{"b_red"}, \code{"b_green"}, \code{"b_blue"},
  \code{"b_yellow"}). Setting \code{table = TRUE} corresponds to \code{table = "b_gray"}.

  In addition to controlling the behavior of the exam and its questions via the standard
  QTI 2.1 options (see \code{\link[exams]{exams2qti21}}), it is possible to specify
  certain OpenOlat-specific configurations via an additional XML file to be added to the
  ZIP output. This is why these options are not part of \code{exams2qti21} but are provided in the
  separate wrapper function \code{openolat_config}. As the resulting XML configuration file
  is not part of the official OpenOlat interface, it may be subject to more changes in the future.
}

\value{
  \code{exams2openolat} returns a list of exams as generated by \code{\link[exams]{xexams}}.
}

\seealso{
  \code{\link[exams]{exams2qti12}},
  \code{\link[exams]{exams2qti21}}
}

\examples{
## load package and enforce par(ask = FALSE)
library("exams")
options(device.ask.default = FALSE)

## define an exams (= list of exercises)
myexam <- list(
  "boxplots.Rmd",
  c("tstat.Rmd", "ttest.Rmd", "confint.Rmd"),
  c("regression.Rmd", "anova.Rmd"),
  c("scatterplot.Rmd", "boxhist.Rmd"),
  "relfreq.Rmd"
)

## output directory
dir.create(mydir <- tempfile())

## generate .zip with OpenOlat test in temporary directory
exams2openolat(myexam, n = 3, dir = mydir)
dir(mydir)
}

\keyword{utilities}
