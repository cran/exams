\name{exams2ilias}
\alias{exams2ilias}

\title{Generation of Exams in ILIAS Format}

\description{
  Automatic generation of exams in QTI 1.2 with some tweaks
  (still under development) for the learning management system ILIAS.
}

\usage{
  exams2ilias(file, n = 1L, nsamp = NULL, dir = ".",
    name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL,
    verbose = FALSE, resolution = 100, width = 4, height = 4, svg = FALSE,
    encoding  = "UTF-8", num = list(fix_num = FALSE, minvalue = NA),
    mchoice = list(maxchars = c(3, NA, 3), minvalue = NA),
    schoice = mchoice, string = NULL, cloze = NULL,
    template = "ilias",
    duration = NULL, stitle = "Exercise", ititle = "Question",
    adescription = "Please solve the following exercises.",
    sdescription = "Please answer the following question.", 
    maxattempts = 1, cutvalue = 0, solutionswitch = TRUE, zip = TRUE,
    points = NULL, eval = list(partial = TRUE, negative = FALSE),
    converter = "pandoc-mathjax", xmlcollapse = TRUE,
    metasolution = FALSE, \dots)
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
  \item{name}{character. A name prefix for resulting exercises and ZIP file.}
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
  \item{resolution, width, height}{numeric. Options for rendering PNG (or SVG)
    graphics passed to \code{\link[exams]{xweave}}.}
  \item{svg}{logical. Should graphics be rendered in SVG or PNG (default)?}
  \item{encoding}{character, ignored. The encoding is always assumed to be UTF-8.}
  \item{num}{function or named list applied to numerical (i.e., type \code{num})
    questions. If \code{num} is a function, \code{num} will be used
    for generating the item body of the question, see function \code{make_itembody_qti12()}
    (or \code{make_itembody_qti21()}).
    If \code{num} is a named list, these arguments will be passed to function
    \code{make_itembody_qti12()} (or \code{make_itembody_qti21()} using \code{exams2qti21()}).}
  \item{mchoice, schoice, string, cloze}{function or named list applied to multiple choice,
    single choice, string, and cloze questions (i.e., type \code{mchoice}, \code{schoice},
    \code{string}, and \code{cloze}), respectively. See argument \code{num} for more details.}
  \item{template}{character. The IMS QTI 1.2 or 2.1 template that should be used. Currently,
    the package provides \code{"qti12.xml"} and \code{"qti21.xml"}.}
  \item{duration}{integer. Set the duration of the exam in minutes.}
  \item{stitle}{character. A title that should be used for the sections. May be a vector of
    length 1 to use the same title for each section, or a vector containing different section
    titles.}
  \item{ititle}{character. A title that should be used for the assessment items. May be a vector
    of length 1 to use the same title for each item, or a vector containing different item
    titles. Note that the maximum of different item titles is the number of sections/questions
    that are used for the exam.}
  \item{adescription}{character. Description (of length 1) for the overall assessment (i.e., exam).}
  \item{sdescription}{character. Vector of descriptions for each section, omitted if empty
    (or \code{NULL} or \code{FALSE}).}
  \item{maxattempts}{integer. The maximum attempts for one question. This may also be a vector
    so that the maximum number of attempts varies across questions. A value of \code{Inf} or
    \code{0} signals that the attempts per question are not limited.}
  \item{cutvalue}{numeric. The cutvalue at which the exam is passed.}
  \item{solutionswitch}{logical. Should the question/item solutionswitch be enabled?
    In OLAT this means that the correct solution is shown after an incorrect solution
    was entered by an examinee (i.e., this is typically only useful if \code{maxattempts = 1}).}
  \item{zip}{logical. Should the resulting XML file (plus supplements) be zipped?}
  \item{points}{integer. How many points should be assigned to each exercise? Note that this
    argument overules any exercise points that are provided within an \code{"\expoints{}"} tag
    in the \code{.Rnw} file. The vector of points supplied is expanded to the number of exercises in
    the exam.}
  \item{eval}{named list, specifies the settings for the evaluation policy, see function
    \code{\link{exams_eval}}.}
  \item{converter}{character. Argument passed on to \code{make_exercise_transform_html}.
    The default for \code{converter} is set to \code{"ttm"} unless there are Rmd
    exercises in \code{file} where \code{"pandoc"} is used.}
  \item{xmlcollapse}{logical or character. Should line breaks be collapsed in the XML
    code. If \code{TRUE} everything is collapsed with spaces (\code{" "}) but other collapse
    characters could be supplied.}
  \item{metasolution}{logical. Should the solution be added to the XML
    output as qtimetadata tag? This currently only works for ILIAS essay
    questions.}
  \item{\dots}{further arguments passed on to \code{make_exercise_transform_html}.}
}

\details{
  \code{exams2ilias} is a convenience interface to \code{\link[exams]{exams2qti12}}
  for generating QTI 1.2 with some tweaks for ILIAS (\url{https://www.ilias.de/}).
  The support for ILIAS so far is somewhat rudimentary. Not all question
  types are currently supported. What has been tested most extensively, are
  string questions with open answers and solutions; these are rendered into
  ILIAS essay questions within a question pool. Also mchoice and schoice
  questions render in ILIAS, but have received less amount of testing.
  Numeric and cloze questions do not work yet.
}

\value{
  \code{exams2ilias} returns a list of exams as generated by \code{\link[exams]{xexams}}.

  If \code{zip} is \code{TRUE} (default), an ILIAS Questionpool object is generated.
}

\seealso{
  \code{\link[exams]{exams2qti12}},
}

\examples{
## load package and enforce par(ask = FALSE)
library("exams")
options(device.ask.default = FALSE)

## define an exams (= list of exercises)
myexam <- list(
  ## multiple-choice with graphics
  c("boxplots.Rmd", "scatterplot.Rmd"),

  ## multiple-choice with output or tables
  c("ttest.Rmd", "anova.Rmd", "relfreq.Rmd"),

  ## single-choice
  c("deriv2.Rmd", "swisscapital.Rmd"),

  ## string (closed)
  "function.Rmd",
  
  ## string (open-ended)
  "essayreg.Rmd"
)

## output directory
dir.create(mydir <- tempfile())

## generate .zip with ILIAS Questionpool in temporary directory
## using a few customization options
exams2ilias(myexam, n = 3, dir = mydir,
  maxattempts = 3,
  num = list(digits = 1),
  mchoice = list(shuffle = TRUE, enumerate = FALSE),
  solutionswitch = FALSE
)
dir(mydir)
}

\keyword{utilities}
