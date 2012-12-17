\name{xexams}
\alias{xexams}
\alias{exams_metainfo}

\title{Extensible Generation of Exams}

\description{
  Extensible automatic generation of exams including
  multiple choice questions and arithmetic problems.
}

\usage{
  xexams(file, n = 1L, nsamp = NULL,
    driver = list(sweave = NULL, read = NULL, transform = NULL, write = NULL),
    dir = ".", edir = NULL, tdir = NULL, sdir = NULL)
  exams_metainfo(x, \dots)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files,
    for details see below.}
  \item{n}{integer. The number of copies to be taken from \code{file}.}
  \item{nsamp}{integer. The number of exercise files sampled from each
    list element of \code{file}. Sampling without replacement is used
    if possible. (Only if some element of \code{nsamp} is larger than
    the length of the corresponding element in \code{file}, sampling
    with replacement is used.)}
  \item{driver}{list with elements \code{sweave} (weaver function or list of arguments
    for the default \code{\link[utils]{Sweave}}), \code{read} (function
    for reading exercise files, defaulting to \code{\link[exams]{read_exercise}}),
    \code{transform} (function to transform each exercise, by default
    no transformations are done), \code{write} (function to write exams
    to output files, by default nothing is written). For more details,
    see below.}
  \item{dir}{character. The output directory passed on to \code{driver$write}.}
  \item{edir}{character specifying the path of the directory in which
    the files in \code{file} are stored (see also below).}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempdir}}.}
  \item{sdir}{character specifying a directory for storing supplements, by
    default this is chosen via \code{\link[base]{tempdir}}.}
  \item{x}{a list as returned by \code{xexams}.}
  \item{\dots}{currently not used.}
}

\details{
  \code{xexams} is meant to provide an extensible framework for generating exams
  based on exercises in \code{\link[utils]{Sweave}} format and rendering them
  into various output formats such as PDF, HTML, or XML (e.g., for Moodle or IMS QTI).
  \code{xexams} is typically not called by the user directly but is used as a common
  infrastructure for functions such as \code{\link[exams]{exams2pdf}}, \code{\link[exams]{exams2html}},
  \code{\link[exams]{exams2moodle}}, \code{\link[exams]{exams2qti12}}, or
  \code{\link[exams]{exams2lops}}.
  
  \code{xexams} generates exams from lists (or vectors) of Sweave source files by:
  (1) running \code{driver$sweave} on each Sweave exercise (by default \code{\link[utils]{Sweave}}),
  (2) running \code{driver$read} on the resulting LaTeX file which by default
    uses \code{\link[exams]{read_exercise}} to read question/solution texts plus
    metainformation and stores the result in a list,
  (3) running \code{driver$transform} on this list for possible transformations
    (e.g., from LaTeX to HTML),
  (4) running \code{driver$write} on the list of exercises within each exam.

  Each exercise in an exam is essentially a standalone Sweave source file
  that \code{xexams} knows (almost) nothing about, it just calls \code{driver$sweave}
  in each iteration and assumes that \code{driver$read} can read the resulting
  LaTeX file into a list.
  
  The specification in \code{file} should be either of form \code{"foo"}
  or equivalently \code{"foo.Rnw"}, where the file \code{"foo.Rnw"} should
  either be in the local directory, the \code{edir} directory or in
  the \code{exercises} directory of the package. \code{file} can either
  be a simple vector or a list of vectors. In the latter case, exercises
  are chosen randomly within each list element. For example, the specification
  \code{file = list(c("a", "b"), "xyz")} will result in an exam with two
  exercises: the first exercise is chosen randomly between \code{"a"} and
  \code{"b"} while \code{"xyz"} is always included as the second exercise.
}

\value{
  A list of exams (of length \code{n}),
  each of which is a list of exercises (whose length depends on the length of \code{file} and \code{nsamp}),
  each of which is a list (whose length/contents depends on \code{driver$read}).

  When using the default reader, the resulting list can be simplified
  using \code{exams_metainfo}, returning the same (classed) structure
  as the older \code{\link[exams]{exams}} interface.
}

\seealso{
  \code{\link[utils]{Sweave}},
  \code{\link[exams]{exams}},
  \code{\link[exams]{exams2pdf}},
  \code{\link[exams]{exams2html}},
  \code{\link[exams]{exams2moodle}},
  \code{\link[exams]{exams2qti12}},
  \code{\link[exams]{exams2lops}}
  }

\examples{
## define an exam with five exercises
myexam <- list(
  "boxplots",
  c("tstat", "ttest", "confint"),
  c("regression", "anova"),
  "scatterplot",
  "relfreq"
)

## run exams with default drivers (i.e., no transformations or writer)
x <- xexams(myexam, n = 2)
## x is a list of 2 exams,
## each of which contains 5 exercises,
## each of which contains LaTeX code for question(list) and solution(list),
## plus metainformation and potential supplements

## The first exercise in each exam is "boxplots", a multiple choice question.
## Its general question text is
x[[1]][[1]]$question
## with a list of multiple choice questions given as
x[[1]][[1]]$questionlist
## the corresponding graphic is in supplementary file
x[[1]][[1]]$supplements

## The metainformation is a list read for the \ex*{} items
x[[1]][[1]]$metainfo

## The metainformation can also be extracted/printed as
## in the old exams() (rather than xexams()) interface
exams_metainfo(x)
}

\keyword{utilities}
