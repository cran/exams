\name{read_exercise}
\alias{read_exercise}
\alias{extract_command}
\alias{extract_extra}
\alias{extract_environment}
\alias{extract_items}
\alias{read_metainfo}

\title{Reading LaTeX/Markdown Exercise Files}

\description{
  Reading an exercise in either LaTeX format (i.e.,
  after \code{\link[utils]{Sweave}} was run) or
  Markdown format (i.e., after \code{\link[knitr]{knit}}
  was run).
}

\usage{
  read_exercise(file, markup = NULL)
  read_metainfo(file, markup = NULL)
}

\arguments{
  \item{file}{character. Name of the LaTeX (\code{.tex}) or
    Markdown (\code{.md}) file that should be read into R.}
  \item{markup}{character specifying whether \code{file} is a \code{"latex"}
    or \code{"markdown"} exercise. By default (\code{NULL}) it is
    inferred from the \code{file} extension.}
  
}

\details{
  \code{read_exercise} extracts the LaTeX/Markdown code from the question
  and solution environments/sections of the exercise file, extracting
  the corresponding answerlists separately (if any). Paths to supplementary
  files (such as graphics or data files) are stored and the 
  metainformation is extracted (by calling \code{read_metainfo} which
  also includes sanity checks).
  
  The supported metainformation commands are described in detail in
  \code{vignette("exams2", package = "exams")}, see Table 2. Essentially
  the \code{extype} command in is mapped to the \code{type}
  element of the returned list etc. (see the Value section below), using
  the right storage mode for each command (numeric, character, logical).
  Additionally, there is an \code{exextra} command which allows to set
  up arbitrary additional metainformation elements.
}

\value{
  \code{read_exercise} returns a list with elements
  \item{question}{a character vector with LaTeX/Markdown code from the
    question environment (excluding the answerlist environment,
    if any).}
  \item{questionlist}{a character vector with LaTeX/Markdown code from the
    answerlist environment within the question environment (if any).}
  \item{solution}{a character vector with LaTeX/Markdown code from the
    solution environment (excluding the answerlist environment,
    if any).}
  \item{solutionlist}{a character vector with LaTeX/Markdown code from the
    answerlist environment within the solution environment (if any).}
  \item{metainfo}{a list of metainformation options, see below.}
  \item{supplements}{a character vector with paths to supplementary
    files such as graphics or data files (if any).}

  \code{read_metainfo} returns a list with the following elements.
  Most elements may also be \code{NULL} (or empty) if the underlying
  information is not specified in the \code{file}. If \code{file} specifies
  extra information, there may also be additional list elements.
  \item{file}{character with file name (without extension) of the exercise
    template.}
  \item{markup}{character indicating whether \code{"latex"} or \code{"markdown"}
    markup is used in the exercise.}
  \item{type}{character indicating exercise type: \code{num}, \code{mchoice},
    \code{schoice}, \code{string}, or \code{cloze}.}
  \item{name}{character with short name/description (to be used for printing within R).}
  \item{title}{character with a pretty longer title.}
  \item{section}{character with sections for groups of exercises (using slashes for
    subsections like a URL).}
  \item{version}{character with version of exercise.}
  \item{solution}{correct solution. The type/value depends on the
    \code{type} of the exercise: \code{num} leads to a numeric vector (of length 1
    or 2), \code{mchoice}/\code{schoice} lead to logical vector, \code{string} to
    a character vector (of length 1), and \code{cloze} leads to a list of
    solutions (depending on \code{clozetype}).}
  \item{tolerance}{numeric tolerance limits (of length 1 or 2) for numeric
    solutions.}
  \item{clozetype}{character indicating the types of the elements of a
    \code{cloze} exercise.}
  \item{points}{numeric with (default) points for correct solution.}
  \item{time}{numeric with (default) time (in seconds) for solution.}
  \item{shuffle}{logical indicating whether \code{mchoice}/\code{schoice}
    answers should be shuffled (in Moodle or other e-learning systems).}
  \item{single}{logical indicating whether radio buttons should be
    used in Moodle.}
  \item{length}{numeric with length of \code{solution}.}
  \item{string}{character with a collapsed string of the solution (and tolerance)
    for backward compatibility with \code{\link[exams]{exams}}.}
  \item{maxchars}{character with the maximum number of characters
    allowed in QTI text answers (exercise type: string).}
  \item{abstention}{character with the label to be used for an abstention
    button in schoice/mchoice answers (currently only supported by Moodle).}
}

\seealso{
  \code{\link[exams]{xexams}}
}

\examples{
## xexams() uses read_exercise() by default to read in
## each individual exercise, e.g., here for only a single
## exam with only a single exercise the result is:
set.seed(1090)
xexams("tstat.Rnw")[[1]][[1]]

## the corresponding Markdown version has:
set.seed(1090)
xexams("tstat.Rmd")[[1]][[1]]
}

\keyword{utilities}
