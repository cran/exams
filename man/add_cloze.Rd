\name{add_cloze}
\alias{add_cloze}
\alias{format_metainfo}
\alias{initialize_exercise}

\title{Functions for Managing the Meta-Information of Exercises}

\description{
  Functions for adding elements to the meta-information of
  an exercise and for formatting the collected meta-information,
  respectively.
}

\usage{
  add_cloze(solution, choices = NULL, type = NULL, tolerance = NULL,
    answertag = TRUE, \dots)

  format_metainfo(field)

  initialize_exercise()
}
\arguments{
  \item{solution}{solution for an answer field in an exercise.
    See also the details below.}
  \item{choices}{vector of answer alternatives in a single-choice or
    multiple-choice exercise. See also the details below.}
  \item{type}{character. Optional specification of the type for an
    answer field, by default inferred from \code{solution}. Currently,
    \code{"num"}, \code{"string"}, \code{"schoice"}, and \code{"mchoice"}
    are the standard types. Additionally, some \code{exams2xyz} interfaces
    support \code{"essay"}, \code{"file"}, or \code{"verbatim"} as part
    of a cloze.}
  \item{tolerance}{numeric. Tolerance to be used in case of a numeric
    \code{solution}. By default selected via \code{\link[exams]{num_to_tol}}.}
  \item{answertag}{logical. Should an ANSWERi tag be returned for
    inclusion in the text?}
  \item{\dots}{optional arguments for formatting a numeric \code{solution}
    via \code{\link[exams]{fmt}} (only applied if any arguments are specified).}
  \item{field}{character indicating whether the \code{"type"}, \code{"solution"},
    \code{"tolerance"}, or the \code{"answerlist"} for the exercise should
    be returned, formatted suitably.}
}

\details{
  The function \code{add_cloze(solution)} adds meta-information to an
  internal list in order to facilitate tracking of the elements in an
  exercise, especially in cloze exercises.
  
  If \code{solution} is numeric, a \code{num} element is added along with
  the corresponding \code{tolerance}, by default selected via
  \code{num_to_tol(solution)}. Optionally, the \code{solution} can be formatted
  via \code{\link[exams]{fmt}}, e.g., to obtain a rounding to a given number
  of \code{digits} etc.
  
  If \code{solution} is a character, a \code{string} element is added.
  
  Additionally, \code{solution} can be a numeric or character vector or
  matrix in which case all elements are added (row-wise) to the meta-information
  and suitably-formatted answer tags are included as well
  (if \code{answertag = TRUE}, default).
  
  Finally, if \code{solution} is a named logical vector, an \code{mchoice} or
  \code{schoice} element is added. The \code{type} can either be specified
  explicitly and otherwise \code{schoice} is used if \code{solution} has exactly
  one \code{TRUE} element. Alternatively, \code{add_cloze(solution, choices)} can be used
  to define single-choice or multiple-choice exercises where \code{choices}
  contains a vector of answer alternatives and \code{solution} a subset of
  \code{choices} with the correct solution(s). See also the examples below.

  Using \code{format_metainfo(type)} the collected meta-information can be extracted,
  suitably formatted for inclusion in an exercise. The \code{format_metainfo("answerlist")}
  is intended to be included at the end of the question section while the other
  types (\code{"type"}, \code{"solution"}, and \code{"tolerance"}) are intended
  to be included in the meta-information tags.

  The function \code{initialize_exercise()} resets and initializes the exercise
  meta-information in the package's internal environment. Usually, this does
  not have be called by the user because it is called automatically prior to
  processing an exercise file. However, it is still provided for testing
  and illustration purposes as in the examples below.
}

\seealso{\code{\link[exams]{answerlist}}}

\examples{
## initialize meta-information (done automatically within xweave())
initialize_exercise()

## add num, string, schoice, and mchoice elements
add_cloze(123.456789)

add_cloze("ABC")

add_cloze("high", c("low", "neutral", "high"))

add_cloze(c(
  "Zurich is the capital of Switzerland." = FALSE,
  "Italian is an official language in Switzerland." = TRUE,
  "Switzerland is part of the European Union." = FALSE
), type = "mchoice")

## add a numeric matrix
tab <- matrix(10^(1:4), ncol = 2, byrow = TRUE,
  dimnames = list(c("Top", "Bottom"), c("Left", "Right")))
add_cloze(tab)

## format collected meta-information
format_metainfo("type")
format_metainfo("solution")
format_metainfo("tolerance")
writeLines(format_metainfo("answerlist"))

## re-initialize cloze meta-information for cleanup
initialize_exercise()


## equivalent specifications of single/multiple-choice elements
add_cloze("high", c("low", "neutral", "high"), type = "schoice")
add_cloze(c("low" = FALSE, "neutral" = FALSE, "high" = TRUE), type = "schoice")

## format collected meta-information
format_metainfo("type")
format_metainfo("solution")
writeLines(format_metainfo("answerlist"))

## re-initialize cloze meta-information for cleanup
initialize_exercise()
}

\keyword{utilities}
