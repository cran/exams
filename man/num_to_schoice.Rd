\name{num_to_schoice}
\alias{num2schoice}
\alias{num_to_schoice}

\title{Generate Single-Choice Question List from Numeric Solution}

\description{
  A function for generating a single-choice question list for one
  correct numeric solution along with four wrong solutions.
}

\usage{
num_to_schoice(correct, wrong = NULL, range = c(0.5, 1.5) * correct,
  delta = 1, digits = 2, method = c("runif", "delta"), sign = FALSE,
  verbose = getOption("num_to_choice_warnings"))
}

\arguments{
  \item{correct}{numeric vector of length 1 with correct solution.}
  \item{wrong}{numeric vector (optional) with wrong solutions.}
  \item{range}{numeric vector of length 2 with range of random wrong solutions.}
  \item{delta}{numeric. Minimal distance between solutions.}
  \item{digits}{integer. Digits that should be displayed.}
  \item{method}{character specifying method for generating random results.}
  \item{sign}{logical. Should the sign be changed randomly?}
  \item{verbose}{logical. Should warnings be issued if no suitable
    set of wrong solutions can be found?}
}

\details{
  The function \code{num_to_schoice} (or equivalently \code{num2schoice})
  can be used for generating a single-choice
  question list for a numeric \code{correct} solution. The question list
  always comprises five elements, one of which is the correct solution. The
  wrong solutions can be provided or are generated randomly. If \code{wrong}
  is provided only up to 2 elements of it are used in order to assure some
  random solutions.
  
  Two methods can be used to generate the wrong solutions: Either simply
  \code{\link[stats]{runif}} or otherwise a full equi-distant grid for the
  \code{range} with step size \code{delta} is set up from which a discrete
  uniform sample is drawn. The former is preferred if the \code{range} is large
  enough while the latter performs better if the \code{range} is small (as
  compared to \code{delta}.
  
  Exercise templates using \code{num_to_schoice} should be thoroughly
  tested in order to avoid problems with too small ranges or almost
  identical correct and wrong answers! This can potentially cause problems,
  infinite loops, etc.
}

\value{
  \code{num_to_schoice}/\code{num2schoice} returns either \code{NULL} (if no suitable question
  list can be found) or a list with the following components:
  \item{solutions}{a logical vector of length 5 indicating the \code{correct} solution,}
  \item{questions}{a character vector of length 5 with question list.}
}

\seealso{\code{\link{matrix_to_schoice}}}

\examples{
set.seed(1)
## just a correct solution
num_to_schoice(123.45)

## or equivalently
set.seed(1)
num2schoice(123.45)

## just a correct integer solution
num_to_schoice(123, digits = 0)

## a correct solution with a wider range
num_to_schoice(123.45, range = c(0, 200))

## here, the defaults can't work...
## num_to_schoice(0.1234)

## alternatives could be
num_to_schoice(0.1234, range = c(0, 1), delta = 0.03, method = "delta")
num_to_schoice(0.1234, range = c(-5, 5), delta = 0.05)
num_to_schoice(0.1234, wrong = c(0.2749, 1.9723), delta = 0.05)
num_to_schoice(0.1234, wrong = c(0.2749, 1.9723), range = c(-5, 5), delta = 0.05)
}

\keyword{utilities}
