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
  format = TRUE, order = getOption("num_to_schoice_order", FALSE),
  maxit = getOption("num_to_schoice_maxit", Inf),
  verbose = getOption("num_to_schoice_verbose", TRUE))
}

\arguments{
  \item{correct}{numeric vector of length 1 with correct solution.}
  \item{wrong}{numeric vector (optional) with wrong solutions.}
  \item{range}{numeric vector of length 2 with range of random wrong solutions.}
  \item{delta}{numeric. Minimal distance between solutions.}
  \item{digits}{integer. Digits that should be displayed.}
  \item{method}{character specifying method for generating random results.}
  \item{sign}{logical. Should the sign be changed randomly?}
  \item{format}{logical or character. Should the question list be formatted
    to a character vector with LaTeX math markup? If \code{TRUE} (default, or
    equivalently \code{"latex"}) the question list is formatted with the 
    same number of \code{digits} and LaTeX math markup is added.
    If \code{FALSE} (or equivalently \code{"numeric"}) then the question
    list is returned as a numeric vector. If \code{"character"} then
    the question list is a formatted character vector but without LaTeX
    math markup.}
  \item{order}{logical. Should the question list be ordered numerically?
    If \code{FALSE} (default) the question list is shuffled randomly.}
  \item{maxit}{numeric. Maximum number of iterations to try to find a
    suitable set of wrong solutions for the question list. If the
    number of iterations exceed \code{abs(maxit)}, \code{NULL} is returned
    (with a warning if \code{verbose = TRUE}) except if \code{maxit < 0}
    (which stops with an error). See the examples below.}
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
  
  The function tries to avoid patterns in the question list that could be
  used for guessing the correct solution, e.g., situations where (almost)
  always the highest (or always the lowest) answer is the correct one.
  Therefore, internally \code{num_to_schoice} first randomly decides how
  many of the 4 wrong solutions should be to the left or to the right of
  the correct solution, respectively. And in a second step the sampling
  \code{method} is used to find these fixed numbers of wrong solutions
  to the left and right (if possible!).

  By default (\code{format = TRUE} or \code{format = "latex"}), the question
  list in the output is formatted to a character vector enclosed in LaTeX
  math markup to assure consistent rendering. Optionally, using
  \code{format = FALSE} or \code{format = "numeric"} the numeric vector
  without any formatting can be returned as well. This is useful if
  the numeric question list should be processed further, e.g., adding
  annotation etc. Finally, \code{format = "character"} returns a formatted
  character vector with the same number of \code{digits} but without
  LaTeX math markup. This is useful if the output format of the exam
  does not provide LaTeX support.

  Exercise templates using \code{num_to_schoice} should be thoroughly
  tested in order to avoid problems with too small ranges or almost
  identical correct and wrong answers! This can potentially cause problems,
  infinite loops, etc. See \url{https://www.R-exams.org/tutorials/stresstest/}
  for some comments/hints regarding stress-testing of such exercise templates.
}

\value{
  \code{num_to_schoice}/\code{num2schoice} returns either \code{NULL} (if no suitable question
  list can be found) or a list with the following components:
  \item{solutions}{a logical vector of length 5 indicating the \code{correct} solution,}
  \item{questions}{a vector of length 5 with the question list, by default formatted
    as character but optionally also numeric (if \code{format = FALSE} or \code{"numeric"}).}
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

## here, the defaults can't work
num_to_schoice(0.123, verbose = FALSE)
## warning: specified 'range' is too small for 'delta'
## (suppressed here via verbose = FALSE)

## alternatives could be
num_to_schoice(0.123, range = c(0, 1), delta = 0.03, method = "delta")
num_to_schoice(0.123, range = c(-5, 5), delta = 0.05)
num_to_schoice(0.123, wrong = c(0.275, 1.972), delta = 0.05)
num_to_schoice(0.123, wrong = c(0.275, 1.972), range = c(-5, 5), delta = 0.05)

## ## caveat: num_to_schoice() can result in virtually infinite loops, e.g.,
## set.seed(5)
## num_to_schoice(10, range = c(7, 13))
## 
## ## return NULL with warning after 10 iterations
## set.seed(5)
## num_to_schoice(10, range = c(7, 13), maxit = 10)
##
## ## stop with error after 10 iterations
## set.seed(5)
## num_to_schoice(10, range = c(7, 13), maxit = -10)
}

\keyword{utilities}
