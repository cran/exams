\name{matrix_to_schoice}
\alias{matrix2schoice}
\alias{matrix_to_schoice}
\alias{matrix2mchoice}
\alias{matrix_to_mchoice}
\alias{det2schoice}
\alias{det_to_schoice}

\title{Generate Single- and Multiple-Choice Question Lists for Matrix Solutions}

\description{
  Functions for generating single- and multiple-choice question lists for
  a matrix solution. (Optimized for integer matrices.)
}

\usage{
matrix_to_schoice(x, y = NULL, lower = FALSE, name = "a",
  delta = 0.5, digits = 0)

matrix_to_mchoice(x, y = NULL, lower = FALSE, name = "a",
  comparisons = c("==", "<", ">", "<=", ">="))

det_to_schoice(x, y = NULL, range = NULL, delta = 0.5, digits = 0)
}

\arguments{
  \item{x}{matrix (correct result).}
  \item{y}{numeric vector (optional) with (potentially) wrong solutions/comparisons.}
  \item{lower}{logical. Should only elements from the lower triangle be assessed?}
  \item{name}{character. Base name for matrix elements.}
  \item{delta}{numeric. Minimal distance between solutions.}
  \item{digits}{integer. Digits that should be displayed.}
  \item{comparisons}{character. Vector of logical comparisons that
    should be employed.}
  \item{range}{numeric vector of length 2 (optional) with range of random wrong solutions.}
}

\details{
  The function \code{matrix_to_schoice} (or equivalently \code{matrix2schoice})
  can be used for generating a single-choice
  question list for a correct result matrix \code{x}. One element is picked
  randomly from the matrix and chosen to be the correct solution. Other values
  from the observed absolute range are used as wrong solutions by default
  (if \code{y} does not provide an alternative list of potential solutions).

  The function \code{matrix_to_mchoice} (or equivalently \code{matrix2mchoice})
  can be used for generating a multiple-choice
  question list for a correct result matrix \code{x}. Each item from the question
  list is a logical comparison of one matrix element with a comparion value.
  By default the comparisons are picked randomly from the observed absolute range
  (unless \code{y} specifies a different list of comparisons).

  The function \code{det_to_schoice}  (or equivalently \code{det2schoice})
  can be used for generating a single-choice
  question list for the determinant of a 2x2 matrix. It has been optimized
  for matrices with single-digit integer elements. It may not yield very
  balanced random solutions for other scenarios.
}

\value{
  \code{matrix_to_schoice}/\code{matrix2schoice} returns a list with the following components:
  \item{index}{numeric vector with matrix index of the correct solution chosen.}
  \item{name}{character with LaTeX code for the correct matrix element chosen.}
  \item{solutions}{a logical vector of length 5 indicating the \code{correct} solution,}
  \item{questions}{a character vector of length 5 with question list.}

  \code{matrix_to_mchoice}/\code{matrix2mchoice} returns a list with the following components:
  \item{solutions}{a logical vector of length 5 indicating the \code{correct} solution,}
  \item{questions}{a character vector of length 5 with question list.}
  \item{explanations}{a character vector of length 5 with explanations why the
    solutions are correct or wrong.}

  \code{det_to_schoice}/\code{det2schoice} returns a list with the following components:
  \item{solutions}{a logical vector of length 5 indicating the \code{correct} solution,}
  \item{questions}{a character vector of length 5 with question list.}
}

\seealso{\code{\link{num_to_schoice}}}

\examples{
A <- matrix(c(-9, 0, 5, -2), ncol = 2)
matrix_to_schoice(A)
matrix_to_mchoice(A)
det_to_schoice(A)

B <- matrix(1:9, ncol = 3)
matrix_to_schoice(B)
matrix_to_mchoice(B)
}

\keyword{utilities}
