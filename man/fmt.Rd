\name{fmt}
\alias{fmt}
\alias{round2}
\alias{toLatex.matrix}
\alias{char_with_braces}
\alias{num_to_tol}
\alias{num2tol}

\title{Auxiliary Formatting Functions}

\description{
  Auxiliary functions for displaying numeric elements in exercises.
}

\usage{
fmt(x, digits = 2L, zeros = digits < 4L, \dots)

round2(x, digits = 0) 

char_with_braces(x)

num_to_tol(x, reltol = 0.0002, min = 0.01, digits = 2)

\method{toLatex}{matrix}(object, skip = FALSE, fix = getOption("olat_fix"),
  escape = TRUE, \dots)
}

\arguments{
  \item{x}{numeric vector.}
  \item{digits}{integer. Digits that should be used for rounding.}
  \item{zeros}{logical. Should trailing zeros be added?}
  \item{reltol}{numeric. Relative tolerance (relative to correct
    solution \code{x}).}
  \item{min}{numeric. Minimum absolute tolerance.}
  \item{object}{matrix.}
  \item{skip}{logical. Should an additional skip be added between rows?}
  \item{fix}{logical. Should an additional empty column be added between
    all columns? This is a workaround for OLAT that collapses spaces
    between columns in MathML.}
  \item{escape}{logical. Should LaTeX commands be escaped (as appropriate
    for \code{\link[utils]{Sweave}}) or not (as appropriate for
    \code{\link[knitr]{knit}})?}
  \item{\dots}{passed to \code{\link[base]{format}} for \code{fmt},
    currently not used in the \code{toLatex} method.}
}

\details{
  Various functions that help displaying numerical results in exercises:

  The function \code{fmt} rounds and adds trailing zeros (by default if \code{digits} is
  lower than 4).
  
  The function \code{round2} does what is known in German as kaufmaennisches Runden
  (rounding away from zero for trailing 5s).
  
  The function \code{char_with_braces} adds parentheses for negative elements
  (in order to facilitate their display in equations).
  
  The function \code{num_to_tol} (or equivalently \code{num2tol})
  computes the absolute tolerance based on a
  numeric solution \code{x} and a relative tolerance \code{reltol}.

  The \code{toLatex} method sets up a matrix array with parentheses.
}

\examples{
## emulate how students round
## (rather than using the round-to-even strategy R employs)
round2(c(0.005, 0.015), digits = 2)
round(c(0.005, 0.015), digits = 2)

## this is also employed internally in the fmt() formatting function
fmt(c(0.005, 0.015))

## the main purpose of fmt() is that some numeric result can be displayed
## both at high accuracy and then at the rounding that students should do
## (e.g., with 2 or 3 digits)
sol <- runif(1)
fmt(sol, 6)
fmt(sol, 2)

## but fmt() also assures showing a very high numer of significant digits
## (up to 12)
sol <- 123456 + sol
sol
fmt(sol, 6)
fmt(sol, 2)

## and fmt() also takes care of adding trailing zeros (if digits < 4)
fmt(1)
fmt(1, digits = 3)
fmt(1, digits = 6)

## char_with_braces() is for adding parentheses, e.g., before constructing a sum
paste(char_with_braces(-2:2), collapse = " + ")

## for including a matrix in a LaTeX formula
x <- matrix(1:4, ncol = 2)
toLatex(x)
toLatex(x, skip = TRUE)

## compute absolute tolerances:
## minimum is 0.01
num_to_tol(1)
## but can be larger for larger solutions
num_to_tol(100)
}

\keyword{utilities}
