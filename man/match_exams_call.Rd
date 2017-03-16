\name{match_exams_call}
\alias{match_exams_call}

\title{Query Last xexams/exams2xyz Call}

\description{
  Query the last call made to \code{xexams} (typically through
  some \code{exams2xyz} interface).
}

\usage{
match_exams_call(which = 1L, deparse = TRUE)
}

\arguments{
  \item{which}{integer. Specifies the hierarchy level
    at which the \code{exams2xyz} call should be extracted.}
  \item{deparse}{logical. Should only the deparsed function name
    be computed (or the entire call)?}
}

\details{
The function \code{match_exams_call} is useful for determining
within an exercise which \code{exams2xyz} interface is used in order
to behave slightly differently, e.g., for PDF vs. HTML output.

This feature only works from R 3.2.0 onwards.
}

\examples{
## call exams2nops
tdir <- tempdir()
exams2nops("tstat2.Rnw", dir = tdir)
match_exams_call()

## exams2nops called exams2pdf called xexams:
match_exams_call(which = NULL)

## get full exams2nops call
match_exams_call(deparse = FALSE)

## but note that convenience wrappers etc. are included
e2n <- function(...) exams2nops(...)
e2n("tstat2.Rnw", dir = tdir)
match_exams_call(which = NULL)
}

\keyword{utilities}
