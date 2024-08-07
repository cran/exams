\name{match_exams_call}
\alias{match_exams_call}
\alias{match_exams_device}
\alias{match_exams_iteration}

\title{Query Information from Last xexams/exams2xyz Call}

\description{
  \code{match_exams_call} queries the last call made to \code{xexams}
  (typically through some \code{exams2xyz} interface). \code{match_exams_device}
  queries the graphics device used in the last \code{xweave} call.
  \code{match_exams_iteration} queries the iteration (within n replications)
  that is currently processed by \code{xexams}.
}

\usage{
match_exams_call(which = 1L, deparse = TRUE)
match_exams_device()
match_exams_iteration()
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
(This feature only works from R 3.2.0 onwards.)

Similarly, the function \code{match_exams_device} can be used within
an exercise to use the same graphics device that \code{xweave} is
using.

Finally, the function \code{match_exams_iteration} can be used to
find out which iteration (within \code{n} replication) is currently
processed by \code{xexams} or \code{exams2xyz}. This is useful if an
exercise wants to iterate through a certain grid of parameter settings.
}

\examples{
## call exams2nops
dir.create(tdir <- tempfile())
exams2nops("tstat2.Rmd", dir = tdir)
match_exams_call()

## graphics device used
match_exams_device()

## exams2nops called exams2pdf called xexams:
match_exams_call(which = NULL)

## get full exams2nops call
match_exams_call(deparse = FALSE)

\donttest{
## but note that convenience wrappers etc. are included
e2n <- function(...) exams2nops(...)
e2n("tstat2.Rmd", dir = tdir)
match_exams_call(which = NULL)
}
}

\keyword{utilities}
