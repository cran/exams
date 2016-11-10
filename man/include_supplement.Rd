\name{include_supplement}
\alias{include_supplement}

\title{Copy (Static) Supplement Files for Inclusion in Exercises}

\description{
  Copy (static) files (e.g., graphics, data sets, etc.)
  for inclusion as supplements in an exercise.
}

\usage{
include_supplement(file, dir = NULL, recursive = FALSE)
}

\arguments{
  \item{file}{character. A (vector of) file name(s).}
  \item{dir}{character. The directory where \code{file} can
    be found. If used within the code chunks of exercises, the
    default is to use the directory in which the exercises are stored.}
  \item{recursive}{logical. Should also sub-directories of \code{dir}
    be searched for \code{file}?}
}

\details{
Usually, supplement files are created dynamically within an exercise,
e.g., data is simulated and then plotted or stored in a file etc.
However, sometimes an exercises wants to include a static supplement
file that is available in some directory on the system. Then,
the \code{include_supplement} is a convenience function that
copies such a file from its directory into the supplements of
an exercise. Then it can be included/referenced as usual in the
question/solution text.
}

\examples{
## The "Rlogo" exercise uses a static image which is provided
## within the "exams" package.
if(interactive()) {
exams2html("Rlogo.Rnw")
}
}

\keyword{utilities}
