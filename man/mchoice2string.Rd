\name{mchoice2string}
\alias{mchoice2string}
\alias{string2mchoice}
\alias{mchoice2text}

\title{Convenience Functions for Exam Generation}

\description{
  A collection of convenience functions for exam generation
  that can be used for switching between suitable logical/text/numeric
  representations of multiple choice solutions.
}

\usage{
  mchoice2string(x)
  string2mchoice(x)
  mchoice2text(x)
}
\arguments{
  \item{x}{an object, see below for examples}
}

\details{
  Three convenience functions for facilitating work with
  multiple choice solutions of exams. All have almost
  trivial definitions, see also examples below.
 
}

\seealso{\code{\link[exams]{exams}}}

\examples{
  ## multiple choice answer
  mc <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
  
  ## switching to string representation
  mchoice2string(mc)
  
  ## reverse string encoding
  string2mchoice("10100")

  ## switching to text
  mchoice2text(mc)
}

\keyword{utilities}
