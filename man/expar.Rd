\name{expar}
\alias{expar}

\title{Fix Parameters in Dynamic Exercises}

\description{
  Set parameters, defined in the first code chunk of an exercise file,
  to specific values instead of their definition in the exercise file.
}

\usage{
expar(file, \dots)
}

\arguments{
  \item{file}{character with (path to) an exercise file.}
  \item{\dots}{paramters to be fixed within \code{file} (or a single
    list of parameters to be fixed).}
}

\details{
  To set certain parameters that are randomly generated within an exercise
  file to specific values, a copy of the exercise file is generated in
  the temporary directory of the R session. In the temporary copy of the
  exercise file the first assignment to the specified parameter in the first
  code chunk is replaced with the definition provided in \code{expar}.
  
  To work properly, the parameter of interest must be defined with
  a standard assignment in the first code chunk at the beginning of a line.
  The original definition of the parameter must be in a single line only
  (typically using something like \code{sample} or \code{runif} etc.).
  
  After replacing the code chunk, \code{expar} returns the path to the
  temporary file with the modified exercise. This can then be processed
  with \code{exams2xyz} "as usual".
}

\value{
  A character string with the file path of the modified exercise.
}

\examples{
## fix parameters "a" and "c" in deriv.Rmd
## (but still generate "b" randomly)

## HTML output
if(interactive()){
exams2html(expar("deriv.Rmd", a = 1, c = 0))
}

## just the question text
x <- xexams(expar("deriv.Rmd", a = 1, c = 0))
writeLines(x[[1]][[1]]$question)
}

\keyword{utilities}
