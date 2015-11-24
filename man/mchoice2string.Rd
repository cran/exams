\name{mchoice2string}
\alias{mchoice2string}
\alias{string2mchoice}
\alias{mchoice2text}
\alias{answerlist}

\title{Convenience Functions for Exam Formatting}

\description{
  A collection of convenience functions for formatting in exam generation
  that can be used for switching between suitable logical/text/numeric
  representations of multiple choice solutions.
}

\usage{
  mchoice2string(x, single = FALSE)
  string2mchoice(x, single = FALSE)
  mchoice2text(x, markup = c("latex", "markdown"))
  answerlist(\dots, sep = ". ", markup = c("latex", "markdown"))
}
\arguments{
  \item{x}{an object, see below for examples.}
  \item{single}{logical. Should the function check whether exactly
    a single answer is true?}
  \item{\dots}{character vectors to be included in answer lists.}
  \item{sep}{character for separation between vectors, see below
    for examples.}
  \item{markup}{character indicating which markup (LaTeX vs. Markdown)
    should be generated.}
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

## generating answerlist based on questions,
## solutions and explanations
qu <- c("Zurich is the capital of Switzerland.",
        "Italian is an official language in Switzerland.",
        "Switzerland is part of the European Union.")
sol <- c(FALSE, TRUE, FALSE)
ex <- c("The capital of Switzerland is Bern.",
        "The four official languages are: German, French, Italian, Romansh.",
	"Switzerland is part of the Schengen Area but not the European Union.")
answerlist(qu)
answerlist(ifelse(sol, "True", "False"), ex)
}

\keyword{utilities}
