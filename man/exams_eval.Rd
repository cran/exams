\name{exams_eval}
\alias{exams_eval}

\title{Auxiliary Tools for Evaluating Exams}

\description{
  Generation various helper functions for evaluating exams.
}

\usage{
exams_eval(partial = TRUE, negative = FALSE,
  rule = c("false2", "false", "true", "all", "none"))
}

\arguments{
  \item{partial}{logical. Should single/multiple-choice answers be evaluated
    as a whole pattern (\code{partial = FALSE}) or should partial credits
    be assigned to each of the choices (\code{partial = TRUE})?}
  \item{negative}{logical or numeric. Handling of negative points for an
    exercise, for details see below.}
  \item{rule}{character specifying which rule to use for negative
    partial credits.}
}

\details{
  The function \code{exams_eval} is a convenience wrapper for specifying
  various types of evaluation policies. It returns a set of auxiliary
  functions that may be useful in the evaluation of exams.

  Exercises of types \code{"num"} or \code{"string"} can essentially be
  just correct or wrong. In the former case they will give 100 percent of
  all points, in the latter either 0 percent or some negative percentage
  can be assigned. If negative percentages are used (e.g., \code{negative = 0.25}),
  then it needs to be distinguished between solved incorrectly and not attempted
  to solve (which should yield 0 percent).

  However, for multiple-choice answers the evaluation policy can either
  pertain to the answer pattern as a whole (which can be correct or wrong, see above)
  or it can employ a partial credit strategy. In the latter case, each
  selected correct choice will yield the fraction 1/ncorrect of points.
  When an incorrect choice is selected, it should lead to negative points.
  Five strategies are currently implemented: \code{"false"} uses
  1/nwrong while \code{"false2"} uses 1/max(nwrong, 2);
  \code{"true"} uses 1/ncorrect (so that each wrong selection cancels
  one correct selection); \code{"all"} uses 1 (so that a single wrong
  selection cancels all correct selections); and \code{"none"} uses 0
  (so that wrong selections have no effect at all). When aggregating the partial percentages,
  the overall points can become negative. By setting \code{negative}
  a lower bound can be set: \code{negative = TRUE} sets no bound while
  \code{negative = FALSE} sets the bound to zero. Any other numeric
  value could be set as well, e.g., \code{negative = 0.25}.

  The functions returned by \code{exams_eval} internally just distinguish
  between \code{num}, \code{string}, and \code{mchoice} answers.
  Thus, if evaluations for \code{schoice} or \code{cloze} exercises are
  required, these have to be built by appropriately reusing the building
  blocks for \code{num}/\code{string}/\code{mchoice}. For example,
  the components of \code{cloze} exercises have to be evaluated individually
  and then aggregated as desired. Or, if a distinction between \code{mchoice}
  and \code{schoice} regarding partial credits is needed, one evaluation
  has to be set up with \code{partial = TRUE} and the other with
  \code{partial = FALSE}. Different evaluations for different item types may be
  set as in: \code{exams2qti12(..., eval = eval1, schoice = list(eval = eval2))}. 
  Then \code{eval = eval1} is used as the default for all exercise types except 
  \code{schoice} where \code{eval = eval2} is used.

  Thus, \code{exams_eval} might not give the complete finished evaluation
  policy for an entire exam but supplies the most important building blocks
  for setting this up \dQuote{by hand}. Internally, \code{exams_eval} is also used by
  \code{\link[exams]{exams2moodle}}, \code{\link[exams]{exams2qti12}} and 
  \code{\link[exams]{exams2blackboard}} for writing the evaluation 
  specifications in the respective XML specifications.
}

\value{
  \code{exams_eval} returns a list with the input parameters
  \code{partial}, \code{negative}, and \code{rule} along with
  the following functions:
  \item{checkanswer}{function with arguments
    \code{(correct}, \code{answer}, and \code{tolerance = 0}.
    It checks whether \code{answer} (sufficiently) matches
    \code{correct} or not. It returns \code{1} for correct, \code{-1}
    for wrong and \code{0} for not attempted. In case of \code{partial = TRUE}, the
    functions returns a vector for multiple-choice questions.}
  \item{pointvec}{function with argument \code{correct = NULL}.
    It computes the vector of points for correct and wrong answers,
    respectively.}
  \item{pointsum}{function with arguments
    \code{(correct}, \code{answer}, and \code{tolerance = 0}.
    It computes the overall number of points.}
}

\seealso{
  \code{\link[exams]{exams2moodle}},
  \code{\link[exams]{exams2qti12}},
  \code{\link[exams]{exams2blackboard}}
}

\examples{
## binary evaluation policy with solutions being either correct
## or wrong: partial = FALSE, negative = FALSE
ee <- exams_eval(partial = FALSE, negative = FALSE)

## points that can be achieved are 0/1
ee$pointvec()

## checkanswer() returns 1 for correct, -1 for incorrect and 0 for missing answer
ee$checkanswer(1.23, 1.23)
ee$checkanswer(1.23, "1.23")
ee$checkanswer(1.23, "1,23")
ee$checkanswer(1.23, 1.24)
ee$checkanswer(1.23, 1.24, tolerance = 0.01)
ee$checkanswer(1.23, NA)
ee$checkanswer(1.23, NULL)
ee$checkanswer(1.23, "")

## similarly for logical (mchoice/schoice) answers
## (which allows either string or logical specification)
ee$checkanswer("10000", "10000")
ee$checkanswer(c(TRUE, FALSE, FALSE, FALSE, FALSE), c(TRUE, FALSE, FALSE, FALSE, FALSE))
ee$checkanswer(c(TRUE, FALSE, FALSE, FALSE, FALSE), "10000")
ee$checkanswer("10000", "01000")
ee$checkanswer("10000", "11000")

## and analogously for strings
ee$checkanswer("foo", "foo")
ee$checkanswer("foo", "bar")
ee$checkanswer("foo", "")

## obtain points achieved
ee$pointsum("10000", "10000")
ee$pointsum("10000", "01000")
ee$pointsum("10000", "00000")
ee$pointsum("10000", NA)

## ---------------------------------------------------------
## evaluation policy with -25% penalty for wrong answers
ee <- exams_eval(partial = FALSE, negative = -0.25)

## points that can be achieved are 1/-0.25 (or zero)
ee$pointvec()

## obtain points achieved
ee$pointsum("10000", "10000")
ee$pointsum("10000", "01000")
ee$pointsum("10000", "00000")
ee$pointsum("10000", NA)
ee$pointsum(1.23, 1.23)
ee$pointsum(1.23, 2.34)
ee$pointsum(1.23, NA)
ee$pointsum(1.23, 1.24)
ee$pointsum(1.23, 1.24, tolerance = 0.1)

## ---------------------------------------------------------
## default evaluation policy with partial points
## (but without negative points overall)
ee <- exams_eval()

## points that can be achieved are 1/3 (1/#true)
## or -1/2 (1/#false)
ee$pointvec("10101")

## obtain points achieved
ee$pointsum("10101", "10101")
ee$pointsum("10101", "10100")
ee$pointsum("10101", "11100")
ee$pointsum("10101", "01010")
ee$pointsum("10101", "00000")

## show individual answer check
ee$checkanswer("10101", "10101")
ee$checkanswer("10101", "10100")
ee$checkanswer("10101", "11100")
ee$checkanswer("10101", "01010")
ee$checkanswer("10101", "00000")

## numeric/string answers are not affected by partial=TRUE
ee$checkanswer(1.23, 1.23)
ee$pointsum(1.23, 1.23)
ee$checkanswer(1.23, 2.34)
ee$pointsum(1.23, 2.34)

## ---------------------------------------------------------
## evaluation policy with partial points
## (and with up to -25% negative points overall)
ee <- exams_eval(partial = TRUE, negative = -0.25)

## points that can be achieved are 1/3 (1/#true)
## or -1/2 (1/#false)
ee$pointvec("10101")

## obtain points achieved
ee$pointsum("10101", "10101")
ee$pointsum("10101", "01010")
ee$pointsum("10101", "00000")

## show individual answer check
ee$checkanswer("10101", "10101")
ee$checkanswer("10101", "10100")
ee$checkanswer("10101", "11100")
ee$checkanswer("10101", "01010")
ee$checkanswer("10101", "00000")

## numeric/string answers are not affected by partial=TRUE
ee$pointsum(1.23, 1.23)
ee$pointsum(1.23, 2.34)
}

\keyword{utilities}
