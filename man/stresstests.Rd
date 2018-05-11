\name{stresstest_exercise}
\alias{stresstest_exercise}
\alias{stresstest}
\alias{plot.stress}

\title{
  Stress Testing Exercises
}

\description{
  In order to check the correct behavior of an exercise it is compiled several times.
  In each iteration the objects created by the exercise are collected and its values
  can be inspected.
}

\usage{
## Stresstest function.
stresstest_exercise(file, n = 100, verbose = TRUE, seeds = NULL,
  stop_on_error = length(as.character(unlist(file))) < 2,
  ...)

## Plotting stresstest results.
\method{plot}{stress}(x, type = c("overview", "solution", "rank", "runtime"),
  threshold = NULL, variables = NULL,
  spar = TRUE, ask = TRUE, ...)
}

\arguments{
  \item{file}{character. A specification of an exercise file. If multiple files
    should be tested, argument \code{file} can also be a vector, matrix or list of
    files. The latter case sets argument \code{plot = FALSE}.}
  \item{n}{integer. The number of replications.}
  \item{verbose}{logical. Should the seeds used for compiling the exercise
    be prompted on the console.}
  \item{seeds}{The seeds that should be used when compiling the exercise. The
    default is \code{seeds = 1:n}.}
  \item{stop_on_error}{logical. Should the function stop on error or return
    the the seed, the file name and the error message. Useful when testing
    a number of exercises.}
  \item{x}{An object returned from function \code{stresstest_exercise}.}
  \item{type}{character. \code{type == "overview"} plots the basic overview, i.e, the runtimes,
    numeric solutions, position/number/rank of correct solution(s), if available.
    If \code{type == "solution"}, the numeric solutions
    are plotted against all input parameters stored in the \code{objects} element of \code{x}.
    \code{type == "rank"} draws \code{\link[graphics]{spineplot}}s of ranks
    vs. input parameters stored in \code{objects}. \code{type == "runtime"} plots the compiling
    runtimes vs. \code{objects}.}
  \item{threshold}{numeric. Can be used to set a threshold, e.g., for numeric solutions a factor
    is created, \code{factor(solution <= threshold)}, that is used on the y-axis of a
    \code{\link[graphics]{spineplot}}.}
  \item{variables}{character. The variables that should be used from the \code{objects} for
    plotting.}
  \item{spar}{logical. Should graphical parameters be set or not.}
  \item{ask}{logical. For multiple plots, should the user be asked to hit the return key to see the
    next plot.}
  \item{\dots}{Arguments passed to \code{\link{xexams}}.}
}

\details{
  In order to check the correct behavior of an exercise function \code{stresstest_exercise} runs
  \code{\link{xexams}} \code{n} times using different seeds. If an error occurs when compiling,
  the error can be reproduced by setting the seed that is prompted at the console and create
  the exercise again, e.g., with \code{\link{exams2html}}. This way errors can be detected
  systematically.

  All objects with length 1, which are created by the exercise, are collected in a data frame.
  These objects are assumed to be input parameters that control the output of the exercise. This
  can be used to detect certain input values that, e.g., lead to very long run times, or
  drive the number of correct answers in multiple choice exercises, etc.

  For single and multiple choice type question the position(s) of the correct solution(s) is
  returned. For single choice questions that are created from a numeric version, e.g., using
  function \code{\link{num_to_schoice}} the answers are again converted to numeric and the rank
  of the correct solution is reported. The rank is sometimes heavily driven by some input
  parameters, e.g., the correct solution is always the largest or the smallest.
  For non-numeric choice questions, the rank is based on the lexicographical order of
  the answerlist.
}

\value{
  Function \code{stresstest_exercise} returns an object of class \code{"stress"} (a named list) with
  the following elements:
  \item{seeds}{The seeds that where used.}
  \item{runtime}{Compiling times for each iteration.}
  \item{objects}{A data frame of length 1 objects that are created by the exercise.}
  \item{solution}{The numeric solution, availability is depending on the type of exercise.}
  \item{position}{A matrix indicating the position of correct solutions.}
  \item{rank}{The rank of the correct solution, only available for choice exercises.}
  \item{ntrue}{The number of correct answers in multiple choice type questions.}
}

\seealso{
  \code{\link{xexams}}, \code{\link{num_to_schoice}}
}

\examples{
\dontrun{## Stress testing.
t1 <- stresstest_exercise("tstat.Rnw", n = 100)
t2 <- stresstest_exercise("tstat2.Rnw", n = 100)

## Plotting.
plot(t1, type = "overview")
plot(t1, type = "solution")
plot(t1, type = "solution", threshold = 30)
plot(t2, type = "rank")
plot(t2, type = "runtime")

## For custom inspection, object can be
## transformed to a data.frame.
head(as.data.frame(t2))

## Multiple testing.
files <- list(
  "boxplots",
  c("tstat", "ttest", "confint"),
  c("regression", "anova"),
  "scatterplot",
  "relfreq"
)
t3 <- stresstest_exercise(files, n = 100)
plot(t3)
}
}

\keyword{utilities}
