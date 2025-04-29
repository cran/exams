\name{stresstest_exercise}
\alias{stresstest_exercise}
\alias{stresstest}
\alias{plot.stress}
\alias{summary.stress}

\title{
  Stress Testing Exercises
}

\description{
  In order to check the correct behavior of an exercise it is compiled several times.
  In each iteration the objects created by the exercise are collected and its values
  can be inspected.
}

\usage{
stresstest_exercise(file, n = 100, verbose = TRUE, seeds = NULL,
  stop_on_error = length(as.character(unlist(file))) < 2, timeout = NULL,
  maxit = getOption("num_to_schoice_maxit", -10000L), ...)

\method{plot}{stress}(x, type = c("overview", "solution", "rank", "runtime", "warnings", "error"),
  threshold = NULL, variables = NULL, spar = TRUE, ask = TRUE, ...)
\method{summary}{stress}(object, ...)
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
    the seed, the file name and the error message. Useful when testing
    a number of exercises.}
  \item{timeout}{\code{NULL} or numeric vector. See section 'Timeout' for details.}
  \item{maxit}{numeric. Maximum number of iterations passed on to \code{\link{num_to_schoice}},
    in case that is used for finding a suitable set of wrong solutions for a question list.
    Exceeding \code{abs(maxit)} yields either a warning (if \code{maxit > 0}) or an
    error (if \code{maxit < 0}).}
  \item{x,object}{An object returned from function \code{stresstest_exercise}.}
  \item{type}{character. \code{type == "overview"} plots the basic overview, i.e, the runtimes,
    numeric solutions, position/number/rank of correct solution(s), if available.
    If \code{type == "solution"}, the numeric solutions
    are plotted against all input parameters stored in the \code{objects} element of \code{x}.
    \code{type == "rank"} draws \code{\link[graphics]{spineplot}}s of ranks
    vs. input parameters stored in \code{objects}. \code{type == "runtime"} plots the compiling
    runtimes vs. \code{objects}. Type \code{"warning"} and \code{"error"} plots how errors
    have been logged (if any) and how often and where warnings were thrown.}
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

\section{Timeout}{
  Stresstest allows to set execution timeouts. By default, \code{timeout} is set \code{NULL}
  (no limits). If \code{timeout} is a numeric vector of positive values:

  \itemize{
    \item If \code{timeout} is of length \code{1}: Used to limit execution time (cpu and elapsed time)
      per randomization to \code{timeout} seconds.
    \item If \code{timeout} is of length \code{2}: Specifies cpu time (\code{timeout[1L]}) and
      per randomization time elapsed (\code{timeout[2L]}).
    \item If \code{timeout} is of length \code{3}: The first two elements are used to specify
      cpu/time elapsed per randomization (see above), the thirs element specifies the
      overall time elapsed for all randomizations.
  }

  The thirs element (\code{timeout[3]}) is checked after each iteration. If a question
  stalls (infinite loop) this can still cause \code{stresstest()} to stall as well.
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

  If \code{stop_on_error = FALSE} the function also records warnings and errors
  thrown. Depending on whether or not warnings or errors have been recorded, the
  following elements will be added to the object returned.
  \item{count}{Named vector with total number of warnings and errors recorded.
      There can be more warnings than randomizations, if exercises throw multiple
      warnings.}
  \item{warnings}{Only added if at least one warning was recorded, character vector.
      Warning messages and number of occurrences, or \code{NA} if no warnings were raised.}
  \item{error}{Only added if at least one error was recorded, character vector.
      Error message or \code{NA} if no error was thrown.}
}

\seealso{
  \code{\link{xexams}}, \code{\link{num_to_schoice}}
}

\examples{
\dontrun{## Stress testing.
t1 <- stresstest_exercise("tstat.Rmd", n = 100)
t2 <- stresstest_exercise("tstat2.Rmd", n = 100)

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
  "boxplots.Rmd",
  c("tstat.Rmd", "ttest.Rmd", "confint.Rmd"),
  c("regression.Rmd", "anova.Rmd"),
  "scatterplot.Rmd",
  "relfreq.Rmd"
)
t3 <- stresstest_exercise(files, n = 100)
plot(t3)
}
}

\keyword{utilities}
