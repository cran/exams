\name{moodle2exams}
\alias{moodle2exams}

\title{Convert Moodle XML Quizzes to R/exams Exercises}

\description{
  Function to convert Moodle quiz exercises of type numerical, multichoice, shortanswer, and essay
  to R/exams exercises of type num, schoice/mchoice, and string.
}

\usage{
moodle2exams(x, markup = c("markdown", "latex"), dir = ".",
  exshuffle = TRUE, names = NULL)
}

\arguments{
  \item{x}{character. Path to a Moodle XML file. If a character vector with more than
    one element is provided, it is assumed to be an XML file as read with \code{\link[base]{readLines}}.}
  \item{markup}{character. Markup language to convert to, i.e., \code{"markdown"} (default)
    corresponds to Rmd exercises and \code{"latex"} to Rnw exercises.}
  \item{dir}{character. Directory where the converted exercises should be saved.
    If set to \code{NULL} no files are saved.}
  \item{exshuffle}{logical or numeric. Meta-information tag used for single-choice and
    multiple-choice items.}
  \item{names}{character. Optional file names (without suffix) that should be used for the
    R/exams exercise files. The default is to use the name tags from the Moodle XML file
    (with some fix-ups, avoiding certain special characters). Alternatively, names can also
    be supplied and will then be used for both the file names and the  \code{exname}
    meta-information tag, thus overwriting other names specified in the Moodle XML file.}
}

\details{
  The function aims to facilitate converting an existing Moodle question pool to R/exams exercises.
  The resulting exercise files can subsequently be edited further, e.g., for making them dynamic.

  The function takes a Moodle XML quiz and converts each exercise into a R/Markdown (Rmd) or
  R/LaTeX (Rnw) R/exams exercise. The HTML answers and questions from the Moodle XML are converted using
  pandoc (via \code{\link[rmarkdown]{pandoc_convert}}). It is recommended to check the outcome
  in case certain HTML markup, or mathematical equations, etc., cannot be converted fully automatically.
  Currently only the Moodle XML exercise types numerical, multichoice, shortanswer, and essay 
  are properly supported.

  There is limited support for cloze exercises, but the resulting meta-information needed by
  R/exams will be incorrect. Hence, a warning is issued when converting cloze exercises.
}

\value{
  A list of character vectors containing the R/exams exercise code with one element per exercise.
  If \code{dir} is specified (default), these character vectors are saved in one file per exercise
  (using \code{\link[base]{writeLines}}). In this case the list is returned invisibly. If \code{dir = NULL}
  no files are saved and the list is returned visibly.
}

\seealso{\code{\link{exams2moodle}}}

\examples{
if(requireNamespace("xml2")) {
## path to a Moodle XML quiz file (provided in the package)
moodle_xml <- file.path(find.package("exams"), "xml", "moodlequiz.xml")

## create a temporary directory for R/exams exercise files
dir.create(tdir <- tempfile())

## convert all exercises from the Moodle XML to R/Markdown files
ex_converted <- moodle2exams(moodle_xml, dir = tdir)
print(dir(tdir))

## additionally the source code of the Rmd files is also return invisible
## in 'ex_converted' and can be inspected manually, e.g., via writeLines()
names(ex_converted)
writeLines(ex_converted[[1]])

## clean up temporary directory
unlink(tdir)
}
}

\keyword{utilities}
