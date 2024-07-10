\name{testvision2exams}
\alias{testvision2exams}

\title{Convert TestVision XML Questions to R/exams Exercises}

\description{
  Function to convert TestVision questions of type 'invul (numeriek)', 'een-uit-meer', 'meer-uit-meer', and 'open'
  to R/exams exercises of type num, schoice, mchoice, and string, respectively.
}

\usage{
testvision2exams(x, markup = c("markdown", "latex"), rawHTML = FALSE, dir = ".",
  exshuffle = TRUE, name = NULL, shareStats = FALSE, css = FALSE)
}

\arguments{
  \item{x}{character. Path to a TestVision XML file.}
  \item{markup}{character. Markup language to convert to, i.e., \code{"markdown"} (default)
    corresponds to Rmd exercises and \code{"latex"} to Rnw exercises.}
  \item{rawHTML}{logical. If \code{rawHTML = TRUE} (and \code{markup = "markdown"}), instead of
  conversion via pandoc, the function merely obtains the raw HTML inside the exercise.}
  \item{dir}{character. Directory where the converted exercises should be saved.
    If set to \code{NULL} no files are saved.}
  \item{exshuffle}{logical or numeric. Meta-information tag used for single-choice and
    multiple-choice items.}
  \item{name}{character. Optional file name (without suffix) that should be used for the
    R/exams exercise file. The default is to use the name tag from the TestVision XML file
    (with some fix-ups, avoiding certain special characters). Alternatively, a name can also
    be supplied and will then be used for both the file name and the  \code{exname}
    meta-information tag, thus overwriting other name specified in the TestVision XML file.}
  \item{shareStats}{logical indicating whether the taxonomy of statistics exercises as specified
    by the \code{shareStats} project is incorporated in the TestVision XML file. If \code{shareStats = TRUE},
    the function searches for a text string as specified under 'Leerstof' ('Study Reference' in the
    English version) in TestVision and converts it into elements of metainformation, such as
    \code{exsection} and \code{exextra[Level]}.}
  \item{css}{logical indicating whether css files (typically stored in the zip-file's directory 'css') should be read. 
    This allows for copying style definitions for external images such width and height (for now this only 
    works when \code{markup = "markdown"}).}    
}

\details{
  The function aims to facilitate converting an existing TestVision question to an R/exams exercise.
  The resulting exercise file can subsequently be edited further, e.g., for making it dynamic.

  The function takes a TestVision XML question and converts it into an R/Markdown (Rmd) or
  R/LaTeX (Rnw) R/exams exercise. If \code{markup = "latex"} the HTML answers and questions from the TestVision XML
  are converted using pandoc (via \code{\link[rmarkdown]{pandoc_convert}}). Similarly, if \code{markup = "markdown"}
  pandoc converts the content to markdown, but when \code{rawHTML = TRUE} the function simply copies the HTML
  content from the TestVision XML (equations are stored within \code{<math> tags}). In the latter case, if conversion
  aims at creating dynamic exercises and displaying equations, it is advised to select and adjust the content in the
  \code{<annotation>} tag which is a raw latex specification of the equation. It is recommended to check the outcome
  in case certain HTML markup, or mathematical equations, etc., cannot be converted fully automatically.

  Currently only the TestVision XML exercise types 'invul (numeriek)', 'een-uit-meer', 'meer-uit-meer', and 'open'
  are properly supported. There is not yet support exercises of type cloze, in TestVision called 'invul
  (meervoudig)'. Hence, in case of cloze questions the execution of the function is stopped and a
  warning is issued.

  The TestVision XML may contain links to media content such as data files and images. In the zip-file
  that TestVision produces such files are typically stored in the directory 'mediafiles'; the function assumes
  that the TestVision XML file and this directory (and its subdirectories with full content) are unzipped in
  the same directory. If media files cannot be found a warning is issued.

  Since TestVision uses a single XML file for each question, it may be cumbersome to run the function for each
  question separately, and it advised to use iteration to convert questions in batch.
}

\value{
  A list of character vectors containing the R/exams exercise code.
  If \code{dir} is specified (default), this character vector is saved in a file
  (using \code{\link[base]{writeLines}}). In this case the list is returned invisibly. If \code{dir = NULL}
  no files are saved and the list is returned visibly.
}

\seealso{\code{\link{exams2testvision}}}

\examples{
if(requireNamespace("xml2")) {
## path to a TestVision XML file (provided in the package)
testvision_xml <- file.path(find.package("exams"), "xml", "testvision_question.xml")

## create a temporary directory for R/exams exercise files
dir.create(tdir <- tempfile())

## convert all exercises from the TestVision XML to R/Markdown files
ex_converted <- testvision2exams(testvision_xml, dir = tdir)
print(dir(tdir))

## additionally the source code of the Rmd file is also return invisible
## in 'ex_converted' and can be inspected manually, e.g., via writeLines()
names(ex_converted)
writeLines(ex_converted[[1]])

## clean up temporary directory
unlink(tdir)
}
}

\keyword{utilities}
