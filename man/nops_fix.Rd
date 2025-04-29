\name{nops_fix}
\alias{nops_fix}

\title{Fix Data from Scanned NOPS Exams}

\description{
  Fix scanned NOPS exams produced with \code{\link[exams]{nops_scan}}
  and update the corresponding ZIP file.
}

\usage{
  nops_fix(scans = dir(pattern = "^nops_scan_[[:digit:]]*\\\\.zip$"),
    exam = NULL, id = NULL, field = NULL, answer = NULL, check = NULL, display = NULL,
    string = NULL, language = "en")
}

\arguments{
  \item{scans}{character. File name of the ZIP file with scanning results
    (containing Daten.txt and PNG files) as produced by
    \code{\link[exams]{nops_scan}}.}
  \item{exam}{integer. Rows number from the scanned data which should (potentially)
    be fixed (default: all rows).}
  \item{id}{integer or character. Either a vector with full 11-digit exam IDs
    for the exams to be checked. Or the last five digits of the full 11-digit ID
    (where leading zeros can be omitted).}
  \item{field}{character indicating which field(s) from the scanned data should
    be fixed. One or more of \code{"type"} (3-digit exam sheet type), \code{"id"}
    (11-digit exam ID), \code{"registration"} (registration ID, can be between 7 and
    10 digits), \code{"answers"} (individual answer checkboxes). By default all fields
    with invalid entries have to be fixed.}
  \item{answer}{integer indicating which answer checkboxes should (potentially) be
    fixed if \code{field} contains \code{"answers"} (default: all answers).}
  \item{check}{character indicating additional check conditions for the answers:
    If set to \code{"missing"} only the missing answers are considered. For
    \code{"schoice"} answers with more than one checked box are considered. For
    \code{"mchoice"} answers with all boxes checked are considered.}
  \item{display}{character indicating how the scanned image should be displayed:
    \code{"plot"} (display scanned excerpt in R plot),
    \code{"browser"} (display full scanned image in browser), and
    \code{"interactive"} (display full scanned image along with interactive HTML form in browser).
    By default, \code{"plot"} is used if the \pkg{png} package is available
    unless errors occurred during scanning. In the latter case \code{"interactive"}
    is used because typically the errors occur when scanned images cannot be
    read at all.}
  \item{string}{logical. Is the ZIP file from scanning marked string
    exercises (rather than single/multiple choice exercises)? The default is
    \code{TRUE} if the \code{file} name starts with \code{nops_string_scan_}
    and \code{FALSE} otherwise.}
  \item{language}{character. The language specification from \code{\link[exams]{nops_language}}
    for the annotation in the interactive HTML form (in case \code{display = "interactive"}
    is used).}
}

\details{
  \code{nops_fix} is a companion function for \code{\link[exams]{exams2nops}}
  and \code{\link[exams]{nops_scan}}. To fix problems that potentially occurred
  when extracting the exam information from the scanned PDF/PNG images,
  \code{nops_fix} can iterate through the scanned data and interactively prompt
  for fields that (potentially) need to be fixed. Typical cases are when errors
  occurred in entering the registration ID or when the entire exam sheet could
  not be read correctly. See below for a couple of typical application examples.
  
  For each specified field the user is prompted for a potential update. By just
  pressing ENTER the current value of the field is preserved. If an update is not
  a valid specification of the field, the prompt is repeated (along with some additional
  instructions) until a valid specification is entered by the user.
  
  The type of sheet needs to be a 3-digit number, the exam ID needs to be an 11-digit
  number, and the registration ID needs to be a number with 7-10 digits.

  All answer fields eventually store the multiple-choice answers with 0/1 indicators
  of length 5 (even if the question actually used fewer answer alternatives).
  The following input formats are accepted:
  \itemize{
  \item 0/1 indicators of length up to 5. If less then 5 digits are specified, the remaining digits are filled with 0s.
  \item Letters from a to e indicating the box(es) that have been checked.
  \item Integers from 1 to 5 indicating which single box has been checked.
  \item No checked box can be indicated by \code{"0"}, \code{"-"}, or \code{" "}.
  }

  Instead of iterating through the fields sequentially via prompts in R, it is
  also possible to use \code{display = "interactive"} and enter all scan information
  in an interactive HTML form that eventually can pass back all results for an
  entire sheet. This is typically useful if the scan image was rotated too much
  to be read at all. In this case users should complete/correct the scanned data
  in the HTML form and eventually click the OK button in the bottom right corner.
  This copies all the data entered into the clipboard (in a JSON string) and R
  will either attempt to read it from there or users can paste it into the R prompt.
  On Windows, reading from the clipboard should always work, while on other platforms
  the R package \code{clipr} would need to be installed to read from the clipboard
  reliably. In any case, users should make sure that the clipboard is not modified
  before reading or pasting the clipboard again in R.
}

\value{
  Data frame with one line per scanned file is returned invisibly. The output contains the
  following columns: file name, sheet ID (11 digits), scrambling (2 digits),
  type of sheet (3 digits, coding the number of questions rounded up to steps of 5
  and the length of the registration number),
  0/1 indicator whether the replacement sheet was used,
  registration number (7-10 digits),
  45 multiple choice answers of length 5 (all 00000 if unused).
}

\seealso{\code{\link[exams]{exams2nops}}, \code{\link[exams]{nops_scan}}, \code{\link[exams]{nops_eval}}}

\keyword{utilities}

\examples{
## typical application cases (not run), all assume that there is
## a single nops_scan_*.zip file in the current working directory

## fix all rows/fields that can be detected as incorrect
## nops_fix()

## fix answer 6 in exam 3
## nops_fix(exam = 3, answer = 6)

## fix all answers in exam 3
## nops_fix(exam = 3, field = "answers")  

## fix all fields exam 3
## nops_fix(exam = 3, field = c("type", "id", "registration", "answers"))

## fix answer 6 in exam with id 23112900129
## nops_fix(id = "23112900129", answer = 6)
## nops_fix(id = 129, answer = 6)

## fix all answers in questions 1 to 8 where more than one box was checked
## nops_fix(answer = 1:8, check = "schoice")
}
