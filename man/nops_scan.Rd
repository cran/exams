\name{nops_scan}
\alias{nops_scan}

\title{Read Scanned NOPS Exams}

\description{
  Read scanned NOPS exams produced with \code{\link[exams]{exams2nops}}.
}

\usage{
  nops_scan(
    images = dir(pattern = "\\\.PNG$|\\\.png$|\\\.PDF|\\\.pdf$",
      path = dir, full.names = TRUE),
    file = NULL, dir = ".",
    verbose = TRUE, rotate = FALSE, cores = NULL, n = NULL,
    density = 300,
    size = 0.03, threshold = c(0.04, 0.42), trim = 0.3, minrot = 0.002,
    string = FALSE)
}

\arguments{
  \item{images}{character. Names of the PDF/PNG images containing the scanned exams.
    By default all PDF/PNG images in the current working directory are used.}
  \item{file}{character or logical. Optional file name for the output ZIP archive
    containing the PNG images and the scan results. If \code{file = FALSE} no
    ZIP archive is created. By default a suitable name using the current time/date
    is used.}
  \item{dir}{character. Directory in which the ZIP \code{file} should be created.
    By default the current working directory.}
  \item{verbose}{logical. Should progress information be displayed?}
  \item{rotate}{logical. Should the input PDF/PNG images be rotated by 180 degrees
    first?}
  \item{cores}{numeric. If set to an integer \code{\link[parallel]{mclapply}} 
    (or \code{\link[parallel]{parLapply}} on Windows) is
    called internally using the desired number of \code{cores} to read the scanned
    exams in parallel.}
  \item{n}{numeric. The number of answer fields to read (in multiples of 5),
    i.e., 5, 10, \dots, 45. By default taken from the type field.}
  \item{density}{numeric. Resolution used in the conversion of PDF images to
    PNG. This requires ImageMagick's convert to be available on the system.}
  \item{size}{numeric. Size of the boxes containing the check marks relative to the
    image height. Should typically be between 0.025 and 0.045.}
  \item{threshold}{numeric. Vector of thresholds for the gray levels in the check
    mark boxes. If the average gray level is between the gray levels, the box is
    checked. If it is above the second threshold, some heuristic is employed for
    judging whether the box contains a cross or not.}
  \item{trim}{numeric. Amount of trimming to shave the borders of the boxes
    before determining the gray level within the check boxes. Should usually be
    between 0.25 (default up to version 2.3-1) and 0.35.}
  \item{minrot}{numeric. Minimum angle for rotating images, i.e., images with
    a lower angle are considered to be ok.}
  \item{string}{logical. Are the files to be scanned manually marked string
    exercises (rather than single/multiple choice exercises)?}
}

\details{
  \code{nops_scan} is a companion function for \code{\link[exams]{exams2nops}}.
  Exams generated with \code{exams2nops} can be printed and the filled out
  answer sheet can be scanned. Then, \code{nops_scan} can be employed to read
  the information in the scanned PDF/PNG images. The results are one text line
  per image containing the information in a very simple space-separated format.
  
  If \code{images} only contains PNG files, then the R function \code{\link[png]{readPNG}}
  is sufficient for reading the images into R. If \code{images} contains PDF files,
  these need to be converted to PNG first, which is carried out using the R packages
  \pkg{qpdf} and \pkg{magick}. (Instead of using these R packages it is also possible
  to use system calls to PDFTk, GhostScript, and ImageMagick's convert, which was
  the only implementation up to R/exams version 2.4-0.)
  
  Tutorial for NOPS workflow: \url{https://www.R-exams.org/tutorials/exams2nops/}.
  
  Practical recommendations:
  
  The scanned images produced by scanners or copying
  machines typically become smaller in size if the images are read in just black/white
  (or grayscale). This may sometimes even improve the reliability of reading the
  images afterwards. Also make sure that the resulting images have a good contrast
  and are neither too light nor too dark because too many or too little dark pixels
  increase the probability of incorrect scanning results.
  
  Make sure that the sheets are fed firmly into the scanner, e.g., by tightening
  the tracks of the feeder.
  
  The printed exams are often stapled in the top left corner which has to be unhinged
  somehow by the exam participants. Although this may damage the exam sheet, this is
  usually no problem for scanning it. However, the copying machine's sheet feeder
  may work better if the sheets are turned upside down (so that the damaged corner
  is not fed first into the machine). This often improves the scanning results
  considerably and can be accomodated by setting \code{rotate = TRUE} in \code{nops_scan}.
}

\value{
  A character vector with one element per scanned file (returned invisily if
  written to an output ZIP archive). The output contains the following space-separated
  information: file name, sheet ID (11 digits), scrambling (2 digits),
  type of sheet (3 digits, coding the number of questions
  and the length of the registration number),
  0/1 indicator whether the replacement sheet was used,
  registration number (7-10 digits),
  45 multiple choice answers of length 5 (all 00000 if unused).
}

\seealso{\code{\link[exams]{exams2nops}}, \code{\link[exams]{nops_eval}}}

\keyword{utilities}

\examples{
if(requireNamespace("png")) {
## scanned example images stored in exams package
img <- dir(system.file("nops", package = "exams"), pattern = "nops_scan",
  full.names = TRUE)

## read content
res <- nops_scan(img, file = FALSE)
writeLines(res)
}
}
