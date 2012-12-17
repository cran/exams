\name{tex2image}
\alias{tex2image}

\title{Transforming LaTeX Code Using ImageMagick}

\description{
  Transformation of LaTeX code into an image by compiling
  to PDF and then transforming to PNG (by default) via
  ImageMagick's \code{convert} command.
}

\usage{
tex2image(tex, format = "png", width = 6, pt = 12, density = 350,
  dir = NULL, tdir = NULL, idir = NULL,
  width.border = 0L, col.border = "white", resize = 650, shave = 4,
  packages = c("amsmath", "amssymb", "amsfonts"),
  header, header2 = NULL, Sweave = TRUE, show = TRUE,
  name = "tex2image")
}

\arguments{
  \item{tex}{character vector or list of character vectors. Each character
    vector is either the name of a LaTeX file or a vector containing LaTeX
    code directly.}
  \item{format}{character. Suffix for the type of graphic to convert to.}
  \item{width}{numeric. Width of the text in inch.}
  \item{pt}{numeric. Pointsize of the text.}
  \item{density}{numeric. Resolution density of the image.}
  \item{dir}{character specifying the output directory.}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempdir}}.}
  \item{idir}{character specifying the path additional LaTeX inputs
    required.}
  \item{width.border}{numeric. Width of the framebox border.}
  \item{col.border}{character. Color of framebox border.}
  \item{resize}{numeric. Number of pixels for resizing the image.}
  \item{shave}{numeric. Number of pixels to shave the framebox from the edges.}
  \item{packages}{character. Names of LaTeX packages to be included.}
  \item{header}{character. LaTeX code to be included in the header of the
    LaTeX file before the beginning of the document. By default the parindent
    is set to 0 and sans serif fonts (phv) are used for both text and math.}
  \item{header2}{character. LaTeX code to be included in the header of the
    LaTeX file after the beginning of the document. }
  \item{Sweave}{logical. Should the LaTeX package Sweave.sty be included
    in the header?}
  \item{show}{logical. Show the resulting image(s) using \code{\link[utils]{browseURL}}.}
  \item{name}{character. Base name of the image file.}
}

\details{
  \code{tex2image} converts LaTeX code to image files, e.g., for inclusion
  in web pages. It proceeds in the following steps: (1) LaTeX code is
  embedded into a suitable .tex file. (2) This is compiled to PDF using
  \code{\link[tools]{texi2dvi}}. (3) The PDF is converted to an image
  file (PNG by default) using ImageMagick's \code{convert} function.
  
  To enable suitable clipping and cropping, each vector of LaTeX code
  has to fit onto a single page (A4). It is automatically embedded into a
  framebox that is removed in the conversion to image.
  
  If \code{tex} is a list of LaTeX chunks, then these are compiled to
  separate pages of a single PDF in a single LaTeX run. Each page is
  subsequently converted to a separate image.
  
  For conversion to images, ImageMagick's \code{convert} command is
  assumed to be installed and available in the search path.
}

\value{
  Character vector with path(s) to image(s) generated from the LaTeX
  code.
}

\seealso{
  \code{\link[tools]{texi2dvi}}
}

\examples{
\dontrun{
tex <- c("This is \\\\textbf{bold} and this \\\\textit{italic}.",
  "Points on the unit circle: $x^2 + y^2 = 1$.")
tex2image(tex)
tex2image(tex, header = NULL)
}
}

\keyword{utilities}
