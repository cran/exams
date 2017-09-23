\name{tex2image}
\alias{tex2image}

\title{Transforming LaTeX Code Using ImageMagick or pdf2svg}

\description{
  Transformation of LaTeX code into an image by compiling
  to PDF and then transforming to PNG (by default) via
  ImageMagick's \code{convert} command or to SVG via \code{pdf2svg}.
}

\usage{
tex2image(tex, format = "png", width = NULL, pt = 12, density = 350,
  dir = NULL, tdir = NULL, idir = NULL,
  width.border = 0L, col.border = "white", resize = 650,
  packages = c("amsmath", "amssymb", "amsfonts"),
  header, header2 = NULL, tikz = NULL, Sweave = TRUE, show = FALSE,
  name = "tex2image")
}

\arguments{
  \item{tex}{character vector or list of character vectors. Each character
    vector is either the name of a LaTeX file or a vector containing LaTeX
    code directly.}
  \item{format}{character. Suffix for the type of graphic to convert to.}
  \item{width}{numeric. Width of the text in inch. If \code{NULL} (or \code{0}),
    the width is chosen to fit the image in \code{tex}.}
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
  \item{packages}{character. Names of LaTeX packages to be included.}
  \item{header}{character. LaTeX code to be included in the header of the
    LaTeX file before the beginning of the document. By default the parindent
    is set to 0 and sans serif fonts (phv) are used for both text and math.}
  \item{header2}{character. LaTeX code to be included in the header of the
    LaTeX file after the beginning of the document. }
  \item{tikz}{character. Options to be passed to \code{\\usetikzlibrary{}}. If set, the tikz package
    is loaded per default.}
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
  file. By default, conversion is to PNG using \R package \pkg{magick} functionalities
  or alternatively to SVG via \code{pdfcrop} followed by \code{pdf2svg}.

  The LaTeX code is fit into the \code{standalone} document class using the
  \code{tikzpicture} environment as a default.
  
  If \code{tex} is a list of LaTeX chunks, then these are compiled to
  separate pages of a single PDF in a single LaTeX run. Each page is
  subsequently converted to a separate image.
  
  In case of SVG output, the respective image manipulation tools, i.e., 
  \code{pdfcrop}/\code{pdf2svg}, are assumed to be installed and available
  in the search path.
}

\value{
  Character vector with path(s) to image(s) generated from the LaTeX code.
}

\seealso{
  \code{\link[tools]{texi2dvi}}
}

\examples{
\dontrun{
## some simple LaTeX
tex <- c("This is \\\\textbf{bold} and this \\\\textit{italic}.",
  "Points on the unit circle: $x^2 + y^2 = 1$.")

## default settings: PNG with sans serif fonts
tex2image(tex, show = interactive())

## with fixed widths
tex2image(tex, width = 6, show = interactive())
tex2image(tex, width = 2, show = interactive())

## switch off header (-> LaTeX uses its standard serif fonts)
tex2image(tex, header = NULL, show = interactive())

## SVG output (system requirements: pdfcrop & pdf2svg)
tex2image(tex, format = "svg", show = TRUE)
}
}

\keyword{utilities}
