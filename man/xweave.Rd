\name{xweave}
\alias{xweave}

\title{Wrapper Function for Weaving Either Rnw or Rmd Exercises}

\description{
  Simple wrapper function that either calls \code{\link[utils]{Sweave}}
  for Rnw exercises or \code{\link[knitr]{knit}} for Rmd exercises.
}

\usage{
  xweave(file, quiet = TRUE, encoding = NULL, engine = NULL, envir = new.env(),
    pdf = TRUE, png = FALSE, svg = FALSE, height = 6, width = 6, resolution = 100,
    highlight = FALSE, \dots)
}

\arguments{
  \item{file, quiet, encoding}{arguments passed to \code{\link[utils]{Sweave}}
    or \code{\link[knitr]{knit}}, respectively.}
  \item{engine}{character indicating whether \code{"Sweave"} (default) or
    \code{"knitr"} should be used for rendering Rnw exercises.}
  \item{envir}{argument passed to \code{\link[knitr]{knit}}. By default,
    or if \code{envir = NULL} a \code{new.env()} is created for each
    \code{xweave()} call.}
  \item{pdf, png, svg, height, width, resolution, highlight, \dots}{arguments passed to
    \code{\link[utils]{Sweave}} or \code{\link[knitr]{opts_chunk}}, respectively.
    In the latter case: \code{pdf}/\code{png}/\code{svg} are mapped to \code{dev};
    \code{height}/\code{width} are mapped to \code{fig.height}/\code{fig.width};
    and \code{resolution} is mapped to \code{dpi}. \code{highlight} is ignored for
    \code{\link[utils]{Sweave}}.}
}

\details{
  Depending on whether \code{file} has an \code{.Rnw} or \code{.Rmd} suffix,
  either \code{\link[utils]{Sweave}} or \code{\link[knitr]{knit}} is called for
  weaving the file by default. Rnw exercises can optionally also be weaved by
  \code{\link[knitr]{knit}} by setting \code{engine = "knitr"}.
  
  If \code{png = TRUE} or \code{svg = TRUE} when calling \code{Sweave}, then
  the resulting includegraphics statements are supplemented with the \code{.png}
  or \code{.svg} suffix of the corresponding graphics. For \code{svg} a simple
  graphics device hook \code{.xweave_svg_grdevice} is provided on-the-fly
  for plug-in into \code{Sweave}.
}

\seealso{
  \code{\link[utils]{Sweave}},
  \code{\link[knitr]{knit}}
}

\keyword{utilities}
