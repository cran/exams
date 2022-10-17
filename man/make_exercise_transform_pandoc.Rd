\name{make_exercise_transform_pandoc}
\alias{make_exercise_transform_pandoc}

\title{Transform Exercises via Pandoc}

\description{
  Generate an exercise transformer function based on
  \code{\link[rmarkdown]{pandoc_convert}}.
}

\usage{
  make_exercise_transform_pandoc(to = "latex", base64 = to != "latex",
    attachfile = FALSE, \dots)
}

\arguments{
  \item{to}{character. Specification of the output text format, typically
    \code{"latex"}, \code{"html"}, or \code{"markdown"}.}
  \item{base64}{logical. Should supplementary files be embedded using Base 64 coding?
    Argument \code{base64} may also be a character vector of file suffixes that should be encoded, e.g.
    \code{base64 = c("png", "rda")} will only encode PNG images and binary \code{.rda} files.
    If set to \code{NULL} only image files will be encoded.}
  \item{attachfile}{logical. Should \code{attachfile} rather than \code{url} be used
    in LaTeX, e.g., for embedding data files in PDF.}
  \item{\dots}{arguments to be passed on to \code{\link[rmarkdown]{pandoc_convert}}.}
}

\details{
  The generator function \code{make_exercise_transform_pandoc} returns a function
  that can be used for the \code{transform} steps in \code{\link[exams]{xexams}}.
  It is a wrapper to \code{\link[rmarkdown]{pandoc_convert}} from \pkg{rmarkdown}
  but adds a couple of convenience features that are typically needed in R/exams
  exercises. Supplementary files can be handled by Base 64 encoding (often used
  for HTML output) or via attachfile in LaTeX (sometimes useful for PDF output).
  Some additional LaTeX commands and environments are supported, e.g., Sweave environments
  or negated logical comparison symbols. Finally, some default options from
  pandoc are changed, namely the defaults \code{--wrap=preserve} (instead of
  \code{--wrap=auto}) and \code{--columns=99999} (instead of \code{--columns=72}).
}

\value{
  \code{make_exercise_transform_pandoc} returns a function that is suitable for being
  supplied as \code{driver$transform} to \code{\link[exams]{xexams}}.
}

\seealso{
  \code{\link[exams]{xexams}},
  \code{\link[exams]{make_exercise_transform_html}},
  \code{\link[rmarkdown]{pandoc_convert}}
}

\examples{
## load package and enforce par(ask = FALSE)
options(device.ask.default = FALSE)

## default transformation to LaTeX output
textrafo <- make_exercise_transform_pandoc()

## custom transformation to Markdown output without Base 64 files
mdtrafo <- make_exercise_transform_pandoc(to = "markdown", base64 = FALSE)

## read "lm" exercise via xexams, i.e., without transformation
## Rmd yields Markdown, Rnw yields LaTeX
lm_md <- xexams("lm.Rmd")[[1]][[1]]
lm_tex <- xexams("lm.Rnw")[[1]][[1]]

## original Markdown and transformed LaTeX
writeLines(lm_md$question)
writeLines(textrafo(lm_md)$question)

## original LaTeX and transformed Markdown
writeLines(lm_tex$question)
writeLines(mdtrafo(lm_tex)$question)
}

\keyword{utilities}
