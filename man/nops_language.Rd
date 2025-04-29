\name{nops_language}
\alias{nops_language}

\title{Read NOPS Language Specifciation}

\description{
  Read a NOPS language specification from a DCF file and optionally
  convert the language text to HTML.
}

\usage{
  nops_language(file, converter = c("none", "tth", "pandoc"), \dots)
}

\arguments{
  \item{file}{character. Path to a DCF file with a language specification.
    Currently, the package ships with a number of languages such es English (\code{"en"}),
    Spanish (\code{"es"}), etc. See below for further details.}
  \item{converter}{character. The HTML converter to be used for the language
    text elements. Can be \code{"none"}, \code{"tth"}, or \code{"pandoc"}.}
  \item{\dots}{further arguments passed to the \code{converter} (if any), e.g.,
    for obtaining output formats other than HTML.}
}

\details{
  The NOPS exams infrastructure is internationalized and can be customized through
  DCF files (borrowing the format from Debian configuration files). For a detailed
  description see: \url{https://www.R-exams.org/tutorials/nops_language/}.
  The DCF files typically either contain special characters as LaTeX commands or
  in UTF-8 encoding. To handle the former case, a \code{converter} can be applied
  to convert the language texts to HTML.
}

\value{
  A list with all language components provided by the DCF file.
}

\seealso{\code{\link[exams]{exams2nops}}, \code{\link[exams]{nops_eval}}}

\examples{
## English
en <- nops_language("en")
names(en)
en$NoChanges

## French (LaTeX vs. HTML)
nops_language("fr", converter = "none")$NoChanges
nops_language("fr", converter = "tth")$NoChanges

## French (HTML or Markdown via pandoc)
nops_language("fr", converter = "pandoc")$NoChanges
nops_language("fr", converter = "pandoc", to = "markdown_strict")$NoChanges
}

\keyword{utilities}
