\name{include_tikz}
\alias{include_tikz}

\title{Including Figures from TikZ Code in Exercises}

\description{
  Include figures from TikZ code in an exercise after
  compiling it with \code{\link[exams]{tex2image}}. 
}

\usage{
include_tikz(tikz, name = "tikzpicture", format = NULL,
  library = NULL, width = NULL, markup = "tex", \dots)
}

\arguments{
  \item{tikz}{character vector with the TikZ code.}
  \item{name}{character. Name prefix of the graphics file
    to be produced.}
  \item{format}{character. The graphics format requested from
    \code{\link[exams]{tex2image}}, e.g., \code{"png"} (default),
    \code{"svg"}, \code{"pdf"}. If set to \code{"tex"} then
    \code{\link[exams]{tex2image}} is not called but the
    \code{tikz} code is included directly.}
  \item{library}{character. Names of TikZ libraries required
    for compiling the \code{tikz} code (if any).}
  \item{width}{character. The width with which the resulting
    graphic should be included in LaTeX.}
  \item{markup}{character. Which type of markup should be written?
    Can be \code{"tex"} (default), \code{"markdown"}, or \code{"none"}.}
  \item{\dots}{arguments passed to \code{\link[exams]{tex2image}}.}
}

\details{
The function \code{include_tikz} takes a character vector with \code{tikz}
code, if necessary adds a \code{{tikzpicture}} environment, renders it
into a graphics file via \code{\link[exams]{tex2image}}, and returns
LaTeX or Markdown code that embeds the graphics into an exercise.

If \code{format = "tex"} and \code{markup = "tex"} the TikZ code
is included directly (possibly adding \code{library} and \code{{tikzpicture}},
if necessary).
}

\value{
A character vector is returned. This contains just the name of the graphics
file produced (i.e., \code{name.format}) except for \code{format = "tex"}
where the TikZ code is returned. For \code{markup = "tex"} or \code{"markdown"}
the value is returned invisibly.
}

\examples{
## TikZ code for a logic gate
tz <- "
  \\\\node[left,draw, logic gate inputs=nn, xor gate US,fill=white,,scale=2.5] (G1) at (0,0) {};
  \\\\draw (G1.output) --++ (0.5,0) node[right] (y) {$y$};
  \\\\draw (G1.input 1) --++ (-0.5,0) node[left] {$a$};
  \\\\draw (G1.input 2) --++ (-0.5,0) node[left] {$b$};
"

## switch to temporary directory
wd <- getwd()
td <- tempfile()
dir.create(td)
setwd(td)
dir()

## produce PDF figure and produce includegraphics statement
include_tikz(tz, name = "logicgate", format = "pdf",
  library = c("arrows", "shapes.gates.logic.US", "calc"),
  width = "2.5cm")
dir()

## alternatively produce just the complete TikZ code
include_tikz(tz, name = "logicgate", format = "tex",
  library = c("arrows", "shapes.gates.logic.US", "calc"))

## switch back to original working directory
setwd(wd)
}

\keyword{utilities}
