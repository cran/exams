\name{exams2arsnova}
\alias{exams2arsnova}
\alias{make_exams_write_arsnova}

\title{Generation of Exam/Quiz Sessions in ARSnova Format}

\description{
  Interface for generating interactive sessions in the JSON format of the
  audience response system ARSnova. As ARSnova has been superseded by Particify,
  it is recommended to use \code{\link[exams]{exams2particify}} instead.
  \code{exams2arsnova} will be removed in future versions of the package.
}

\usage{
exams2arsnova(file, n = 1L, dir = ".",
  name = "R/exams", sname = NULL, qname = NULL,
  quiet = TRUE, resolution = 100, width = 4, height = 4, svg = FALSE,
  encoding = "UTF-8", envir = NULL, engine = NULL,
  url = "https://arsnova.eu/api", sessionkey = NULL, jsessionid = NULL,
  active = TRUE, votingdisabled = FALSE, showstatistic = FALSE, showanswer = FALSE,
  abstention = TRUE, variant = "lecture", ssl.verifypeer = TRUE,
  fix_choice = TRUE, \dots)

make_exams_write_arsnova(url = "https://arsnova.eu/api", sessionkey = NULL,
  jsessionid = NULL, name = "R/exams", sname = NULL, qname = NULL,
  active = TRUE, votingdisabled = FALSE, showstatistic = FALSE, showanswer = FALSE,
  abstention = TRUE, variant = "lecture", ssl.verifypeer = TRUE, fix_choice = TRUE)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files.}
  \item{n}{integer. The number of copies to be compiled from \code{file}.}
  \item{dir}{character. The default is either display on the screen or
    the current working directory.}
  \item{name}{character. A name prefix for resulting exercises and RDS file.}
  \item{sname}{character. A vector of length 2 with the session name (maximum of 50
    characters) and its abbreviation (maximum of 8 characters). Ignored if the \code{sessionkey}
    of an existing session is supplied and otherwise copied by default from \code{name}.}
  \item{qname}{character. A vector of names for each question/exercise in \code{file}.
    By default, the \code{name} is used.}
  \item{quiet}{logical. Should output be suppressed when calling
    \code{\link[exams]{xweave}} and \code{\link[tools]{texi2dvi}}.}
  \item{resolution, width, height}{numeric, passed to \code{\link[exams]{xweave}}.}
  \item{svg}{logical. Should graphics be rendered in SVG or PNG (default)?}
  \item{encoding}{character, ignored. The encoding is always assumed to be UTF-8.}
  \item{envir}{argument passed to \code{\link[exams]{xweave}} (which passes
    it to \code{\link[knitr]{knit}}).}
  \item{engine}{argument passed to \code{\link[exams]{xweave}} indicating whether
    \code{"Sweave"} (default) or \code{"knitr"} should be used for rendering Rnw exercises.}
  \item{url,sessionkey,jsessionid}{character specifying (1) the base URL of the ARSnova API,
    (2) the 8-digit ARSnova session key, (3) the JSESSIONID cookie of an active ARSnova session.
    If all are provided all questions are imported directly into the existing ARSnova session.
    Otherwise, a JSON import file is generated.}
  \item{active}{logical. Should the question be active (i.e., released for students)
    or locked?}
  \item{votingdisabled}{logical. Should voting be disabled?}
  \item{showstatistic}{logical. Should statistics be shown?}
  \item{showanswer}{logical. Should answers be shown?}
  \item{abstention}{logical. Are abstentions allowed?}
  \item{variant}{character. Should the question be a \code{"lecture"} or
    a \code{"preparation"} questions?}
  \item{ssl.verifypeer}{logical. Should SSL certificates be validated when
    connecting via https?}
  \item{fix_choice}{logical. Should math markup be removed in single and multiple
    choice lists? (This may be needed for older ARSnova versions where math
    markup is rendered in the question itself but not the choice list.)}
  \item{\dots}{arguments passed on to \code{\link[exams]{xexams}}.}
}

\details{
  \code{exams2arsnova} generates exams in the JSON format for ARSnova
  using \code{\link[exams]{xexams}}. It proceeds by (1) calling \code{\link[exams]{xweave}}
  on each exercise, (2) reading the resulting Markdown or LaTeX text, (3) transforming
  the text to Markdown, and (4) embedding the Markdown text into the
  JSON format for ARSnova (and optionally imports it into a running
  ARSnova session).

  Since 2020 the development of ARSnova has been shifted to a new tool
  called Particify and hence the arsnova.eu server is not
  hosted anymore. For an export function to Particify see
  \code{\link[exams]{exams2particify}}.
  
  For steps (1) and (2) the standard drivers in \code{xexams} are used.  
  For step (3) a suitable transformation function is set up on the fly
  using \code{\link[exams]{make_exercise_transform_pandoc}}.
  For step (4) a simple writer function is set up on the fly that
  embeds the transformed Markdown code into a hard-coded JSON template
  using \code{\link[RJSONIO]{toJSON}} and either writes a single JSON
  file for each exam or imports these directly into an ARSnova session.
  
  When \code{url}, \code{sessionkey}, and \code{jsessionid} are all supplied,
  \code{\link[RCurl]{curlPerform}} is used to import tall questions directly into
  the existing ARSnova session. Otherwise, a file is written to the disk and
  then needs to be imported manually into an ARSnova server. This file
  is either a JSON file for a whole new session (if \code{sessionkey} is
  \code{NULL}, the default) or a CSV file with the questions only.
}

\value{
  A list of exams as generated by \code{\link[exams]{xexams}} is
  returned invisibly.
}

\seealso{\code{\link[exams]{exams2particify}}}

\examples{
## load package and enforce par(ask = FALSE)
library("exams")
options(device.ask.default = FALSE)

\dontrun{
## exams2arsnova can either create text files with JSON data
exams2arsnova("tstat2")

## or directly post this to an active ARSnova session (for which the
## server URL, the 8-digit session key, and the JSESSIONID cookie are needed)
exams2arsnova("tstat2", url = "https://arsnova.eu/api",
  sessionkey = "49061284", jsessionid = "A5BEFDA4141816BB425F2204A602E4B3")
}
}

\keyword{utilities}
