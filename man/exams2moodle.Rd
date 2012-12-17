\name{exams2moodle}
\alias{exams2moodle}
\alias{make_question_moodle23}

\title{Generation of Exams in Moodle XML Format}

\description{
  Automatic generation of exams in Moodle XML format.
}

\usage{
  exams2moodle(file, n = 1L, nsamp = NULL, dir = ".",
    name = NULL, quiet = TRUE, edir = NULL,
    tdir = NULL, sdir = NULL, resolution = 100,
    width = 4, height = 4, iname = TRUE, stitle = NULL,
    testid = FALSE, zip = FALSE, num = NULL, mchoice = NULL,
    schoice = mchoice, string = NULL, cloze = NULL,
    \dots)

  make_question_moodle23(name = NULL, solution = TRUE,
    shuffle = FALSE, penalty = 0, answernumbering = "abc",
    usecase = FALSE, cloze_mchoice_display = "MULTICHOICE",
    truefalse = c("True", "False"), enumerate = TRUE)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files.}
  \item{n}{integer. The number of copies to be compiled from \code{file}.}
  \item{nsamp}{integer. The number of exercise files sampled from each
    list element of \code{file}. Sampling without replacement is used
    if possible. (Only if some element of \code{nsamp} is larger than
    the length of the corresponding element in \code{file}, sampling
    with replacement is used.)}
  \item{dir}{character. The default is the current working directory.}
  \item{name}{character. A name prefix for resulting exercises and ZIP file.}
  \item{quiet}{logical. Should output be suppressed when calling
    \code{\link[utils]{Sweave}}?}
  \item{edir}{character specifying the path of the directory in which
    the files in \code{file} are stored (see also below).}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempdir}}.}
  \item{sdir}{character specifying a directory for storing supplements, by
    default this is chosen via \code{\link[base]{tempdir}}.}
  \item{resolution, width, height}{numeric. Options for rendering PNG
    graphics passed to \code{\link[utils]{Sweave}}.}
  \item{iname}{logical. Should the exam \code{name} be included in the path in the \code{<category>}
    tag in the final XML file? This option may be useful when questions should be added to
    certain already existing question banks, i.e. \code{iname = TRUE} will include the exam
    \code{name} by \code{$course$/ExamName/}.}
  \item{stitle}{character. For the questions specified in argument \code{file}, additional section
    titles may be set. The section titles will then be added to the \code{<category>}
    tag in the final XML file (see also argument \code{iname}), i.e. the section name
    for each question will be written to \code{$course$/ExamName/SectionName}. Note that section
    names may also be provided in the \code{\\exsection{}} tag in the \code{.Rnw} file of the
    question. However, section names that are specified in \code{stitle} will overwrite
    \code{\\exsection{}} tags. \code{stitle} may also include \code{NA}, e.g.
    \code{stitle = c("Exercise 1", NA, "Exercise 3")}.}
  \item{testid}{logical. Should an unique test id be added to the exam \code{name}.}
  \item{zip}{logical. Should the resulting XML file (plus supplements) be zipped?}
  \item{num}{function or named list applied to numerical (i.e., type \code{num})
    questions. If \code{num} is a function, \code{num} will be used
    for generating the item body of the question, see function \code{make_itembody_qti12()}.
    If \code{num} is a named list, these arguments will be passed to function
    \code{make_itembody_qti12()}.}
  \item{mchoice, schoice, string, cloze}{function or named list applied to multiple choice, 
    single choice, string, and cloze questions (i.e., type \code{mchoice}, \code{schoice},
    \code{string}, and \code{cloze}), respectively. See argument \code{num} for more details.}
  \item{solution}{logical. Should the question solution, if available, be added in the question
     XML?}
  \item{shuffle}{For \code{mchoice} and \code{schoice} exercises, if set to \code{TRUE} will
    force Moodle to additionally shuffle the provided answer list.}
  \item{penalty}{numeric. Specifies the penalty tag for a question.}
  \item{answernumbering}{character. Specifies how choice questions should be numbered.}
  \item{usecase}{logical. Should string questions be case sensitive or not.}
  \item{cloze_mchoice_display}{character. In \code{cloze} type questions, the user may set the
    visual appearance of choice questions in either a drop down menue (default)
    \code{"MULTICHOICE"}, vertical alignment \code{"MULTICHOICE_V"} or horizontal
    \code{"MULTICHOICE_H"}.}
  \item{truefalse}{character of length 2. For single choice answers in \code{cloze} questions,
    the user may specify the possible options shown.}
  \item{enumerate}{logical. In \code{cloze} questions, if set to \code{TRUE}, the answerlist and
    solutionlist will be enumerated.}
  \item{\dots}{arguments passed on to \code{make_exercise_transform_html}.}
}

\details{
  \code{exams2moodle} will produce a XML file that may be uploaded in Moodle. It proceeds by (1)
  calling \code{\link[utils]{Sweave}} on each exercise, (2) reading the resulting LaTeX code, (3)
  transforming the LaTeX code to HTML, and (4) embedding the HTML code in a XML file using the
  Moodle 2.3 standards for exams/quizzes.

  For steps (1) and (2) the standard drivers in \code{xexams} are used. In step (3), a suitable
  transformation function is set up on the fly using \code{make_exercise_transform_html}, see also
  the details section in \code{\link{exams2html}}.

  For step (4), the function will cycle through all questions and exams to generate the final XML
  file in Moodle 2.3 standard. The structure of the resulting XML file is such that one category
  will be set for the exam/quiz using the exam/quiz \code{name} (or this category may be
  suppressed (i.e., not included in the XML) by setting \code{iname = FALSE}), followed by one
  category/section for each question, while the replicates of each question will be included in the
  corresponding category/section. Note that category/section names may also be provided in the
  \code{\\exsection{}} tag in the \code{.Rnw} files, or within argument \code{stitle} in
  \code{exams2moodle}. This may be useful when questions should automatically be added to already
  existing Moodle question banks. (See also the argument descriptions above)

  The XML code for each question is then generated using function \code{make_question_moodle23}.
  Note that for each question type, either the arguments of \code{make_question_moodle23} may be set
  within \code{num}, \code{mchoice}, \code{schoice}, \code{string} and \code{cloze} in
  \code{exams2moodle}, by providing a named list of specifications that should be used, or for each
  questiontype, a function that produces the question XML code may be provided to \code{num},
  \code{mchoice}, \code{schoice}, \code{string} and \code{cloze}. E.g., to suppress the solution
  for numeric questions one may set \code{num = list(solution = FALSE)}.
}

\value{
  \code{exams2moodle} returns a list of exams as generated by \code{\link[exams]{xexams}}.

  \code{make_question_moodle23} returns a function that generates the XML code
  for the question in Moodle's XML standard 2.3.
}

\references{
  Dougiamas M, et al. (2012). \emph{Moodle, Version 2.3}.
  \url{http://moodle.org/}.

  MoodleDocs (2012).
  \emph{Moodle XML Format}.
  \url{http://docs.moodle.org/23/en/Moodle_XML}
  
  Qedoc (2010).
  \emph{Moodle XML Format}.
  \url{http://www.qedoc.org/en/index.php?title=Moodle_XML_format}
}

\seealso{
  \code{\link[exams]{xexams}},
  \code{\link[tth]{ttm}},
  \code{\link[tth]{tth}},
  \code{\link[exams]{tex2image}},
  \code{\link[exams]{make_exercise_transform_html}},
}

\examples{
## load package and enforce par(ask = FALSE)
library("exams")
options(device.ask.default = FALSE)

## define an exams (= list of exercises)
myexam <- list(
  "boxplots",
  c("tstat", "ttest", "confint"),
  c("regression", "anova"),
  c("scatterplot", "boxhist"),
  "relfreq"
)

## output directory
mydir <- tempdir()

## generate moodle quiz in temporary directory
## using a few customization options
exams2moodle(myexam, n = 3, dir = mydir,
  num = list(solution = FALSE),
  mchoice = list(shuffle = TRUE)
)
dir(mydir)
}

\keyword{utilities}
