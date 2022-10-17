\name{exams2moodle}
\alias{exams2moodle}
\alias{make_question_moodle}
\alias{make_question_moodle23}

\title{Generation of Exams in Moodle XML Format}

\description{
  Automatic generation of exams in Moodle XML format.
}

\usage{
  exams2moodle(file, n = 1L, nsamp = NULL, dir = ".",
    name = NULL, quiet = TRUE, edir = NULL,
    tdir = NULL, sdir = NULL, verbose = FALSE, rds = FALSE,
    resolution = 100, width = 4, height = 4, svg = FALSE, encoding = "UTF-8",
    iname = TRUE, stitle = NULL,
    testid = FALSE, zip = FALSE, num = NULL, mchoice = NULL,
    schoice = mchoice, string = NULL, cloze = NULL,
    points = NULL, rule = NULL, pluginfile = TRUE, forcedownload = FALSE,
    converter = "pandoc-mathjax", envir = NULL,
    table = NULL, css = NULL, \dots)

  make_question_moodle(name = NULL, solution = TRUE,
    shuffle = FALSE, penalty = 0, answernumbering = "abc",
    usecase = FALSE, cloze_mchoice_display = NULL, cloze_schoice_display = NULL,
    truefalse = c("True", "False"), enumerate = TRUE, abstention = NULL,
    eval = list(partial = TRUE, negative = FALSE, rule = "false2"),
    essay = NULL, numwidth = NULL, stringwidth = NULL,
    css = NULL)
}

\arguments{
  \item{file}{character. A specification of a (list of) exercise files.}
  \item{n}{integer. The number of copies to be compiled from \code{file}.}
  \item{nsamp}{integer. The number(s) of exercise files sampled from each
    list element of \code{file}. Sampling without replacement is used
    if possible. (Only if some element of \code{nsamp} is larger than
    the length of the corresponding element in \code{file}, sampling
    with replacement is used.)}
  \item{dir}{character. The default is the current working directory.}
  \item{name}{character. A name prefix for resulting exercises and ZIP file.}
  \item{quiet}{logical. Should output be suppressed when calling
    \code{\link[exams]{xweave}}?}
  \item{edir}{character specifying the path of the directory (along with its
    sub-directories) in which the files in \code{file} are stored
    (see also \code{\link{xexams}}).}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempfile}}. Note that this
    is cleaned up (i.e., existing files are deleted) and only certain
    temporary files are preserved.}
  \item{sdir}{character specifying a directory for storing supplements, by
    default this is chosen via \code{\link[base]{tempfile}}.}
  \item{verbose}{logical. Should information on progress of exam generation
    be reported?}
  \item{rds}{logical indicating whether the return list should also be
    saved as an RDS data file.}
  \item{resolution, width, height}{numeric. Options for rendering PNG (or SVG)
    graphics passed to \code{\link[exams]{xweave}}.}
  \item{svg}{logical. Should graphics be rendered in SVG or PNG (default)?}
  \item{encoding}{character, ignored. The encoding is always assumed to be UTF-8.}
  \item{envir}{passed to \code{\link[exams]{xweave}}.}
  \item{table}{character. An optional class assigned to \code{<table>} elements
    (without a class) in the question (only supported for pandoc-based \code{converter}).}
  \item{css}{character. A character string (or vector) containing the path(s) to
    CSS style file(s). Alternatively, a string (or vector) with a \code{<style>}
    tag to be included in every question (e.g., for formatting \code{<table>} elements).}
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
  \item{zip}{logical. Should the resulting XML file be zipped?}
  \item{num}{function or named list applied to numerical (i.e., type \code{num})
    questions. If \code{num} is a function, \code{num} will be used
    for generating the item body of the question, see function \code{make_itembody_qti12()}.
    If \code{num} is a named list, these arguments will be passed to function
    \code{make_itembody_qti12()}.}
  \item{mchoice, schoice, string, cloze}{function or named list applied to multiple choice, 
    single choice, string, and cloze questions (i.e., type \code{mchoice}, \code{schoice},
    \code{string}, and \code{cloze}), respectively. See argument \code{num} for more details.}
  \item{points}{integer. How many points should be assigned to each exercise? Note that this
    argument overules any exercise points that are provided within an \code{"\expoints{}"} tag
    in the \code{.Rnw} file. The vector of points supplied is expanded to the number of exercises in
    the exam.}
  \item{rule}{character specifying which rule to use for negative partial credits. see function
    \code{\link{exams_eval}}. Note that the default using cloze exercises is \code{rule = "none"}.}
  \item{pluginfile}{logical. Should supplements be included in the Moodle XML file via Moodle's
    Pluginfile mechanism? This is the default but may not work with older versions of Moodle (<2.5).
    If set to \code{FALSE} supplements like graphics and data are included as data URIs.}
  \item{forcedownload}{logical. Should all supplementary links be forced to download when clicked
    (as opposed to opening in the browser)? Only supported if \code{pluginfile = TRUE}.    
    If \code{forcedownload = FALSE} the behavior typically depends on the browser,
    user settings, and file type.}
  \item{solution}{logical. Should the question solution, if available, be added in the question
     XML?}
  \item{shuffle}{For \code{mchoice} and \code{schoice} exercises, if set to \code{TRUE} will
    force Moodle to additionally shuffle the provided answer list.}
  \item{penalty}{numeric. Specifies the penalty tag for a question.}
  \item{answernumbering}{character. Specifies how choice questions should be numbered,
    allowed values are: \code{"abc"} (default), \code{"ABCD"}, \code{"123"} or \code{"none"}.}
  \item{usecase}{logical. Should string questions be case sensitive or not.}
  \item{cloze_mchoice_display, cloze_schoice_display}{character. In \code{cloze} questions,
    the user may set the visual appearance of choice questions. If \code{NULL} (default),
    \code{"MULTIRESPONSE"} (column of checkboxes) is used for \code{mchoice} questions and
    \code{"MULTICHOICE"} (drop-down menu) for \code{schoice} questions unless math markup is present in the question list.
    The latter is not rendered in drop-down menus and hence \code{"MULTICHOICE_V"} (radio buttons, vertical column) are used.
    Other options include a horizontal row of either checkboxes (\code{"MULTIRESPONSE_H"}) or
    radio buttons (\code{"MULTICHOICE_H"}), respectively. Shuffled versions of all display types are also
    available (since Moodle 3.0) by appending an "S", e.g., \code{"MULTICHOICE_S"} or
    \code{"MULTICHOICE_VS"} etc.}
  \item{truefalse}{character of length 2. For single choice answers in \code{cloze} questions,
    the user may specify the possible options shown.}
  \item{enumerate}{logical. In \code{cloze} questions, if set to \code{TRUE}, the answerlist and
    solutionlist will be enumerated.}
  \item{abstention}{character or logical. Should an explicit abstention option be added
    in single/multiple choice exercises? The character text specified is used for an
    extra button in Moodle which (when selected) always leads to zero points.}
  \item{eval}{named list, specifies the settings for the evaluation policy, see function
    \code{\link{exams_eval}}.}
  \item{essay}{logical. Should \code{string} questions be rendered into Moodle \code{shortanswer}
    or \code{essay} questions? The default is to use \code{shortanswer} unless either
    \code{essay=TRUE} or the exercise's metainformation is set to \code{essay}.}
  \item{numwidth, stringwidth}{logical, numeric, or character. Should the width of
    all \code{num} or \code{string} sub-items, respectively, in a \code{cloze} be fixed to
    the same width? This can be accomplished by adding a wrong solution with a suitable
    length to all sub-items in Moodle XML. The default (\code{NULL} or equivalently
    \code{FALSE}) is not to do so but let Moodle decide the width of each cell based
    on the respective correct solution. Alternatively, the arguments can be set to
    \code{TRUE} (then the maximum width from the correct solutions is used), an
    integer (indicating the maximum width) or a character (like \code{"1111111"},
    to be used as the wrong solution). Both arguments can also be set through
    exextra tags in each of the exercises' meta-information.}
  \item{converter, \dots}{arguments passed on to \code{make_exercise_transform_html}.
    The default for \code{converter} is \code{"pandoc-mathjax"} which assumes that the quiz
    is imported in a Moodle site with MathJax plugin activated (which is the default setting in
    Moodle). For using MathML instead of MathJax the \code{converter} can be set to
    \code{NULL} or \code{"pandoc-mathml"} etc. For details see Zeileis (2019).}
}

\details{
  \code{exams2moodle} produces an XML file that may be uploaded into Moodle. It proceeds by (1)
  calling \code{\link[exams]{xweave}} on each exercise, (2) reading the resulting LaTeX code, (3)
  transforming the LaTeX code to HTML, and (4) embedding the HTML code in a XML file using the
  Moodle standards for exams/quizzes.

  For steps (1) and (2) the standard drivers in \code{\link{xexams}} are used. In step (3), a suitable
  transformation function is set up on the fly using \code{make_exercise_transform_html}, see also
  the details section in \code{\link{exams2html}}.

  For step (4), the function will cycle through all questions and exams to generate the final XML
  file in Moodle standard. The structure of the resulting XML file is such that one category
  will be set for the exam/quiz using the exam/quiz \code{name} (or this category may be
  suppressed (i.e., not included in the XML) by setting \code{iname = FALSE}), followed by one
  category/section for each question, while the replicates of each question will be included in the
  corresponding category/section. Note that category/section names may also be provided in the
  \code{\\exsection{}} tag in the \code{.Rnw} files, or within argument \code{stitle} in
  \code{exams2moodle}. This may be useful when questions should automatically be added to already
  existing Moodle question banks. (See also the argument descriptions above.)

  The XML code for each question is then generated using function \code{make_question_moodle}.
  Note that for each question type, either the arguments of \code{make_question_moodle} may be set
  within \code{num}, \code{mchoice}, \code{schoice}, \code{string} and \code{cloze} in
  \code{exams2moodle}, by providing a named list of specifications that should be used, or for each
  questiontype, a function that produces the question XML code may be provided to \code{num},
  \code{mchoice}, \code{schoice}, \code{string} and \code{cloze}. E.g., to suppress the solution
  for numeric questions one may set \code{num = list(solution = FALSE)}.

  When specifying cloze exercises, two approaches are possible: Either a \code{answerlist} with
  all questions is provided within the \code{question} or, alternatively, the answer fields can
  be placed anywhere in the \code{question} text. For the latter, the strings \code{##ANSWER1##},
  \code{##ANSWER2##}, etc., have to be used, see the exercises \code{"boxhist2.Rnw"} and
  \code{"fourfold2.Rnw"} for illustration and Appendix C in Zeileis et al. (2014) for further
  details.
  
  To fix the width of numeric answer fields withing cloze exercises (in order not to convey any
  clues about the length of the correct solution), the \code{\\exextra[numwidth]} metainformation
  command can be used in the \code{.Rnw} exercise. For example, it can be set to
  \code{\\exextra[numwidth,logical]{TRUE}}, \code{\\exextra[numwidth,numeric]{5}}, or
  \code{\\exextra[numwidth,character]{100.0}}.

  In order to generate free text questions in moodle one may
  specify extra parameters
  via \code{\\exextra}. Currently the following options are supported:
  \itemize{
  \item \code{essay}: logical. Enables the essay function.
  \item \code{format}: character. Type of text field (one of: \code{plain}, \code{editor}, editorfilepicker monospaced noinline)
  \item \code{required}: logical. Whether an answer is required.
  \item \code{attachments}: numeric. How many attachments can be uploaded.
  \item \code{attachmentsrequired}: numeric. The number of required attachments.
  }
}

\references{
Dougiamas M, et al. (2019). \emph{Moodle, Version 3.6}.
  \url{https://moodle.org/}.

MoodleDocs (2019). \emph{Moodle XML Format}.
  \url{https://docs.moodle.org/en/Moodle_XML}

Zeileis A, Umlauf N, Leisch F (2014).
  Flexible Generation of E-Learning Exams in R: Moodle Quizzes, OLAT Assessments, and Beyond.
  \emph{Journal of Statistical Software}, \bold{58}(1), 1--36.
  \doi{10.18637/jss.v058.i01}.

Zeileis A (2019).
  \emph{Mathematical Notation in Online R/exams}.
  \url{https://www.R-exams.org/tutorials/math/}
}

\value{
  \code{exams2moodle} returns a list of exams as generated by \code{\link[exams]{xexams}}.

  \code{make_question_moodle} returns a function that generates the XML code
  for the question in Moodle's XML standard.
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
  "boxplots.Rmd",
  c("tstat.Rmd", "ttest.Rmd", "confint.Rmd"),
  c("regression.Rmd", "anova.Rmd"),
  c("scatterplot.Rmd", "boxhist.Rmd"),
  "relfreq.Rmd"
)

## output directory
dir.create(mydir <- tempfile())

## generate moodle quiz in temporary directory
## using a few customization options
exams2moodle(myexam, n = 3, dir = mydir,
  num = list(solution = FALSE),
  mchoice = list(shuffle = TRUE)
)
dir(mydir)
}

\keyword{utilities}
