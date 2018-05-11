\name{exams2blackboard}
\alias{exams2blackboard}
\alias{make_itembody_blackboard}

\title{Generation of Exams in Blackboard Format}

\description{
  Automatic generation of exams in Blackboard format (which is
  partially based on QTI 1.2).
}

\usage{
  exams2blackboard(file, n = 1L, nsamp = NULL, dir = ".",
    name = NULL, quiet = TRUE, edir = NULL,
    tdir = NULL, sdir = NULL, verbose = FALSE,
    resolution = 100, width = 4, height = 4, encoding = "",
    num = NULL, mchoice = NULL,
    schoice = mchoice, string = NULL, cloze = NULL,
    template = "blackboard",
    pdescription = "This is an item from an item pool.",
    tdescription = "This is today's test.",
    pinstruction = "Please answer the following question.",
    tinstruction = "Give an answer to each question.",
    maxattempts = 1, zip = TRUE, points = NULL,
    eval = list(partial = TRUE, negative = FALSE),
    base64 = FALSE, converter = NULL,
    \dots)

  make_itembody_blackboard(rtiming = FALSE, shuffle = FALSE,
    rshuffle = shuffle, minnumber = NULL, maxnumber = NULL,
    defaultval = NULL, minvalue = NULL, maxvalue = NULL,
    cutvalue = NULL, enumerate = TRUE, digits = NULL,
    tolerance = is.null(digits), maxchars = 12,
    eval = list(partial = TRUE, negative = FALSE),
    qti12 = FALSE)
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
  \item{edir}{character specifying the path of the directory in which
    the files in \code{file} are stored (see also below).}
  \item{tdir}{character specifying a temporary directory, by default
    this is chosen via \code{\link[base]{tempdir}}. Note that this
    is cleaned up and potentially temporary files are deleted.}
  \item{sdir}{character specifying a directory for storing supplements, by
    default this is chosen via \code{\link[base]{tempdir}}.}
  \item{verbose}{logical. Should information on progress of exam generation
    be reported?}
  \item{resolution, width, height}{numeric. Options for rendering PNG
    graphics passed to \code{\link[exams]{xweave}}.}
  \item{encoding}{character, passed to \code{\link[exams]{xweave}}.}
  \item{num}{function or named list applied to numerical (i.e., type \code{num})
    questions. If \code{num} is a function, \code{num} will be used
    for generating the item body of the question, see function \code{make_itembody_blackboard()}.
    If \code{num} is a named list, these arguments will be passed to function
    \code{make_itembody_blackboard()}.}
  \item{mchoice, schoice, string, cloze}{function or named list applied to multiple choice,
    single choice, string, and cloze questions (i.e., type \code{mchoice}, \code{schoice},
    \code{string}, and \code{cloze}), respectively. See argument \code{num} for more details.}
  \item{template}{character. The IMS QTI 1.2 or 2.1 template that should be used. Currently,
    the package provides \code{"blackboard.xml"}.}
  \item{pdescription}{character. Description (of length 1) of the item pool (i.e., the set of copies).}
  \item{tdescription}{character. Description (of length 1) of the overall assessment (i.e., exam).}
  \item{pinstruction}{character. Instruction (of length 1) for the item pool (i.e., set of copies).}
  \item{tinstruction}{character. Instruction (of length 1) for the overall assessment (i.e., exam).}
  \item{maxattempts}{integer. The maximum attempts for one question, may also be set to
    \code{Inf}.}
  \item{zip}{logical. Should the resulting XML file (plus supplements) be zipped?}
  \item{points}{integer. How many points should be assigned to each exercise? Note that this
    argument overules any exercise points that are provided within an \code{"\expoints{}"} tag
    in the \code{.Rnw} file. The vector of points supplied is expanded to the number of exercises in
    the exam.}
  \item{eval}{named list, specifies the settings for the evaluation policy, see function
    \code{\link{exams_eval}}.}
  \item{base64}{logical. Should supplementary files be embedded using Base 64 coding?
    Argument \code{base64} may also be a character vector of file suffixes that should be encoded, e.g.
    \code{base64 = c("png", "rda")} will only encode PNG images and binary \code{.rda} files.
    If set to \code{NULL} only image files will be encoded.}
  \item{converter, \dots}{arguments passed on to \code{make_exercise_transform_html}.
    The default for \code{converter} is set to \code{"ttm"} unless there are Rmd
    exercises in \code{file} where \code{"pandoc"} is used.}
  \item{rtiming, shuffle, rshuffle, minnumber, maxnumber, defaultval, minvalue,
    maxvalue}{arguments used for IMS QTI 1.2 item construction, for details see the XML
    specification (see IMS Global Learning Consortium, Inc. 2012), especially Section 4.}
  \item{cutvalue}{numeric. The cutvalue at which the exam is passed.}
  \item{enumerate}{logical. Insert potential solutions in enumerated list?}
  \item{digits}{integer. How many digits should be used for \code{num} exercises?}
  \item{tolerance}{logical. Should tolerance intervals be used for checking if the supplied
    \code{num} answer/number is correct? The default is to use tolerance intervals if \code{digits = NULL}.}
  \item{maxchars}{numeric. Lower bound for the number of characters in fill-in-blank
    fields. The actual number of characters is selected as the maximum number of
    characters of this value and the actual solution.}
  \item{qti12}{logical. For reverse compability to plain QTI 1.2 XML format.}
}

\details{
  Blackboard employs an XML format that essentially uses the Question & Test
  Interoperability (QTI) standard, version 1.2, see IMS Global Learning
  Consortium, Inc. (2012). However, as this deviates from the plain QTI 1.2
  standard in several places, the \code{\link{exams2qti12}} cannot be used
  directly. Instead, \code{exams2blackboard} is a new interface that is likely
  to be improved in future versions.

  \code{exams2blackboard} produces a \code{.zip} file that may be uploaded into
  Blackboard. This includes the final XML file of the exam/assessment as well as
  possible supplement folders that include images, data sets etc. used for the
  exam.  After uploading the test into Blackboard, the material will appear
  under \sQuote{Course Tools}: the test will be available in \sQuote{Tests},
  and each pool within the test  will also appear in \sQuote{Pools}.
  
  
  \code{exams2blackboard} proceeds by (1) calling \code{\link[exams]{xweave}} on
  each exercise, (2) reading the resulting LaTeX code, (3) transforming the
  LaTeX code to HTML, and (4) embedding the HTML code in a XML file using
  Blackboard's QTI standards for assessments and question items. For steps (1)
  and (2) the standard drivers in \code{xexams} are used. In step (3), a
  suitable transformation function is set up on the fly using
  \code{make_exercise_transform_html}, see also the details section in
  \code{\link{exams2html}}. For step (4), the function will cycle through all
  questions and exams to generate the final XML file in the Blackboard QTI
  standard. Therefore, each question will be included in the XML as one section.
  The replicates of each question will be written as question items of the
  section.

  The function uses the XML template for Blackboard's QTI standards for
  assessments and items to generate the exam (per default, this is the XML file
  \code{blackboard.xml} provided in the \code{xml} folder of this package). The
  assessment template must provide one section including one item.
  \code{exams2blackboard} will then use the single item template to generate all
  items, as well as the assessment and section specifications set within the
  template.

  The default template will generate exams/assessments that sample one replicate
  of a question/item for each section. The usual procedure in exam/assessment
  generation would be to simply copy \& paste the XML template of the package
  and adapt it to the needs of the user. Note that all specifiers that have a
  leading \code{##} in the XML template will be replaced by suitable code in
  \code{exams2blackboard} and should always be provided in the template. I.e.,
  the user may add additional tags to the XML template or modify certain
  specifications, like the number of replicates/items that should be sampled for
  each section etc.

  Per default, the individual question/item bodies are generated by function
  \code{make_itembody_blackboard}, i.e., \code{make_itembody_blackboard} checks
  the type of the question and will produce suitable XML code. Note that for
  each question type, either the arguments of \code{make_itembody_blackboard}
  may be set within \code{num}, \code{mchoice}, \code{schoice} and \code{string}
  in \code{exams2blackboard}, by providing a named list of specifications that
  should be used, or for each questiontype, a function that produces the item
  body XML code may be provided to \code{num}, \code{mchoice}, \code{schoice}
  and \code{string}. E.g., \code{mchoice = list(shuffle = TRUE)} will force only
  multiple choice questions to have a shuffled answerlist.

  Note that in Blackboard \code{cloze} items are not officially supported, and
  hence this type of item is  not supported in the current version of
  \code{exams2blackboard} either. It is currently investigated if a  workaround
  may be implemented to allow for \code{cloze} items.  
}

\references{
Blackboard, Inc. (2016). \emph{Blackboard Help: Question types}.
  \url{http://en-us.help.blackboard.com/Learn/Instructor/Tests_Pools_Surveys/040_Question_Types}

IMS Global Learning Consortium, Inc. (2012).
\emph{IMS Question & Test Interoperability: ASI XML Binding Specification Final Specification Version 1.2}.
\url{http://www.imsglobal.org/question/qtiv1p2/imsqti_asi_bindv1p2.html}

Zeileis A, Umlauf N, Leisch F (2014).
  Flexible Generation of E-Learning Exams in R: Moodle Quizzes, OLAT Assessments, and Beyond.
  \emph{Journal of Statistical Software}, \bold{58}(1), 1--36.
  \url{http://www.jstatsoft.org/v58/i01/}.
}

\value{
  \code{exams2blackboard} returns a list of exams as generated by \code{\link[exams]{xexams}}.

  \code{make_itembody_blackboard} returns a function that generates the XML code
  for the itembody tag in Blackboard's version of the IMS QTI 1.2 format.
}

\seealso{
  \code{\link[exams]{exams2qti12}}
}

\examples{
## load package and enforce par(ask = FALSE)
library("exams")
options(device.ask.default = FALSE)

## define an exams (= list of exercises)
myexam <- list(
  "boxplots",
  "ttest",
  "anova",
  "scatterplot",
  "relfreq"
)

## output directory
mydir <- tempdir()

## generate .zip with Blackboard exam in temporary directory
exams2blackboard(myexam, n = 3, dir = mydir)
dir(mydir)
}

\keyword{utilities}
