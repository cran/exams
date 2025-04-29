\name{exams2testvision}
\alias{exams2testvision}
\alias{make_itembody_testvision}

\title{Generation of Exams in TestVision Format}

\description{
  Automatic generation of exams in TestVision format (still under development) for the online testing system TestVision Online.
}

\usage{
  exams2testvision(file, n = 1L, nsamp = NULL, dir = ".",
    name = NULL, quiet = TRUE, edir = NULL,
    tdir = NULL, sdir = NULL, verbose = FALSE,
    resolution = 100, width = 4, height = 4, svg = FALSE,
    encoding = "UTF-8", envir = NULL, engine = NULL,
    num = NULL, mchoice = NULL,
    schoice = mchoice, string = NULL, cloze = NULL,
    template = "testvision",
    stitle = "Exercise", ititle = "Question",
    adescription = "Please solve the following exercises.",
    sdescription = "Please answer the following question.",
    maxattempts = 1, solutionswitch = TRUE, zip = TRUE, points = NULL,
    eval = list(partial = TRUE, rule = "false2", negative = FALSE), converter = "pandoc",
    base64 = FALSE, mode = "hex", \dots)

  make_itembody_testvision(shuffle = FALSE, defaultval = NULL,
    minvalue = NULL, maxvalue = NULL,
    enumerate = FALSE, digits = NULL,
    tolerance = is.null(digits), maxchars = 12,
    eval = list(partial = TRUE, rule = "false2", negative = FALSE), solutionswitch = TRUE)
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
  \item{resolution, width, height}{numeric. Options for rendering PNG
    graphics passed to \code{\link[exams]{xweave}}.}
  \item{svg}{logical. Should graphics be rendered in SVG or PNG (default)?}
  \item{encoding}{character, ignored. The encoding is always assumed to be UTF-8.}
  \item{envir}{argument passed to \code{\link[exams]{xweave}} (which passes
    it to \code{\link[knitr]{knit}}).}
  \item{engine}{argument passed to \code{\link[exams]{xweave}} indicating whether
    \code{"Sweave"} (default) or \code{"knitr"} should be used for rendering Rnw exercises.}
  \item{num}{function or named list applied to numerical (i.e., type \code{num})
    questions. If \code{num} is a function, \code{num} will be used
    for generating the item body of the question, see function \code{make_itembody_testvision()}.
    If \code{num} is a named list, these arguments will be passed to function
    \code{make_itembody_testvision()}.}
  \item{mchoice, schoice, string, cloze}{function or named list applied to multiple choice,
    single choice, string, and cloze questions (i.e., type \code{mchoice}, \code{schoice},
    \code{string}, and \code{cloze}), respectively. See argument \code{num} for more details.}
  \item{template}{character. The IMS QTI 1.2 or 2.1 template that should be used. Currently,
    the package provides \code{"testvision.xml"}.}
  \item{stitle}{character. A title that should be used for the sections. May be a vector of
    length 1 to use the same title for each section, or a vector containing different section
    titles.}
  \item{ititle}{character. A title that should be used for the assessment items. May be a vector
    of length 1 to use the same title for each item, or a vector containing different item
    titles. Note that the maximum of different item titles is the number of sections/questions
    that are used for the exam.}
  \item{adescription}{character. Description (of length 1) for the overall assessment (i.e., exam).}
  \item{sdescription}{character. Vector of descriptions for each section, omitted if empty
    (or \code{NULL} or \code{FALSE}).}
  \item{maxattempts}{integer. The maximum attempts for one question, may also be set to
    \code{Inf}.}
  \item{solutionswitch}{logical. Should the question/item solutionswitch be enabled?}
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
  \item{converter}{character. Argument passed on to \code{make_exercise_transform_html}.
    The default for \code{converter} is set to \code{"pandoc"} unless \code{"ttm"} is required. The default works best in TestVision.}
  \item{mode}{character. See function \code{\link[tth]{tth}}.}
  \item{shuffle,
        defaultval,
        minvalue,
        maxvalue}{arguments used for IMS QTI 2.1 item construction, for details see the XML
    specification (see IMS Global Learning Consortium, Inc. 2012), especially Section 4.}
  \item{enumerate}{logical. Insert potential solutions in enumerated list?}
  \item{digits}{integer. How many digits should be used for \code{num} exercises?}
  \item{tolerance}{logical. Should tolerance intervals be used for checking if the supplied
    \code{num} answer/number is correct? The default is to use tolerance intervals if \code{digits = NULL}.}
  \item{maxchars}{numeric. Lower bound for the number of characters in fill-in-blank
    fields. The actual number of characters is selected as the maximum number of
    characters of this value and the actual solution.}
  \item{\dots}{further arguments passed on to \code{make_exercise_transform_html}.}
}

\details{
  TestVision employs an XML format that essentially uses the Question & Test
  Interoperability (QTI) standard, version 2.1, see IMS Global Learning
  Consortium, Inc. (2012). However, as this deviates substantially from the plain QTI 2.1
  standard in several places, the \code{\link{exams2qti21}} cannot be used
  directly. Instead, \code{exams2testvision} is a new interface that allows for meeting TestVision's
  requirements for XML-imports.

  \code{exams2testvision} produces a \code{.zip} file that may be uploaded into
  TestVision. This includes the final XML file of the exam/assessment as well as
  possible supplement folders that include images, data sets etc. used for the
  exam.


  \code{exams2testvision} proceeds by (1) calling \code{\link[exams]{xweave}} on
  each exercise, (2) reading the resulting LaTeX code, (3) transforming the
  LaTeX code to HTML, and (4) embedding the HTML code in a XML file using
  TestVision's QTI standards for assessments and question items. For steps (1)
  and (2) the standard drivers in \code{xexams} are used. In step (3), a
  suitable transformation function is set up on the fly using
  \code{make_exercise_transform_html}, see also the details section in
  \code{\link{exams2html}}. For step (4), the function will cycle through all
  questions and exams to generate the final XML file in the TestVision QTI
  standard. The questions appear as separate files in the system.

  The function uses the XML template for TestVision's QTI standards for
  assessments and items to generate the exam (per default, this is the XML file
  \code{testvision.xml} provided in the \code{xml} folder of this package). The
  assessment template must provide one or more sections for including the items.
  \code{exams2testvision} will then use the single item template to generate all
  items, as well as the assessment and section specifications set within the
  template.

  The default template will generate exams/assessments that sample one replicate
  of a question/item for each section. The usual procedure in exam/assessment
  generation would be to simply copy & paste the XML template of the package
  and adapt it to the needs of the user. Note that all specifiers that have a
  leading \code{##} in the XML template will be replaced by suitable code in
  \code{exams2testvision} and should always be provided in the template. I.e.,
  the user may add additional tags to the XML template or modify certain
  specifications, like the number of replicates/items that should be sampled for
  each section etc.

  Per default, the individual question/item bodies are generated by function
  \code{make_itembody_testvision}, i.e., \code{make_itembody_testvision} checks
  the type of the question and will produce suitable XML code. Note that for
  each question type, either the arguments of \code{make_itembody_testvision}
  may be set within \code{num}, \code{mchoice}, \code{schoice} and \code{string}
  in \code{exams2testvision}, by providing a named list of specifications that
  should be used, or for each questiontype, a function that produces the item
  body XML code may be provided to \code{num}, \code{mchoice}, \code{schoice}
  and \code{string}. E.g., \code{mchoice = list(shuffle = TRUE)} will force only
  multiple choice questions to have a shuffled answerlist.
  
  Please note the following for \code{cloze} questions: Although any combination
  of the four item types can be successfully uploaded into TestVision, \code{cloze} 
  questions containing \code{mhoice} do not work properly (it is therefore 
  advised to not use \code{mchoice} within \code{cloze}). 

}

\references{
TestVision (2020). \emph{English introduction tutorial on TestVision}.
  Formerly available at https://testvision.nl/en/experience-testvision/tutorials-2/

IMS Global Learning Consortium, Inc. (2012).
\emph{IMS Question & Test Interoperability (QTI) XSD Binding Version 2.1 Final}.
\url{https://www.imsglobal.org/question/qtiv2p1/imsqti_bindv2p1.html}

Zeileis A, Umlauf N, Leisch F (2014).
  Flexible Generation of E-Learning Exams in R: Moodle Quizzes, OLAT Assessments, and Beyond.
  \emph{Journal of Statistical Software}, \bold{58}(1), 1--36.
  \doi{10.18637/jss.v058.i01}.
}

\value{
  \code{exams2testvision} returns a list of exams as generated by \code{\link[exams]{xexams}}.

  \code{make_itembody_testvision} returns a function that generates the XML code
  for the itembody tag in TestVision's version of the IMS QTI 2.1 format.
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
  "tstat",
  "tstat2",
  "relfreq",
  "essayreg",
  "dist2",
  "boxhist2"
)

## output directory
dir.create(mydir <- tempfile())

## generate .zip with set of TestVision exercises
exams2testvision(myexam, n = 3, dir = mydir)
dir(mydir)
}

\keyword{utilities}
