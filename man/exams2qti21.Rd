\name{exams2qti21}
\alias{exams2qti21}
\alias{make_itembody_qti21}

\title{Generation of Exams in QTI 2.1 Format}

\description{
  Automatic generation of exams in QTI 2.1 format.
}

\usage{
  exams2qti21(file, n = 1L, nsamp = NULL, dir = ".",
    name = NULL, quiet = TRUE, edir = NULL,
    tdir = NULL, sdir = NULL, verbose = FALSE, rds = FALSE,
    resolution = 100, width = 4, height = 4, svg = FALSE, encoding  = "UTF-8",
    num = NULL, mchoice = NULL,
    schoice = mchoice, string = NULL, cloze = NULL,
    template = "qti21", duration = NULL,
    stitle = NULL, ititle = NULL,
    adescription = "Please solve the following exercises.", sdescription = "",
    maxattempts = 1, cutvalue = NULL, solutionswitch = TRUE,
    casesensitive = TRUE, cloze_schoice_display = "auto",
    navigation = "nonlinear", allowskipping = TRUE, allowreview = FALSE,
    allowcomment = FALSE, shufflesections = FALSE, zip = TRUE, points = NULL,
    eval = list(partial = TRUE, negative = FALSE),
    converter = NULL, envir = NULL, base64 = TRUE, mode = "hex",
    include = NULL, selection = c("pool", "exam"), \dots)

  make_itembody_qti21(shuffle = FALSE, defaultval = NULL,
    minvalue = NULL, maxvalue = NULL, enumerate = TRUE,
    digits = NULL, tolerance = is.null(digits), maxchars = 12,
    eval = list(partial = TRUE, negative = FALSE),
    solutionswitch = TRUE, casesensitive = TRUE,
    cloze_schoice_display = c("auto", "buttons", "dropdown"))
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
  \item{num}{function or named list applied to numerical (i.e., type \code{num})
    questions. If \code{num} is a function, \code{num} will be used
    for generating the item body of the question, see function \code{make_itembody_qti21()}.
    If \code{num} is a named list, these arguments will be passed to function
    \code{make_itembody_qti21()}.}
  \item{mchoice, schoice, string, cloze}{function or named list applied to multiple choice,
    single choice, string, and cloze questions (i.e., type \code{mchoice}, \code{schoice},
    \code{string}, and \code{cloze}), respectively. See argument \code{num} for more details.}
  \item{template}{character. The QTI 2.1 template that should be used. Currently,
    the package provides \code{"qti21.xml"}.}
  \item{duration}{integer. Set the duration of the exam in minutes.}
  \item{stitle}{character. A title that should be used for the sections. May be a vector of
    length 1 to use the same title for each section, or a vector containing different section
    titles.}
  \item{ititle}{character or \code{NULL}. A title that should be used for the assessment items.
    May be a vector
    of length 1 to use the same title for each item, or a vector containing different item
    titles. Note that the maximum of different item titles is the number of sections/questions
    that are used for the exam. Note, if \code{ititle = NULL}, simple numbering for the exercises
    will be used, however, if the exercise contains meta info extitle, this title will be used.}
  \item{adescription}{character. Description (of length 1) for the overall assessment (i.e., exam).}
  \item{sdescription}{character. Vector of descriptions for each section, omitted if empty
    (or \code{NULL} or \code{FALSE}).}
  \item{maxattempts}{integer. The maximum attempts for one question. This may also be a vector
    so that the maximum number of attempts varies across questions. A value of \code{Inf} or
    \code{0} signals that the attempts per question are not limited.}
  \item{cutvalue}{numeric. The number of points at which the exam is passed. If set to
    \code{NULL} (default) or equivalently \code{NA}, no cutvalue is set and the result
    of the exam is just the number of points.}
  \item{solutionswitch}{logical. Should the question/item solutionswitch be enabled?
    In OLAT this means that the correct solution is shown after an incorrect solution
    was entered by an examinee (i.e., this is typically only useful if \code{maxattempts = 1}).}
  \item{casesensitive}{logical. Should the evaluation of string exercises be case sensitive?}
  \item{cloze_schoice_display}{character. For \code{schoice} answers in \code{cloze} exercises,
    select the display of the possible answers. By default (\code{cloze_schoice_display = "auto"}),
    radio \code{"buttons"} are used if the answer list appears in its own paragraph and
    a \code{"dropdown"} menu is used if the answer list appears inline (and has no mathematical
    markup). Both options can also be enforced explicitly, independently from the answer list
    appearing in a separate paragraph or inline. Note that in dropdown menus the answer list
    will typically be rendered as plain text which means that mathematical notaion may become
    unintelligible.}
  \item{navigation}{character. Mode of navigation, can either be \code{"nonlinear"} (default)
    or \code{"linear"}. The former means that test participants can switch back and forth
    between questions while the latter implies that the questions need to be answered sequentially.}
  \item{allowskipping}{logical. Can a question/section be skipped (default) or must it be answered?}
  \item{allowreview}{logical. Can questions be viewed again at the end of a test/exam or not (default)?}
  \item{allowcomment}{logical. Are comments allowed?}
  \item{shufflesections}{logical. Should the order of the
     exercises be shuffled? For \code{selection = "pool"} setting
     \code{shufflesections = TRUE} this corresponds to shuffling the
     sections that contain the pools of exercises. For
     \code{selection = "exam"} it corresponds to shuffling the exercises
     within each exam section.}
  \item{zip}{logical. Should the resulting XML file (plus supplements) be zipped?}
  \item{points}{integer. How many points should be assigned to each exercise? Note that this
    argument overules any exercise points that are provided within an \code{"\expoints{}"} tag
    in the \code{.Rnw} file. The vector of points supplied is expanded to the number of exercises in
    the exam.}
  \item{eval}{named list, specifies the settings for the evaluation policy, see function
    \code{\link{exams_eval}}.}
  \item{converter}{character. Argument passed on to \code{make_exercise_transform_html}.
    The default for \code{converter} is set to \code{"ttm"} unless there are Rmd
    exercises in \code{file} where \code{"pandoc"} is used.}
  \item{envir}{passed to \code{\link[exams]{xweave}}.}
  \item{base64}{logical. Should supplementary files be embedded using Base 64 coding?
    Argument \code{base64} may also be a character vector of file suffixes that should be encoded, e.g.
    \code{base64 = c("png", "rda")} will only encode PNG images and binary \code{.rda} files.
    If set to \code{NULL} only image files will be encoded.}
  \item{mode}{character. See function \code{\link[tth]{tth}}.}
  \item{include}{character, paths of extra files that should be included within the final
    \code{.zip}-file.}
  \item{selection}{character. If \code{selection = "pool"}, the function creates one section
    for each exercise from which one replication will be selected in the exam. If
    \code{selection = "exam"} each section contains all questions and one section will
    be selected for the exam. The \code{"exam"} variant has the advantage that questions
    that build on each other can be used in the exam.}
  \item{shuffle, defaultval, minvalue, maxvalue}{arguments used for item construction,
    for details see the XML specification (see IMS Global Learning Consortium, Inc. 2012),
    especially Section 4.}
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
  The Question & Test Interoperability (QTI) is an international XML standard for specifying
  e-learning tests established by the IMS Global Learning Consortium, Inc. (2002, 2012).
  The standard evolved over various versions with the first release culminating in the QTI 1.2 standard
  and the most commonly used stable version of the second release being QTI 2.1.
  While both versions share many similarities, they also differ in many details.
  Hence, separate functions \code{\link[exams]{exams2qti12}} and \code{exams2qti21} are provided.
  Moreover, due to the flexibility of the QTI standard, different learning management systems employ
  the standard in slightly different ways. Therefore, dedicated interfaces
  \code{\link{exams2canvas}} and \code{\link{exams2ilias}} (both based on QTI 1.2),
  and \code{\link{exams2openolat}} (based on either QTI 2.1 or 1.2) are provided
  for the learning management systems Canvas, ILIAS, and OLAT/OpenOlat, respectively.
  In addition, the interfaces \code{\link[exams]{exams2blackboard}}, \code{\link[exams]{exams2testvision}},
  and the separate package \pkg{exams2sakai} started as forks of the \code{exams2qti12} and
  \code{exams2qti21} functions, respectively.

  \code{exams2qti21} produces a \code{.zip} file that may be uploaded into different learning
  management systems (see above). This includes the final XML file of the exam/assessment,
  its exercises as well as possible supplement folders that include images, data sets etc.
  It proceeds by (1) calling \code{\link[exams]{xweave}} for each exercise, (2) reading the
  resulting Markdown and/or LaTeX code using \code{\link[exams]{read_exercise}},
  (3) transforming the Markdown/LaTeX code to HTML, and (4) embedding the HTML code in XML
  files using the QTI 2.1 standard for assessments and question items.

  For steps (1) and (2) the standard drivers in \code{xexams} are used. In step (3), a suitable
  transformation function is set up on the fly using \code{make_exercise_transform_html}, see also
  the details section in \code{\link{exams2html}}.
  For step (4), the function will cycle through all questions and exams to generate the final XML
  files in QTI 2.1 format: separate XML files for each random replication of each exercise (labeled
  items in sections in QTI), one XML file tying the overall exam/assessment together based on
  the individual exercises and one overall \file{imsmanifest.xml}.
  
  For arranging the individual exercise replications (called "items") in so-called "sections"
  two different types of \code{selection} are available: First, a \code{"pool"} of replications/items
  can be put into a section and then one item will be selected randomly for each participant from
  each section. As learning management systems typically sample the sections independently, this means that
  one participant might receive the first random replication from
  the first exercise but the third random replication from the second exercise, and so on.
  Alternatively, \code{selection = "exam"} specifies that each section contains
  a complete set of exercises and one section will be selected randomly for each participant.
  The \code{"exam"} variant is less commonly used but has the advantage that questions that build
  on each other can be used in the exam. Also, duplicated exercise types can be avoided when
  using \code{nsamp} to sample a certain number of exercises without replacement.
  
  Templates other than the default \code{qti21.xml} provided in the \code{xml} folder of
  this package could in principle also be specified. However, adapting these is a bit technical.
  The assessment template must provide one section including one item. \code{exams2qti21} will then
  use the single item template to generate all items, as well as the assessment and section specifications set within the template.
  Note that all specifiers that have a leading \code{##} in the XML template will be replaced by suitable code in
  \code{exams2qti21} and should always be provided in the template. Thus, the user may add
  additional tags to the XML template or modify certain specifications, like the number of
  replicates/items that should be sampled for each section etc.

  By default, the individual question/item bodies are generated by function
  \code{make_itembody_qti21} which checks the type of the question and
  will produce suitable XML code. Note that for each question type, either the arguments of
  \code{make_itembody_qti21} may be set within \code{num}, \code{mchoice}, \code{schoice},
  \code{string} and \code{cloze} in \code{exams2qti21}, by providing a named list of specifications
  that should be used, or for each question type, a function that produces the item body XML code may
  be provided to \code{num}, \code{mchoice}, \code{schoice}, \code{string} and \code{cloze}. E.g.,
  \code{mchoice = list(shuffle = TRUE)} will force only multiple choice questions to have a shuffled
  answerlist.
}

\references{
IMS Global Learning Consortium, Inc. (2002).
\emph{IMS Question & Test Interoperability: ASI XML Binding Specification Final Specification Version 1.2}.
\url{https://www.imsglobal.org/question/qtiv1p2/imsqti_asi_bindv1p2.html}

IMS Global Learning Consortium, Inc. (2012).
\emph{IMS Question & Test Interoperability (QTI) XSD Binding Version 2.1 Final}.
\url{https://www.imsglobal.org/question/qtiv2p1/imsqti_bindv2p1.html}

Zeileis A, Umlauf N, Leisch F (2014).
  Flexible Generation of E-Learning Exams in R: Moodle Quizzes, OLAT Assessments, and Beyond.
  \emph{Journal of Statistical Software}, \bold{58}(1), 1--36.
  \doi{10.18637/jss.v058.i01}.
}

\value{
  \code{exams2qti21} returns a list of exams as generated by \code{\link[exams]{xexams}}.

  \code{make_itembody_qti21} returns a function that generates the XML code
  for the itembody tag in QTI 2.1 format.
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

## generate .zip with QTI 2.1 exam in temporary directory
## using a few customization options
exams2qti21(myexam, n = 3, dir = mydir,
  maxattempts = 3,
  num = list(digits = 1),
  mchoice = list(shuffle = TRUE, enumerate = FALSE)
)
dir(mydir)
}

\keyword{utilities}
