\name{exams2qti12}
\alias{exams2qti12}
\alias{make_itembody_qti12}

\title{Generation of Exams in IMS QTI 1.2 Format}

\description{
  Automatic generation of exams in IMS QTI 1.2 format.
}

\usage{
  exams2qti12(file, n = 1L, nsamp = NULL, dir = ".",
    name = NULL, quiet = TRUE, edir = NULL,
    tdir = NULL, sdir = NULL, resolution = 100,
    width = 4, height = 4, encoding = "",
    num = NULL, mchoice = NULL,
    schoice = mchoice, string = NULL, cloze = NULL,
    template = "qti12", duration = NULL,
    stitle = "Exercise", ititle = "Question",
    adescription = "Please solve the following exercises.",
    sdescription = "Please answer the following question.", 
    maxattempts = 1, cutvalue = 0, solutionswitch = TRUE,
    zip = TRUE, points = NULL,
    eval = list(partial = TRUE, negative = FALSE),
    \dots)

  make_itembody_qti12(rtiming = FALSE, shuffle = FALSE,
    rshuffle = shuffle, minnumber = NULL, maxnumber = NULL,
    defaultval = NULL, minvalue = NULL, maxvalue = NULL,
    cutvalue = NULL, enumerate = TRUE, digits = NULL,
    tolerance = is.null(digits), maxchars = 12,
    eval = list(partial = TRUE, negative = FALSE))
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
  \item{encoding}{character, passed to \code{\link[utils]{Sweave}}.}
  \item{num}{function or named list applied to numerical (i.e., type \code{num})
    questions. If \code{num} is a function, \code{num} will be used
    for generating the item body of the question, see function \code{make_itembody_qti12()}.
    If \code{num} is a named list, these arguments will be passed to function
    \code{make_itembody_qti12()}.}
  \item{mchoice, schoice, string, cloze}{function or named list applied to multiple choice, 
    single choice, string, and cloze questions (i.e., type \code{mchoice}, \code{schoice},
    \code{string}, and \code{cloze}), respectively. See argument \code{num} for more details.}
  \item{template}{character. The IMS QTI 1.2 template that should be used. Currently,
    the package provides \code{"qti12.xml"}.}
  \item{duration}{integer. Set the duration of the exam in minutes.}
  \item{stitle}{character. A title that should be used for the sections. May be a vector of
    length 1 to use the same title for each section, or a vector containing different section
    titles.}
  \item{ititle}{character. A title that should be used for the assessement items. May be a vector 
    of length 1 to use the same title for each item, or a vector containing different item
    titles. Note that the maximum of different item titles is the number of sections/questions
    that are used for the exam.}
  \item{adescription}{character. Description (of length 1) for the overall assessment (i.e., exam).}
  \item{sdescription}{character. Vector of descriptions for each section.}
  \item{maxattempts}{integer. The maximum attempts for one question, may also be set to
    \code{Inf}.}
  \item{cutvalue}{numeric. The cutvalue at which the exam is passed.}
  \item{solutionswitch}{logical. Should the question/item solutionswitch be enabled?
    In OLAT this means that the correct solution is shown after an incorrect solution
    was entered by an examinee (i.e., this is typically only useful if \code{maxattempts = 1}).}
  \item{zip}{logical. Should the resulting XML file (plus supplements) be zipped?}
  \item{points}{integer. How many points should be assigned to each exercise? Note that this
    argument overules any exercise points that are provided within an \code{"\expoints{}"} tag
    in the \code{.Rnw} file. The vector of points supplied is expanded to the number of exercises in
    the exam.}
  \item{eval}{named list, specifies the settings for the evaluation policy, see function
    \code{\link{exams_eval}}.}
  \item{rtiming, shuffle, rshuffle, minnumber, maxnumber, defaultval, minvalue,
    maxvalue}{arguments used for IMS QTI 1.2 item construction, for details see the XML 
    specification (see IMS Global Learning Consortium, Inc. 2012), especially Section 4.}
  \item{enumerate}{logical. Insert potential solutions in enumerated list?}
  \item{digits}{integer. How many digits should be used for \code{num} exercises?}
  \item{tolerance}{logical. Should tolerance intervals be used for checking if the supplied
    \code{num} answer/number is correct? The default is to use tolerance intervals if \code{digits = NULL}.}
  \item{maxchars}{numeric. Lower bound for the number of characters in fill-in-blank
    fields. The actual number of characters is selected as the maximum number of
    characters of this value and the actual solution.}
  \item{\dots}{arguments passed on to \code{make_exercise_transform_html}.}
}

\details{
  \code{exams2qti12} will produce a \code{.zip} file that may be uploaded (e.g. in OLAT) including
  the final XML file of the exam/assessement as well as possible supplement folders that include
  images, data sets etc. used for the exam. It proceeds by (1) calling \code{\link[utils]{Sweave}}
  on each exercise, (2) reading the resulting LaTeX code, (3) transforming the LaTeX code to HTML,
  and (4) embedding the HTML code in a XML file using the IMS QTI 1.2 standards for assessements and
  question items.

  For steps (1) and (2) the standard drivers in \code{xexams} are used. In step (3), a suitable
  transformation function is set up on the fly using \code{make_exercise_transform_html}, see also
  the details section in \code{\link{exams2html}}.

  For step (4), the function will cycle through all questions and exams to generate the final XML
  file in IMS QTI 1.2 standard. Therefore, each question will be included in the XML as one section.
  The replicates of each question will be written as question items of the section.

  The function uses the hard-coded XML template for IMS QTI 1.2 assessements and items to generate
  the exam (per default, this is the XML file \code{qti12.xml} provided in the \code{xml} folder of
  this package). The assessement template must contain of one section including one item.
  \code{exams2qti12} will then use the single item template to generate all items, as well as the
  assessement and section specifications set within the template.

  The default template will generate exams/assessements that sample one replicate of a question/item
  for each section. The ususal procedure in exam/assessement generation would be to simply copy \&
  paste the XML template of the package and adapt it to the needs of the user. Note that all
  specifiers that have a leading \code{##} in the XML template will be replaced by suitable code in
  \code{exams2qti12} and should always be provided in the template. I.e., the user may add
  additional tags to the XML template or modify certain specifications, like the number of
  replicates/items that should be sampled for each section etc.

  Per default, the individual question/item bodies are generated by function
  \code{make_itembody_qti12}, i.e. \code{make_itembody_qti12} checks the type of the question and
  will produce suitable XML code. Note that for each question type, either the arguments of
  \code{make_itembody_qti12} may be set within \code{num}, \code{mchoice}, \code{schoice},
  \code{string} and \code{cloze} in \code{exams2qti12}, by providing a named list of specifications
  that should be used, or for each questiontype, a function that produces the item body XML code may
  be provided to \code{num}, \code{mchoice}, \code{schoice}, \code{string} and \code{cloze}. E.g.,
  \code{mchoice = list(shuffle = TRUE)} will force only multiple choice questions to have a shuffled
  answerlist.
  
  Note that in OLAT/OpenOLAT \code{num} exercises are not officially supported but in fact work
  correctly. The only drawback is that the correct solution is not shown at the end of the assessment
  (although it is used for all internal computations). A potential workaround is to implement
  \code{num} exercises via \code{string} by exactly matching the given solution to the correct
  solution at a given rounding. This may be controlled by argument in \code{digits} in \code{make_itembody_qti12}.  
  I.e. in a test, the answer must be provided exactly to the decimal places specified in \code{digits} and has no upper
  or lower tolerance level (e.g., if the exact solution is 16.4562 and \code{digits = 2}, then the
  correct answer in the test will be \code{"16.46"}, i.e., a character string of 5 characters).
}

\references{
  IMS Global Learning Consortium, Inc. (2012).
  \emph{IMS Question & Test Interoperability: ASI XML Binding Specification Final Specification Version 1.2}.
  \url{http://www.imsglobal.org/question/qtiv1p2/imsqti_asi_bindv1p2.html}
}

\value{
  \code{exams2qti12} returns a list of exams as generated by \code{\link[exams]{xexams}}.
  
  \code{make_itembody_qti12} returns a function that generates the XML code
  for the itembody tag in IMS QTI 1.2 format.
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

## generate .zip with QTI 1.2 exam in temporary directory
## using a few customization options
exams2qti12(myexam, n = 3, dir = mydir,
  maxattempts = 3,
  num = list(digits = 1),
  mchoice = list(shuffle = TRUE, enumerate = FALSE)
)
dir(mydir)
}

\keyword{utilities}
