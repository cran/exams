# exams 2.3-5

* New function `expar()` that helps to fix parameters (defined in the first
  code chunk of an exercise). For example `expar("deriv.Rmd", a = 1, c = 0)`
  generates a temporary file where the parameters `a` and `c` of the
  `deriv.Rmd` exercise are fixed to the values `1` and `0`, respectively.
  This can then be processed with `exams2xyz()` "as usual".

* Added `tinytex::latexmk()` as an alternative to `tools::texi2dvi()` when
  converting to PDF output (`exams2nops()`, `exams2pdf()`, ...) as suggested
  by Yihui Xie. The default is to use tinytex `if(requireNamespace("tinytex"))`.
  However, by setting `options(exams_tex = "tools")` instead of
  `exams_tex = "tinytex"` the old behavior can be enforced even if tinytex
  is installed. When no other LaTeX distribution (like TeXLive or MikTeX)
  is installed, yet, `tinytex::install_tinytex()` can easily install the
  TeXLive-based TinyTeX system.

* Started enabling setting random `seed`s for each individual exercise
  in an exam, to enable custom exercise selection (e.g., when only
  putting together a single exam). Currently, only supported in 
  `xexams()`, `exams2pdf()`, `exams2html()`, `exams2nops()`, and `exams2blackboard()`.

* To make the fixed `seed`s above applicable to static exercises where
  only the `exshuffle` tag is enabled, the `seed` is not only set prior
  to weaving but also to reading the exercise. 

* Improved random shuffling if `exshuffle` is numeric where previously the
  correct solution(s) were not completely randomly distributed across the
  possible answers.

* In addition to the vector or list specification of the exercise `file`
  in the `exams2xyz()` interfaces, `file` can now also be a matrix. This
  enables customization of the exact selection of exercises in each
  exam. The `seed` can be a matrix of the same dimension. Currently,
  only supported in  `xexams()`, `exams2pdf()`, `exams2html()`, `exams2nops()`,
  and `exams2blackboard()`.

* Improved NOPS language support: Added Korean (`ko`, Saewon Jeong),
  Japanese (`ja`, Kohei Watanabe), Norwegian (Bokmal, `no`, Tormod Boe),
  Slovenian (`sl`, Matjaz Jeran), Vietnamese (`vi`, Tran Thi Hoang Ha).
  Some languages require that markboth/pagestyle are set after
  `\begin{document}` and that a `\newpage` is included directly before
  `\end{document}`, both of which were adapted correspondingly.
  
* Improvements in `nops_eval()`: Show exam ID in HTML report and round
  points to four digits. The `language=` file paths do not have to be
  absolute anymore. For `interactive` checking/fixing of registration IDs
  the width of scanned subimage is now adapted according to the `reglength`.

* The actual writing of nops_eval results has been modularized (with contributions
  from Kenji Sato). Different `nops_eval_write_<flavor>` functions can be plugged in.
  At the moment there is only the default writer (optimized for OpenOLAT)
  but further flavors are planned (including a standalone workflow and
  one for Moodle).

* Function `nops_language()` is now exported as part of the user interface
  (only internal previously). Based on a language specification in a DCF
  file this sets up a list with language texts (possibly converted to HTML).

* New exercise `capitals.Rmd`/`Rnw` that illustrates how a certain number
  of true/false items can be chosen randomly from a longer list. The exercise
  can also be switched from `mchoice` to `schoice` without any further
  necessary modifications. Finally, the exercise illustrates that UTF-8
  encoding is needed for the Rmd version while in the Rnw version the few
  special characters can be coded in ASCII using LaTeX commands.

* All LaTeX templates that use `\fontfamily{phv}` now also gained a
  `\usepackage{helvet}` to ensure that the corresponding LaTeX packages
  are installed. When using `tinytex` (see above) the corresponding LaTeX
  packages will be installed if not yet available upon first usage.

* In R/Markdown exercises the question/solution answerlist can now use
  not only `*` but also `-` as the symbol for bullet points (suggested
  by Ben Kasel).

* Fixed a bug in `exams2pdf()` where the case of multiple duplicated supplement
  names was not handled correctly (reported by TwelveFifths on StackOverflow).

* Improved `xexams()` so that full file paths to exercise files also work on
  Windows.

* `read_metainfo()` now assures a non-`NULL` `exname`. The default is to use
  "R/exams exercise".

* Fixed a bug in `extract_items()` which incorrectly dropped the last element
  in an answerlist if it was completely empty (even without a trailing space).
  This was a problem for cloze exercises with ##ANSWER## patterns, leading to
  a questionlist that was too short (reported by Julia Guggenberger).

* Various fixes and improvements in `exams2qti21()` (and thus inherited by
  `exams2openolat()`): Support of cloze and essay type exercises has been
  much improved, fixing bugs in the XML and meta-information handling.
  String exercises work again. The internally-generated XML labels are
  more robust now avoiding spaces and leading integers.
  
* Dependency on R >= 3.4.0 now which enables plugging a custom svg device
  into `Sweave()`. In previous versions of the package a workaround was
  included that work for R < 3.4.0 (but required writing into the global
  environment which is avoided now).


# exams 2.3-4

* Avoid execution of examples on CRAN that would necessitate `pandoc` availability.
  Currently, the `rmarkdown` package but not the underlying `pandoc` support
  is installed for the CRAN checks on Solaris and OS X.


# exams 2.3-3

* Added new interface `exams2canvas()` for the open-source Canvas learning
  management system (<https://www.instructure.com/canvas/>). This is essentially
  a convenience wrapper to `exams2qti12()` along with a few Canvas-specific
  modifications of the QTI XML specification. The function has only received
  limited testing so far and is likely to improve in future version. The
  supported exercise types are num, schoice, mchoice, and string (but not
  cloze, yet).

* The default in `exams2moodle()` has been changed to `converter = "pandoc-mathjax"`.
  Thus, instead of relying on the browser to render mathematical notation
  from MathML, the MathJax plugin is now assumed to be enabled on the
  Moodle server (which is the default when setting up a new Moodle server).
  To employ MathML one can use `converter = NULL` or `converter = "pandoc-mathml"`
  etc. See the discussion at <http://www.R-exams.org/tutorials/math/> for more
  details.

* `exams_skeleton()` has been updated. The new default is `markup = "markdown"`
  as this appears to be the more common choice for newcomers to R/exams.
  Furthermore, the list of exercises in the `demo-*.R` scripts has been
  updated to use newer exercises as well (along with links to the web
  page: <http://www.R-exams.org/templates/>).

* Assure in `nops_scan()` that scanned image file names contain no spaces.

* Fixed bugs in `exams2qti21()` (and hence `exams2openolat()`) for certain
  combinations of `maxchars` specifications in `string` exercises.

* Added `\usepackage[...]{babel}` support to `language` specification of
  `exams2nops()`. In addition to a new `Babel: ...` field, the DCF file can
  also provide a `Header: ...` field, e.g., for changing the font encoding
  or adding further options.

* For convenience, when `exams2html(..., converter = "pandoc-mathjax")`,
  `mathjax = TRUE` is added automatically (unless explicitly switched off).

* Added `envir` argument in `exams2moodle()` that is passed on to `xweave()`.

* The `print()` method for `exams_metainfo` objects received a new `block`
  argument so that `print(exams_metainfo(x), which = 1, block = 5)` prints
  the metainformation (type/solution) of the first exam in the object `x`
  (output from any `exams2xyz()` function) in blocks of 5 exercises.

* For `exams2openolat()` the handling of the internal `pandoc` fixups was
  extended to replace `align="right"` in tables with `style="text-align: right;"`
  (and analogously for `"left"` and `"center"`).

* In `nops_eval()` duplicated sheets (original and replacement) were sometimes
  omitted even if the student ID was just `000000` (i.e., not read correctly
  by `nops_scan()`). A check has been added that avoids this.

* Changed the default of `fix_choice` in `exams2arsnova()`. Current versions of
  the official ARSnova server (<https://arsnova.eu/>) have the LaTeX rendering
  in the choice options switched off. Hence, by setting `fix_choice = TRUE`
  by default the LaTeX math markup is removed.

* Add `\setkeys{Gin}{keepaspectratio}` in the default `exams2pdf()` template for
  `pandoc` (`plain8.tex`) and in `exams2nops()`. This is necessitated by a change
  in `pandoc` that now always sets `height=\textheight` when `width=` is specified
  (see <https://pandoc.org/faqs.html>).


# exams 2.3-2

* `nops_scan()` gained a new argument `trim = 0.3` that controls how much of
  check boxes is trimmed in order to shave the borders prior to determining
  the average gray level. In versions up to 2.3-1 this implicitly had a
  value of `0.25` hard-coded. Now the default was increased to `0.3` in order
  to shave box borders more reliably.

* `nops_scan()` tries to process registration numbers more reliably.
  In case one of the registration columns contains more than one potential
  mark, the heuristics of determining the intended mark have been improved.

* New exercise `confint3.Rmd`/`Rnw` that illustrates how to use the `"verbatim"`
  `clozetype` for Moodle. The exercises yields 100% of the points for the
  correct solution based on t quantiles but still 50% for a partially
  correct solution based on normal quantiles (contributed by Ulrike
  Groemping).

* `nops_eval()` gained a new argument `labels = NULL` that can be used to
  give labels for the marks that differ from the default
  `(length(mark) + 1):1`.

* In all LaTeX templates that use Helvetica (phv) as the font for the main
  text, this is also used now in math mode by `\usepackage[helvet]{sfmath}`
  (rather than `\usepackage{sfmath}`, as employed previously). In particular,
  this also affects `exams2nops()` and `tex2image()` output.

* In `exams2nops()` the `header` argument can also be specified simply as
  `header = "\\mycommand{value}"` rather than `header = list(mycommand = "value")`.
  The former is more flexible, e.g., for passing additional options or more
  complex commands. Internally, the former is turned into an unnamed list
  which is then processed correspondingly by `exams2pdf()`.

* Various bug fixes in `exams2qti21()` pertaining to points and correct/incorrect
  computations for schoice/mchoice exercises.

* New language support in exams2nops: Russian (`ru`, contributed by
  Boris Demeshev) and Serbian (`sr`, contributed by Tatjana Kecojevic).
  Croatian (`hr`) was streamlined along with Serbian (by Tatjana Kecojevic).
  
* In `include_supplement()` an argument `target = NULL` was added to optionally
  include the supplement with a different file name than the original
  file name.

* Bug fix in the `pandoc` interface which previously erroneously produced
  unbalanced <p> tags in certain situations.

* Rather than fully importing the basic dependencies `stats`, `graphics`,
  `grDevices`, `tools`, and `utils`, the required functions are imported
  selectively only. (Issue raised by Florian Oswald.)


# exams 2.3-1

* Added new interface `exams2openolat()` for the open-source OpenOLAT learning
  management system (<https://www.openolat.com/>). This is only a convenience
  wrapper to `exams2qti12()` or `exams2qti21()` with some dedicated tweaks
  for optimizing MathJax output for OpenOLAT.

* When using `exams2html(..., mathjax = TRUE)` for testing purposes,
  mathjax.rstudio.com is used now rather than cdn.mathjax.org which
  is currently redirecting and will eventually be shut down completely.

* Bug fixes and improvements in HTML transformers:
  - Only `="file.ext"` (with `="`) is replaced now by Base 64 version.
  - `href="file.ext"` is replaced by `href="file.ext" download="file.ext"`
    prior to Base 64 replacement.
  - `alt="file.ext"` and `download="file.ext"` are preserved without
    Base 64 encoding.

* In `exams2html()` and other interfaces based on `make_exercise_transform_html()`
  `base64 = TRUE` now uses Base 64 encoding for all file extensions (known
  to the package) whereas `base64 = NULL` only encodes image files (previous
  default behavior).

* After setting a random seed `exams2nops()` and `exams2pdf()` now yield the
  same random versions of the exercises. Previously, this was not the case
  because `exams2nops()` internally generates a single random trial exam first for
  a couple of sanity checks. Now, the `.GlobalEnv$.Random.seed` is restored
  after generating the trial exam.

* Fixed the support for `nsamp` argument in `exams2nops()`. Furthermore,
  current limitations of `exams2nops()` are pointed out more clearly and
  error messages and edge cases caught.

* Include further file URIs for Base 64 supplements, in particular .sav
  for SPSS data files.

* New language support in `exams2nops()`: Croatian (`hr`, contributed by
  Krunoslav Juraic), Danish (`da`, contributed by Tue Vissing Jensen & Jakob
  Messner), Slovak (`sk`, contributed by Peter Fabsic), Swiss
  German (`gsw`, contributed by Reto Stauffer), Turkish (`tr`, contributed by
  Emrah Er). Furthermore, Portuguese has been distinguished into
  `pt-PT` (Portuguese Portuguese) vs. `pt-BR` (Brazilian Portuguese) with
  `pt` defaulting to the former (contributed by Thomas Dellinger).

* `include_supplement(..., dir = "foo")` now also works if `"foo"` is
  sub-directory to the exercise directory (`edir`).

* Allow varying points within a certain exercise in `nops_eval()`.

* `exams2blackboard(..., base64 = FALSE, ...)` was erroneously ignored.
  No matter how base64 was specified essentially `base64 = TRUE` was used,
  it is honored again now.

* Fixed a bug in `stresstest_exercise()` where the "rank" (previously called
  "order") of the correct solution was computed incorrectly. Enhancement
  in plots and labels.

* Fixed a bug for `tex2image(..., tikz = TRUE)` where erroneously
  `\usetikzlibrary{TRUE}` was included. Also `tex2image(..., Sweave = TRUE)`
  (the default) did not run properly on Windows, fixed now.

* New function `include_tikz()` that facilitates compiling standalone
  tikz figures and including it in an exercise (especially for
  non-LaTeX-based output).

* Added support for `\tightlist` (as produced by `pandoc`) in all current
  LaTeX templates as well as `exams2nops()`.

* `\exshuffle{<num>}` can now also be used for schoice exercises with more
  than one true answer. In a first step only one of the true answers is
  selected and then `<num>-1` items from the false answers.
  
* Better warnings if `\exshuffle{<num>}` could not be honored due to a lack of
  sufficiently many (suitable) answer alternatives.

* Enable passing of `envir` argument from `exams2html()` to `xweave()` in
  case of Rmd exercises.

* Bug fix in CSV export of `exams2arsnova()`. Recent ARSnova versions use
  "mc" (rather than "MC") and "abcd" (rather than "SC") for multiple-choice
  and single-choice questions, respectively.


# exams 2.3-0

* A new web page accompanying the package is now available at
  <http://www.R-exams.org/>. This already contains a first overview of the
  package, some tutorials, and a gallery of all exercise templates
  provided in the package. More tutorials will follow in the form of
  blog articles.
  
* A few exercise templates were slightly changed or improved for the new
  web page. Hence slightly different formulations or formatting may be
  used in some places.

* `tex2image()` has been rewritten to use the LaTeX class {standalone}
  instead of manually handling boxes and cropping of the PDF output.
  For converting to raster graphics like PNG (the default), R package
  `magick` is now used rather than `system("convert ...")` calls to
  ImageMagick on the system. For conversion to SVG it is still necessary
  to call `pdfcrop` and `pdf2svg` on via `system()`.

* More fixups in LaTeX code prior to processing with `pandoc_convert()`,
  especially `\url{...}` is replaced by `\href{...}{\texttt{...}}` now to
  ensure typesetting in typewriter font.

* When including R input code chunks in Rmd exercises, code decoration
  is switched off by default now (knitr option `highlight = FALSE`) so that
  the output can be combined with standard LaTeX templates. The default
  can be changed either in the individual Rmd code chunks, e.g.,
  {r, highlight=TRUE}, or by setting `highlight = TRUE` in the call to
  `xweave()`. For the latter case a new LaTeX template `plain-highlight.tex`
  is also provided, e.g., to be used via:
  `exams2pdf("lm.Rmd", highlight = TRUE, template = "plain-highlight")`

* `exams2pdf()` gained an argument `texdir = NULL`. By default a temporary
  directory is created for running LaTeX (and cleaned up subsequently).
  But using `texdir = "/path/to/dir"` an output directory can be specified
  so that the LaTeX files can be inspected, e.g., for debugging.

* In `exams2pdf()` the `control` argument was extended to encompass a new
  `cloze.collapse` option to fine tune the display of mchoice/schoice
  alternatives within a cloze subexercise. By default the separator is
  `" / "` for collapsing the choice alternatives. Alternatively, for example,
  it could be set to `"\\\\"` to add line breaks between the alternatives.
  Finally, `control = list(cloze.collapse = "enumerate")` uses a nested
  enumerate environment.

* `nops_scan()` now uses the `dir` argument not only for the output ZIP
  file but also for the input PDF/PNG image files.

* `exams2pdf()` and hence also `exams2nops()` now try to `iconv()` the header
  information to the specified `encoding` (if any) rather than relying
  on header information (such as institution name etc.) are specified
  in LaTeX.


# exams 2.2-1

* Added new interface `exams2pandoc()` which can generate a wide variety
  of output formats via `pandoc`. The main purpose is to generate Docx
  output files (or ODT). The output format can be controlled through
  a template in either LaTeX, HTML, or Markdown format.

* New function `stresstest_exercise()` for "stress testing" of exercises,
  i.e., rerunning the exercise many times and capturing how the outcome
  depends on the simulated input parameters.

* New (and still somewhat experimental) function `match_exams_call()`
  that can be employed to query which `exams2xyz()` function was called
  to create a certain exercise/exam. Only works from R 3.2.0 onwards.

* Bug fix in `read_exercise()` for the case when `exshuffle` is integer.
  This was not correctly reflected in `metainfo$length`.

* Avoid <p> formatting in questionlist/solutionlist when using
  `converter = "pandoc"` in the conversion to HTML.

* Formatting of exercises without "solution" section improved in
  `exams2html()` and `exams2moodle()`.

* In `exams2qti12()` the case of no answer was not handled correctly
  if `partial = FALSE, negative = ...` was used. No answer was handled
  as incorrect (= negative points) rather than neutral (= zero points)
  but has been fixed now.


# exams 2.2-0

* Added first release version of `exams2blackboard()`. This is essentially
  based on `exams2qti12()` but incorporates Blackboard's own flavor of
  QTI 1.2. It is still likely to change due to improvements in future
  versions. After uploading a test into Blackboard the material will
  appear under "Course Tools": The test will be available in "Tests"
  and each pool within the test will also appear in "Pools".

* Added a new convenience function `include_supplement()` which facilitates
  including static supplement files (e.g., graphics or datasets). This
  essentially just calls `file.copy()` but facilitates finding supplement
  files that are available along with the Rnw/Rmd exercise files.

* `xweave()` gained an extra argument `svg = FALSE` to optionally produce scalable
  vector graphics (SVG) instead of PNG for later rendering into HTML. All
  HTML-based `exams2xyz()` functions gained an `svg = FALSE` argument that can be
  set to `TRUE` to produce SVG instead of PNG. Base64 encoding is also used
  for SVG graphics by default.

* `xweave()` gained an argument `engine = NULL` that can optionally be set to
  `engine = "knitr"` to enable processing Rnw exercises by `knitr::knit()` instead
  of the default `utils::Sweave()`.

* `exams2nops()` now supports registration IDs with more than 7 digits.
  Up to now, 7 digits were hard-coded but now values of 8, 9, or 10 are
  also possible by setting the argument `reglength` in `exams2nops()`
  accordingly.

* In `exams2nops()` the argument `blank` can now be a vector of length 2
  with the number of blank pages before and after the extra "pages".

* Added arguments `samepage = FALSE` and `twocolumn = FALSE` in `exams2nops()`.
  If set to `TRUE`, samepage adds a {samepage} environment around the
  question and solution lists in order to prevent page breaks within
  these lists. And the twocolumn option, if set to `TRUE`, is added in
  the `\documentclass` so that the exam is displayed in two-column
  format.

* New language support in `exams2nops()`: Hungarian (`hu`, contributed by
  Gergely Daroczi and Denes Toth), Finnish (`fi`, contributed by Klaus
  Nordhausen). Improvements in: Portugese (`pt`, contributed by Mauricio
  Calvao).

* Improvements and bug fixes in `exams2tcexam()`. Various HTML tags are now
  supported in the development version by adding a new [html] tag in TCExam.
  This has been integrated into UIBK's version of TCExam but is not in the
  official release yet.

* Fixed a bug in `exams2html()` with option `solution = FALSE` or
  `question = FALSE`. Now these correctly suppress the corresponding element
  of the exercise. In previous versions only the header had been set to `NA`
  erroneously.

* `exams2moodle()` can now optionally turn "string" exercises in "essay" quiz
  items rather than the default "shortanswer". Various customization options
  can be set the `exextra` metainformation. The most important options are
  illustrated in the new "essayreg" exercise.

* `\exshuffle{}` can now also be used in `read_exercise()` for subsampling
  rather than permutations only, e.g., via \exshuffle{3}. This first permutes
  the alternatives, always uses the first permuted true (if any), the first
  permuted false (if any), and then the first remaining alternatives.
  This facilitates static schoice/mchoice exercises which have many
  alternatives but should only use a limited number (say 5 for `exams2nops()`)
  in each random replication.

* Bug fix in `extract_extra()` which prevented `exextra` metainformation from
  being read correctly in the previous version.
  
* Improvement in `xexams()` to better support using the same exercise template
  several times within the same exam (e.g., for generating a single PDF
  with multiple versions of the same exercise). The temporary file names
  employed are now run through `make.unique()` first.
  
* Improvement in `exams2pdf()` to assure that duplication in file names for
  graphics (across exercises within the same exam) are handled correctly.

* Within several `exams2xyz()` functions `gsub(...)` calls were changed to
  `gsub(..., fixed = TRUE)` for replacing placeholders that potentially
  have slashes.    

* Updated `exams2arsnova()` to assure that `\( ... \)` instead of `$ ... $`
  is used for inline math. Also the JSON output was augmented with
  (almost empty) "publicPool" information to keep current ARSnova versions
  happy.

* New exercises "deriv"/"deriv2" (both in Rnw and Rmd) that illustrate
  a simple question about derivatives to be solved by the product rule.
  The "deriv" exercise is in num format and "deriv2" is the same in
  schoice (suggested by Martin Obradovits).

* The `tth`/`ttm`-based HTML conversion now makes sure that empty
  question/solution/list elements remain empty and do not introduce line
  breaks.

* Added copies of `num_to_schoice()`, `num_to_tol()`, ... called `num2schoice()`,
  and `num2tol()`, ...


# exams 2.1-0

* Exercises can now either be in Rnw format (R/LaTeX, as in previous versions)
  or in Rmd format (R/Markdown). A new function `xweave()` is provided for
  weaving either format by either calling `utils::Sweave()` for Rnw and
  `knitr::knit()` for Rmd. If Rmd exercises are used, the weaved exercises
  require conversion by `pandoc` (see below).

* The vignettes have not yet been updated to incorporate the R/Markdown
  information or the new interfaces described below. For examples and some
  introductory comments, `exams_skeleton()` can generate demo scripts.

* Added new `make_exercise_transform_pandoc()` transfomer generator, interfacing
  `rmarkdown::pandoc_convert()`. This provides an additional option for generating
  HTML output and additionally enables Markdown output (e.g., for ARSnova, see
  below).

* Added new interface `exams2nops()` for generating PDF exams that can be
  printed, scanned, and automatically evaluated. Accompanying functions
  `nops_scan()` (using the `png` package) and `nops_eval()` are provided as well.
  The "nops" exams can be internationalized. So far there is language support
  for English (`en`), Dutch (`nl`, contributed by Niels Smits), French (`fr`,
  contributed by Arthur Allignol), German (`de`), Italian (`it`, contributed by
  Domenico Zambella), Portuguese (`pt`, contributed by Fabian Petutschnig),
  Romanian (`ro`, contributed by Cristian Gatu), Spanish (`es`, contributed by
  Maria Kogelnik).

* Added new interface `exams2arsnova()` for exporting exams/quizzes to the
  interactive audience response system ARSnova (<https://arsnova.eu/>). This
  can either create JSON output files (via `RJSONIO::toJSON`) that need to be
  manually imported into ARSnova - or the JSON string can directly be imported
  into an active ARSnova session (via `RCurl`).

* Added `abstention` option in schoice/mchoice questions in `exams2moodle()`.
  This can also be set directly through an `\exabstention{}` tag in the
  corresponding Rnw exercise.

* Added various convenience functions for creating single- or multiple-choice
  questions from either a single numeric result (`num_to_schoice`) or from a
  matrix solution (`matrix_to_schoice`, `matrix_to_mchoice`, `det_to_schoice`).

* Added various convenience functions for formatting numbers within exercises:
  `fmt()` for conveniently displaying a certain number of digits, `round2()`
  for rounding away from zero, `char_with_braces()` for adding braces to negative
  numbers, and a `toLatex()` method for `"matrix"` objects.

* `tex2image()` now also supports `format = "svg"` via `pdfcrop` followed by `pdf2svg`
  (instead of ImageMagick's `convert`).

* Fixed bug in file paths created for "media" supplements in QTI XML formats.
  File names had an unnecessary trailing slash which lead to unrecognized
  suffixes in some browsers.


# exams 2.0-2

* Added an optional `transform = NULL` argument in `exams2pdf()`.

* Avoid `require(...)` calls for suggested packages.


# exams 2.0-1

* The default encoding in `exams2moodle()` is now ensured to be `"UTF-8"` rather
  than `"utf8"` for improved cross-platform compatibility.

* `\exshuffle{TRUE}` is now also used in `read_exercise()` so that schoice/mchoice
  exercises (or schoice/mchoice components in cloze exercises) are shuffled
  upon reading exercises. This facilitates static schoice/mchoice exercises.

* New cloze type "verbatim" which is not processed further (e.g., to include
  Moodle control structures).

* Empty questionlists for readily prepared Moodle cloze exercises can now
  be handled in `exams2pdf()`.

* Functions from non-imported packages (e.g., only suggested) are now always
  called with package::function to make R-devel checks happy.

* Bug fix in processing of `duration` in `exams2qti12()`.


# exams 2.0-0

* New version accompanying publication of exams version 2 in the
  _Journal of Statistical Software_, **58**(1), 1-36. See
  `vignette("exams2", package = "exams")` for more details.

* License extended from GPL-2 to GPL-2 | GPL-3.

* New `exams2qti21()` interface for generating QTI 2.1 (rather than QTI 1.2)
  XML. So far this was only tested with the ONYX Player within
  OpenOLAT. Currently, some switches do not appear to fully work, yet,
  at least within ONYX. The function still needs further thorough
  testing and might be further improved in future versions.

* Added `verbose = FALSE` option to `xexams()` and `exams2xyz()` interfaces.
  If set to `TRUE`, progress information on the exam generation is
  printed.

* The XML output in `exams2qti12()` sometimes did not yield fully correct
  QTI 1.2 output. This has been fixed now and test output was validated
  against the QTI 1.2 DTD.

* Added a new `pluginfile = TRUE` option in `exams2moodle()`. By default,
  supplementary files (graphics, data files, etc.) are now included using
  Moodle's Pluginfile mechanism. This may not work with older versions of
  Moodle (< 2.5) but in newer versions it is better and more robust than
  using data URIs.

* Added `fix_num = TRUE` option in `exams2qti12()` interface.
  Because OLAT does not display the correct answer of response_num
  items with tolerances (vargt/varlt), a workaround was added:
  In addition to the version with tolerances an exact equality
  is checked (varequal), adding and removing a 0.00001 points.
  Thus, evaluation of the exams is not affected but the display
  in OLAT is fixed.

* The {answerlist} in the {solution} environment can now also be
  omitted (in mchoice, schoice, cloze exercises). It is still recommended
  to include it but all `exams2xyz()` interfaces can process exercises
  correctly that do not provide it.


# exams 1.9-6

* Changed `tests/encodings.R` to use `encoding = "Latin-9"` (rather
  than `"latin9"` which does not work on Solaris, see `?iconv`).


# exams 1.9-5

* The new function `exams_skeleton()` helps users to create their first
  exam. It creates a directory with a `demo.R` script illustrating the
  use of the various `exams2xyz()` interfaces. Subdirectories with
  copies of all demonstration exercise Rnw files and templates for
  different output formats (LaTeX, HTML, or XML) are also created.

* Encodings are now explicitly supported by all `exams2xyz()` interfaces.
  Essentially, the encoding argument is just passed on to `Sweave()`
  and the user has to make sure that the template (for LaTeX, HTML,
  or XML output) has the appropriate encoding. `exams_skeleton()`
  also supports this and especially the encoding UTF-8 is now fully
  integrated. The encodings ISO-8859-1 (aka latin1) or ISO-8859-15
  (aka latin9) have also been tested while other encodings may need
  further touch-ups. Furthermore, the developers recommend to _not_
  rely on encodings but to code special characters with their
  LaTeX commands because these work in all locales on all platforms.

* The new function `exams_eval()` captures various types of evaluation
  policies for exams (with/without partial credits for multiple choice,
  with/without negative points). The resulting functions aid generation
  of e-learning exams or evaluation of an exam "by hand".

* If the `edir` argument is specified in `exams2xyz()`, then all
  files in edir _and_ its subdirectories are searched recursively.

* Asymmetric tolerance intervals are not supported in the `exams2xyz()`
  interfaces anymore. Instead, vectors of solutions in cloze
  exercises can have vectors of tolerances.

* The original `exam.tex`/`solution.tex` templates have been renamed to
  `oexam.tex`/`osolution.tex`. The function `exams()` needs to be used
  with oexam/osolution while exam/solution are now optimized for
  `exams2pdf()`. The reason is that the questionnaire command has
  been changed. For a multiple-choice question with five answers
  and correct solutions 1 and 4, say, `exams()` produces
  \exmchoice{X}{ }{ }{X}{ } while `exams2pdf()` now uses
  \exmchoice{X}[ ][ ][X][ ], i.e., with optional instead of required
  arguments. The latter solution is not restricted to multiple-choice
  questions with exactly five answers anymore.


# exams 1.9-4

* Several functions now use `find.package()` rather than `.find.package()`
  internally (as the latter will be deprecated soon).


# exams 1.9-3

* Enforced .tex suffix in LaTeX files used internally by `exams2pdf()`.
  The lack of .tex suffixes led to errors on certain Windows systems.


# exams 1.9-2

* New demo exercises:
  - `confint2.Rnw` shows how a confidence interval can be embedded into a
    cloze question (for all version 2 interfaces).
  - `dist.Rnw` is a very short and simple example that is intelligible
    across a wide range of disciplines (not for statistics audiences only).
  - `dist2.Rnw` is an extension of `dist.Rnw` that is written as a cloze.

* Questions in `exams2moodle()` were forced to be <pre>-formatted,
  now embedded in normal HTML (without <pre> tags).


# exams 1.9-1

* Supplementary files had not been correctly inferred on Solaris
  systems. This has been fixed now so that the package behaves the
  same across different platforms.
  
* The code underlying the old `"exams"` vignette now avoids opening PDF
  viewers.


# exams 1.9-0

* Major revision of the `exams` package, adding flexible support for
  generation of exams in different formats including PDF, HTML,
  Moodle XML, and QTI 1.2 (for OLAT). This new version of the exams
  package (called "version 2") is described in a new vignette:
  `vignette("exams2", package = "exams")`


# exams 1.0-4

* Added an {answerlist} environment in the LaTeX templates (`inst/tex`)
  so that each \item in the {answerlist} is automatically labeled
  with (a), (b), (c), ...

* Replaced the old {itemize} environments with {answerlist} in
  `anova.Rnw`, `boxplots.Rnw`, `relfreq.Rnw`, `scatterplot.Rnw`, `ttest.Rnw`.
  (The resulting PDFs do not change.)


# exams 1.0-3

* Small improvements in solution string formatting.


# exams 1.0-2

* The argument `nsamp` was added to `exams()` in order to allow
  sampling more than one exercise from each element of the
  argument `file`.


# exams 1.0-1

* The man page of `exams()` was modified to use suitable markup commands.

* The questions in the ANOVA example were slightly changed to more
  explicitly indicate which hypothesis is tested. A typo in the
  vignette was fixed.


# exams 1.0-0

* New version accompanying publication in the _Journal of
  Statistical Software_. See `vignette("exams", package = "exams")`
  for more details.
  
* New CITATION file pointing to the JSS paper, see
  `citation("exams")`.


# exams 0.9-5

* The possible types of questions were extended to allow also text
  results. The new type is specified by "string".

* To specify the allowed tolerance for numeric solutions now
  different tolerances can be supplied for falling below or
  exceeding the true value via `\extol{lower}{upper}`.

* The "discussion" section in the vignette was extended to include
  several remarks and pointers on strategies for exercise
  generation.


# exams 0.9-0

* First CRAN release version.

* New `vignette("exams", package = "exams")` that explains how to use the package.

* New set of exercises for illustation in examples and vignette.


# exams 0.2-0

* First stable version for internal use at WU Wien.

