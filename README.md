\
![R/exams logo](https://www.R-exams.org/assets/img/logo_wide.svg)

**A One-for-All Exams Generator**

## Overview

The [open-source R package 'exams'](https://www.R-exams.org/resources/) provides a
[one-for-all approach](https://www.R-exams.org/intro/oneforall/) to automatic exams generation. Based on
[exercise templates](https://www.R-exams.org/intro/dynamic/) large numbers of personalized exams/quizzes/tests can be
created for various systems: PDFs for classical [written exams](https://www.R-exams.org/intro/written/) (with
automatic evaluation), imports for [learning management systems](https://www.R-exams.org/intro/elearning/) (like
Moodle, Canvas, OpenOlat, or Blackboard), live voting (via ARSnova or Particify), and
custom output in PDF, HTML, Docx, etc.

## Installation

The stable version of the R package `exams` is available from
[CRAN](https://CRAN.R-project.org/package=exams). It can be installed
from within R along with all of its dependencies via:

``` r
install.packages("exams", dependencies = TRUE)
```

In case, you need features or bug fixes from
the latest development version, this can be installed from
[R-universe](https://zeileis.R-universe.dev/exams):

``` r
install.packages("exams", repos = "https://zeileis.R-universe.dev")
```

In addition to the R packages some further tools (like Pandoc or
Ghostscript) may be needed, depending on the tasks that R/exams should
carry out. See the installation tutorial, linked below, for more details.


## Get started

Follow the tutorials on:

- [Installation](https://www.R-exams.org/tutorials/installation/)
- [First Steps](https://www.R-exams.org/tutorials/first_steps/)

Subsequently, start creating:

- [Exercises](https://www.R-exams.org/intro/dynamic/)
- [E-learning materials](https://www.R-exams.org/intro/elearning/)
- [Written exams](https://www.R-exams.org/intro/written/)

See also the available [video tutorials on YouTube](https://www.youtube.com/playlist?list=PLsEZAAbioUw1IBnhtBi9eIo0uqMHmqDor).


## License

The package is available under the
[General Public License version 3](https://www.gnu.org/licenses/gpl-3.0.html) or
[version 2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
