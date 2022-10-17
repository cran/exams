if(!identical(Sys.getlocale(), "C")) {
library("exams")
exams2pdf("currency8.Rnw", dir = tempfile())
}
