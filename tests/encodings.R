if(!identical(Sys.getlocale(), "C")) {
library("exams")
exams2pdf("currency8", encoding = "utf8",   template = "plain8", dir = ".")
exams2pdf("currency9", encoding = "latin9", template = "plain9", dir = ".")
}
