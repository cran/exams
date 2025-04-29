### R code from vignette source 'exams.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(width = 70, prompt = "R> ", continue = "+  ")
library("exams")
combine <- function(x, sep, width) {
  cs <- cumsum(nchar(x))
  remaining <- if (any(cs[-1] > width)) combine(x[c(FALSE, cs[-1] > width)], sep, width)
  c(paste(x[c(TRUE, cs[-1] <= width)], collapse= sep), remaining)
}
prettyPrint <- function(x, sep = " ", linebreak = "\n\t", width = getOption("width")) {
  x <- strsplit(x, sep)[[1]]
  paste(combine(x, sep, width), collapse = paste(sep, linebreak, collapse = ""))
}


###################################################
### code chunk number 2: exams.Rnw:229-232
###################################################
invisible(file.copy(system.file("exercises", "tstat.Rnw", package = "exams"), "tstat.Rnw"))
Rnw <- readLines("tstat.Rnw")
cat(c("\\begin{verbatim}", Rnw, "\\end{verbatim}"), sep = "\n")


###################################################
### code chunk number 3: exams.Rnw:245-249
###################################################
set.seed(1090)
Sweave("tstat.Rnw")
tex <- readLines("tstat.tex")
file.remove(c("tstat.Rnw", "tstat.tex"))


###################################################
### code chunk number 4: exams.Rnw:251-252
###################################################
cat(c("\\begin{verbatim}", tex, "\\end{verbatim}"), sep = "\n")


###################################################
### code chunk number 5: exams.Rnw:265-266
###################################################
cat(tex, sep = "\n")


###################################################
### code chunk number 6: tstat-interacive (eval = FALSE)
###################################################
## library("exams")
## set.seed(1090)
## tstat_ex <- exams2pdf("tstat.Rnw")


###################################################
### code chunk number 7: tstat-non-interactive
###################################################
set.seed(1090)
dir.create(tdir <- tempfile())
tstat_ex <- exams2pdf("tstat.Rnw", dir = tdir)
tstat_sol <- exams_metainfo(tstat_ex)


###################################################
### code chunk number 8: exams.Rnw:339-340
###################################################
exams_metainfo(tstat_ex)


###################################################
### code chunk number 9: exams.Rnw:396-400
###################################################
tex <- readLines(system.file("tex", "plain.tex", package = "exams"))
a <- grep("newenvironment{answerlist}", tex, fixed = TRUE)
tex <- c(tex[1:(a-1)], substr(tex[a], 1, 72), paste(" ", substr(tex[a], 73, nchar(tex[a]))), tex[-(1:a)])
cat(c("\\begin{verbatim}", tex, "\\end{verbatim}"), sep = "\n")


###################################################
### code chunk number 10: exams.Rnw:416-420
###################################################
tstat_char <- strsplit(gsub("\\.", "", as.character(tstat_sol[[1]][[1]]$solution)), "")[[1]]
tstat_exnum <- rep("", 9)
tstat_exnum[(10 - length(tstat_char)):9] <- tstat_char
tstat_exnum <- paste("{", tstat_exnum, "}", sep = "", collapse = "")


###################################################
### code chunk number 11: exams.Rnw:487-490
###################################################
cat(prettyPrint(prompt(exams2pdf, filename = NA)$usage[[2]], sep = ", ", 
  linebreak = paste("\n", paste(rep(" ", nchar("exams2pdf") + 1), collapse = ""), sep= ""),
  width = 60))


###################################################
### code chunk number 12: exams.Rnw:503-508
###################################################
myexam <- list("boxplots",
               c("confint", "ttest", "tstat"),
               c("anova", "regression"),
               "scatterplot",
               "relfreq")


###################################################
### code chunk number 13: exams.Rnw:565-566
###################################################
odir <- tempfile()


###################################################
### code chunk number 14: exams.Rnw:573-576
###################################################
getID <- function(i) 
  paste("myexam", gsub(" ", "0", format(i, width = 2)), sep = "")
getID(1)


###################################################
### code chunk number 15: exams.Rnw:580-584
###################################################
set.seed(1090)
ex <- exams2pdf(myexam, n = 5, nsamp = c(1, 2, 1, 1, 1), dir = odir, 
 template = c("exam", "solution"), 
 header = list(ID = getID, Date = Sys.Date()))


###################################################
### code chunk number 16: exams.Rnw:593-594
###################################################
list.files(odir)


###################################################
### code chunk number 17: exams.Rnw:602-605
###################################################
sol <- exams_metainfo(ex)
print(sol, 1)
print(sol, "exam5")


