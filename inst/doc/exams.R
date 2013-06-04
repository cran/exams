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
### code chunk number 2: exams.Rnw:226-229
###################################################
invisible(file.copy(system.file("exercises", "tstat.Rnw", package = "exams"), "tstat.Rnw"))
Rnw <- readLines("tstat.Rnw")
cat(c("\\begin{verbatim}", Rnw, "\\end{verbatim}"), sep = "\n")


###################################################
### code chunk number 3: exams.Rnw:242-246
###################################################
set.seed(1090)
Sweave("tstat.Rnw")
tex <- readLines("tstat.tex")
file.remove(c("tstat.Rnw", "tstat.tex"))


###################################################
### code chunk number 4: exams.Rnw:248-249
###################################################
cat(c("\\begin{verbatim}", tex, "\\end{verbatim}"), sep = "\n")


###################################################
### code chunk number 5: exams.Rnw:262-263
###################################################
cat(tex, sep = "\n")


###################################################
### code chunk number 6: tstat-interacive (eval = FALSE)
###################################################
## library("exams")
## set.seed(1090)
## tstat_sol <- exams("tstat.Rnw")


###################################################
### code chunk number 7: tstat-non-interacive
###################################################
set.seed(1090)
tdir <- tempdir()
tstat_sol <- exams("tstat.Rnw", dir = tdir)


###################################################
### code chunk number 8: exams.Rnw:336-337
###################################################
tstat_sol


###################################################
### code chunk number 9: exams.Rnw:384-386
###################################################
tex <- readLines(system.file("tex", "plain.tex", package = "exams"))
cat(c("\\begin{verbatim}", tex, "\\end{verbatim}"), sep = "\n")


###################################################
### code chunk number 10: exams.Rnw:402-406
###################################################
tstat_char <- strsplit(gsub("\\.", "", as.character(tstat_sol[[1]][[1]]$solution)), "")[[1]]
tstat_exnum <- rep("", 9)
tstat_exnum[(10 - length(tstat_char)):9] <- tstat_char
tstat_exnum <- paste("{", tstat_exnum, "}", sep = "", collapse = "")


###################################################
### code chunk number 11: exams.Rnw:484-487
###################################################
cat(prettyPrint(prompt(exams, filename = NA)$usage[[2]], sep = ", ", 
  linebreak = paste("\n", paste(rep(" ", nchar("exams") + 1), collapse = ""), sep= ""),
  width = 60))


###################################################
### code chunk number 12: exams.Rnw:500-505
###################################################
myexam <- list("boxplots",
               c("confint", "ttest", "tstat"),
               c("anova", "regression"),
               "scatterplot",
               "relfreq")


###################################################
### code chunk number 13: exams.Rnw:559-560
###################################################
odir <- tempfile()


###################################################
### code chunk number 14: exams.Rnw:567-570
###################################################
getID <- function(i) 
  paste("myexam", gsub(" ", "0", format(i, width = 2)), sep = "")
getID(1)


###################################################
### code chunk number 15: exams.Rnw:574-578
###################################################
set.seed(1090)
sol <- exams(myexam, n = 5, nsamp = c(1, 2, 1, 1, 1), dir = odir, 
 template = c("oexam", "osolution"), 
 header = list(ID = getID, Date = Sys.Date()))


###################################################
### code chunk number 16: exams.Rnw:587-588
###################################################
list.files(odir)


###################################################
### code chunk number 17: exams.Rnw:595-597
###################################################
print(sol, 1)
print(sol, "exam5")


###################################################
### code chunk number 18: control-interactive (eval = FALSE)
###################################################
## mycontrol <- list(mchoice.print = list(True = LETTERS[1:5], False = "_"))
## (exams(myexam, n = 1, template = "oexam", control = mycontrol))


###################################################
### code chunk number 19: control-non-interactive
###################################################
mycontrol <- list(mchoice.print = list(True = LETTERS[1:5], False = "_"))
(exams(myexam, n = 1, template = "oexam", control = mycontrol, dir = tdir))


