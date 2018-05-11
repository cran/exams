tex2image <- function(tex, format = "png", width = NULL, pt = 12,
  density = 350, dir = NULL, tdir = NULL, idir = NULL,
  width.border = 0L, col.border = "white", resize = 650,
  packages = c("amsmath", "amssymb", "amsfonts"),
  header = c("\\renewcommand{\\sfdefault}{phv}",
    "\\IfFileExists{sfmath.sty}{\n\\RequirePackage{sfmath}\n\\renewcommand{\\rmdefault}{phv}}{}"),
  header2 = NULL, tikz = NULL,
  Sweave = TRUE, show = FALSE, name = "tex2image")
{
  ## directory handling
  if(is.null(tdir)) {
    tdir <- tempfile()
    on.exit(unlink(tdir))
  }
  tdir <- file.path(path.expand(tdir), "tex2image")
  dir.create(tdir, recursive = TRUE, showWarnings = FALSE)

  if(!is.list(tex) && (length(text) < 2L) && file.exists(tex)) {
    texfile <- file_path_as_absolute(tex)
    tex <- readLines(con = texfile)
    texdir <- dirname(texfile)
    cfiles <- list.files(texdir)
    cfiles <- cfiles[cfiles != "tex2image"]
    cfiles <- cfiles[cfiles != basename(texfile)]
    if(length(pdfs <- grep("pdf", file_ext(cfiles))))
      cfiles <- cfiles[-pdfs]
    file.copy(file.path(texdir, cfiles), file.path(tdir, cfiles))
    texfile <- paste("tex2image-", basename(texfile), sep = "")
    name <- file_path_sans_ext(texfile)
  } else {
    texdir <- tempdir()
  }

  if(any(grepl("\\documentclass", tex, fixed = TRUE))) {
    begin <- grep("\\begin{document}", tex, fixed = TRUE)
    end <- grep("\\end{document}", tex, fixed = TRUE)
    tex <- tex[(begin + 1):(end - 1)]
  }

  if(is.null(dir)) dir <- texdir
  if(dir == ".") dir <- getwd()

  owd <- getwd()
  setwd(tdir)
  on.exit(setwd(owd), add = TRUE)

  ## output formats
  format <- tolower(format)

  ## LaTeX packages  
  packages <- unique(c(packages, c("graphicx", "url", "color", "varwidth")))
  if(!is.null(tikz) & !any(grepl("tikz", packages)))
    packages <- c(packages, "tikz")

  if(length(graphics <- grep("includegraphics", unlist(tex), fixed = TRUE, value = TRUE))) {
    if(is.null(idir))
      idir <- texdir
    idir <- path.expand(idir)
    files <- list.files(idir)
    cp <- NULL
    for(k in 1L:length(graphics)) {
      graphics[k] <- extract_command(graphics[k], "includegraphics")
      cp <- c(cp, grep(graphics[k], files, fixed = TRUE, value = TRUE))
    }
    if(length(cp)) {
      for(f in cp) 
        file.copy(from = file.path(idir, f), to = file.path(tdir, f), overwrite = TRUE)
    } else stop(paste("graphic is missing in ", texdir, "!", sep = ""))
  }
  
  ## auxiliary LaTeX file
  texlines <- paste("\\documentclass[tikz]{standalone}", sep = "")
  for(i in packages) {
    brackets <- if(grepl("{", i, fixed = TRUE)) NULL else c("{", "}")
    texlines <- c(texlines, paste("\\usepackage", brackets[1], i, brackets[2], sep = ""))
  }
  if(any(grepl("tikz", packages)) && is.character(tikz))
    texlines <- c(texlines, paste0("\\usetikzlibrary{", paste(tikz, collapse = ",", sep = ""), "}"))
  if(Sweave) texlines <- c(texlines, "\\usepackage{Sweave}")
  texlines <- c(texlines, paste0("\\tikzset{font={\\fontsize{", pt, "pt}{12}\\selectfont}}"))
  texlines <- c(texlines, header)
  texlines <- c(texlines, "\\begin{document}")
  texlines <- c(texlines, header2)
  tex <- if(!is.list(tex)) list(tex) else tex
  nt <- length(tex)
  pic_names <- if(is.null(names(tex))) {
    if(nt > 1) paste(name, 1:length(tex), sep = "_") else name
  } else {
    paste(name, names(tex), sep = "_")
  }
  pic_names <- paste(pic_names, format, sep = ".")

  ## handling width etc.
  if(is.null(width)) width <- 0
  if(is.logical(width)) c(0, 6)[1 + width]
  if(any(is.na(width))) width[is.na(width)] <- 0
  width <- rep_len(width, nt)
  node <- rep_len("varblock", nt)
  node[width > 0] <- "fixblock"
  nodes <- rep_len(node, nt)
  width[width < 1] <- 6

  for(i in 1:nt) {
    tikz <- any(grepl("begin{tikzpicture}", tex[[i]], fixed = TRUE))
    if(!tikz) {
      texlines <- c(texlines, "\\begin{tikzpicture}[auto,->=stealth]")
      if(nodes[i] == "varblock") {
        texlines <- c(texlines, "\\node{", paste("\\begin{varwidth}{", width[i], "in}", sep = ""))
      } else {
        texlines <- c(texlines, paste("\\node[text width=", width[i], "in]{", sep = ""))
      }
    }
    texlines <- c(texlines, tex[[i]])
    if(!tikz) {
      if(nodes[i] == "varblock") {
        texlines <- c(texlines, "\\end{varwidth}")
      }
      texlines <- c(texlines, "};", "\\end{tikzpicture}")
    }
  }
  texlines <- c(texlines, "\\end{document}")

  writeLines(text = texlines, con = file.path(tdir, paste0(name, ".tex")))

  ## compile LaTeX into PDF
  tools::texi2dvi(file = paste(name, ".tex", sep = ""), pdf = TRUE, clean = TRUE, quiet = TRUE)

  ## shell command on Windows
  shcmd <- Sys.getenv("COMSPEC")
  if(shcmd != "") shcmd <- paste(shQuote(shcmd), "/c")

  ## convert to images
  image <- paste(name, if(nt > 1) 1:nt else NULL, ".", format, sep = "")
  dirout <- rep(NA, length(name))

  imgs <- if(!(format %in% c("pdf", "svg"))) magick::image_read(paste0(name, ".pdf"), density = density) else NULL

  for(i in 1:nt) {
     if(!(format %in% c("pdf", "svg"))) {
      width.border <- as.integer(width.border)
      if(width.border > 0L) {
        width.border <- paste(width.border, "x", width.border, sep = "")
        imgs[i] <- magick::image_border(imgs[i], col.border, geometry = width.border)
      }
      if(!is.null(resize))
        imgs[i] <- magick::image_scale(imgs[i], as.character(resize))
      magick::image_write(image = imgs[i], path = image[i], format = format)
    } else {
      system(paste(shcmd, "pdfcrop --clip", paste0(name, ".pdf"), paste0(name, ".pdf")), ignore.stdout = TRUE)
      if(format == "svg") system(paste(shcmd, "pdf2svg", paste0(name, ".pdf"), paste0(name, if(nt > 1) "_%d" else NULL, ".svg"), "all"), ignore.stdout = TRUE)
    }
    dirout[i] <- file.path(path.expand(dir), pic_names[i])
    file.copy(from = file.path(tdir, image[i]), 
      to = dirout[i], overwrite = TRUE)
    dirout[i] <- normalizePath(dirout[i])
    if(show) browseFile(dirout[i])
  }
  
  return(invisible(dirout))
}

