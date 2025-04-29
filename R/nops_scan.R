## main function
nops_scan <- function(
  images = dir(pattern = "\\.PNG$|\\.png$|\\.PDF|\\.pdf$", path = dir, full.names = TRUE),
  file = NULL, dir = ".",
  verbose = TRUE, rotate = FALSE, cores = NULL, n = NULL,
  density = 300,
  size = 0.03, threshold = c(0.04, 0.42), trim = 0.3, minrot = 0.002,
  string = FALSE)
{
  ## required packages
  stopifnot(requireNamespace("png"))
  if(!is.null(cores)) {
    if(!requireNamespace("parallel")) cores <- NULL
  }

  ## directory handling
  dir <- file_path_as_absolute(dir)
  owd <- getwd()
  dir.create(tdir <- tempfile())
  on.exit(unlink(tdir))

  ## check whether images exist
  if(!all(im <- file.exists(images))) {
    warning(paste("The following images cannot be found:", paste(images[!im], collapse = ", ")))
    images <- images[im]
  }
  if(length(images) < 0L) {
    stop("No images found.")
  }

  ## rotate images and convert PDF to PNG (if necessary)
  pdfs <- grepl("\\.pdf$", tolower(images))
  if(any(!pdfs) && rotate) {
    rotate_pngs(images[!pdfs], dir = tdir, cores = cores)
  }
  if(any(pdfs)) {
    pngs <- pdfs2pngs(images[pdfs], density = density, cores = cores, dir = tdir,
      verbose = verbose, rotate = rotate, prefix = if(string) "T" else "S")
    images <- if(length(images) > sum(pdfs)) c(images[!pdfs], pngs) else pngs
  }
  
  ## copy PNG files (assuring file basenames have no spaces)
  file.copy(images, file.path(tdir, images <- gsub("[[:space:]]", "_", basename(images))))
  setwd(tdir)
  on.exit(setwd(owd), add = TRUE)

  ## read nops images
  if(verbose) cat("Reading PNG files:\n")

  read_nops_all <- function(file)
  {
    err <- paste(file, "ERROR")

    if(verbose) {
      if(is.null(cores)) {
        cat(paste(file, ": Trimming PNG", sep = ""))
      } else {
        cat(paste("Reading ", file, ".\n", sep = ""))
      }
    }
    ss <- try(trim_nops_scan(file, verbose = verbose & is.null(cores), minrot = minrot))
    if(inherits(ss, "try-error")) {
      if(verbose) cat(", ERROR")
      return(err)
    }
    
    if(verbose & is.null(cores)) cat(", extracting information")
    ss <- if(!string) {
      ssty <- read_nops_digits(ss, "type")
      regextra <- as.numeric(substr(ssty, 1L, 1L)) # 0=regular; 1/2/3=regextra; 4/5/6=regextra+backup
      if(is.na(regextra)) {
        if(verbose) cat(", ERROR\n")
        return(err)
      }

      sbackup <- if(regextra == 0L) {
        read_nops_backup(ss, threshold = threshold, size = size)
      } else {
        as.character(as.numeric(regextra > 3L))
      }
      if(regextra > 3L) regextra <- regextra - 3L
      try(paste(
        file,
        read_nops_digits(ss, "id"),
        if(regextra == 0L) read_nops_digits(ss, "scrambling") else "00",
	ssty,
	sbackup,
        read_nops_registration(ss, threshold = threshold, size = size * 1.2, trim = trim, regextra = regextra), ## allow bigger size in registration
        read_nops_answers(ss, threshold = threshold, size = size, trim = trim, n = if(is.null(n)) as.numeric(substr(ssty, 2L, 3L)) else n)
      ))
    } else {
      try(paste(
        file,
        read_nops_digits(ss, "id", adjust = TRUE),
	read_nops_digits(ss, "type", adjust = TRUE),
        substr(read_nops_answers(ss, threshold = threshold, size = size, trim = trim, n = 3L, adjust = TRUE), 1, 17)
      ))
    }
    
    if(inherits(ss, "try-error")) {
      if(verbose) cat(", ERROR")
      return(err)
    }
  
    if(verbose & is.null(cores)) cat(", done.\n")
    return(ss)
  }
  read_nops <- function(x) as.vector(sapply(x, read_nops_all))
  
  rval <- if(!is.null(cores)) {
    applyfun <- if(.Platform$OS.type == "windows") {
      cl_cores <- parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cl_cores))
      function(X, FUN, ...) parallel::parLapply(cl = cl_cores, X, FUN, ...)
    } else {
      function(X, FUN, ...) parallel::mclapply(X, FUN, ..., mc.cores = cores)
    }
    xi <- split(images, ceiling(seq_along(images) / (length(images) / cores)))
    unlist(applyfun(seq_along(xi), function(j) { read_nops(xi[[j]]) }))
  } else {
    read_nops(images)
  }

  ## return output
  if(!identical(file, FALSE)) {
    if(verbose) cat("\nCreating ZIP file:\n")

    ## file name
    if(is.null(file) || !is.character(file)) file <- paste(if(string) "nops_string_scan" else "nops_scan",
      format(Sys.time(), "%Y%m%d%H%M%S"), sep = "_")
    if(substr(tolower(file), nchar(file) - 3L, nchar(file)) != ".zip") file <- paste(file, "zip", sep = ".")

    ## check for errors in scanned data
    if(any(grepl("ERROR", rval, fixed = TRUE))) warning(sprintf(
      "errors in scanned data, please run nops_fix() on '%s' prior to nops_eval()", file))

    ## create zip file
    writeLines(rval, file.path(tdir, if(string) "Daten2.txt" else "Daten.txt"))
    zip(zipfile = file, files = list.files(tdir))
    file.copy(file, file.path(dir, file))
    invisible(rval)
  } else {
    return(rval)
  }
}


## auxiliary functions (not to be exported)

shsystem <- function(cmd, ...) {
  sh <- Sys.getenv("COMSPEC")
  if(sh != "") sh <- paste(shQuote(sh), "/c")
  system(paste(sh, cmd), ...)
}

## rotate PNG images
rotate_pngs <- function(x, dir = NULL, cores = NULL, verbose = TRUE)
{
  ## PNG handling via R package "magick" or system command "mogrify"?
  magick_available <- function() requireNamespace("magick")
  mogrify_available <- function() {
    magic <- try(shsystem("mogrify --version", intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
    !inherits(magic, "try-error")
  }
  if(magick_available()) {
    pngengine <- "magick"
  } else if(mogrify_available()) {
    pngengine <- "mogrify"
  } else {      
    stop("neither R package 'magick' nor system command 'mogrify' are available for rotating PNGs")
  }

  ## actual rotation function
  rotate_png <- function(pngs) {
    ## shell command on Windows
    for(i in seq_along(pngs)) {
      png_i <- pngs[i]
      if(verbose) cat(paste(png_i, ": Rotating PNG.\n", sep = ""))
      if(pngengine == "magick") {
        img_i <- magick::image_read(png_i)
        img_i <- magick::image_rotate(img_i, degrees = 180)
        magick::image_write(img_i, path = png_i, format = "png")
        magick::image_destroy(img_i)
        if(i %% 5L == 0L) gc() ## triggering gc to avoid exhausting cache in magick
      } else {
        cmd <- paste("mogrify -rotate 180", png_i)
        shsystem(cmd)
      }
    }
  }

  if(!is.null(cores)) {
    applyfun <- if(.Platform$OS.type == "windows") {
      cl_cores <- parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cl_cores))
      function(X, FUN, ...) parallel::parLapply(cl = cl_cores, X, FUN, ...)
    } else {
      function(X, FUN, ...) parallel::mclapply(X, FUN, ..., mc.cores = cores)
    }
    xi <- split(x, ceiling(seq_along(x) / (length(x) / cores)))
    applyfun(1:cores, function(j) { rotate_png(xi[[j]]) })
  } else {
    rotate_png(x)
  }
  if(verbose) cat("\n")
  return(x)
}

## conversion of PDF to PNG images
pdfs2pngs <- function(x, density = 300, dir = NULL, cores = NULL, verbose = TRUE, rotate = FALSE, prefix = "S")
{
  ## copy to temporary directory
  dir.create(tdir <- tempfile())
  if(!is.null(dir)) {
    dir <- file_path_as_absolute(dir)
    on.exit(unlink(tdir))
  } else {
    dir <- dirname(x)
  }
  owd <- getwd()  
  file.copy(x, file.path(tdir, x <- basename(x)))
  setwd(tdir)

  ## PDF handling via R package "qpdf" or system command "pdftk"?
  qpdf_available <- function() requireNamespace("qpdf")
  pdftk_available <- function() {
    pdftk <- try(shsystem("pdftk --version", intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
    !inherits(pdftk, "try-error")
  }
  pdfengine <- getOption("exams_pdfengine") ## NOTE: option undocumented, only for testing purposes
  if(is.null(pdfengine)) {
    if(qpdf_available()) {
      pdfengine <- "qpdf"
    } else if(pdftk_available()) {
      pdfengine <- "pdftk"
    } else {      
      stop("neither R package 'qpdf' nor system command 'pdftk' are available for merging/rotating/splitting PDFs")
    }
  }
  pdfengine <- match.arg(pdfengine, c("qpdf", "pdftk"))
  if(pdfengine == "qpdf") {
    if(!qpdf_available()) stop("package 'qpdf' is not available for merging/rotating/splitting PDFs")
  } else {
    if(!pdftk_available()) stop("system command 'pdftk' is not available for merging/rotating/splitting PDFs")
  }
  
  ## PNG handling via R package "magick" or system command "convert"?
  magick_available <- function() requireNamespace("magick")
  convert_available <- function() {
    magic <- try(shsystem("convert --version", intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
    !inherits(magic, "try-error")
  }
  pngengine <- getOption("exams_pngengine") ## NOTE: option undocumented, only for testing purposes
  if(is.null(pngengine)) {
    if(magick_available()) {
      pngengine <- "magick"
    } else if(convert_available()) {
      pngengine <- "convert"
    } else {      
      stop("neither R package 'magick' nor system command 'convert' are available for converting PDF to PNG")
    }
  }
  pngengine <- match.arg(pngengine, c("magick", "convert"))
  if(pngengine == "magick") {
    if(!magick_available()) stop("package 'magick' is not available for converting PDF to PNG")
  } else {
    if(!convert_available()) stop("system command 'convert' is not available for converting PDF to PNG")
  }

  ## if necessary: merge PDFs, otherwise rename only
  if(length(x) > 1L) {
    if(verbose) cat("Merging PDF files")
    if(pdfengine == "qpdf") {
      qpdf::pdf_combine(x, "_NOPS_.pdf")
    } else {
      shsystem(sprintf("pdftk %s cat output _NOPS_.pdf", paste(x, collapse = " ")))
    }
    file.remove(x)
    if(verbose) cat(", done.\n")
  } else {
    file.rename(x, "_NOPS_.pdf")
  }

  ## if requested: rotate PDFs
  if(rotate) {
    if(verbose) cat("Rotating PDF files")
    if(pdfengine == "qpdf") {
      qpdf::pdf_rotate_pages("_NOPS_.pdf", angle = 180, relative = TRUE, output = "_NOPS_2_.pdf")
    } else {
      shsystem("pdftk _NOPS_.pdf rotate 1-enddown output _NOPS_2_.pdf")
    }
    file.remove("_NOPS_.pdf")
    file.rename("_NOPS_2_.pdf", "_NOPS_.pdf")
    if(verbose) cat(", done.\n")
  }

  ## burst PDF into individual pages
  if(verbose) cat("Splitting PDF files")
  if(pdfengine == "qpdf") {
    qpdf_out <- qpdf::pdf_split("_NOPS_.pdf")
    for(i in seq_along(qpdf_out)) file.rename(qpdf_out[i], sprintf("%s%07d.pdf", prefix, i))
  } else {
    shsystem(paste0("pdftk _NOPS_.pdf burst output ", prefix, "%07d.pdf"))
    file.remove("doc_data.txt")
  }
  file.remove("_NOPS_.pdf")
  if(verbose) cat(", done.\n")
  x <- dir(pattern = "\\.pdf$")

  ## actual conversion function
  pdf2png <- function(pdfs) {
    ## shell command on Windows
    for(i in seq_along(pdfs)) {
      pdf_i <- pdfs[i]
      if(verbose) cat(paste(pdf_i, ": Converting PDF to PNG.\n", sep = ""))
      if(pngengine == "magick") {
        img_i <- magick::image_read(pdf_i, density = density)
        img_i <- magick::image_convert(img_i, "png")
        magick::image_write(img_i, path = paste0(file_path_sans_ext(pdf_i), ".PNG"), format = "png")
        magick::image_destroy(img_i)
        if(i %% 5L == 0L) gc() ## triggering gc to avoid exhausting cache in magick
      } else {
        cmd <- paste("convert -density", density, pdf_i, paste0(file_path_sans_ext(pdf_i), ".PNG"))
        shsystem(cmd)
      }
    }
  }

  if(!is.null(cores)) {
    applyfun <- if(.Platform$OS.type == "windows") {
      cl_cores <- parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cl_cores))
      function(X, FUN, ...) parallel::parLapply(cl = cl_cores, X, FUN, ...)
    } else {
      function(X, FUN, ...) parallel::mclapply(X, FUN, ..., mc.cores = cores)
    }
    xi <- split(x, ceiling(seq_along(x) / (length(x) / cores)))
    applyfun(1:cores, function(j) { pdf2png(xi[[j]]) })
  } else {
    pdf2png(x)
  }
  pngs <- gsub(".pdf", ".PNG", x, fixed = TRUE)  
  file.copy(pngs, pngs <- file.path(dir, pngs))
  setwd(owd)

  if(verbose) cat("\n")
  return(pngs)
}

## select sub-image from a pixel matrix
subimage <- function(x, center, prop = 0.01) {
  prop <- rep(prop, length.out = 2L)
  if(center[1L] < 1) center[1L] <- round(center[1L] * nrow(x))
  if(center[2L] < 1) center[2L] <- round(center[2L] * ncol(x))
  topleft  <- center - round(nrow(x) * prop/2)
  botright <- center + round(nrow(x) * prop/2)
  x[max(1L, topleft[1L]):min(nrow(x), botright[1L]), max(1L, topleft[2L]):min(ncol(x), botright[2L])]
}

"subimage<-" <- function(x, center, prop = 0.01, value) {
  prop <- rep(prop, length.out = 2L)
  if(center[1L] < 1) center[1L] <- round(center[1L] * nrow(x))
  if(center[2L] < 1) center[2L] <- round(center[2L] * ncol(x))
  topleft  <- center - round(nrow(x) * prop/2)
  botright <- center + round(nrow(x) * prop/2)
  x[max(1L, topleft[1L]):min(nrow(x), botright[1L]), max(1L, topleft[2L]):min(ncol(x), botright[2L])] <- value
  x
}

## shave (almost) white margins of a pixel matrix
shave <- function(x, zap = 0.07) {
  ix <- rowMeans(x) > zap
  if(any(ix)) {
    ix[min(which(ix)):max(which(ix))] <- TRUE
  } else {
    ix[] <- TRUE
  }
  x <- x[ix, ]

  ix <- colMeans(x) > zap
  if(any(ix)) {
    ix[min(which(ix)):max(which(ix))] <- TRUE
  } else {
    ix[] <- TRUE
  }
  x[, ix]
}

## shave box (and white margins) of a pixel matrix
shave_box <- function(x, border = 0.1, clip = TRUE)
{  
  rm <- which(rowMeans(x) > 0.38)
  cm <- which(colMeans(x) > 0.38)
  if(length(rm) < 1L || length(cm) < 1L) stop("no box found")
  rm <- range(rm)
  cm <- range(cm)
  x <- x[rm[1L]:rm[2L], cm[1L]:cm[2L]]
  n <- ceiling(min(dim(x) * border))
  x <- x[n:(nrow(x) - n), n:(ncol(x) - n)]
  if(clip) shave(x) else x
}

## ignore single white rows/columns in has_mark()
ignore_single_true_line <- function(x) {
  y <- rle(x)
  if(length(y$lengths[y$values]) < 2L) return(x)
  if(all(y$lengths[y$values] > 1L) || all(y$lengths[y$values] <= 1L)) return(x)
  y$values[y$values & y$lengths == 1L] <- FALSE
  inverse.rle(y)
}

## determine whether a pixel matrix has a check mark
has_mark <- function(x, threshold = c(0.04, 0.42), fuzzy = FALSE, trim = 0.3, shave = TRUE)
{
  ## could there be any mark?
  rm <- which(rowMeans(x) > 0.38)
  cm <- which(colMeans(x) > 0.38)
  if(length(rm) < 2L || length(cm) < 2L) return(0L)

  ## simple clipping vs. refined shaving
  if(!shave) {
    if(diff(range(rm)) < 5L || diff(range(cm)) < 5L) return(0L)
    ri <- range(rm)
    ci <- range(cm)
    x <- x[ri[1L]:ri[2L], ci[1L]:ci[2L], drop = FALSE]  
  }

  ## iterate:
  ## first shave areas outside of almost white rows and/or columns
  ## then shave to range of very dark rows and columns
  while(shave) {
    ri <- rowMeans(x) < 0.04
    if(sum(ri) > nrow(x) * trim/3) {
      ri <- ignore_single_true_line(ri)
      ri <- range(which(ri))
      ri <- if((ri[1L] - 1L) <= (nrow(x) - ri[2L])) c(ri[1L], nrow(x)) else c(1L, ri[2L])
    } else {
      ri <- c(1L, nrow(x))
    }
    ci <- colMeans(x) < 0.04
    if(sum(ci) > ncol(x) * trim/3) {
      ci <- ignore_single_true_line(ci)
      ci <- range(which(ci))
      ci <- if((ci[1L] - 1L) <= (ncol(x) - ci[2L])) c(ci[1L], ncol(x)) else c(1L, ci[2L])
    } else {
      ci <- c(1L, ncol(x))
    }
    x <- x[ri[1L]:ri[2L], ci[1L]:ci[2L], drop = FALSE]
    ri <- which(rowMeans(x) > 0.38)
    ci <- which(colMeans(x) > 0.38)
    if(length(ri) > 0L && length(ci) > 0L) {
      ri <- range(ri)
      ci <- range(ci)
      if(diff(ri) < nrow(x) * trim/2) {
        ri <- if(mean(ri) > nrow(x)/2) c(1L, ri[2L]) else c(ri[1L], nrow(x))
        rshave <- FALSE
      } else {
        rshave <- NULL
      }
      if(diff(ci) < ncol(x) * trim/2) {
        ci <- if(mean(ci) > ncol(x)/2) c(1L, ci[2L]) else c(ci[1L], ncol(x))
        cshave <- FALSE
      } else {
        cshave <- NULL
      }
      x <- x[ri[1L]:ri[2L], ci[1L]:ci[2L], drop = FALSE]
      if(is.null(rshave)) rshave <- sum(rowMeans(x) < 0.04) > nrow(x) * trim/3
      if(is.null(cshave)) cshave <- sum(colMeans(x) < 0.04) > ncol(x) * trim/3
      shave <- rshave | cshave
    } else {
      x <- x[0L, 0L, drop = FALSE]
      shave <- FALSE
    }
  }
  
  ## almost empty
  if(any(dim(x) < 5L)) return(0L)

  ## extract inside of box
  x <- subimage(x, c(0.5, 0.5), 1 - trim) ## NOTE: some more/less trimming here? Or computing rm/cm based on means rather than extremes?

  ## almost empty
  if(mean(x) < threshold[1L]) return(0L)

  ## moderately full or too full
  if(fuzzy) return(mean(x))
  if(mean(x) < threshold[2L]) {
    return(1L)
  } else {
    edges <- c(
      mean(subimage(x, c(0.5, 0.05), 0.1)),
      mean(subimage(x, c(0.5, 0.95), 0.1)),
      mean(subimage(x, c(0.05, 0.5), 0.1)),
      mean(subimage(x, c(0.95, 0.5), 0.1))
    )
    if(sort(edges)[2L] <= 0.1) {
      return(1L)
    } else {
      return(0L)
    }
  }
}

## read scanned PNG image into b/w pixel matrix and trim margins
trim_nops_scan <- function(x, verbose = FALSE, minrot = 0.002)
{
  ## read gray levels
  if(is.character(x)) {
    file <- x
    x <- png::readPNG(x)
  } else {
    file <- NULL
  }
  if(length(dim(x)) > 2L) {
    x <- if(dim(x)[3L] > 2L) pmin(x[, , 1L], x[, , 2L], x[, , 3L]) else x[, , 1L]
  }
  x <- matrix(as.integer(x < 0.7), nrow = nrow(x), ncol = ncol(x))
  d <- dim(x)

  ## force margins to be white
  x[, c(1L:round(0.02 * ncol(x)), round(0.98 * ncol(x)):ncol(x))] <- 0L
  x[c(1L:round(0.02 * nrow(x)), round(0.98 * nrow(x)):nrow(x)), ] <- 0L

  rot <- NULL
  while(is.null(rot) || (abs(rot) > minrot & abs(rot) < 0.05)) {

  ## rotate (if necessary)  
  if(!is.null(rot)) {
    rot <- 0.71 * rot ## try to avoid over-rotation
    if(verbose) cat(", rotating PNG")
    proj <- matrix(c(cos(rot), -sin(rot), sin(rot), cos(rot)), ncol = 2L)
    xcoord <- t(which(x > 0.5, arr.ind = TRUE))
    xcoord <- xcoord/d - 0.5
    xcoord <- proj %*% xcoord
    xcoord <- t(round(d * (xcoord + 0.5)))
    x[] <- 0L
    x[xcoord] <- 1L
  }

  ## find bottom markings (starting from bottom 3% of pixel rows)
  rb <- 0.97
  xbl <- x[seq(round(rb * d[1L]), d[1L]), seq(1, round(0.17 * d[2L]))]
  xbr <- x[seq(round(rb * d[1L]), d[1L]), seq(round(0.83 * d[2L]), d[2L])]

  while(mean(xbl) < 0.0016 | mean(xbr) < 0.0016 | mean(rowMeans(xbl) > 0.03) < 0.005 | mean(rowMeans(xbr) > 0.03) < 0.005) {
    rb <- rb - 0.01
    xbl <- x[seq(round(rb * d[1L]), d[1L]), seq(1, round(0.17 * d[2L]))]
    xbr <- x[seq(round(rb * d[1L]), d[1L]), seq(round(0.83 * d[2L]), d[2L])]  
  }

  get_mark <- function(x, type = c("row", "col"), zap = 0.35)
  {
    x[rowMeans(x) >= zap,] <- 0
    x[,colMeans(x) >= zap] <- 0

    type <- match.arg(type, c("row", "col"))
    x <- if(type == "row") rowMeans(x) else colMeans(x)
    which(x > mean(range(x)))
  }
  get_mean <- function(x, maxdist = 10) {
    mean(x[abs(x - median(x)) < maxdist])
  }
  
  rbl <- get_mark(xbl, "row")
  rbr <- get_mark(xbr, "row")
  rb <- round(get_mean(unique(c(rbl, rbr))))
  rb <- as.vector(d[1L] - (nrow(xbl) - rb))

  cl <- round(get_mean(get_mark(xbl, "col")))
  cr <- round(get_mean(get_mark(xbr, "col")))
  cl <- as.vector(cl)
  cr <- as.vector(d[2L] - (ncol(xbr) - cr))

  ## rotation angle
  rot <- asin((get_mean(rbl) - get_mean(rbr)) / (cr - cl))
  }
  if(abs(rot) > 0.05) stop("image is too skewed, cannot be rotated")

  ## find top markings
  ctl <- round(cl + (cr - cl) * (30 - 20) / (190 - 20))
  ctr <- round(cl + (cr - cl) * (160 - 20) / (190 - 20))
  xtl <- x[seq(1L, round(0.13 * d[1L])), seq(1, round(1.15 * ctl))]
  xtr <- x[seq(1L, round(0.13 * d[1L])), seq(0.9 * ctr, d[2L])]
  xtl[, seq(1, 0.33 * ncol(xtr))] <- 0
  xtl[seq(1, 0.25 * nrow(xtr)), ] <- 0
  xtr[, seq(0.4 * ncol(xtr), ncol(xtr))] <- 0

  rtl <- get_mark(xtl, "row")
  rtr <- get_mark(xtr, "row") ## may be affected by text close to the mark, hence not used by default
  if(length(rtl) > 0L) {
    rt <- as.vector(round(get_mean(unique(rtl))))
  } else {
    rt <- as.vector(min(rtr)) ## use top right scanner marking as fallback if top left is missing
  }
  
  if(abs((rb - rt) / (cr - cl) - (270 - 13) / (190 - 20)) > 0.02)
    warning("PNG does not seem to be correctly scaled")

  ## extract subimage within markings
  x[rt:rb, cl:cr]
}

## set up row x col regressors with gray values of pixels
digit_regressors <- function(x, nrow = 7, ncol = 5)
{
  d <- dim(x)
  if(any(d < c(nrow, ncol))) stop("image too small to extract digits")
  rw <- round(d[1L] * (0L:nrow/nrow))
  cl <- round(d[2L] * (0L:ncol/ncol))
  ix <- as.matrix(expand.grid(1:nrow, 1:ncol))
  rw1 <- rw[ix[, 1L]] + 1L
  rw2 <- rw[ix[, 1L] + 1L]
  cl1 <- cl[ix[, 2L]] + 1L
  cl2 <- cl[ix[, 2L] + 1L]
  rval <- sapply(1:nrow(ix), function(i) round(mean(x[rw1[i]:rw2[i], cl1[i]:cl2[i]]), digits = 4L))
  rval <- as.data.frame(t(rval))
  names(rval) <- paste("x", 1:ncol(rval), sep = "")
  rval$width <- round(d[2L]/d[1L], digits = 4L)
  return(rval)
}

## classify digits 
read_nops_digits <- function(x, type = c("type", "id", "scrambling"), adjust = FALSE)
{
  ## adjustment for coordinates (e.g. for reading 2nd string page)
  if(identical(adjust, TRUE)) adjust <- c(0.2065, 0)
  if(identical(adjust, FALSE)) adjust <- c(0, 0)
  
  ## extract image of numbers
  type <- match.arg(type, c("type", "id", "scrambling"))
  n <- switch(type,
    "type" = 3L,
    "id" = 11L,
    "scrambling" = 2L)
  err <- paste(rep.int("X", n), collapse = "")
  z <- try(switch(type,
    "type" = shave_box(subimage(x, c(0.3925 - adjust[1L], 0.074 - adjust[2L]), c(0.035, 0.078))),
    "id" = shave_box(subimage(x, c(0.3925 - adjust[1L], 0.275 - adjust[2L]), c(0.035, 0.19))),
    "scrambling" = {
      y <- shave_box(subimage(x, c(0.337 - adjust[1L], 0.545 - adjust[2L]), c(0.035, 0.078)), clip = FALSE)
      y[round(0.7 * nrow(y)):nrow(y), round(0.43 * ncol(y)):round(0.57 * ncol(y))] <- 0
      shave(y)
    }), silent = TRUE)
  if(inherits(z, "try-error")) return(err)

  ## split
  le <- NULL
  thresh <- 0
  while(length(le) != (2L * n - 1L) & thresh < 0.2) {
    thresh <- thresh + 0.01
    le <- rle(colMeans(z) < thresh)$lengths
  }
  if(length(le) != (2L * n - 1L)) return(err)
  le <- cumsum(c(0L, le))
  d <- lapply(1L:(length(le)/2L), function(i) z[, (le[2 * i - 1L] + 1L):(le[2 * i]), drop = FALSE])
  
  ## transform to regressors
  d <- try(do.call("rbind", lapply(d, digit_regressors)), silent = TRUE)
  if(inherits(d, "try-error")) return(err)

  ## get digits
  y <- ifelse(d$width < 0.5, 1L,
    ifelse(d$x8 < 0.15, 4L,
    ifelse(d$x30 < 0.15, 5L,
    ifelse(d$x1 > 0.55, 7L,
    ifelse((d$x7 + d$x12) > 1.05, 2L,
    ifelse((d$x17 + d$x18 + d$x19) < 0.18, 0L,
    ifelse((d$x4 + d$x11) < 0.5, 3L,
    ifelse((d$x4 + d$x32) < 1.05, 8L, 
    ifelse(d$x31 > 0.4, 9L,
    6L)))))))))
  y <- paste(y, collapse = "")

  return(y)
}

read_nops_answers <- function(x, threshold = c(0.04, 0.42), size = 0.03, trim = 0.3, n = 45L, adjust = FALSE)
{
  ## adjustment for coordinates (e.g. for reading 2nd string page)
  if(identical(adjust, TRUE)) adjust <- c(0.4243, -0.50025)
  if(identical(adjust, FALSE)) adjust <- c(0, 0)

  ## number of answer fields to read
  if(!(is.numeric(n) && isTRUE(n %in% 1L:45L))) n <- 45L

  ## 1-15
  coord1 <- cbind(0.5532 + rep(0L:2L, each = 25L) * 0.148 + rep(0L:4L, each = 5L) * 0.027,
    0.04125 + rep(0L:4L, 15L) * 0.047)
  ## 16-30
  coord2 <- coord1 + cbind(rep(0, 5 * 15), 0.376)
  ## 31-45
  coord3 <- coord2 + cbind(rep(0, 5 * 15), 0.376 * 60/64)
  coord <- rbind(coord1, coord2, coord3)

  ## ## zap numbers next to the boxes
  ## subimage(x, c(0.7542,         0.0095), c(0.42, 0.019)) <- 0L
  ## subimage(x, c(0.7542, 0.373 + 0.0095), c(0.42, 0.019)) <- 0L
  ## subimage(x, c(0.7542, 0.723 + 0.0095), c(0.42, 0.019)) <- 0L

  y <- matrix(sapply(1:(n * 5L), function(i)
    has_mark(subimage(x, coord[i,] - adjust, size), threshold = threshold, trim = trim)), ncol = 5L, byrow = TRUE)
  rval <- paste(apply(y, 1, paste, collapse = ""), collapse = " ")
  if(n < 45L) rval <- paste(rval, paste(rep.int("00000", 45L - n), collapse = " "))
  return(rval)
}

read_nops_registration <- function(x, threshold = c(0.04, 0.42), size = 0.036, trim = 0.3, regextra = 0L)
{
  coord <- cbind(0.166 + rep(0L:9L, each = 7L + regextra) * 0.027,
    0.681 + rep(-regextra:6L, 10L) * 0.047)
  err <- paste(rep.int("0", 7L + regextra), collapse = "")
  
  y <- try(matrix(sapply(1:nrow(coord), function(i)
    has_mark(subimage(x, coord[i,], size), threshold = threshold, fuzzy = TRUE, trim = trim)), ncol = 7L + regextra, byrow = TRUE),
    silent = TRUE)
  if(inherits(y, "try-error")) return(err)

  ## checked boxes per column
  cs <- colSums(y > 0)

  ## any column without checked box? -> return error
  ## NOTE: could optionally return 0 in those columns instead
  if(any(cs < 1L)) return(err)

  ## in columns with more than one checked box:
  ## use maximum within thresholds (if any) or minimum above threshold[2]
  for(i in which(cs > 1L)) {
    if(any(y[,i] >= threshold[1L] & y[,i] <= threshold[2L])) {
      y[y[,i] > threshold[2L], i] <- 0
    } else {
      y[y[,i] > min(y[y[,i] > 0, i]) + 0.0001, i] <- 0
    }
  }
  paste(apply(y, 2L, which.max) - 1, collapse = "")
}

read_nops_backup <- function(x, threshold = 0.15, size = 0.01)
  format(as.numeric(mean(subimage(x, c(0.381, 0.574), size)) > threshold[1L]))

## simple plotting function
imageplot <- function(x, ...) {
  d <- dim(x)
  xcoord <- t(which(x > 0.5, arr.ind = TRUE))
  xcoord <- t(xcoord/d)
  par(mar = rep(1, 4))
  plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, xlim = c(0, 1), ylim = c(0, 1), ...)
  if(prod(dim(xcoord)) > 0L) rect(xcoord[,2L] - 1/d[2L], 1 - (xcoord[,1L] - 1/d[1L]),
    xcoord[,2L], 1 - xcoord[,1L], col = "black", border = "transparent") 
}
