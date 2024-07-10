named_tags <- function(x) {
  if(length(x) < 1L) return(list())
  x <- strsplit(x, "=", fixed = TRUE)
  names(x) <- sapply(x, "[", 1L)
  sapply(x, function(z) if(length(z) > 1L) paste(z[-1L], collapse = "=") else "TRUE")
}

exams_metainfo <- function(x, class = "exams_metainfo", tags = TRUE, factors = FALSE, ...) {
  ## return class exams_metainfo (list of lists) or data frame?
  class <- match.arg(class, c("exams_metainfo", "data.frame"))

  ## default (only option up to 2.4-0):
  ## list of lists with meta-information only (i.e., without question/questionlist/etc.)
  if(class == "exams_metainfo") {
    if(inherits(x, "exams_metainfo")) {
      return(x)
    } else {
      return(structure(lapply(x, function(xi) lapply(xi, "[[", "metainfo")),
        class = "exams_metainfo"))
    }
  }

  ## alternatively: data.frame with all metainfo elements (as list columns)
  n <- length(x)
  m <- length(x[[1L]])
  nm <- n * m
  d <- data.frame(replication = rep.int(1L:n, rep.int(m, n)), exercise = rep.int(1L:m, n))
  for(i in 1L:n) {
    for(j in 1L:m) {
      for(k in names(x[[i]][[j]]$metainfo)) {
        if(!(k %in% names(d))) d[[k]] <- vector(nm, mode = "list")
        if(!is.null(x[[i]][[j]]$metainfo[[k]])) d[[k]][[(i - 1L) * m + j]] <- x[[i]][[j]]$metainfo[[k]]
      }
    }
  }

  ## process tags to separate data.frame and merge (FIXME: prefix tags?)
  if(is.character(tags)) {
    tags_prefix <- tags
    tags <- TRUE
  } else {
    tags <- as.logical(tags)
    tags_prefix <- if(tags) "tag_" else ""
  }
  if(tags && any(lengths(d$tags) > 0L)) {
    tags_list <- lapply(d$tags, named_tags)
    lab <- unique(names(unlist(tags_list)))
    d2 <- matrix(NA_character_, nrow = nm, ncol = length(lab), dimnames = list(NULL, lab))
    for(i in 1L:nm) {
      if(length(tags_list[[i]]) > 0L) {
        d2[i, names(tags_list[[i]])] <- tags_list[[i]]
      }
    }
    d2 <- as.data.frame(d2)
    names(d2) <- paste0(tags_prefix, names(d2))
    d <- cbind(d, d2)
  }
  
  ## simplify list columns (if possible)
  for(i in seq_along(d)) {
    z <- d[[i]]
    iz <- lengths(z)
    if(all(iz %in% 0L:1L)) {
      iz <- which(iz == 1L)
      z <- unlist(z)
      if(all(z == "TRUE", na.rm = TRUE)) {
        z <- as.logical(z)
        z[is.na(z)] <- FALSE
      } else {
        zn <- suppressWarnings(as.numeric(z))
        if(all(!is.na(zn[!is.na(z)]))) z <- zn
      }
      if(factors && (is.character(z) || is.logical(z))) {
        z <- factor(z)
        d[[i]] <- factor(d[[i]], levels = levels(z))
      } else {
        d[[i]] <- rep.int(NA, nrow(d))
      }
      d[[i]][iz] <- z
    }
  }
  
  return(d)
}

print.exams_metainfo <- function(x, which = NULL, block = NULL, ...) {
  which <- if(is.null(which)) names(x) else {
    if(is.numeric(which)) names(x)[which] else which
  }
  n <- length(x[[1L]])
  for(i in which) {
    cat("\n", i, "\n", sep = "")
    for(j in 1L:n) {
      writeLines(strwrap(
        paste0(j, ". ", x[[i]][[j]]$string),
	indent = 4 + nchar(format(n)) - nchar(format(j)), exdent = 6 + nchar(format(n))
      ))
      if(!is.null(block) && j %% as.integer(block) == 0L) cat("\n")
    }
  }
  cat("\n")
  invisible(x)
}

