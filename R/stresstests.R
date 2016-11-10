## Test exercises.
stresstest_exercise <- function(file, n = 100, plot = TRUE,
  verbose = TRUE, seeds = NULL,
  stop_on_error = length(as.character(unlist(file))) < 2, ...)
{
  file <- as.character(unlist(file))
  if(length(file) > 1) {
    rval <- list()
    for(i in seq_along(file)) {
      rval[[file[i]]] <- stresstest_exercise(file[i], n = n, plot = FALSE,
        verbose = verbose, seeds = seeds, stop_on_error = stop_on_error, ...)
    }
    class(rval) <- c("stresstest_exercise_list", "stresstest_exercise", "list")
  } else {
    sq <- objects <- vector("list", length = n)
    seeds <- if(!is.null(seeds)) rep(seeds, length.out = n) else 1:n
    times <- rep(0, n)
    if(verbose)
      cat("testing file:", file, "\n---\n")
    for(i in 1:n) {
      set.seed(seeds[i])
      if(verbose) cat(seeds[i])
      .global_obj_before <- ls(envir = .GlobalEnv)
      times[i] <- system.time(xtmp <- try(xexams(file, driver = list("sweave" = list("envir" = .GlobalEnv)), ...),
        silent = TRUE))["elapsed"]
      .global_obj_after <- ls(envir = .GlobalEnv)
      ex_objects <- .global_obj_after[!(.global_obj_after %in% .global_obj_before)]
      objects[[i]] <- list()
      for(j in ex_objects)
        objects[[i]][[j]] <- get(j, pos = 1)
      remove(list = ex_objects, pos = 1)
      if(inherits(xtmp, "try-error")) {
        cat(xtmp)
        msg <- paste('an error occured when running file: "', file, '" using seed ', seeds[i], '!', sep = '')
        if(stop_on_error) {
          stop(msg)
        } else {
          warning(msg)
          return(list("file" = file, "seed" = seeds[i], "error" = xtmp))
        }
      }
      sq[[i]] <- list("solution" = xtmp[[1]][[1]]$metainfo$solution,
        "questionlist" = xtmp[[1]][[1]]$questionlist)
      if(i < n & verbose) cat("/")
    }
    if(verbose) cat("\n")

    extype <- xtmp[[1]][[1]]$metainfo$type

    solutions <- lapply(sq, function(x) { x$solution })
    questions <- lapply(sq, function(x) { x$questionlist })

    objects <- lapply(objects, function(x) {
      isf <- unlist(sapply(x, is.function))
      n <- unlist(sapply(x, length))
      if(is.null(isf))
        isf <- rep(FALSE, length(x))
      x[which((n == 1) & !isf)]
    })
    nobj <- unique(unlist(lapply(objects, names)))
    objects <- lapply(objects, function(x) {
      x <- as.data.frame(x[names(x) %in% nobj])
      if(!all(ok <- nobj %in% names(x))) {
        for(j in nobj[!ok])
          x[[j]] <- NA
      }
      x <- x[, nobj]
      x
    })
    objects <- do.call("rbind", objects)

    if(any(names(objects) %in% (no <- ls(envir = .GlobalEnv))))
      rm(list = names(objects)[names(objects) %in% no])

    rval <- list("seeds" = seeds, "runtime" = times)
    if(nrow(objects) > 0)
      rval$objects <- objects

    if(extype %in% c("num", "schoice", "mchoice")) {
      if(extype == "num") {
        if(length(solutions[[1]]) > 1)
          solutions <- lapply(solutions, mean)
        rval$solution <- unlist(solutions)
      }
      if(extype == "schoice") {
        pmat <- do.call("rbind", solutions)
        pmat <- t(t(pmat) * 1:ncol(pmat))
        rval$position <- pmat
        if(!any(is.na(as.numeric(gsub("$", "", questions[[1]], fixed = TRUE))))) {
          ordering <- do.call("rbind", lapply(questions, order, decreasing = TRUE))
          questions <- do.call("rbind", lapply(questions, function(x) {
            as.numeric(gsub("$", "", x, fixed = TRUE)) }))
          ordering <- (pmat > 0) * ordering
          i <- as.integer(rowSums(pmat))
          sol_num <- rep(NA, nrow(pmat))
          for(j in 1:nrow(pmat))
            sol_num[j] <- questions[j, i[j]]
          rval$ordering <- as.factor(rowSums(ordering))
          rval$solution <- sol_num
        }
      }
      if(extype == "mchoice") {
        ex_mat <- do.call("rbind", solutions)
        pmat <- t(t(ex_mat) * 1:ncol(ex_mat))
        rval$position <- pmat
        rval$ntrue <- apply(ex_mat, 1, sum)
      }
    } else {
      rval$solutions <- solutions
    }

    class(rval) <- c("stresstest_exercise", "list")
    attr(rval, "exinfo") <- c("file" = file, "type" = extype)

    if(plot) plot(rval, ...)
  }

  return(rval)
}


plot.stresstest_exercise <- function(x, ...)
{
  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  ask <- list(...)$ask; spar <- TRUE ## FIXME: should this function be exported?
  if(is.null(ask))
    ask <- TRUE

  if(ask) par("ask" = TRUE)
  if(spar) par(mfrow = c(2, 2))

  rainbow <- function(n) hcl(h = seq(0, 360 * (n - 1)/n, length = n), c = 50, l = 70)

  if(inherits(x, "stresstest_exercise_list")) {
    for(i in names(x)) {
      cat("stresstest plots for file:", i, "\n")
      plot.stresstest_exercise(x[[i]], ...)
    }
  } else {
    tr <- range(x$runtime)
    hist(x$runtime, freq = FALSE,
      main = paste("Runtimes ", fmt(min(tr), 4), "-", fmt(max(tr), 4), sep = ""),
      xlab = "Time", col = "lightgray")

    if(!is.null(x$solution)) {
      hist(x$solution, freq = FALSE,
        main = "Histogram of numeric solutions", xlab = "Solutions",
        col = "lightgray")
    }

    if(!is.null(x$objects)) {
      for(j in names(x$objects)) {
        if(is.numeric(x$objects[[j]]) | is.factor(x$objects[[j]]))
          plot(x$objects[[j]], x$runtime, xlab = j, ylab = "Runtime", main = paste("Runtimes vs.", j))
      }
      if(!is.null(x$solution)) {
        for(j in names(x$objects)) {
          if(is.numeric(x$objects[[j]]) | is.factor(x$objects[[j]])) {
            plot(x$objects[[j]], x$solution, xlab = j, ylab = "Solution", main = paste("Solutions vs.", j))
          }
        }
      }
    }

    if(!is.null(x$position)) {
      ptab <- table(x$position)
      ptab <- ptab[names(ptab) != "0"]
      barplot(ptab, ylab = "n", main = "Position of correct solution",
        xlab = "Position", col = rainbow(ncol(x$position)))
      image(t(x$position), col = c("white", rainbow(ncol(x$position))), axes = FALSE,
        main = "Position of correct solution", xlab = "Position")
      box()
      axis(1, at = seq(0, 1, length = ncol(x$position)), labels = 1:ncol(x$position))

      pfac <- as.factor(apply(x$position, 1, paste, collapse = "|"))
      if(nlevels(pfac) > length(names(ptab))) {
        barplot(table(pfac), main = "Combinations of correct solutions positions", ylab = "n",
          col = rainbow(nlevels(pfac)))
      }

      if(!is.null(x$ntrue)) {
        barplot(table(x$ntrue), main = "Number of correct solutions", ylab = "n",
          col = rainbow(length(unique(x$ntrue))))
      }
    }

    if(!is.null(x$ordering) & !is.null(x$objects)) {
      for(j in names(x$objects)) {
        if(is.numeric(x$objects[[j]]) | is.factor(x$objects[[j]])) {
          if(length(unique(x$objects[[j]])) > 1) {
            breaks <- if(is.numeric(x$objects[[j]])) {
              unique(quantile(x$objects[[j]],
                seq(0, 1, length = min(c(floor(0.5 * length(unique(x$objects[[j]]))), 10))),
                na.rm = TRUE))
            } else NULL
            if(length(breaks) < 2)
              breaks <- NULL
            spineplot(x$objects[[j]], x$ordering, xlab = j,
              ylab = "Solution order", main = paste("Conditional density:", j),
              breaks = breaks)
            plot(x$objects[[j]], x$solution, xlab = j, ylab = "Numeric solution")
          }
        }
      }
    }
  }

  invisible(NULL)
}

