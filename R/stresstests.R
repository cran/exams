## Test exercises.
stresstest_exercise <- function(file, n = 100,
  verbose = TRUE, seeds = NULL,
  stop_on_error = length(as.character(unlist(file))) < 2, ...)
{
  file <- as.character(unlist(file))
  if(length(file) > 1) {
    rval <- list()
    for(i in seq_along(file)) {
      attr(n, "stress.list") <- TRUE
      rval[[file[i]]] <- stresstest_exercise(file[i], n = n,
        verbose = verbose, seeds = seeds, stop_on_error = stop_on_error, ...)
    }
    class(rval) <- c("stress.list", "stress", "list")
  } else {
    stress_env <- .GlobalEnv

    ## stress_env <- new.env()
    ## on.exit(rm(stress_env))

#    loadNamespace("tools")

#    stress_EvalWithOpt <- function(expr, options) {
#      if(options$eval) {
#        res <- try(withVisible(eval(expr, stress_env)), silent = TRUE)
#        if(inherits(res, "try-error")) return(res)
#        if(options$print | (options$term & res$visible))
#          print(res$value)
#      }
#      return(res)
#    }
#    runcode <- makeRweaveLatexCodeRunner(evalFunc = stress_EvalWithOpt)

    sq <- objects <- vector("list", length = n)
    seeds <- if(!is.null(seeds)) rep(seeds, length.out = n) else 1:n
    times <- rep(0, n)
    if(verbose & !is.null(attr(n, "stress.list")))
      cat("---\ntesting file:", file, "\n---\n")
    for(i in 1:n) {
      set.seed(seeds[i])
      if(verbose) cat(seeds[i])
      .global_obj_before <- ls(envir = stress_env)
      times[i] <- system.time(xtmp <- try(xexams(file, driver = list("sweave" = list("envir" = stress_env)), ...),
        silent = TRUE))["elapsed"]
      .global_obj_after <- ls(envir = stress_env)
      ex_objects <- .global_obj_after[!(.global_obj_after %in% .global_obj_before)]
      objects[[i]] <- list()
      for(j in ex_objects)
        objects[[i]][[j]] <- get(j, envir = stress_env)
      remove(list = ex_objects, envir = stress_env)
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

    if(any(names(objects) %in% (no <- ls(envir = stress_env))))
      rm(list = names(objects)[names(objects) %in% no])

    rval <- list("seeds" = seeds, "runtime" = times)
    if(nrow(objects) > 0)
      rval$objects <- objects

    if(extype %in% c("num", "schoice", "mchoice")) {
      if(extype == "num") {
        if(length(solutions[[1]]) > 1)
          solutions <- lapply(solutions, mean)
        rval$solution <- unlist(solutions)
	nchoice <- 0
      }
      if(extype == "schoice") {
        pmat <- do.call("rbind", solutions)
        pmat <- t(t(pmat) * 1:ncol(pmat))
        rval$position <- pmat
        rank <- do.call("rbind", lapply(questions, function(x) {
          x <- gsub("$", "", gsub(" ", "", x, fixed = TRUE), fixed = TRUE)
          if(!all(is.na(suppressWarnings(as.numeric(x)))))
            x <- as.numeric(x)
          rank(x, ties.method = "min")
        }))
        if(all(!is.na(suppressWarnings(as.numeric(gsub("$", "", questions[[1]], fixed = TRUE)))))) {
          questions <- lapply(questions, function(x) {
            as.numeric(gsub("$", "", x, fixed = TRUE)) })
          questions <- do.call("rbind", questions)
          i <- as.integer(rowSums(pmat))
          sol_num <- rep(NA, nrow(pmat))
          for(j in 1:nrow(pmat))
            sol_num[j] <- questions[j, i[j]]
          rval$solution <- sol_num
        }
        rank <- (pmat > 0) * rank
        rval$rank <- as.factor(as.integer(rowSums(rank)))
	nchoice <- NCOL(pmat)
      }
      if(extype == "mchoice") {
        ex_mat <- do.call("rbind", solutions)
        pmat <- t(t(ex_mat) * 1:ncol(ex_mat))
        rval$position <- pmat
        rval$ntrue <- apply(ex_mat, 1, sum)

        rank <- lapply(questions, function(x) {
          order(gsub("$", "", gsub(" ", "", x, fixed = TRUE), fixed = TRUE), decreasing = TRUE)
        })
        rank <- do.call("rbind", rank)
        rval$rank <- rank * do.call("rbind", solutions)
	nchoice <- NCOL(pmat)
      }
    } else {
      rval$solutions <- solutions
      nchoice <- 0
    }

    class(rval) <- c("stress", "list")
    attr(rval, "exinfo") <- c("file" = file, "type" = extype, "nchoice" = nchoice)
  }

  return(rval)
}


as.data.frame.stress <- function(x, ...)
{
  names(x) <- paste(".", names(x), sep = "")
  do.call("cbind", x)
}


plot.stress <- function(x, type = c("overview", "solution", "rank", "runtime"),
  threshold = NULL, variables = NULL, spar = TRUE, ask = TRUE, ...)
{
  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  type <- match.arg(type)

  rainbow <- function(n) hcl(h = seq(0, 360 * (n - 1)/n, length = n), c = 50, l = 70)

  if(inherits(x, "stress.list")) {
    par("ask" = ask)
    for(i in names(x)) {
      cat("stresstest plots for file:", i, "\n")
      plot.stress(x[[i]], type = type, threshold = threshold,
        variables = variables, spar = spar, ask = ask, ...)
    }
  } else {
    if(type == "overview") {
      k <- 0
      for(j in c("runtime", "solution", "position", "rank", "ntrue")) {
        if(!is.null(x[[j]]) & !is.list(x[[j]]))
          k <- k + 1
      }

      if(spar) {
        if(k < 3)
          par(mfrow = c(1, k))
        else
          par(mfrow = c(2, 2))
      }

      if(!is.null(x$runtime)) {
        tr <- range(x$runtime)
        hist(x$runtime, freq = FALSE,
          main = paste("Runtimes ", fmt(min(tr), 4), "-", fmt(max(tr), 4), sep = ""),
          xlab = "Time", col = "lightgray")
      }

      if(!is.null(x$solution) & !is.list(x$solution)) {
        hist(x$solution, freq = FALSE,
          main = "Histogram of numeric solutions", xlab = "Solutions",
          col = "lightgray")
      }

      nchoice <- as.numeric(attr(x, "exinfo")["nchoice"])
      if(!is.null(x$position)) {
        ptab <- table(factor(x$position, levels = 0:nchoice))
        ptab <- ptab[names(ptab) != "0"]
        barplot(ptab, ylab = "n", main = "Position of correct solution",
          xlab = "Position", col = rainbow(ncol(x$position)))
#        image(t(x$position), col = c("white", rainbow(ncol(x$position))), axes = FALSE,
#          main = "Position of correct solution", xlab = "Position")
#        box()
#        axis(1, at = seq(0, 1, length = ncol(x$position)), labels = 1:ncol(x$position))
      }

      if(!is.null(x$rank)) {
        ptab <- table(factor(x$rank, levels = 0:nchoice))
        ptab <- ptab[names(ptab) != "0"]
        barplot(ptab, ylab = "n", main = "Rank of correct solution",
          xlab = "Rank", col = rainbow(ncol(x$position)))
      }

      if(!is.null(x$ntrue)) {
        barplot(table(x$ntrue), main = "Number of correct solutions", ylab = "n",
          col = rainbow(length(unique(x$ntrue))))
      }
    }

    spineplot2 <- function(x, y, threshold = NULL, ...) {
      if(is.numeric(x) | is.factor(x)) {
        if(is.factor(x)) {
          if(nlevels(x) > 10)
            return(invisible(NULL))
        }
        if(length(unique(x)) > 1) {
          breaks <- if(is.numeric(x)) {
            unique(quantile(x,
              seq(0, 1, length = min(c(floor(0.5 * length(unique(x))), 10))),
              na.rm = TRUE))
          } else NULL
          if(length(breaks) < 2)
            breaks <- NULL
          if((length(unique(x)) < 10) & !is.factor(x))
            spineplot(as.factor(x), y, ...)
          else
            spineplot(x, y, breaks = breaks, ...)
        }
      }
    }

    plot2 <- function(x, y, threshold = NULL, ylab = NULL, ...) {
      if((is.numeric(x) | is.factor(x)) & (length(unique(x)) > 1)) {
        if(is.factor(x)) {
          if(nlevels(x) > 10)
            return(invisible(NULL))
        }
        if(is.null(threshold)) {
          plot(x, y, ylab = ylab, ...)
        } else {
          breaks <- if(is.numeric(x)) {
            unique(quantile(x,
              seq(0, 1, length = min(c(floor(0.5 * length(unique(x))), 10))),
              na.rm = TRUE))
          } else NULL
          if(length(breaks) < 2)
            breaks <- NULL
          ylab <- paste(ylab, "<=", threshold)
          if((length(unique(x)) < 10) & !is.factor(x))
            spineplot(as.factor(x), factor(y <= threshold), breaks = breaks, ylab = ylab, ...)
          else
            spineplot(x, factor(y <= threshold), breaks = breaks, ylab = ylab, ...)
        }
      }
    }

    if(!is.null(x$objects)) {
      if(is.null(variables)) {
        variables <- names(x$objects)
      } else {
        v2 <- NULL
        for(j in variables)
          v2 <- c(v2, grep(j, variables, value = TRUE, fixed = TRUE))
        variables <- unique(v2)
      }

      par("ask" = ask)
      if(spar) {
        if(length(variables) > 2) {
          if(length(variables) > 3)
            par(mfrow = c(2, 2))
          else
            par(mfrow = c(1, 3))
        } else {
          par(mfrow = c(1, length(variables)))
        }
      }

      if(type == "runtime") {
        for(j in variables) {
          plot2(x$objects[[j]], x$runtime, threshold = threshold,
            xlab = j, ylab = "Runtime", main = paste("Runtimes vs.", j), ...)
        }
      }

      if((type == "solution") & !is.list(x$solution) & !is.null(x$solution)) {
        for(j in variables) {
          plot2(x$objects[[j]], x$solution, threshold = threshold,
            xlab = j, ylab = "Solution", main = paste("Solutions vs.", j), ...)
        }
      }

      if((type == "rank") & !is.null(x$rank)) {
        if(is.matrix(x$rank))
          x$rank <- as.factor(apply(x$rank, 1, paste, collapse = "|"))
        for(j in variables) {
          spineplot2(x$objects[[j]], x$rank, xlab = j,
            ylab = "Solution rank", main = paste("Solution rank frequencies:", j), ...)
        }
      }
    }
  }

  invisible(NULL)
}

