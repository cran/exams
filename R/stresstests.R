## Test exercises.
stresstest_exercise <- function(file, n = 100, verbose = TRUE, seeds = NULL,
                                stop_on_error = length(as.character(unlist(file))) < 2,
                                timeout = NULL, maxit = getOption("num_to_schoice_maxit", -10000L), ...) {

  ## process specifications of timeout, stop_on_error, and maxit
  stopifnot("'timeout' must be NULL or a vector of positive numerics of length 1, 2, or 3" =
        is.null(timeout) || (is.numeric(timeout) && length(timeout) >= 1L && length(timeout) <= 3L && all(timeout > 0)))

  stop_on_error <- as.logical(stop_on_error)[[1L]]
  stopifnot("'stop_on_error' must evaluate to logical TRUE or FALSE" = isTRUE(stop_on_error) || isFALSE(stop_on_error))

  maxit_orig <- getOption("num_to_schoice_maxit")
  options(num_to_schoice_maxit = maxit)
  on.exit(options(num_to_schoice_maxit = maxit_orig))

  file <- as.character(unlist(file))
  if(length(file) > 1L) {
    rval <- list()
    for(i in seq_along(file)) {
      attr(n, "stress.list") <- TRUE
      rval[[file[i]]] <- stresstest_exercise(file[i], n = n,
        verbose = verbose, seeds = seeds, stop_on_error = stop_on_error, timeout = timeout, ...)
    }
    class(rval) <- c("stress.list", "stress", "list")
  } else {
    # If timeout is numeric
    # - Length 1: Use 'timeout' for max time on cpu and time elapsed
    # - Length 2: Use 'timeout[1]' for cpu, 'timeout[2]' for time elapsed
    # - Length 3: Use 'timeout[1]' for cpu, 'timeout[2]' for time elapsed, and timeout[3] for "overall time"
    if (is.numeric(timeout)) {
      if (length(timeout) == 1L) {
        timeout <- list(cpu = timeout, elapsed = timeout, total = Inf)
      } else if (length(timeout) == 2L) {
        timeout <- list(cpu = timeout[[1]], elapsed = timeout[[2]], total = Inf)
      } else {
        timeout <- list(cpu = timeout[[1]], elapsed = timeout[[2]], total = timeout[[3]])
      }
    }

    # Stop if the file does not exist (and is not one of the built-in files)
    if (!(tolower(substr(file, nchar(file) - 3L, nchar(file))) %in% c(".rnw", ".rmd")))
        file <- paste0(file, ".Rnw")
    if (!file.exists(file) && !file.exists(file.path(find.package("exams"), "exercises", file)))
        stop(sprintf("Cannot find file: %s.", file))

    stress_env <- .GlobalEnv

    # If length(seed) < n: set n to length(seed).
    # If length(seed) > n: take first n entries of seed.
    if(!is.null(seeds)) {
        if (length(seeds) < n) { n <- length(seeds) } else { seeds <- seeds[1:n] }
    } else { seeds <- 1:n }

    ## Setting up objects to store objects generated by the question,
    ## solution and question list (sq), execution times, as well as
    ## warnings and errors.
    objects         <- lapply(seq_len(n), function(x) list())
    sq              <- vector("list", n)
    times           <- rep(0, n)
    xexams_we_count <- c(warnings = 0, errors = 0)
    ## rep(charvec0, ...) as a fast implementation
    charvec0        <- list(vector("character"))
    xexams_warnings <- xexams_errors <- rep(charvec0, n)
    xexams_extype   <- rep(NA_character_, n)

    if(verbose & !is.null(attr(n, "stress.list")))
      cat("---\nTesting file:", file, "\n")

    ## Number format for output
    tmp_num_fmt  <- paste0("%", nchar(as.character(n)), "d")
    tmp_seed_fmt <- paste0("(seed = %", nchar(max(seeds)), "d)")

    for (i in seq_len(n)) {
      set.seed(seeds[i])
      if (verbose && stop_on_error) {
          cat(if (i > 1L) "/", seeds[i], sep = "")
      } else if (verbose) {
          cat(sprintf(paste0("Randomization: ", tmp_num_fmt, "/", tmp_num_fmt, " %s"),
              i, n, sprintf(tmp_seed_fmt, seeds[i])), "   ",
              sprintf("[warn %d, err %d]", xexams_we_count["warnings"], xexams_we_count["errors"]), "\r")
      }

      ## Catching environment to trace/store objects generated in the exercise
      .global_obj_before <- ls(envir = stress_env)

      ## Setting timeout (max cpu/elapsed time allowed to render the exam)
      ## FIXME: plus workaround checking open hooks
      if (!is.null(timeout)) {
        setTimeLimit(cpu = timeout$cpu, elapsed = timeout$elapsed, transient = TRUE)
        .hooks_before <- ls(envir = .userHooksEnv)
      }

      ## Empty vectors for fetching warnings and errors
      xtmp_warnings <- xtmp_errors <- vector("character")
      times[i] <- system.time({
        ## tryCatch: Catches errors where execution fails; will return 'error' object (exception, `e`)
        ## withCallingHandlers: Fetching warnings and errors; not terminating execution but
        ##           fetching all thrown warnings/errors in the two vectors xtmp_warnings, xtmp_erros.
        ##           Used to report suspicious/errornous questions if stop_on_error = FALSE.
        xtmp <- ##suppressMessages(
                  tryCatch(
                    withCallingHandlers(
                        xexams(file, driver = list("sweave" = list(envir = stress_env)), ...),
                        warning = function(w) xtmp_warnings <<- c(xtmp_warnings, w$message),
                        error   = function(e) xtmp_errors   <<- c(xtmp_errors,   e$message)
                    ), ## End withCallingHandlers
                  error = function(e) e) ## End tryCatch
                ##) ## End suppressMessages
      })["elapsed"] ## End of system.time

      ## Resetting timeout
      ## FIXME: plus workaround closing additional open hooks (from `evaluate` pkg)
      if (!is.null(timeout)) {
        setTimeLimit(cpu = Inf, elapsed = Inf, transient = TRUE)
        .hooks_after <- ls(envir = .userHooksEnv)
        .hooks_to_remove <- setdiff(.hooks_after, .hooks_before)
        .hooks_to_remove <- .hooks_to_remove[!(startsWith(.hooks_to_remove, "UserHook::") & endsWith(.hooks_to_remove, "::onLoad"))]
        for(hook in .hooks_to_remove) setHook(hook, NULL, "replace")
      }

      ## tryCatch reported termination (error) and stop_on_error = TRUE: Stop execution.
      if (stop_on_error && inherits(xtmp, "error")) {
          if (grepl("reached elapsed time limit", xtmp$message)) {
            stop(sprintf("Elapsed time limit of %s seconds reached.", format(timeout$elapsed)))
          } else {
            stop(xtmp)
          }
      } else if (inherits(xtmp, "error")) {
          warning("an error occurred when running file: \"", file, "\" using seed ", seeds[i], "!")
      }

      ## Checking 'overall time'. If set (and exceeded) fill remaining result
      ## with dummy values and break the loop immediately.
      if (!is.null(timeout)) {
        if (sum(times) > timeout$total) {
          msg <- sprintf("Overall time limit of %.1f seconds exceeded", timeout$total)
          if (stop_on_error) stop(msg)
          if (verbose) cat("\n ...", msg, "\n", sep = "")
          for (j in seq.int(i, n)) {
            xexams_extype[j]   <- NA
            xexams_errors[[j]] <- msg
            sq[[j]] <- list(solution = NA, questionlist = NA)
          }
          break
        }
      }

      ## Else (tryCatch reported termination but stop_on_error is FALSE); set xtmp to NULL
      ## for the rest of this function. If everything worked, `xtmp` is a list
      ## (as returned by xexams).
      if (inherits(xtmp, "error")) xtmp <- NULL

      ## Increasing overall counter for warnings and errors (xexams_we_count)
      ## as well as combining and storing these warnings and errors
      xexams_we_count      <- xexams_we_count + c(length(xtmp_warnings), length(xtmp_errors))
      xexams_warnings[[i]] <- xtmp_warnings
      xexams_errors[[i]]   <- xtmp_errors
      rm(xtmp_warnings, xtmp_errors)

      ## Extracting new objects generated by the exercise
      .global_obj_after <- ls(envir = stress_env)
      ex_objects <- .global_obj_after[!(.global_obj_after %in% .global_obj_before)]

      ## Store newly created objects for iteration i and remove them from the stress_env (globalEnv)
      for (j in ex_objects) objects[[i]][[j]] <- get(j, envir = stress_env)
      remove(list = ex_objects, envir = stress_env)

      ## Store solution and question list. Set to NA in case we encountered an error
      ## and, therefore, have no solution/questionlist returned by xexams().
      if (is.null(xtmp)) {
        xexams_extype[i] <- NA
        sq[[i]] <- list(solution     = NA,
                        questionlist = NA)
      } else {
        xexams_extype[i] <- xtmp[[1]][[1]]$metainfo$type
        sq[[i]] <- list(solution     = xtmp[[1]][[1]]$metainfo$solution,
                        questionlist = xtmp[[1]][[1]]$questionlist)
      }
    }

    ## Final output; updates the last printed line to show the correct overall total
    ## number of warnings and errors recorded.
    if (verbose && !stop_on_error) {
       cat(sprintf(paste0("Randomization: ", tmp_num_fmt, "/", tmp_num_fmt, " %s"),
           i, n, sprintf(tmp_seed_fmt, seeds[i])), "   ",
           sprintf("[warn %d, err %d]", xexams_we_count["warnings"], xexams_we_count["errors"]), "\r")
    }
    cat("\n")

    ## If all failed we have no information about the type of the question, store NA.
    ## Else we take the first type (as it is always the same question).
    extype <- if (all(is.na(xexams_extype))) NA else na.omit(xexams_extype)[[1]]
    rm(xexams_extype)

    ## Combine solutions and questions from i = 1, ..., n
    solutions <- lapply(sq, function(x) { x$solution })
    questions <- lapply(sq, function(x) { x$questionlist })

    ## Combining objects created by the questions from i = 1, ..., n
    objects <- lapply(objects, function(x) {
      if (length(x) == 0) return(NULL)
      isf <- unlist(sapply(x, is.function))
      len <- unlist(sapply(x, length))
      x[which(len == 1 & !isf)]
    })

    ## If any of the elements in 'objects' is empty (due to an error occurred
    ## in xexams()) we need to 'fake' it.
    obj_isnull <- sapply(objects, is.null)
    if (any(obj_isnull) && !all(obj_isnull)) {
        obj_dummy <- objects[[which(!obj_isnull)[[1]]]]
        obj_dummy <- setNames(as.list(rep(NA, length(obj_dummy))), names(obj_dummy))
        for (k in which(obj_isnull)) objects[[k]] <- obj_dummy
        rm(obj_isnull, obj_dummy)
    }

    ## Turn 'objects' into data.frame
    nobj <- unique(unlist(lapply(objects, names)))
    objects <- lapply(objects, function(x) {
      x <- as.data.frame(x[names(x) %in% nobj])
      if (!all(ok <- nobj %in% names(x))) {
        for (j in nobj[!ok]) x[[j]] <- NA
      }
      x[, nobj, drop = FALSE]
    })

    objects <- do.call("rbind", objects)

    if(any(names(objects) %in% (no <- ls(envir = stress_env))))
      rm(list = names(objects)[names(objects) %in% no])

    rval <- list("seeds" = seeds, "runtime" = times)

    ## Appending warnings and errors (list of character vectors which
    ## can be of length 0 if all's fine) and overall total counts of
    ## warnings and errors (can exceed 'n').
    rval$warnings <- xexams_warnings
    rval$errors   <- xexams_errors
    rval$count    <- xexams_we_count

    if(nrow(objects) > 0)
      rval$objects <- objects

    if(extype %in% c("num", "schoice", "mchoice")) {

      ## Preparing return for numeric exercises
      if(extype == "num") {
        if(length(solutions[[1]]) > 1)
          solutions <- lapply(solutions, mean)
        rval$solution <- unlist(solutions)
        nchoice <- 0
      }

      ## Preparing return for schoice exercises
      if(extype == "schoice") {
        pmat <- do.call("rbind", solutions)
        pmat <- t(t(pmat) * 1:ncol(pmat))
        rval$position <- pmat
        rank <- do.call("rbind", lapply(questions, function(x) {
          x <- gsub("$", "", gsub(" ", "", x, fixed = TRUE), fixed = TRUE)
          if(!anyNA(suppressWarnings(as.numeric(x)))) x <- as.numeric(x)
          rank(x, ties.method = "min")
        }))
        if(!anyNA(suppressWarnings(as.numeric(gsub("$", "", questions[[1]], fixed = TRUE))))) {
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

      ## Preparing return for mchoice exercises
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

    ## Fallback for other question types (not num, schoice, mchoice)
    } else {
      rval$solutions <- solutions
      nchoice <- 0
    }

    class(rval) <- c("stress", "list")
    attr(rval, "exinfo") <- c("file" = file, "type" = extype, "nchoice" = nchoice)
  }

  return(rval)
}


as.data.frame.stress <- function(x, ...) {
  x <- x[!names(x) %in% c("count")]
  names(x) <- paste(".", names(x), sep = "")
  do.call("cbind", x)
}


plot.stress <- function(x, type = c("overview", "solution", "rank", "runtime", "warnings", "error"),
  threshold = NULL, variables = NULL, spar = TRUE, ask = TRUE, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(op)))

  type <- match.arg(type)

  rainbow <- function(n) hcl(h = seq(0, 360 * (n - 1)/n, length = n), c = 50, l = 70)

  ## Checking input argument 'variables' if set.
  if (!is.null(variables)) {
      variables <- unique(variables)
      stopifnot("not all variables defined on 'variables' found in 'x$object'" =
                all(variables %in% names(x$objects)))
  }

  ## Need this for later (plot type = 'warnings')
  arg_variables <- variables

  ## Helper function to show a label top right in case the plot does
  ## not show results for all randomizations as some ran into erros,
  ## not providing solutions, objects, ranks, ...
  add_note <- function(x) {
    tmp <- with(list(z = x), c(shown = sum(!is.na(z)), total = length(z)))
    if (tmp["total"] > tmp["shown"])
        mtext(side = 3, adj = 1, line = 0.2, cex = 1, col = "deeppink", font = 2,
              sprintf("Shown: %d/%d", tmp["shown"], tmp["total"]))
  }

  if (inherits(x, "stress.list")) {
    par("ask" = ask)
    for(i in names(x)) {
      cat("stresstest plots for file:", i, "\n")
      plot.stress(x[[i]], type = type, threshold = threshold,
        variables = variables, spar = spar, ask = ask, ...)
    }
  } else {
    ## No question type stored? Most likely as all randomizations in the
    ## stresstest encountered errors!
    if (is.na(attr(x, "exinfo")[["type"]]) && all(!is.na(x$error))) {
        warning("'stress' object does not provide a type as all randomizations encountered errors.")
        ## setting type = "errors" to at least show the barplot with
        ## error counts and warning counts.
        type <- "error"
    }

    if(type == "overview") {
      k <- 0
      for(j in c("runtime", "solution", "position", "rank", "ntrue")) {
        if(!is.null(x[[j]]) & !is.list(x[[j]])) k <- k + 1
      }

      if(spar) {
        if(k < 3) par(mfrow = c(1, k)) else par(mfrow = c(2, 2))
      }

      if(!is.null(x$runtime)) {
        tr <- range(x$runtime)
        hist(x$runtime, freq = FALSE,
          main = paste("Runtimes ", fmt(min(tr), 4), "-", fmt(max(tr), 4), sep = ""),
          xlab = "Time", col = "lightgray")
        add_note(x$objects[[j]])
      }

      if(!is.null(x$solution) & !is.list(x$solution)) {
        hist(x$solution, freq = FALSE,
          main = "Histogram of numeric solutions", xlab = "Solutions",
          col = "lightgray")
        add_note(x$solution)
      }

      nchoice <- as.numeric(attr(x, "exinfo")["nchoice"])
      if(!is.null(x$position)) {
        ptab <- table(factor(x$position, levels = 0:nchoice))
        ptab <- ptab[names(ptab) != "0"]
        barplot(ptab, ylab = "n", main = "Position of correct solution",
          xlab = "Position", col = rainbow(ncol(x$position)))
        add_note(x$position)
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
        add_note(x$rank)
      }

      if(!is.null(x$ntrue)) {
        barplot(table(x$ntrue), main = "Number of correct solutions", ylab = "n",
          col = rainbow(length(unique(x$ntrue))))
        add_note(x$ntrue)
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

    ### Errors and warnings
    if (type == "error" || type == "warnings") {
        mfhold <- par("mfrow"); par(mfrow = c(1, 1))
        tmp_n <- length(x$seeds)              # Total number of randomizations
        tmp_w <- sum(lengths(x$warnings) > 0) # Nr. of randomizations w/ 1 or more warnings
        tmp_e <- sum(lengths(x$errors)   > 0) # Nr. of randomizations w/ errors
        barplot(c(OK = tmp_n - tmp_w - tmp_e, Warnings = tmp_w, Error = tmp_e),
                col = c("gray80", "tomato", "deeppink"),
                ylab = "Frequency",
                main = paste("Number of randomizations without warnings and errors (OK),",
                             "with at least one warning, and with error", sep = "\n"))
        par(mfrow = mfhold)
    }

    ## Plots including traced objects
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
          add_note(x$objects[[j]] * x$runtime)
        }
      }

      if((type == "solution") & !is.list(x$solution) & !is.null(x$solution)) {
        for(j in variables) {
          plot2(x$objects[[j]], x$solution, threshold = threshold,
            xlab = j, ylab = "Solution", main = paste("Solutions vs.", j), ...)
          add_note(x$objects[[j]] * x$solution)
        }
      }

      if((type == "rank") & !is.null(x$rank)) {
        if(is.matrix(x$rank))
          x$rank <- as.factor(apply(x$rank, 1, paste, collapse = "|"))
        for(j in variables) {
          spineplot2(x$objects[[j]], x$rank, xlab = j,
            ylab = "Solution rank", main = paste("Solution rank frequencies:", j), ...)
          add_note(x$objects[[j]] * x$rank)
        }
      }

      ## Conditional density histogram when/where warnings occurred
      if (type == "warnings" & !is.null(x$warnings)) {
          if (!is.list(x$solution)) {
            mirrored_hist(x$solution, lengths(x$warnings) > 0, main = "Solution | warnings")
            add_note(x$solution)
          }

          # Only numeric variables!
          num_variables <- variables[sapply(x$objects[, variables], is.numeric)]

          for (j in num_variables) {
            mirrored_hist(x$objects[[j]], lengths(x$warnings) > 0, main = sprintf("%s | warnings", j))
            add_note(x$objects[[j]])
          }

          ## Unique combinations (max 5 _OR_ user specified)
          if (length(num_variables) > 5 && is.null(arg_variables)) {
              message("Found ", length(num_variables), " numeric variables in `$object`, ",
                      "only the first five are used. Check argument 'variables' to ",
                      "plot specific variables.")
              num_variables <- head(num_variables, 5L)
          }
          grd <- t(combn(num_variables, 2))
          for (i in seq_len(nrow(grd))) {
              nm  <- grd[i, ]
              tmp <- setNames(data.frame(x$objects[[nm[[1]]]], x$objects[[nm[[2]]]]), nm)
              if (nrow(tmp) == 0L) next
              tmp_w <- lengths(x$warnings) > 0
              plot(as.formula(sprintf("%s ~ %s", grd[i, 1], grd[i, 2])), data = tmp,
                   pch = ifelse(tmp_w, 19, 1),
                   cex = ifelse(tmp_w, 1.5, 0.75),
                   col = ifelse(tmp_w, "tomato", 1),
                   main = sprintf("%s vs. %s", nm[[1]], nm[[2]]))
              add_note(ifelse(complete.cases(tmp), TRUE, NA))
          }
      }
    }
  }

  invisible(NULL)
}


## Mirrored (condition) histogram used for warning plots
mirrored_hist <- function(x, y, ...) {
    stopifnot(is.logical(y))
    ## Calculate useful breaks for solution
    bk <- hist(x, plot = FALSE)$breaks
    ## Calculate density condition on whether or not warnings occurred
    h1 <- if (all(is.na(x[y])) || length(x[y]) == 0L)  {
        list(counts = NA)
    } else {
        hist(x[y],  plot = FALSE, breaks = bk)
    }
    h2 <- if (all(is.na(x[!y])) || length(x[!y]) == 0L) {
        list(counts = NA)
    } else {
        hist(x[!y], plot = FALSE, breaks = bk)
    }

    ## Calculate y limits
    ylim <- max(c(h1$counts, h2$counts), na.rm = TRUE) * c(-1, 1)
    plot(NA, xlim = range(bk), ylim = ylim, xlab = "solution", ylab = "Conditional counts",
         yaxt = "n", ...)
    yat <- pretty(ylim)
    axis(side = 2, at = yat, labels = abs(yat))
    mtext(side = 4, at = max(ylim) / 2, "Warnings",    line = 0.2, col = "tomato")
    mtext(side = 4, at = min(ylim) / 2, "No warnings", line = 0.2, col = "gray50")

    ## Upper histogram where y == TRUE
    if (!all(is.na(h1$counts)))
        rect(xleft   = head(h1$breaks, -1),     xright = tail(h1$breaks, -1),
             ybottom = rep(0, length(h1$mids)), ytop   = h1$counts, col = "tomato")
    ## Bottom histogram where y == TRUE
    if (!all(is.na(h2$counts)))
        rect(xleft   = head(h2$breaks, -1),     xright = tail(h2$breaks, -1),
             ybottom = rep(0, length(h2$mids)), ytop   = -h2$counts, col = "gray80")

    abline(h = 0, col = 1, lwd = 2)
}

print.stress <- function(x, ...) {
  print(summary(x, ...))
  invisible(x)
}

summary.stress <- function(object, ...) {
    fn <- function(x) {
        n      <- length(x$seeds)
        exinfo <- as.list(attr(x, "exinfo"))

        # Summary on warnings and errors
        stats <- list(Warnings = sum(sapply(x$warnings, length) > 0),
                      Errors   = sum(sapply(x$error,    length) > 0))
        stats <- c(list(OK = max(0L, n - sum(unlist(stats)))), stats)
        return(list(n = n, exinfo = exinfo, stats = stats, runtime = sum(x$runtime)))
    }
    return(structure(lapply(if (inherits(object, "stress.list")) object else list(object), fn), class = "stress.summary"))
}

print.stress.summary <- function(x, ...) {
    green  <- function(k, v) paste0("\033[32m", k, ": ", v, "\033[0m")
    yellow <- function(k, v) paste0("\033[33m", k, ": ", v, "\033[0m")
    red    <- function(k, v) paste0("\033[31m", k, ": ", v, "\033[0m")
    black  <- function(k, v) paste0(k, ": ", v)
    for (rec in x) {
        cat(sprintf("[%s] %s\n", rec$exinfo$type, rec$exinfo$file),
            sprintf("  Number of randomizations: %d\n", rec$n),
            sprintf("  Total runtime in seconds: %.1f (Mean %.2f)\n",
                    rec$runtime, rec$runtime / rec$n),
            "  ",
            with(list(k = "OK", v = rec$stats$OK), if (v > 0) green(k, v) else black(k, v)),
            ", ",
            with(list(k = "Warnings", v = rec$stats$Warnings), if (v > 0) yellow(k, v) else black(k, v)),
            ", ",
            with(list(k = "Errors", v = rec$stats$Errors), if (v > 0) red(k, v) else black(k, v)),
            "\n", sep = "")
    }
    invisible(x)
}

