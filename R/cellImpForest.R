#' Cellwise-robust imputation of mixed data with tree ensembles
#'
#' Detects cellwise outliers and imputes missing cells with one tree ensemble per column.
#' Detection uses cross-fitted conditional residuals -- out-of-bag predictions for
#' \code{engine = "ranger"}, K-fold cross-fitted predictions for \code{engine = "xgboost"} --
#' standardised by the MAD of the column's residuals. A continuous cell whose standardised
#' residual exceeds 2.535 in absolute value (Tukey bisquare weight below 0.5 at
#' \code{psi_c = 4.685}) is flagged; a categorical cell is flagged when the cross-fitted
#' probability of its observed level is below \code{rho_min} times that level's base rate, its
#' relative frequency among the column's training rows (observed, unflagged, row not flagged).
#' Flags are added in the first \code{maxit_detect} iterations and then frozen; flagged
#' cells are treated as missing and imputed, and a row with more than half of its cells flagged
#' leaves all fits. After convergence, a release pass judges every flagged cell once more
#' against the final fit of its column, which never saw it, and restores cells that pass.
#' Missing and flagged continuous cells are imputed by the median of the per-tree predictions
#' (ranger) or the booster prediction (xgboost), categorical cells by the most probable level.
#' The cell weights \code{W} are a diagnostic; they do not enter the fits. Columns that cannot
#' be modelled (too few usable rows, or a single level) are reported in a warning: a column
#' that was never modelled is named alone, a column that stopped being modelled is named with
#' the iteration after which it stopped. Detection assumes roughly symmetric conditional
#' residuals: strongly skewed or semicontinuous columns over-flag and should be transformed
#' first (e.g. log), as in the example.
#'
#' The one-pass random-forest detector of the \pkg{outForest} package is the closest relative;
#' \code{cellImpForest()} standardises the residuals robustly, iterates detection with refits,
#' releases swamped cells and covers categorical cells.
#'
#' @param data a data.frame with numeric and categorical (factor, character, logical) columns;
#'   \code{NA} allowed. Logical and character columns are returned with their type; factor
#'   columns keep their declared levels; integer columns are returned as double.
#' @param engine \code{"ranger"} (out-of-bag residuals, default) or \code{"xgboost"}
#'   (K-fold cross-fitting, pseudo-Huber loss)
#' @param aggregate \code{"median"} (default) or \code{"mean"} of the per-tree predictions used
#'   to impute continuous cells (ranger)
#' @param psi_c bisquare tuning constant (> 0); the flag threshold is the weight 0.5,
#'   |z| > 2.535 at the default
#' @param rho_min flag threshold for categorical cells, a share of the observed level's base
#'   rate, in (0, 1]
#' @param maxit maximum number of iterations (a whole number >= 1)
#' @param maxit_detect number of initial iterations in which flags may be added (a whole
#'   number >= 0), at most \code{maxit - 1} (larger values are capped, with a message when
#'   supplied), so that the
#'   release pass judges every flagged cell with a fit that never saw it; \code{0} turns
#'   detection off (a chained forest imputation, also the result of \code{maxit = 1}),
#'   \code{1} is a single detection pass
#' @param eps stopping tolerance, checked once no new flag appears. The stopping quantity is
#'   the maximum over columns of each column's mean absolute change of its imputed (missing or
#'   flagged) cells between two iterations, on that column's robust scale (the MAD of its
#'   observed values, falling back to the SD, then 1); for categorical columns the share of
#'   imputed cells whose level changed.
#' @param K folds for xgboost cross-fitting
#' @param residuals \code{"oob"} (default) or \code{"insample"}; the latter only to illustrate
#'   masking
#' @param uncert stochastic output for imputed cells: \code{"none"} (default), \code{"pmm"}
#'   (one of the 5 nearest donors on the prediction scale) or \code{"quantile"} (a draw from the
#'   forest's conditional quantiles, ranger only: with \code{engine = "xgboost"} it becomes
#'   \code{"pmm"}, with a message); categorical cells are drawn from the class probabilities
#' @param m number of stochastic completions drawn from one fit (a whole number >= 1; values
#'   above 1 need \code{uncert != "none"}); \code{imputed} is then a list. These are not
#'   proper multiple imputations.
#' @param num.trees,mtry,min.node.size passed to \code{ranger::ranger}
#' @param num.threads number of threads, passed to \code{ranger::ranger} and as \code{nthread}
#'   to xgboost. \code{NULL} (default) means ranger's own default (2 threads unless the
#'   environment variable \code{R_RANGER_NUM_THREADS} or the option \code{ranger.num.threads}
#'   or \code{Ncpus} sets another number) and one thread for xgboost.
#' @param trace print per-iteration progress
#' @param ... further arguments to \code{ranger::ranger} or to the xgboost parameter list
#'   (e.g. \code{nrounds}, \code{eta}, \code{max_depth})
#' @return an object of class \code{cellImpForest}: \code{imputed} (data.frame, or a list for
#'   \code{m > 1}), \code{flags} (logical matrix of the cells flagged at the end),
#'   \code{released} (logical matrix of the cells flagged during the loop and restored by the
#'   release pass), \code{W} (cell weights: bisquare weights of the observed continuous cells,
#'   0 for flagged cells, 1 for missing and categorical cells), \code{Z} (standardised
#'   residuals), \code{P} (two-sided normal tail probability; for categorical cells the ratio of
#'   the cross-fitted probability of the observed level to its base rate), \code{scales}
#'   (per-column MAD of the residuals), \code{rowflags}, \code{missing}, \code{iterations},
#'   \code{converged}, \code{engine}, \code{call}. \code{converged} is \code{TRUE} when the
#'   change fell below \code{eps}, and also when it stopped decreasing (missForest's rule); the
#'   previous iteration's imputation is then returned. \code{W}, \code{Z} and \code{P}
#'   describe the last fit of each column, also after that revert; for the unflagged cells of a
#'   row-flagged row, \code{W} keeps the value of the last fit that used the row, and \code{Z}
#'   and \code{P} are \code{NA}.
#'
#'   \code{summary()} returns a data.frame with one row per column: \code{column},
#'   \code{missing} (missing cells), \code{flagged} (cells flagged at the end), \code{released}
#'   (cells restored by the release pass) and \code{scale} (the residual scale; \code{NA} for
#'   categorical columns and for columns never modelled).
#' @seealso \code{\link{imputeCellGLoc}} for the parametric cellwise route,
#'   \code{\link{rangerImpute}} for forest imputation without detection.
#' @examples
#' data(sleep)
#' # body and brain weight span six orders of magnitude: log-transform them first
#' sl <- sleep
#' sl$BodyWgt <- log(sl$BodyWgt)
#' sl$BrainWgt <- log(sl$BrainWgt)
#' set.seed(1)
#' r <- cellImpForest(sl, num.trees = 100)
#' r
#' summary(r)
#' @family imputation methods
#' @export
cellImpForest <- function(data, engine = c("ranger", "xgboost"), aggregate = c("median", "mean"),
                          psi_c = 4.685, rho_min = 0.1, maxit = 10, maxit_detect = 3,
                          eps = 1e-3, K = 5, residuals = c("oob", "insample"),
                          uncert = c("none", "pmm", "quantile"), m = 1L,
                          num.trees = 500, mtry = NULL, min.node.size = 5, num.threads = NULL,
                          trace = FALSE, ...) {
  engine <- match.arg(engine)
  aggregate <- match.arg(aggregate)
  residuals <- match.arg(residuals)
  uncert <- match.arg(uncert)
  check_data(data)
  is_num1 <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  is_count <- function(x, lower) is_num1(x) && x == round(x) && x >= lower
  if (!is_count(m, 1)) stop("'m' must be a whole number >= 1")
  if (!is_count(maxit, 1)) stop("'maxit' must be a whole number >= 1")
  if (!is_count(maxit_detect, 0)) stop("'maxit_detect' must be a whole number >= 0")
  if (!(is_num1(rho_min) && rho_min > 0 && rho_min <= 1)) stop("'rho_min' must lie in (0, 1]")
  if (!(is_num1(psi_c) && psi_c > 0)) stop("'psi_c' must be a positive number")
  if (m > 1L && uncert == "none") stop("m > 1 needs uncert = 'pmm' or 'quantile'")
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  for (j in which(vapply(df, is.integer, logical(1)))) df[[j]] <- as.double(df[[j]])
  n <- nrow(df)
  p <- ncol(df)
  if (p < 2L) stop("cellImpForest() needs at least two columns")
  orig_type <- vapply(df, function(v) {
    if (is.logical(v)) "logical" else if (is.character(v)) "character" else "other"
  }, character(1))
  is_cat <- vapply(df, function(v) is.factor(v) || is.character(v) || is.logical(v), logical(1))
  for (j in which(is_cat)) if (!is.factor(df[[j]])) df[[j]] <- factor(df[[j]])
  bad <- which(!is_cat & !vapply(df, is.numeric, logical(1)))
  if (length(bad)) stop("column(s) ", paste(names(df)[bad], collapse = ", "),
                        " are neither numeric nor categorical")
  M <- is.na(df)
  if (any(colSums(M) == n)) stop("column(s) ", paste(names(df)[colSums(M) == n], collapse = ", "),
                                 " are entirely missing")
  # the release pass must judge flagged cells with fits that never saw them: at least one
  # iteration without new flags follows the detection phase
  if (maxit_detect > maxit - 1) {
    if (!missing(maxit_detect)) {
      message("cellImpForest(): maxit_detect = ", maxit_detect, " capped at maxit - 1 = ",
              maxit - 1)
    }
    maxit_detect <- maxit - 1
  }
  if (uncert == "quantile" && engine != "ranger") {
    message("cellImpForest(): uncert = \"quantile\" needs engine = \"ranger\"; using \"pmm\"")
    uncert <- "pmm"
  }
  nthr <- if (is.null(num.threads)) 1L else num.threads

  # ---- state ----
  X <- df
  for (j in seq_len(p)) if (any(M[, j])) {
    X[M[, j], j] <- if (is_cat[j]) .cif_mode(df[[j]]) else stats::median(df[[j]], na.rm = TRUE)
  }
  dn <- dimnames(M)
  Fl <- matrix(FALSE, n, p, dimnames = dn)
  Rel <- matrix(FALSE, n, p, dimnames = dn)        # flagged in the loop, restored by the release pass
  W <- matrix(1, n, p, dimnames = dn)
  Z <- P <- matrix(NA_real_, n, p, dimnames = dn)
  sg <- rep(NA_real_, p)
  rowflag <- rep(FALSE, n)
  col_scale <- vapply(seq_len(p), function(j) {              # MAD, falling back to SD, then 1
    if (is_cat[j]) 1 else .cif_scale(df[[j]], !is.na(df[[j]]))
  }, numeric(1))
  fits <- vector("list", p)
  last_fit <- integer(p)
  converged <- FALSE
  d_prev <- Inf
  X_prev <- X
  it_done <- 0L
  hard <- psi_c * sqrt(1 - sqrt(0.5))       # |z| at which the bisquare weight is 0.5
  set_col <- function(X, rows, j, value) {
    X[rows, j] <- if (is_cat[j]) as.character(value) else value
    X
  }
  fit_col <- function(j) {
    train <- !M[, j] & !Fl[, j] & !rowflag
    if (sum(train) < 2 * min.node.size) return(NULL)
    yj <- X[train, j]
    if (is_cat[j]) {
      yj <- droplevels(yj)
      if (nlevels(yj) < 2L) return(NULL)
    }
    fit <- if (engine == "ranger") {
      .cif_fit_ranger(yj, X[train, -j, drop = FALSE], aggregate = aggregate,
                      residuals = residuals, num.trees = num.trees, mtry = mtry,
                      min.node.size = min.node.size, num.threads = num.threads,
                      quantreg = (uncert == "quantile"), ...)
    } else {
      .cif_fit_xgboost(yj, X[train, -j, drop = FALSE], residuals = residuals, K = K,
                       nthread = nthr, ...)
    }
    # base rates of the levels among the training rows: the reference of the categorical score
    if (is_cat[j]) fit$base <- c(table(yj)) / length(yj)
    fit
  }

  # ---- iterate: detection phase (it <= maxit_detect), then imputation only ----
  for (it in seq_len(maxit)) {
    it_done <- it
    cand <- matrix(FALSE, n, p)
    for (j in order(colSums(M | Fl))) {
      train <- !M[, j] & !Fl[, j] & !rowflag
      fit <- fit_col(j)
      fits[j] <- list(fit)
      if (is.null(fit)) {
        if (trace) message("column ", names(df)[j], ": too few usable rows or one level, skipped")
        next
      }
      last_fit[j] <- it
      if (is_cat[j]) {
        rho <- rep(NA_real_, n)
        rho[train] <- .cif_rho(fit$oob_prob, X[train, j], fit$base)
        P[, j] <- rho
        cand[, j] <- !is.na(rho) & rho < rho_min
      } else {
        r <- rep(NA_real_, n)
        r[train] <- X[train, j] - fit$oob_pred
        sg[j] <- .cif_scale(r, train)
        Z[, j] <- r / sg[j]
        wj <- .cif_bisquare(Z[train, j], psi_c)
        cand[train, j] <- !is.na(wj) & wj < 0.5
        W[train, j] <- ifelse(is.na(wj), 1, wj)
        P[, j] <- .cif_surprise(Z[, j])
      }
      need <- M[, j] | Fl[, j]
      if (any(need)) X <- set_col(X, need, j, fit$predict(X[need, -j, drop = FALSE]))
    }
    n_new <- 0L
    if (it <= maxit_detect) {
      newF <- cand & !Fl
      n_new <- sum(newF)
      if (n_new > 0L) {
        Fl <- Fl | newF
        for (j in which(colSums(newF) > 0L)) if (!is.null(fits[[j]])) {
          X <- set_col(X, newF[, j], j, fits[[j]]$predict(X[newF[, j], -j, drop = FALSE]))
        }
        rowflag <- rowSums(Fl) / p > 0.5
      }
    }
    W[Fl] <- 0
    d <- 0
    for (j in seq_len(p)) {
      cells <- M[, j] | Fl[, j]
      if (!any(cells)) next
      dj <- if (is_cat[j]) mean(X[cells, j] != X_prev[cells, j])
            else mean(abs(X[cells, j] - X_prev[cells, j])) / col_scale[j]
      d <- max(d, dj)
    }
    if (trace) message("iteration ", it, ": ", n_new, " new flag(s), change ", signif(d, 3))
    if (n_new == 0L && (d < eps || d >= d_prev)) {
      converged <- TRUE
      if (d >= d_prev && d > eps) X <- X_prev
      break
    }
    X_prev <- X
    d_prev <- d
  }
  if (!converged) warning("cellImpForest() stopped at maxit = ", maxit, " without converging")

  # ---- release pass: flagged cells judged once more against the final, out-of-sample fit ----
  if (any(Fl & !M)) {
    for (j in which(colSums(Fl & !M) > 0L)) {
      f <- fits[[j]]
      if (is.null(f)) next
      idx <- which(Fl[, j] & !M[, j])
      Xi <- X[idx, -j, drop = FALSE]
      if (is_cat[j]) {
        rho <- .cif_rho(f$predict_prob(Xi), df[idx, j], f$base)
        P[idx, j] <- rho
        ok <- !is.na(rho) & rho >= rho_min
        rel <- idx[ok]
        if (length(rel)) {
          Fl[rel, j] <- FALSE
          Rel[rel, j] <- TRUE
          X[rel, j] <- df[rel, j]
          W[rel, j] <- 1
        }
      } else {
        z <- (df[idx, j] - f$center(Xi)) / sg[j]
        Z[idx, j] <- z
        P[idx, j] <- .cif_surprise(z)
        ok <- !is.na(z) & abs(z) <= hard
        rel <- idx[ok]
        if (length(rel)) {
          Fl[rel, j] <- FALSE
          Rel[rel, j] <- TRUE
          X[rel, j] <- df[rel, j]
          W[rel, j] <- .cif_bisquare(z[ok], psi_c)
        }
      }
    }
    rowflag <- rowSums(Fl) / p > 0.5
  }

  never <- last_fit == 0L
  starved <- !never & vapply(fits, is.null, logical(1))
  if (any(never)) {
    warning("cellImpForest(): column(s) ", paste(names(df)[never], collapse = ", "),
            " not modelled (too few usable rows or a single level): no detection, ",
            "missing cells keep the median/mode fill")
  }
  if (any(starved)) {
    lbl <- paste0(names(df)[starved], " (after iteration ", last_fit[starved], ")")
    warning("cellImpForest(): column(s) ", paste(lbl, collapse = ", "),
            " not modelled (too few usable rows once cells were flagged): ",
            "flagged and missing cells keep the values of that iteration's fit, ",
            "and flags are not re-judged by the release pass")
  }

  # ---- output ----
  X_out <- X
  if (uncert != "none") {
    draw <- function() .cif_uncert(X, df, M, Fl, rowflag, fits, is_cat, uncert, num.threads)
    X_out <- if (m > 1L) lapply(seq_len(m), function(b) draw()) else draw()
  }
  restore <- function(D) {
    for (j in which(orig_type == "logical")) D[[j]] <- as.logical(as.character(D[[j]]))
    for (j in which(orig_type == "character")) D[[j]] <- as.character(D[[j]])
    D
  }
  X_out <- if (is.data.frame(X_out)) restore(X_out) else lapply(X_out, restore)
  structure(list(imputed = X_out, flags = Fl, released = Rel, W = W, Z = Z, P = P, scales = sg,
                 rowflags = rowflag, missing = M, iterations = it_done, converged = converged,
                 engine = engine, call = match.call()),
            class = "cellImpForest")
}

#' @rdname cellImpForest
#' @param x,object a \code{cellImpForest} object
#' @method print cellImpForest
#' @export
print.cellImpForest <- function(x, ...) {
  cat("cellImpForest (", x$engine, "): ", nrow(x$flags), " x ", ncol(x$flags), " cells, ",
      sum(x$missing), " missing, ", sum(x$flags), " flagged (",
      round(100 * mean(x$flags), 2), "%), ", sum(x$rowflags), " row(s) flagged; ",
      x$iterations, " iteration(s), ", if (x$converged) "converged" else "NOT converged",
      "\n", sep = "")
  fl <- colSums(x$flags)
  if (any(fl > 0)) {
    cat("flags per column:\n")
    print(fl[fl > 0])
  }
  invisible(x)
}

#' @rdname cellImpForest
#' @method summary cellImpForest
#' @export
summary.cellImpForest <- function(object, ...) {
  data.frame(column = colnames(object$flags),
             missing = colSums(object$missing),
             flagged = colSums(object$flags),
             released = colSums(object$released),
             scale = object$scales,
             row.names = NULL)
}

#' @rdname cellImpForest
#' @param main plot title
#' @method plot cellImpForest
#' @export
plot.cellImpForest <- function(x, main = "cell weights (missing cells grey)", ...) {
  W <- x$W
  W[x$missing] <- -0.01
  n <- nrow(W)
  p <- ncol(W)
  graphics::image(seq_len(p), seq_len(n), t(W[n:1, , drop = FALSE]),
                  col = c("grey80", grDevices::colorRampPalette(c("darkred", "white"))(100)),
                  zlim = c(-0.01, 1), xlab = "", ylab = "rows (first at top)", axes = FALSE,
                  main = main, ...)
  graphics::axis(1, at = seq_len(p), labels = colnames(W), las = 2, cex.axis = 0.7)
  invisible(x)
}
