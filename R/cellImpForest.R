#' Cellwise-robust imputation of mixed data with tree ensembles
#'
#' Documentation is completed in Task 7.
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
  if (m > 1L && uncert == "none") stop("m > 1 needs uncert = 'pmm' or 'quantile'")
  df <- as.data.frame(data, stringsAsFactors = FALSE)
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
  if (uncert == "quantile" && engine != "ranger") uncert <- "pmm"
  nthr <- if (is.null(num.threads)) 1L else num.threads

  # ---- state ----
  X <- df
  for (j in seq_len(p)) if (any(M[, j])) {
    X[M[, j], j] <- if (is_cat[j]) .cif_mode(df[[j]]) else stats::median(df[[j]], na.rm = TRUE)
  }
  dn <- dimnames(M)
  Fl <- matrix(FALSE, n, p, dimnames = dn)
  W <- matrix(1, n, p, dimnames = dn)
  Z <- P <- matrix(NA_real_, n, p, dimnames = dn)
  sg <- rep(NA_real_, p)
  rowflag <- rep(FALSE, n)
  col_scale <- vapply(seq_len(p), function(j) {
    if (is_cat[j]) 1 else max(stats::mad(df[[j]], na.rm = TRUE), 1e-8)
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
    if (engine == "ranger") {
      .cif_fit_ranger(yj, X[train, -j, drop = FALSE], aggregate = aggregate,
                      residuals = residuals, num.trees = num.trees, mtry = mtry,
                      min.node.size = min.node.size, num.threads = num.threads,
                      quantreg = (uncert == "quantile"), ...)
    } else {
      .cif_fit_xgboost(yj, X[train, -j, drop = FALSE], residuals = residuals, K = K,
                       nthread = nthr, ...)
    }
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
        rho[train] <- .cif_rho(fit$oob_prob, X[train, j])
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
        rho <- .cif_rho(f$predict_prob(Xi), df[idx, j])
        P[idx, j] <- rho
        ok <- !is.na(rho) & rho >= rho_min
        rel <- idx[ok]
        if (length(rel)) {
          Fl[rel, j] <- FALSE
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
  structure(list(imputed = X_out, flags = Fl, W = W, Z = Z, P = P, scales = sg,
                 rowflags = rowflag, missing = M, iterations = it_done, converged = converged,
                 engine = engine, call = match.call()),
            class = "cellImpForest")
}
