#' Iteration constants of the categorical EM step
#'
#' \code{.gloc_cat_sweeps}: mean-field sweeps for a row with two or more
#' missing categorical cells. \code{.gloc_cat_max_combos}: a row with more level
#' combinations than this is not expanded (see \code{.gloc_cat_candidates}).
#' \code{.gloc_cat_floor}: lower bound on a prior probability before logs, so a
#' level the multinomial fit rules out can still win on the continuous cells.
#' @keywords internal
.gloc_cat_sweeps <- 5L
#' @rdname dot-gloc_cat_sweeps
#' @keywords internal
.gloc_cat_max_combos <- 256L
#' @rdname dot-gloc_cat_sweeps
#' @keywords internal
.gloc_cat_floor <- 1e-10

#' Split a data frame into continuous and categorical columns
#'
#' The rule \code{imputeCellGLoc} has always used: factor, character and
#' logical columns are categorical, everything else continuous.
#' @param data a data frame.
#' @return \code{list(cont, cat)} of column names.
#' @keywords internal
.gloc_split_vars <- function(data) {
  is_cat <- vapply(data, function(x) is.factor(x) || is.character(x) ||
                     is.logical(x), logical(1))
  list(cont = names(data)[!is_cat], cat = names(data)[is_cat])
}

#' Categorical columns as factors, their missing mask and their levels
#'
#' @param data a data frame.
#' @param cat_vars names of the categorical columns.
#' @return \code{list(F, Mc, levels)}: \code{F} holds the columns as factors of
#'   their observed values (NA kept), \code{Mc} is the \eqn{n x k} missing mask,
#'   \code{levels} the observed levels per column.
#' @keywords internal
.gloc_cat_prepare <- function(data, cat_vars) {
  n <- nrow(data)
  F <- data[, cat_vars, drop = FALSE]
  lev <- stats::setNames(vector("list", length(cat_vars)), cat_vars)
  for (v in cat_vars) {
    x <- F[[v]]
    F[[v]] <- if (is.factor(x)) droplevels(x) else factor(x)
    lev[[v]] <- levels(F[[v]])
  }
  Mc <- matrix(FALSE, n, length(cat_vars), dimnames = list(NULL, cat_vars))
  for (v in cat_vars) Mc[, v] <- is.na(F[[v]])
  list(F = F, Mc = Mc, levels = lev)
}

#' Design rows for categorical values with fixed levels
#'
#' Builds the design the way \code{.gloc_design} does, but with every column's
#' levels fixed to \code{levels} and unused levels kept. Candidate rows that
#' fill a missing cell with each level therefore get the columns of the
#' complete-data design. On complete data the result is identical to
#' \code{.gloc_design}.
#' @param Fr data frame of the categorical columns, without NA.
#' @param design one-sided formula.
#' @param levels named list of levels, from \code{.gloc_cat_prepare}.
#' @return a design matrix.
#' @keywords internal
.gloc_design_rows <- function(Fr, design, levels) {
  if (is.null(design)) design <- ~ 1
  if (!length(levels) || identical(all.vars(design), character(0)))
    return(matrix(1, nrow = nrow(Fr), ncol = 1,
                  dimnames = list(NULL, "(Intercept)")))
  df <- Fr
  for (v in names(levels))
    df[[v]] <- factor(as.character(df[[v]]), levels = levels[[v]],
                      ordered = is.ordered(Fr[[v]]))
  mf <- stats::model.frame(design, data = df, na.action = stats::na.pass,
                           drop.unused.levels = FALSE)
  stats::model.matrix(design, mf)
}

#' Give an imputed categorical column back the class of the original
#'
#' @param f factor of values (observed and imputed).
#' @param orig the original column.
#' @return a factor with \code{orig}'s levels and orderedness, a logical, or a
#'   character vector, following \code{orig}.
#' @keywords internal
.gloc_cat_restore <- function(f, orig) {
  if (is.factor(orig))
    return(factor(as.character(f), levels = levels(orig), ordered = is.ordered(orig)))
  if (is.logical(orig)) return(as.logical(as.character(f)))
  as.character(f)
}

#' Weighted multinomial logistic regression on the other categorical columns
#'
#' @param y factor response.
#' @param Xdf data frame of factor predictors.
#' @param w nonnegative case weights.
#' @return an \code{nnet::multinom} fit. \code{multinom} starts from zero
#'   weights (\code{rang = 0}), so it draws no random numbers.
#' @keywords internal
.gloc_multinom <- function(y, Xdf, w) {
  dd <- cbind(data.frame(.y = y), Xdf)
  dd$.w <- w
  # A formula built with "." and then "- .w" leaves .w in the terms object's
  # predvars (a documented R quirk: the "." expansion collects every data
  # column into "variables" before the "-" removes it from term.labels), so
  # predict() on newdata without a .w column then fails with "object '.w' not
  # found". Naming the predictors explicitly avoids the quirk.
  form <- stats::reformulate(names(Xdf), ".y")
  suppressMessages(nnet::multinom(form, data = dd, weights = .w,
                                  trace = FALSE, maxit = 200, MaxNWts = 100000))
}

#' Fit the prior model of every categorical column
#'
#' Each categorical column gets a multinomial logistic regression on the other
#' categorical columns, with main effects and case weights \code{wp}. Pseudo-rows
#' of rows with a missing cell carry their posterior weights, which makes this
#' the M-step of the categorical part. A column that is the only categorical
#' one, or whose weighted response has one level, uses its weighted marginal
#' frequencies. So does a column whose fit fails, with one warning.
#' @param Fp data frame of factor columns without NA (the pseudo-row table).
#' @param wp case weights, one per row of \code{Fp}.
#' @param levels named list of levels.
#' @param fit the fitting function; exposed so the fallback is testable.
#' @return a named list of prior models; see \code{.gloc_cat_prior}.
#' @keywords internal
.gloc_cat_fit_priors <- function(Fp, wp, levels, fit = .gloc_multinom) {
  vars <- names(levels)
  out <- stats::setNames(vector("list", length(vars)), vars)
  failed <- character(0)
  for (v in vars) {
    marg <- vapply(levels[[v]], function(l) sum(wp[Fp[[v]] == l]), numeric(1))
    marg <- marg / sum(marg)
    preds <- setdiff(vars, v)
    y <- droplevels(Fp[[v]])
    m <- NULL
    if (length(preds) && nlevels(y) >= 2L) {
      m <- tryCatch(fit(y, Fp[, preds, drop = FALSE], wp), error = function(e) NULL)
      if (!is.null(m) && !all(is.finite(stats::fitted(m)))) m <- NULL
      if (is.null(m)) failed <- c(failed, v)
    }
    out[[v]] <- if (is.null(m))
      list(type = "marginal", probs = unname(marg), levels = levels[[v]])
    else
      list(type = "multinom", model = m, preds = preds, levels = levels[[v]],
           fitted_levels = levels(y), probs = unname(marg))
  }
  if (length(failed))
    warning(sprintf(paste("cellGLoc: nnet::multinom() failed for categorical",
                          "variable(s) %s; their level probabilities use the",
                          "weighted marginal frequencies instead."),
                    paste(failed, collapse = ", ")), call. = FALSE)
  out
}

#' Prior level probabilities for new rows
#'
#' @param pr one element of \code{.gloc_cat_fit_priors}.
#' @param Fnew data frame of factor columns; the response column is ignored.
#' @return an \eqn{m x L} matrix in level order, floored at
#'   \code{.gloc_cat_floor} and renormalised by row.
#' @keywords internal
.gloc_cat_prior <- function(pr, Fnew) {
  m <- nrow(Fnew); L <- length(pr$levels)
  if (identical(pr$type, "marginal")) {
    P <- matrix(pr$probs, m, L, byrow = TRUE)
  } else {
    pp <- tryCatch(stats::predict(pr$model, newdata = Fnew[, pr$preds, drop = FALSE],
                                  type = "probs"),
                   error = function(e) NULL)   # e.g. a predictor level the fit never saw
    if (is.null(pp)) {
      P <- matrix(pr$probs, m, L, byrow = TRUE)
    } else {
      if (length(pr$fitted_levels) == 2L) pp <- cbind(1 - pp, pp)
      pp <- matrix(pp, nrow = m)
      P <- matrix(0, m, L)
      P[, match(pr$fitted_levels, pr$levels)] <- pp
    }
  }
  if (!is.null(pr$map)) {                      # priors fitted on another level set
    Pm <- matrix(0, m, length(pr$new_levels))
    ok <- !is.na(pr$map)
    Pm[, pr$map[ok]] <- P[, ok, drop = FALSE]
    P <- Pm
  }
  P <- pmax(P, .gloc_cat_floor)
  P / rowSums(P)
}

#' Log-likelihood of candidate design rows from a row's unflagged cells
#'
#' The level assignment of a row with a missing categorical cell conditions on
#' exactly the continuous cells detection and imputation condition on: the
#' peer reliabilities of \code{.gloc_peer_rel}, with the noise-inflated
#' covariance \eqn{G = D \Sigma_{kk} D + \mathrm{diag}(\sigma_{kk}(1 - r))} of
#' \code{.gloc_cond_resid} and \code{.gloc_impute}. A fully reliable cell enters
#' as observed and a flagged one drops out, so a contaminated cell cannot
#' steer the level. \eqn{G} does not depend on the candidate, so candidates of
#' one row differ only through the quadratic form. Rows whose peers are all
#' fully in or out share one inversion per peer pattern.
#' @param X \eqn{n x p} continuous matrix.
#' @param M \eqn{n x p} missing mask.
#' @param W \eqn{n x p} cell weights, or \code{NULL} for every observed cell.
#' @param B \eqn{q x p} coefficients.
#' @param Sigma \eqn{p x p} scatter.
#' @param Ucand candidate design rows, one per candidate.
#' @param row_of the data row of each candidate.
#' @param w_min,band the peer rule.
#' @return one log-likelihood per candidate, constant dropped.
#' @keywords internal
.gloc_cat_loglik <- function(X, M, W, B, Sigma, Ucand, row_of, w_min = 0.5,
                             band = .gloc_peer_band) {
  ll <- numeric(nrow(Ucand))
  if (!length(ll)) return(ll)
  rows <- unique(row_of)
  Rel <- .gloc_peer_rel(!M[rows, , drop = FALSE],
                        if (is.null(W)) NULL else W[rows, , drop = FALSE],
                        w_min = w_min, band = band)
  Mu <- Ucand %*% B
  sdiag <- diag(Sigma)
  key <- apply(Rel, 1L, function(r)
    if (any(r > 0 & r < 1)) NA_character_ else paste0(as.integer(r > 0), collapse = ""))
  inband <- is.na(key)
  key[inband] <- paste0("b", seq_len(sum(inband)))
  cand <- split(seq_along(row_of), factor(match(row_of, rows), levels = seq_along(rows)))
  for (g in split(seq_along(rows), key)) {
    r  <- Rel[g[1L], ]
    kk <- which(r > 0)
    if (!length(kk)) next
    rk <- r[kk]; dd <- sqrt(rk)
    G  <- outer(dd, dd) * Sigma[kk, kk, drop = FALSE] +
            diag(sdiag[kk] * (1 - rk), length(kk))
    Gi <- tryCatch(chol2inv(chol(G)), error = function(e) MASS::ginv(G))
    idx <- unlist(cand[g], use.names = FALSE)
    Z <- (X[row_of[idx], kk, drop = FALSE] - Mu[idx, kk, drop = FALSE]) *
           rep(dd, each = length(idx))
    ll[idx] <- -0.5 * rowSums((Z %*% Gi) * Z)
  }
  ll
}

#' The fixed table of pseudo-rows for the categorical EM
#'
#' Complete rows enter once. A row missing one categorical cell enters once per
#' level of that cell, and a row missing several once per level combination.
#' Only the weights change between iterations, so the design rows are built
#' once. A row with more than \code{max_combos} combinations is left out of
#' the estimation; its missing categorical cells take the most frequent level
#' among the complete rows, and that is warned about once.
#' In \code{pat_pr}, a level combination is present only when a row whose
#' categorical values are all known has it (\code{.gloc_present} on
#' \code{pats_rows}).
#' @param catp result of \code{.gloc_cat_prepare}.
#' @param data the data frame of the fit; unknown categorical cells are NA.
#' @param design one-sided formula.
#' @param max_combos see \code{.gloc_cat_max_combos}.
#' @return a list; see \code{.gloc_cat_estep}.
#' @keywords internal
.gloc_cat_candidates <- function(catp, data, design, max_combos = .gloc_cat_max_combos) {
  F <- catp$F; Mc <- catp$Mc; lev <- catp$levels; vars <- names(lev)
  n <- nrow(F); nmis <- rowSums(Mc); cc <- which(nmis == 0L)
  nlev <- vapply(lev, length, integer(1))
  # A design without categorical variables has no combination table: its design
  # rows are built directly, and the mean step falls back to its row patterns.
  no_table <- is.null(design) || identical(all.vars(design), character(0))
  if (!no_table) {
    # Completed copy: unknown categorical cells set to an observed level, so the
    # design, its level combinations and the EM's level sets agree (NA never
    # becomes a design level).
    dc <- data
    for (v in vars) if (any(Mc[, v])) {
      val <- dc[[v]]; val[Mc[, v]] <- lev[[v]][1L]; dc[[v]] <- val
    }
    n_all <- prod(as.numeric(nlev))
    aux <- .gloc_design_aux(dc, design, vars,
                            max_patterns = if (n_all <= 1e6) max(n_all, .gloc_max_patterns)
                                           else .gloc_max_patterns)
    pats <- aux$patterns
    Uref <- .gloc_design(dc, design, vars)
    if (is.null(pats) || !identical(colnames(pats$P), colnames(Uref)))
      stop(paste("imputeCellGLoc(): categorical = \"em\" could not enumerate the level",
                 "combinations of this design; use categorical = \"level\"."))
  }
  combos <- vapply(seq_len(n), function(i) prod(nlev[Mc[i, ]]), numeric(1))
  many_rows <- which(nmis >= 2L & combos > max_combos)
  parts <- list(F[cc, , drop = FALSE]); rows <- list(cc)
  single <- list(); multi <- list(); N <- length(cc)
  for (v in vars) {
    rv <- which(nmis == 1L & Mc[, v]); L <- nlev[[v]]
    if (!length(rv)) next
    Fc <- F[rep(rv, each = L), , drop = FALSE]
    Fc[[v]] <- factor(rep(lev[[v]], times = length(rv)), levels = lev[[v]],
                      ordered = is.ordered(F[[v]]))
    parts[[length(parts) + 1L]] <- Fc
    rows[[length(rows) + 1L]] <- rep(rv, each = L)
    single[[v]] <- list(rows = rv,
                        pos = matrix(N + seq_len(length(rv) * L), nrow = length(rv),
                                     byrow = TRUE))
    N <- N + length(rv) * L
  }
  for (i in setdiff(which(nmis >= 2L), many_rows)) {
    vs <- vars[Mc[i, ]]
    g <- unname(as.matrix(expand.grid(lapply(nlev[vs], seq_len))))
    Fi <- F[rep(i, nrow(g)), , drop = FALSE]
    for (a in seq_along(vs))
      Fi[[vs[a]]] <- factor(lev[[vs[a]]][g[, a]], levels = lev[[vs[a]]],
                            ordered = is.ordered(F[[vs[a]]]))
    parts[[length(parts) + 1L]] <- Fi
    rows[[length(rows) + 1L]] <- rep(i, nrow(g))
    multi[[length(multi) + 1L]] <- list(row = i, vars = vs, lvl = g,
                                        pos = N + seq_len(nrow(g)))
    N <- N + nrow(g)
  }
  Fp <- do.call(rbind, parts); rownames(Fp) <- NULL
  pr_row <- unlist(rows, use.names = FALSE)
  pr_c <- Up_main <- pat_pr <- pats_rows <- NULL
  if (no_table) {
    Up <- .gloc_design_rows(Fp, design, lev)
  } else {
    # Pure rows are matched exactly against the combination table.
    key <- function(A) apply(A, 1L, function(r) paste(format(r, digits = 15), collapse = "|"))
    to_combo <- function(Fr) {
      Ufix <- .gloc_design_rows(Fr, design, lev)
      if (!identical(colnames(Ufix), colnames(pats$P)))
        stop(paste("imputeCellGLoc(): candidate design columns do not match the design;",
                   "use categorical = \"level\"."))
      idx <- match(key(Ufix), key(pats$P))
      if (anyNA(idx))
        stop(paste("imputeCellGLoc(): a level combination of the categorical EM is",
                   "missing from the combination table; use categorical = \"level\"."))
      idx
    }
    pr_c <- to_combo(Fp)
    Up <- pats$P[pr_c, , drop = FALSE]
    Up_main <- if (is.null(pats$P_main)) NULL else pats$P_main[pr_c, , drop = FALSE]
    pat_pr <- list(id = pr_c, P = pats$P, P_main = pats$P_main, labels = pats$labels)
    pats_rows <- pats
    pats_rows$id[nmis > 0L] <- NA_integer_
    pat_pr$present <- .gloc_present(pats_rows)
  }
  many <- list(rows = many_rows, Umode = NULL, Umode_main = NULL, post = list())
  if (length(many_rows)) {
    Fm <- F[many_rows, , drop = FALSE]
    for (v in vars) {
      freq <- as.vector(table(factor(F[cc, v], levels = lev[[v]])))
      freq <- freq / sum(freq)
      miss <- Mc[many_rows, v]
      if (!any(miss)) next
      Fm[[v]][miss] <- lev[[v]][which.max(freq)]
      many$post[[v]] <- matrix(freq, sum(miss), length(freq), byrow = TRUE,
                               dimnames = list(many_rows[miss], lev[[v]]))
    }
    if (no_table) {
      many$Umode <- .gloc_design_rows(Fm, design, lev)
    } else {
      mc <- to_combo(Fm)
      many$Umode <- pats$P[mc, , drop = FALSE]
      if (!is.null(pats$P_main)) many$Umode_main <- pats$P_main[mc, , drop = FALSE]
    }
    warning(sprintf(paste("cellGLoc: %d row(s) have more than %d combinations of",
                          "missing categorical levels; they are left out of the",
                          "estimation, and their missing categorical cells get",
                          "the most frequent level of the complete rows."),
                    length(many_rows), max_combos), call. = FALSE)
  }
  list(Fp = Fp, Up = Up, Up_main = Up_main, pr_row = pr_row, pr_c = pr_c,
       pat_pr = pat_pr, pats_rows = pats_rows, single = single, multi = multi,
       many = many, const1 = colSums(Up != 1) == 0L,
       const1_main = if (is.null(Up_main)) NULL else colSums(Up_main != 1) == 0L,
       need = lapply(stats::setNames(vars, vars), function(v) which(Mc[pr_row, v])))
}

#' Expected design rows from the pseudo-row weights
#'
#' Sums the weighted pseudo-rows of each data row, gives the capped rows their
#' mode rows, and sets the columns that are 1 in every pseudo-row to 1.
#' @param Up pseudo-row design rows.
#' @param w pseudo-row weights.
#' @param pr_row data row of each pseudo-row.
#' @param rn row names of the data.
#' @param many_rows rows left out of the table (\code{cand$many$rows}).
#' @param Umode design rows of those rows, or \code{NULL}.
#' @param const logical, the columns set to 1.
#' @return a \code{length(rn) x ncol(Up)} matrix.
#' @keywords internal
.gloc_cat_expected_rows <- function(Up, w, pr_row, rn, many_rows, Umode, const) {
  rs <- rowsum(Up * w, pr_row, reorder = TRUE)
  Ub <- matrix(0, length(rn), ncol(Up), dimnames = list(rn, colnames(Up)))
  Ub[as.integer(rownames(rs)), ] <- rs
  if (length(many_rows)) Ub[many_rows, ] <- Umode
  Ub[, const] <- 1
  Ub
}

#' E-step of the categorical EM
#'
#' The weight of a pseudo-row with level \eqn{c} of a missing cell is
#' proportional to the prior of \eqn{c} given the row's other categorical
#' values times the density of the row's unflagged continuous cells
#' (\code{.gloc_cat_loglik}). A row missing several cells runs \code{sweeps}
#' mean-field updates: each cell's log posterior is the expected log prior
#' plus the expected log density over the other cells' current posteriors;
#' the pseudo-row weights are then the products of the marginal posteriors.
#' That is an approximation, because conditional models define no joint
#' distribution.
#' @param X,M,W,B,Sigma the continuous data, mask, weights and current fit;
#'   \code{B = NULL} drops the density (prior-only E-step).
#' @param catp,cand results of \code{.gloc_cat_prepare} and
#'   \code{.gloc_cat_candidates}.
#' @param priors result of \code{.gloc_cat_fit_priors}.
#' @param w_min,band the peer rule.
#' @param sweeps see \code{.gloc_cat_sweeps}.
#' @return \code{list(pr_row, pr_w, Fp, Up, Ubar, post, Umain_bar)}.
#' @keywords internal
.gloc_cat_estep <- function(X, M, W, B, Sigma, catp, cand, priors, w_min = 0.5,
                            band = .gloc_peer_band, sweeps = .gloc_cat_sweeps) {
  lev <- catp$levels; N <- nrow(cand$Fp)
  inc <- rowSums(catp$Mc)[cand$pr_row] > 0L
  w <- rep(1, N)
  ll <- numeric(N)
  if (!is.null(B) && any(inc))
    ll[inc] <- .gloc_cat_loglik(X, M, W, B, Sigma, cand$Up[inc, , drop = FALSE],
                                cand$pr_row[inc], w_min = w_min, band = band)
  lp <- list()
  for (v in names(lev)) {
    pos <- cand$need[[v]]
    if (!length(pos)) next
    P <- .gloc_cat_prior(priors[[v]], cand$Fp[pos, , drop = FALSE])
    lp[[v]] <- numeric(N)
    lp[[v]][pos] <- log(P[cbind(seq_along(pos), as.integer(cand$Fp[[v]][pos]))])
  }
  parts <- lapply(lev, function(l) list())
  for (v in names(cand$single)) {
    s <- cand$single[[v]]
    A <- matrix(lp[[v]][s$pos] + ll[s$pos], nrow = length(s$rows))
    A <- exp(A - apply(A, 1L, max))
    R <- A / rowSums(A)
    w[s$pos] <- R
    parts[[v]][[length(parts[[v]]) + 1L]] <-
      matrix(R, nrow = length(s$rows), dimnames = list(s$rows, lev[[v]]))
  }
  for (mr in cand$multi) {
    nv <- length(mr$vars)
    q <- lapply(mr$vars, function(v) rep(1 / length(lev[[v]]), length(lev[[v]])))
    for (sw in seq_len(sweeps)) for (a in seq_len(nv)) {
      wo <- rep(1, length(mr$pos))
      for (b in setdiff(seq_len(nv), a)) wo <- wo * q[[b]][mr$lvl[, b]]
      term <- lp[[mr$vars[a]]][mr$pos] + ll[mr$pos]
      e <- vapply(seq_along(q[[a]]), function(cl) {
        k <- mr$lvl[, a] == cl
        sum(wo[k] * term[k]) / sum(wo[k])
      }, numeric(1))
      e <- exp(e - max(e))
      q[[a]] <- e / sum(e)
    }
    wk <- rep(1, length(mr$pos))
    for (b in seq_len(nv)) wk <- wk * q[[b]][mr$lvl[, b]]
    w[mr$pos] <- wk
    for (a in seq_len(nv)) {
      v <- mr$vars[a]
      parts[[v]][[length(parts[[v]]) + 1L]] <-
        matrix(q[[a]], 1L, dimnames = list(mr$row, lev[[v]]))
    }
  }
  for (v in names(cand$many$post))
    parts[[v]][[length(parts[[v]]) + 1L]] <- cand$many$post[[v]]
  post <- list()
  for (v in names(lev)) {
    if (!length(parts[[v]])) next
    P <- do.call(rbind, parts[[v]])
    post[[v]] <- P[order(as.integer(rownames(P))), , drop = FALSE]
  }
  rn <- row.names(catp$F)
  Ubar <- .gloc_cat_expected_rows(cand$Up, w, cand$pr_row, rn, cand$many$rows,
                                  cand$many$Umode, cand$const1)
  Umain_bar <- if (is.null(cand$Up_main)) NULL else
    .gloc_cat_expected_rows(cand$Up_main, w, cand$pr_row, rn, cand$many$rows,
                            cand$many$Umode_main, cand$const1_main)
  list(pr_row = cand$pr_row, pr_w = w, Fp = cand$Fp, Up = cand$Up, Ubar = Ubar,
       post = post, Umain_bar = Umain_bar)
}

#' The E-step result for data without missing categorical cells
#' @param catp result of \code{.gloc_cat_prepare}.
#' @param U the design.
#' @keywords internal
.gloc_cat_identity <- function(catp, U) {
  n <- nrow(catp$F)
  list(pr_row = seq_len(n), pr_w = rep(1, n), Fp = catp$F, Up = U, Ubar = U,
       post = list())
}

#' Largest change in the categorical posteriors between two E-steps
#' @param post,old two \code{post} lists with the same layout, or \code{old = NULL}.
#' @keywords internal
.gloc_cat_change <- function(post, old) {
  if (is.null(old)) return(Inf)
  if (!length(post)) return(0)
  max(abs(unlist(post, use.names = FALSE) - unlist(old, use.names = FALSE)))
}
