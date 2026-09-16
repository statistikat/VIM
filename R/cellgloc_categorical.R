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

#' Re-attach a factor's contrasts attribute after the factor was rebuilt
#'
#' \code{droplevels()} and \code{factor()} both drop a \code{contrasts}
#' attribute; subsetting a factor keeps it, and so do \code{.gloc_design} and
#' \code{.gloc_patterns}. A column rebuilt without it would give the candidate
#' design rows different column names from the design built on the completed
#' copy -- \code{fb}, \code{fc} against \code{f1}, \code{f2} under
#' \code{contr.sum} -- and the fit would stop. \code{y} keeps the attribute
#' only when it still has exactly \code{template}'s number of levels.
#' Comparing \code{nrow} of the contrasts matrix against \code{nlevels(y)}
#' is not enough: \code{levels(f) <- c(levels(f), "z")} adds an unused level
#' to \code{f} without going through \code{factor()}, so it leaves a stale
#' contrasts matrix sized for \code{f}'s levels \emph{before} the addition
#' attached to \code{f} itself. If \code{droplevels()} then removes exactly
#' that unused level elsewhere, the stale matrix's row count coincidentally
#' matches the reduced \code{y} and the old check re-attached it, even though
#' the reference design -- built through
#' \code{model.frame(drop.unused.levels = TRUE)}, which drops the attribute
#' whenever it rebuilds a factor to remove an unused level -- had already lost
#' it. Comparing against \code{template}'s own current level count catches
#' that case too.
#' @param y the rebuilt factor.
#' @param template the factor it was rebuilt from.
#' @return \code{y}, carrying \code{template}'s contrasts where they still fit.
#' @keywords internal
.gloc_keep_contrasts <- function(y, template) {
  ct <- attr(template, "contrasts")
  if (!is.null(ct) && nlevels(y) == nlevels(template))
    attr(y, "contrasts") <- ct
  y
}

#' Categorical columns as factors, their missing mask and their levels
#'
#' @param data a data frame.
#' @param cat_vars names of the categorical columns.
#' @return \code{list(F, Mc, levels)}: \code{F} holds the columns as factors of
#'   their observed values (NA kept, a \code{contrasts} attribute carried over by
#'   \code{.gloc_keep_contrasts}), \code{Mc} is the \eqn{n x k} missing mask,
#'   \code{levels} the observed levels per column.
#' @keywords internal
.gloc_cat_prepare <- function(data, cat_vars) {
  n <- nrow(data)
  F <- data[, cat_vars, drop = FALSE]
  lev <- stats::setNames(vector("list", length(cat_vars)), cat_vars)
  for (v in cat_vars) {
    x <- F[[v]]
    y <- if (is.factor(x)) droplevels(x) else factor(x)
    F[[v]] <- .gloc_keep_contrasts(y, x)
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
#' \code{.gloc_design}, contrasts included: a column's \code{contrasts}
#' attribute survives the rebuild (\code{.gloc_keep_contrasts}), so a factor
#' coded with \code{contr.sum} gets the design's columns and not the
#' treatment-coded ones.
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
    df[[v]] <- .gloc_keep_contrasts(
      factor(as.character(df[[v]]), levels = levels[[v]],
             ordered = is.ordered(Fr[[v]])), Fr[[v]])
  mf <- stats::model.frame(design, data = df, na.action = stats::na.pass,
                           drop.unused.levels = FALSE)
  stats::model.matrix(design, mf)
}

#' Give an imputed categorical column back the class of the original
#'
#' \code{factor()} drops a \code{contrasts} attribute like \code{droplevels()}
#' does (see \code{.gloc_keep_contrasts}), so without re-attaching it here an
#' EM-imputed factor would come back from \code{imputeCellGLoc()} coded
#' differently from how the user's own column was coded, even though the
#' level set is unchanged -- \code{levels(orig)} is passed through verbatim.
#' @param f factor of values (observed and imputed).
#' @param orig the original column.
#' @return a factor with \code{orig}'s levels, orderedness and contrasts (where
#'   \code{.gloc_keep_contrasts} keeps them), a logical, or a character vector,
#'   following \code{orig}.
#' @keywords internal
.gloc_cat_restore <- function(f, orig) {
  if (is.factor(orig))
    return(.gloc_keep_contrasts(
      factor(as.character(f), levels = levels(orig), ordered = is.ordered(orig)), orig))
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
    # A predictor that carries a contrasts attribute (.gloc_keep_contrasts) can
    # make predict.multinom()'s own model.matrix() call warn "contrasts dropped
    # from factor ... due to missing levels" whenever Fnew -- one row per
    # candidate level, or a "keep" subset of the pseudo-row table -- does not
    # itself realise every level of that predictor. The returned probabilities
    # do not depend on it (the fit's own xlevels and contrasts still drive the
    # design predict() builds), it fires on essentially every E-step once such
    # a predictor is in play, and it carries none of the "cellGLoc: " prefix
    # dedup() keys on, so left alone it reaches the user every single time.
    pp <- withCallingHandlers(
      tryCatch(stats::predict(pr$model, newdata = Fnew[, pr$preds, drop = FALSE],
                              type = "probs"),
               error = function(e) NULL),   # e.g. a predictor level the fit never saw
      warning = function(w) {
        if (grepl("contrasts dropped", conditionMessage(w), fixed = TRUE))
          invokeRestart("muffleWarning")
      })
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
#' @return one log-likelihood per candidate, constant dropped. A row with no
#'   usable peer -- every continuous cell of it missing or flagged -- gets 0 for
#'   every candidate, so the density says nothing there and the prior alone
#'   decides the level.
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
#'
#' Precondition: at least one row has every categorical variable observed. Those
#' rows are the table's complete part, the data the prior models are first
#' fitted on, and the source of the most frequent level a capped row takes;
#' without one this function has nothing to build from. \code{imputeCellGLoc}
#' enforces it with a clear error before calling here.
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
    # The combination table must cover every level combination the completed
    # copy's design can produce, or to_combo() below has nothing to match a
    # candidate row against (the stop just below fires instead). So the level
    # grid is enumerated in full -- not only up to .gloc_max_patterns (4096)
    # -- whenever it has at most 1e6 rows; only above that does the pattern
    # revert to the combinations that occur in the data. That is a real
    # cliff, not a corner case: about ten categorical variables already put
    # prod(nlev) in the (4096, 1e6] range this widens, e.g. a table of 1e5 to
    # 1e6 rows, and the ceiling exists because full enumeration there is a
    # memory cost (one row per combination, not per data row) that stops
    # paying for itself beyond it.
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
    Fc[[v]] <- .gloc_keep_contrasts(
      factor(rep(lev[[v]], times = length(rv)), levels = lev[[v]],
             ordered = is.ordered(F[[v]])), F[[v]])
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
      Fi[[vs[a]]] <- .gloc_keep_contrasts(
        factor(lev[[vs[a]]][g[, a]], levels = lev[[vs[a]]],
               ordered = is.ordered(F[[vs[a]]])), F[[vs[a]]])
    parts[[length(parts) + 1L]] <- Fi
    rows[[length(rows) + 1L]] <- rep(i, nrow(g))
    multi[[length(multi) + 1L]] <- list(row = i, vars = vs, lvl = g,
                                        pos = N + seq_len(nrow(g)))
    N <- N + nrow(g)
  }
  Fp <- do.call(rbind, parts); rownames(Fp) <- NULL
  # rbind() on data frames rebuilds every factor column and drops its contrasts
  # attribute, which the design on the completed copy keeps. Without this the
  # pseudo-rows would be coded differently from that design and to_combo() below
  # would stop on data whose factors carry their own contrasts.
  for (v in vars) Fp[[v]] <- .gloc_keep_contrasts(Fp[[v]], F[[v]])
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

#' Posterior probability of each observed categorical cell's own level
#'
#' For every observed categorical cell: the probability of the level it holds,
#' computed as if the cell were missing, at the returned fit and without a
#' refit. A miscoded cell whose continuous cells point elsewhere gets a small
#' value. Other missing categorical cells of the row enter through the row's
#' pseudo-rows. The estimation does not use this value.
#'
#' The density is evaluated at the row's expected design row rather than
#' averaged over its level combinations -- a Jensen approximation, taken because
#' the pseudo-rows are already weighted. It is exact for a row whose other
#' categorical cells (every categorical variable except \code{v}, the column
#' being computed) are all observed. \emph{Corrected: an earlier version of
#' this paragraph said that is every row whenever only one categorical
#' variable has missing cells, for every column \code{v}. That is false for
#' any \code{v} other than the one variable with missing cells: computing
#' that other, fully observed variable's column still hits the approximation
#' on any row where the one variable with missing cells is itself missing.
#' The exact case is only \code{v}'s own column, and only because "only one
#' variable has missing cells" then means every other variable has none,
#' anywhere.}
#' @param X,M,W,B,Sigma the continuous data, mask, weights and returned fit.
#' @param catp result of \code{.gloc_cat_prepare}.
#' @param priors result of \code{.gloc_cat_fit_priors}.
#' @param design one-sided formula.
#' @param es the last E-step (or \code{.gloc_cat_identity}).
#' @param w_min,band the peer rule.
#' @return an \eqn{n x k} matrix, \code{NA} where the cell is missing and in
#'   every column of a row above the combination cap, which has no pseudo-rows to
#'   compute it from.
#' @keywords internal
.gloc_cat_prob_observed <- function(X, M, W, B, Sigma, catp, priors, design, es,
                                    w_min = 0.5, band = .gloc_peer_band) {
  lev <- catp$levels; vars <- names(lev)
  out <- matrix(NA_real_, nrow(catp$F), length(vars), dimnames = list(NULL, vars))
  for (v in vars) {
    L <- length(lev[[v]])
    obs_rows <- which(!catp$Mc[, v])
    if (!length(obs_rows)) next
    if (L < 2L) { out[obs_rows, v] <- 1; next }
    keep <- which(!catp$Mc[es$pr_row, v])
    Fq <- es$Fp[keep, , drop = FALSE]; wq <- es$pr_w[keep]; rq <- es$pr_row[keep]
    P0 <- .gloc_cat_prior(priors[[v]], Fq)
    Fc <- Fq[rep(seq_len(nrow(Fq)), each = L), , drop = FALSE]
    Fc[[v]] <- .gloc_keep_contrasts(
      factor(rep(lev[[v]], times = nrow(Fq)), levels = lev[[v]],
             ordered = is.ordered(Fq[[v]])), Fq[[v]])
    Uc <- .gloc_design_rows(Fc, design, lev)
    if (!is.null(rownames(B)) && !identical(colnames(Uc), rownames(B)))
      stop("imputeCellGLoc(): the design columns of cat_prob_observed do not match B.")
    key <- rep(rq, each = L) * (L + 1) + rep(seq_len(L), times = nrow(Fq))
    Ubar <- rowsum(Uc * rep(wq, each = L), key, reorder = TRUE)
    pri <- rowsum(P0 * wq, rq, reorder = TRUE)
    rows <- as.integer(rownames(pri))
    ll <- .gloc_cat_loglik(X, M, W, B, Sigma, Ubar, rep(rows, each = L),
                           w_min = w_min, band = band)
    A <- log(pri) + matrix(ll, ncol = L, byrow = TRUE)
    A <- exp(A - apply(A, 1L, max))
    A <- A / rowSums(A)
    out[rows, v] <- A[cbind(seq_along(rows), as.integer(catp$F[[v]][rows]))]
  }
  out
}

#' A square root of a positive semi-definite matrix
#' @param C symmetric positive semi-definite matrix.
#' @return \code{R} with \code{t(R) \%*\% R} equal to \code{C}.
#' @keywords internal
.gloc_chol_psd <- function(C) {
  tryCatch(chol(C), error = function(e) {
    ev <- eigen((C + t(C)) / 2, symmetric = TRUE)
    t(ev$vectors %*% diag(sqrt(pmax(ev$values, 0)), length(ev$values)))
  })
}

#' One multiple-imputation draw from a cellGLoc fit
#'
#' Draws each missing categorical level from \code{fit$cat_posterior} (several
#' missing cells of one row independently from their marginals, an
#' approximation), builds the design rows for the drawn levels, and draws the
#' missing continuous cells from their conditional normal given those levels
#' and the unflagged peers (\code{.gloc_impute} with \code{cov = TRUE}). The
#' draws use the caller's random-number stream. \code{noise = FALSE} returns
#' the fit's own \code{imputed}: posterior modes and conditional expectations
#' under the expected design rows.
#' @param fit an \code{imputeCellGLoc} result.
#' @param data the data it was fitted on.
#' @param design,peer_w_min,peer_band the values used for the fit.
#' @param noise draw (\code{TRUE}) or return the point imputation.
#' @return an imputed copy of \code{data}.
#' @keywords internal
.gloc_draw_mi <- function(fit, data, design = ~ ., peer_w_min = 0.5,
                          peer_band = .gloc_peer_band, noise = TRUE) {
  sv <- .gloc_split_vars(data)
  X <- as.matrix(data[, sv$cont, drop = FALSE]); storage.mode(X) <- "double"
  X[!is.finite(X)] <- NA_real_
  M <- is.na(X)
  out <- data
  Ud <- fit$U
  post <- fit$cat_posterior
  if (length(post)) {
    catp <- .gloc_cat_prepare(data, sv$cat)
    Fd <- catp$F
    for (v in names(post)) {
      P <- post[[v]]
      rows <- as.integer(rownames(P))
      k <- if (noise)
        pmin(1L + rowSums(stats::runif(nrow(P)) > t(apply(P, 1L, cumsum))), ncol(P))
      else max.col(P, ties.method = "first")
      Fd[[v]][rows] <- colnames(P)[k]
      out[[v]] <- .gloc_cat_restore(Fd[[v]], data[[v]])
    }
    if (noise) Ud <- .gloc_design_rows(Fd, design, catp$levels)
    if (noise && !identical(colnames(Ud), rownames(fit$B)))
      stop(paste(".gloc_draw_mi(): the design columns of the drawn levels do not match",
                 "fit$B; pass the design the fit used."))
  }
  imp <- .gloc_impute(X, Ud, fit$B, fit$Sigma, M, W = fit$W, w_min = peer_w_min,
                      band = peer_band, cov = noise)
  Xi <- if (noise) imp$X else imp
  if (noise) for (nm in names(imp$cond_cov)) {
    i <- as.integer(nm); miss <- which(M[i, ])
    Xi[i, miss] <- Xi[i, miss] +
      drop(stats::rnorm(length(miss)) %*% .gloc_chol_psd(imp$cond_cov[[nm]]))
  }
  for (v in sv$cont) out[[v]] <- .gloc_restore_class(Xi[, v], data[[v]], v)
  out
}

#' Align prior models to another data set's categorical levels
#' @param priors prior models from \code{.gloc_cat_fit_priors}; entries may be
#'   \code{NULL}.
#' @param levels named list of the new data's levels.
#' @return the prior models with \code{map} and \code{new_levels} set, so that
#'   \code{.gloc_cat_prior} returns probabilities over \code{levels}. A level the
#'   models never saw gets the floor; a missing model becomes uniform.
#' @keywords internal
.gloc_cat_align_priors <- function(priors, levels) {
  out <- stats::setNames(vector("list", length(levels)), names(levels))
  for (v in names(levels)) {
    pr <- priors[[v]]
    if (is.null(pr)) {
      L <- length(levels[[v]])
      out[[v]] <- list(type = "marginal", probs = rep(1 / L, L), levels = levels[[v]])
      next
    }
    pr$map <- match(pr$levels, levels[[v]])
    pr$new_levels <- levels[[v]]
    out[[v]] <- pr
  }
  out
}

#' Level posteriors and expected design rows for data under a given fit
#'
#' Bootstrap-proper multiple imputation refits the estimator on a bootstrap
#' sample and imputes the original rows under that fit. This computes what
#' \code{.gloc_draw_mi} needs for the rows of \code{data}: the fit's \code{B}
#' aligned to \code{data}'s design columns (0 where the fit lacks a column,
#' counted in the attribute \code{dropped}), its \code{Sigma}, the cell weights
#' \code{W}, and one E-step at the fit's parameters and prior models.
#' @param fit an \code{imputeCellGLoc} result with \code{cat_priors}.
#' @param data the rows to impute.
#' @param W cell weights for \code{data}; \code{NULL} gives 1 on observed cells.
#' @param design,peer_w_min,peer_band as for the fit.
#' @return \code{list(B, Sigma, W, U, cat_posterior)}.
#' @keywords internal
.gloc_cat_posterior_for <- function(fit, data, W = NULL, design = ~ .,
                                    peer_w_min = 0.5, peer_band = .gloc_peer_band) {
  sv <- .gloc_split_vars(data)
  X <- as.matrix(data[, sv$cont, drop = FALSE]); storage.mode(X) <- "double"
  X[!is.finite(X)] <- NA_real_
  M <- is.na(X)
  if (is.null(W)) W <- (!M) + 0
  catp <- .gloc_cat_prepare(data, sv$cat)
  em <- any(catp$Mc)
  U0 <- if (em) .gloc_design_rows(catp$F[rowSums(catp$Mc) == 0L, , drop = FALSE],
                                  design, catp$levels)
        else .gloc_design(data, design, sv$cat)
  cols <- colnames(U0)
  B <- matrix(0, length(cols), ncol(fit$B), dimnames = list(cols, colnames(fit$B)))
  common <- intersect(cols, rownames(fit$B))
  B[common, ] <- fit$B[common, ]
  out <- list(B = B, Sigma = fit$Sigma, W = W, U = U0, cat_posterior = list())
  if (em) {
    cand <- .gloc_cat_candidates(catp, data, design)
    if (!identical(colnames(cand$Up), cols))
      stop(paste(".gloc_cat_posterior_for(): the candidate design columns do not match",
                 "the data's design."))
    es <- .gloc_cat_estep(X, M, W, B, fit$Sigma, catp, cand,
                          .gloc_cat_align_priors(fit$cat_priors, catp$levels),
                          w_min = peer_w_min, band = peer_band)
    out$U <- es$Ubar
    out$cat_posterior <- es$post
  }
  attr(out, "dropped") <- length(cols) - length(common)
  out
}
