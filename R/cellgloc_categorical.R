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

#' Evaluate an expression without base R's "contrasts dropped" warning
#'
#' Base R drops a factor's contrasts attribute when the factor's levels no longer
#' fit it, and \code{model.matrix()} says so: "contrasts dropped from factor ...
#' due to missing levels". The categorical EM meets it where \code{"level"} does
#' not. It builds its design on a completed copy of the data
#' (\code{.gloc_cat_candidates}), where a factor with a missing cell has no NA
#' level, so \code{addNA()} no longer strips the attribute first and
#' \code{model.frame(drop.unused.levels = TRUE)} drops it with an unused level
#' instead; and it builds the design of \code{"level"} again for its second start
#' (\code{.gloc_na_level_start}). With a factor that has a contrasts attribute and
#' an unused level, 7.4.1 raised that warning twice when the factor was fully
#' observed and never when it had a missing cell. These calls must not raise more
#' copies under \code{"em"} than \code{"level"} raises, so only that warning is
#' muffled there, and the coding the design ends up with is unchanged. The same
#' warning comes from \code{predict.multinom()} in \code{.gloc_cat_prior}.
#' @param expr the expression.
#' @return the value of \code{expr}.
#' @keywords internal
.gloc_no_contrasts_warning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("contrasts dropped", conditionMessage(w), fixed = TRUE))
      invokeRestart("muffleWarning")
  })
}

#' Factors whose own contrasts attribute the design will not use
#'
#' A factor can carry its own \code{contrasts} attribute, and \code{imputeCellGLoc}
#' loses it in two ways, both of which used to pass without a word the user could
#' act on (R65, Ruling R79).
#'
#' The two are listed here in the order this function appends them, which is the
#' order they read in the warning when both apply to one factor and are joined by
#' "and".
#'
#' \emph{First, a missing value that becomes a level.} Under
#' \code{categorical = "level"} the design puts a missing categorical value into a
#' level of its own, through \code{addNA()}, which goes through \code{factor()} and
#' so drops the attribute \emph{silently}: 7.4.1 said nothing here at all.
#'
#' \emph{Second, an unused level.} \code{model.frame(drop.unused.levels = TRUE)}
#' rebuilds the factor without that level and drops the attribute, matrix or
#' character alike, with base R's warning "contrasts dropped from factor ... due to
#' missing levels". That warning is muffled at every design site of the fit
#' (\code{.gloc_no_contrasts_warning}), because the categorical EM builds designs
#' that \code{categorical = "level"} does not -- on the completed copy, and again
#' for the second start -- and would otherwise warn more often than \code{"level"}
#' for the same data.
#'
#' Either way the fit codes the factor with the default contrasts. Nothing the fit
#' returns depends on the coding -- the fitted means, \eqn{\Sigma}, \eqn{W} and the
#' imputations are the same under any full-rank coding of the same level sets, only
#' \code{B}'s rows are named and parameterised differently -- so this reports, it
#' does not repair.
#'
#' Only variables the design uses as bare names are examined. A term that sets a
#' coding itself, such as \code{C(f, contr.sum)}, is not one, and neither is a
#' factor without an attribute of its own, which takes the session's
#' \code{options("contrasts")} as it always did.
#' @param data the data frame of the fit.
#' @param cat_vars names of the categorical columns.
#' @param design one-sided formula.
#' @param na_level \code{TRUE} when the design is the one that turns a missing
#'   categorical value into a level, i.e. whenever the categorical EM does not run.
#' @return a named list, one entry per such factor, holding the reason as a phrase.
#'   Empty when there is nothing to report.
#' @keywords internal
.gloc_contrast_lost <- function(data, cat_vars, design, na_level) {
  if (!length(cat_vars) || is.null(design) ||
      identical(all.vars(design), character(0))) return(list())
  df <- data[, cat_vars, drop = FALSE]
  ve <- tryCatch(as.list(attr(stats::terms(design, data = df), "variables"))[-1L],
                 error = function(e) NULL)
  if (!length(ve)) return(list())
  bare <- vapply(ve, function(e) if (is.name(e)) as.character(e) else NA_character_, "")
  out <- list()
  for (v in intersect(bare[!is.na(bare)], cat_vars)) {
    x <- data[[v]]
    if (!is.factor(x) || is.null(attr(x, "contrasts"))) next
    why <- character(0)
    if (na_level && anyNA(x))
      why <- c(why, "its missing values enter the design as a level of their own")
    unused <- setdiff(levels(x), as.character(x[!is.na(x)]))
    if (length(unused))
      why <- c(why, sprintf("it has the unused level%s %s",
                            if (length(unused) > 1L) "s" else "",
                            paste0("\"", unused, "\"", collapse = ", ")))
    if (length(why)) out[[v]] <- paste(why, collapse = " and ")
  }
  out
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
  # A design site of the fit, so base R's "contrasts dropped" is muffled here as
  # well (R65); .gloc_contrast_lost reports the loss once per factor instead.
  .gloc_no_contrasts_warning({
    mf <- stats::model.frame(design, data = df, na.action = stats::na.pass,
                             drop.unused.levels = FALSE)
    stats::model.matrix(design, mf)
  })
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
    pp <- .gloc_no_contrasts_warning(
      tryCatch(stats::predict(pr$model, newdata = Fnew[, pr$preds, drop = FALSE],
                              type = "probs"),
               error = function(e) NULL))   # e.g. a predictor level the fit never saw
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
#'
#' This is the density term of VIM 7.5.0, with one weight row per observation. It
#' is still the E-step's term in the binary corner, and \code{cat_prob_observed}
#' uses it with the returned \code{W}. Per-level detection (soft corner, 7.5.1)
#' scores each candidate \eqn{k} with its own weight row instead
#' (\code{.gloc_cat_score}):
#' \eqn{\ell_k = -\frac{1}{2} [|K| \log 2\pi + \log\det G + y' G^{-1} y]
#' - \frac{1}{2} \sum_{j \in K} (1 - r_j) p_j - \frac{1}{2} \sum_{j \in O
#' \setminus K} \lambda_j}, which at the endpoints of the band (every reliability
#' 0 or 1) is minus one half times the row's term of the binary-corner objective
#' (\code{.gloc_objective}). When every candidate of a row carries the row's weight
#' row, \eqn{G}, \eqn{K} and the penalties are shared, so \eqn{\ell_k} and this
#' function's value differ by one constant within the row and give the same
#' posteriors. That holds wherever the Cholesky factorisation of \eqn{G} succeeds,
#' which is the ordinary case; on a \eqn{G} that is not positive definite the two
#' fall back differently, this function to \code{MASS::ginv} and the score to the
#' eigenvalues floored at 1e-8 times the largest.
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

#' Score of candidate levels under their own cell weights (per-level detection)
#'
#' Under per-level detection (\code{imputeCellGLoc(categorical = "em")}, soft
#' corner, since 7.5.1) every candidate \eqn{k} of a row \eqn{i} with a missing
#' categorical cell -- one level, or one level combination -- carries its own cell
#' weights \eqn{W_k}, computed at its own design row \eqn{u_k}. This function
#' returns the score \eqn{\ell_k} by which the E-step judges such a candidate: the
#' cellwise-penalised likelihood of the binary corner, with the candidate's
#' posterior proportional to its prior times \eqn{\exp(\ell_k)}.
#'
#' The score is
#'
#' \deqn{\ell_k = -\frac{1}{2} [ |K| \log 2\pi + \log\det G + y' G^{-1} y ]
#'   - \frac{1}{2} \sum_{j \in K} (1 - r_j) p_j - \frac{1}{2}
#'   \sum_{j \in O \setminus K} \lambda_j,}
#'
#' with \eqn{O} the observed continuous cells of row \eqn{i}; \eqn{r_j} the peer
#' reliability of cell \eqn{j} under \eqn{W_k} (\code{.gloc_peer_rel}, band
#' included); \eqn{K = \{j \in O : r_j > 0\}}; \eqn{D = \mathrm{diag}(\sqrt{r_K})}
#' and \eqn{G = D \Sigma_{KK} D + \mathrm{diag}(\sigma_{jj} (1 - r_j))} over
#' \eqn{K}, the noise-inflated construction of \code{.gloc_cond_resid};
#' \eqn{y = \sqrt{r_K} \odot (x_{iK} - (B' u_k)_K)};
#' \eqn{\lambda_j = \chi^2_{1;0.99} + \log 2\pi + \log c_j} with
#' \eqn{c_j = 1 / (\Sigma^{-1})_{jj}} (\code{.gloc_lambda}); and
#' \eqn{p_j = \lambda_j - \log(2\pi\sigma_{jj}) = \chi^2_{1;0.99} + \log(c_j /
#' \sigma_{jj})}.
#'
#' \strong{Endpoints.} When every \eqn{r_j} is 0 or 1, \eqn{\ell_k} is exactly
#' \eqn{-1/2} times row \eqn{i}'s term of the binary-corner objective
#' (\code{.gloc_objective}) at the candidate's residuals \eqn{x_i - B' u_k} and
#' weights, with \eqn{\lambda} from the same \eqn{\Sigma}: the Gaussian
#' log-density of the retained cells minus \eqn{\lambda_j / 2} per flagged observed
#' cell. Missing cells are the same under every candidate and are left out,
#' whatever their weight. \strong{Inside the band} \eqn{\ell_k} is continuous in
#' the weights: as \eqn{r_j \to 0}, \eqn{y_j \to 0} and cell \eqn{j}'s row of
#' \eqn{G} tends to \eqn{\sigma_{jj}}, so the cell contributes
#' \eqn{-\frac{1}{2} \log(2\pi\sigma_{jj})}, and \eqn{p_j} tops that up to
#' \eqn{\lambda_j / 2}. The E-step therefore stays continuous in \eqn{W}, which the
#' fixed-point argument of the peer band needs. When every candidate of a row
#' carries the row's weight row, \eqn{\ell_k} differs from
#' \code{.gloc_cat_loglik} by one constant within the row, wherever the Cholesky
#' factorisation of \eqn{G} succeeds; on a \eqn{G} that is not positive definite
#' the two fall back differently (\code{MASS::ginv} there, the eigenvalue floor
#' here).
#'
#' \strong{Why.} A cell that fits candidate \eqn{c} is retained under \eqn{c} and
#' flagged under a candidate it does not fit, so \eqn{c} gains up to
#' \eqn{\chi^2_{1;0.99} / 2 \approx 3.3} log units from it. With one weight row per
#' observation, computed at the expected design row, the cell that decides the
#' level was flagged under every candidate and dropped from all of them, and the
#' posterior could not move (the flag lock of 7.5.0). A cell that fits no candidate
#' is flagged under every candidate, pays the same penalty under each and leaves
#' the density under each, so its effect on the level is that of a missing cell.
#'
#' \eqn{\Sigma} is the scatter of the current iteration (Ruling R71), so the
#' density and the penalty sit on one scale. If \code{.gloc_lambda(Sigma)} gives no
#' penalty, \eqn{c_j = \sigma_{jj}}, with a warning. \eqn{\log\det G} and
#' \eqn{G^{-1}} come from the Cholesky factor of \eqn{G}; if that fails, from its
#' eigenvalues floored at 1e-8 times the largest, the floor of \code{.gloc_psd}.
#' Candidates whose reliabilities are all 0 or 1 share one factorisation per
#' pattern; a candidate with a cell inside the band gets its own.
#' @param X \eqn{n x p} continuous matrix.
#' @param M \eqn{n x p} missing mask.
#' @param Wc cell weights, one row per candidate, in the order of \code{Ucand}.
#' @param B \eqn{q x p} coefficients.
#' @param Sigma \eqn{p x p} scatter.
#' @param Ucand candidate design rows, one per candidate.
#' @param row_of the data row of each candidate.
#' @param w_min,band the peer rule.
#' @return one score \eqn{\ell_k} per candidate.
#' @keywords internal
.gloc_cat_score <- function(X, M, Wc, B, Sigma, Ucand, row_of, w_min = 0.5,
                            band = .gloc_peer_band) {
  ll <- numeric(nrow(Ucand))
  if (!length(ll)) return(ll)
  sdiag <- diag(Sigma)
  spos <- pmax(sdiag, .Machine$double.eps)
  lam <- .gloc_lambda(Sigma)
  if (is.null(lam)) {
    warning(paste("cellGLoc: the scatter gives no valid flagging penalty (it is",
                  "singular, or its inverse has a diagonal entry that is not finite",
                  "and positive), so the score of a candidate level for a missing",
                  "categorical cell uses each variable's variance in place of its",
                  "conditional variance in that penalty."), call. = FALSE)
    lam <- stats::qchisq(0.99, df = 1) + log(2 * pi) + log(spos)
  }
  lam <- unname(lam)
  pj <- lam - log(2 * pi * spos)
  Obs <- !M[row_of, , drop = FALSE]
  Rel <- .gloc_peer_rel(Obs, Wc, w_min = w_min, band = band)
  In  <- Rel > 0
  # lambda_j for each observed cell outside K, (1 - r_j) p_j for each cell in K
  pen <- drop((Obs & !In) %*% lam) + drop((In * (1 - Rel)) %*% pj)
  Mu <- Ucand %*% B
  key <- apply(Rel, 1L, function(r)
    if (any(r > 0 & r < 1)) NA_character_ else paste0(as.integer(r > 0), collapse = ""))
  inband <- is.na(key)
  key[inband] <- paste0("b", seq_len(sum(inband)))
  for (g in split(seq_along(key), key)) {
    r  <- Rel[g[1L], ]
    kk <- which(r > 0)
    if (!length(kk)) next
    rk <- r[kk]; dd <- sqrt(rk)
    G  <- outer(dd, dd) * Sigma[kk, kk, drop = FALSE] +
            diag(sdiag[kk] * (1 - rk), length(kk))
    L <- tryCatch(chol(G), error = function(e) NULL)
    if (!is.null(L)) {
      Gi <- chol2inv(L)
      logdet <- 2 * sum(log(diag(L)))
    } else {
      ev <- eigen((G + t(G)) / 2, symmetric = TRUE)
      top <- max(ev$values)
      vals <- pmax(ev$values,
                   if (is.finite(top) && top > 0) 1e-8 * top else .Machine$double.eps)
      Gi <- ev$vectors %*% (t(ev$vectors) / vals)
      logdet <- sum(log(vals))
    }
    Z <- (X[row_of[g], kk, drop = FALSE] - Mu[g, kk, drop = FALSE]) *
           rep(dd, each = length(g))
    ll[g] <- -0.5 * (rowSums((Z %*% Gi) * Z) + logdet + length(kk) * log(2 * pi))
  }
  ll - 0.5 * pen
}

#' The posterior mixture of the candidates' cell weights
#'
#' Under per-level detection a row's reported weights are
#' \eqn{W_i = \sum_k r_k W_k} over its candidates. The sum is taken as
#' \eqn{W_{k_0} + \sum_k r_k (W_k - W_{k_0})}, with \eqn{k_0} the row's first
#' candidate, which is the same number up to rounding because the \eqn{r_k} sum to
#' 1, and is exact wherever the candidates agree: a row that is a single candidate,
#' or lies above the combination cap, returns its own weight row, and a cell with
#' the same weight under every candidate (a missing cell, say) returns that weight.
#' At \code{maxit = 0}, where every candidate carries the start's weight row, the
#' start's weights come back unchanged.
#' @param Wk cell weights, one row per candidate.
#' @param r each candidate's posterior weight (1 for a single candidate).
#' @param row_of the data row of each candidate; every row in \code{1:n} has one.
#' @param n the number of rows.
#' @param dn dimnames of the result.
#' @return an \eqn{n x p} matrix.
#' @keywords internal
.gloc_cat_mix_weights <- function(Wk, r, row_of, n, dn = NULL) {
  W <- matrix(0, n, ncol(Wk), dimnames = dn)
  first <- !duplicated(row_of)
  W[row_of[first], ] <- Wk[first, , drop = FALSE]
  S <- rowsum((Wk - W[row_of, , drop = FALSE]) * r, row_of, reorder = TRUE)
  at <- as.integer(rownames(S))
  W[at, ] <- W[at, , drop = FALSE] + S
  W
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
    # The completed copy has no NA level, so a factor with a contrasts attribute
    # and an unused level makes base R warn "contrasts dropped" here, where
    # "level" is silent; see .gloc_no_contrasts_warning.
    aux <- .gloc_no_contrasts_warning(
      .gloc_design_aux(dc, design, vars,
                       max_patterns = if (n_all <= 1e6) max(n_all, .gloc_max_patterns)
                                      else .gloc_max_patterns))
    pats <- aux$patterns
    Uref <- .gloc_no_contrasts_warning(.gloc_design(dc, design, vars))
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
#'
#' With per-candidate weights \code{Wc} (per-level detection, soft corner, since
#' 7.5.1) the density term of a pseudo-row \eqn{k} of a row with a missing cell is
#' the cellwise-penalised score of \code{.gloc_cat_score},
#' \eqn{\ell_k = -\frac{1}{2} [|K| \log 2\pi + \log\det G + y' G^{-1} y]
#' - \frac{1}{2} \sum_{j \in K} (1 - r_j) p_j - \frac{1}{2} \sum_{j \in O
#' \setminus K} \lambda_j}, computed under the candidate's own weights with
#' \eqn{\lambda} from \code{Sigma}; the posterior and the mean-field sweeps are
#' otherwise unchanged, with \eqn{\ell_k} in place of the density term per level
#' combination. At the endpoints of the band \eqn{\ell_k} is minus one half times
#' the row's term of the binary-corner objective (\code{.gloc_objective}); when
#' every candidate carries its row's weight row, the posteriors are those without
#' \code{Wc} up to rounding, wherever the Cholesky factorisation of \eqn{G}
#' succeeds (the two density routes fall back differently otherwise; see
#' \code{.gloc_cat_score}).
#' @param X,M,W,B,Sigma the continuous data, mask, weights and current fit;
#'   \code{B = NULL} drops the density (prior-only E-step). \code{W} is not used
#'   when \code{Wc} is given.
#' @param catp,cand results of \code{.gloc_cat_prepare} and
#'   \code{.gloc_cat_candidates}.
#' @param priors result of \code{.gloc_cat_fit_priors}.
#' @param w_min,band the peer rule.
#' @param sweeps see \code{.gloc_cat_sweeps}.
#' @param Wc \code{NULL} (the 7.5.0 density term, one weight row per observation)
#'   or cell weights with one row per pseudo-row of \code{cand}, in its order;
#'   only the rows of candidates whose row misses a categorical cell are read.
#' @return \code{list(pr_row, pr_w, Fp, Up, Ubar, post, Umain_bar)}.
#' @keywords internal
.gloc_cat_estep <- function(X, M, W, B, Sigma, catp, cand, priors, w_min = 0.5,
                            band = .gloc_peer_band, sweeps = .gloc_cat_sweeps,
                            Wc = NULL) {
  lev <- catp$levels; N <- nrow(cand$Fp)
  inc <- rowSums(catp$Mc)[cand$pr_row] > 0L
  w <- rep(1, N)
  ll <- numeric(N)
  if (!is.null(B) && any(inc))
    ll[inc] <- if (is.null(Wc))
      .gloc_cat_loglik(X, M, W, B, Sigma, cand$Up[inc, , drop = FALSE],
                       cand$pr_row[inc], w_min = w_min, band = band)
    else
      .gloc_cat_score(X, M, Wc[inc, , drop = FALSE], B, Sigma,
                      cand$Up[inc, , drop = FALSE], cand$pr_row[inc],
                      w_min = w_min, band = band)
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

#' The posterior mixture of the candidates' conditional expectations
#'
#' Under per-level detection (\code{imputeCellGLoc(categorical = "em")}, soft
#' corner, since 7.5.1) a missing continuous cell of a row that also misses a
#' categorical cell is imputed by \eqn{\sum_k r_k E[x_{mis} | \textrm{cells clean
#' under } k, u_k]}: \code{.gloc_impute} on the row's candidate rows, each at its
#' own design row with its own cell weights, mixed by the candidates' posterior
#' probabilities. That is the exact posterior mean of the missing cell.
#'
#' Until 7.5.0 such a cell was imputed once, at the row's expected design row
#' \eqn{\bar u_i}. That was exact only because every level shared one weight row
#' and hence one set of clean peers, so the conditional expectation was linear in
#' the design row and the mixture could be taken inside it. Per-level detection
#' breaks that: conditioning at \eqn{\bar u_i} on the mixture weights would
#' condition on a cell with a weight that no candidate gave it, leaving detection
#' and imputation inconsistent (Ruling R74).
#'
#' Only the missing cells of the rows in \code{cw} are replaced, and only where
#' the row has one; every other cell of \code{Xi} is returned untouched, bit for
#' bit. A row above the combination cap is not in \code{cw} and keeps the 7.5.0
#' imputation. \code{imputeCellGLoc} and \code{.gloc_draw_mi(noise = FALSE)} both
#' come through here, which is what makes the second reproduce the first exactly.
#' @param Xi the imputation so far, from \code{.gloc_impute} at the expected
#'   design rows; its non-mixture rows are the result's.
#' @param X,M the continuous data and its missing mask.
#' @param B,Sigma the fit's coefficients and scatter.
#' @param cw the fit's \code{cat_weights}.
#' @param design one-sided formula, the fit's.
#' @param levels named list of levels, from \code{.gloc_cat_prepare}.
#' @param w_min,band the peer rule.
#' @return \code{Xi} with those cells replaced.
#' @keywords internal
.gloc_impute_mix <- function(Xi, X, M, B, Sigma, cw, design, levels,
                             w_min = 0.5, band = .gloc_peer_band) {
  k <- which(rowSums(M[cw$row, , drop = FALSE]) > 0L)
  if (!length(k)) return(Xi)
  rows <- cw$row[k]
  Uk <- .gloc_design_rows(cw$levels[k, , drop = FALSE], design, levels)
  if (!is.null(rownames(B)) && !identical(colnames(Uk), rownames(B)))
    stop(paste(".gloc_impute_mix(): the design columns of the candidate levels do not",
               "match B; pass the design the fit used."))
  Xk <- .gloc_impute(X[rows, , drop = FALSE], Uk, B, Sigma, M[rows, , drop = FALSE],
                     W = cw$W[k, , drop = FALSE], w_min = w_min, band = band)
  S <- rowsum(Xk * cw$prob[k], rows, reorder = TRUE)
  at <- as.integer(rownames(S))
  mm <- M[at, , drop = FALSE]
  sub <- Xi[at, , drop = FALSE]
  sub[mm] <- S[mm]                      # observed cells are left exactly as they were
  Xi[at, ] <- sub
  Xi
}

#' One multiple-imputation draw from a cellGLoc fit
#'
#' Draws each missing categorical level from \code{fit$cat_posterior} (several
#' missing cells of one row independently from their marginals, an
#' approximation), builds the design rows for the drawn levels, and draws the
#' missing continuous cells from their conditional normal given those levels
#' and the unflagged peers (\code{.gloc_impute} with \code{cov = TRUE}). The
#' draws use the caller's random-number stream. \code{noise = FALSE} returns
#' the fit's own \code{imputed}: posterior modes and, for a row that misses a
#' categorical cell, the posterior mixture of \code{.gloc_impute_mix}.
#'
#' With \code{fit$cat_weights} (per-level detection, soft corner, since 7.5.1) a
#' draw imputes each row that misses a categorical cell, and lies below the
#' combination cap, with the cell weights of the candidate whose level
#' combination was drawn for it, not with the row's own weights, which are the
#' posterior mixture over the candidates (Ruling R74). A row above the cap, and a
#' fit without \code{cat_weights} -- the binary corner, \code{categorical =
#' "level"}, or a \code{fit} assembled by \code{.gloc_cat_posterior_for} without
#' them -- keeps the 7.5.0 rule, one weight row per observation.
#'
#' A candidate's weight row whose posterior probability is small may sit up to
#' \code{eps / prob} from its own fixed point, because the stopping rule weights a
#' candidate's weight change by that probability (Ruling R73); a draw that lands
#' on such a candidate therefore conditions on a slightly unsettled flag set.
#' @param fit an \code{imputeCellGLoc} result, or the list
#'   \code{.gloc_cat_posterior_for} returns.
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
  W  <- fit$W
  cw <- fit$cat_weights
  post <- fit$cat_posterior
  catp <- NULL
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
    # The drawn candidate's own cell weights. A row is matched to its candidate
    # by its whole level combination, observed cells included, which is what
    # cat_weights$levels holds; a row above the cap matches nothing and keeps
    # fit$W.
    if (noise && !is.null(cw)) {
      ckey <- do.call(paste, c(lapply(cw$levels, as.character), list(sep = "\r")))
      dkey <- do.call(paste, c(lapply(Fd[names(cw$levels)], as.character),
                               list(sep = "\r")))
      rws <- sort(unique(cw$row))
      kk <- match(paste(rws, dkey[rws]), paste(cw$row, ckey))
      if (anyNA(kk))
        stop(paste(".gloc_draw_mi(): a drawn level combination is not in",
                   "fit$cat_weights; pass the data the fit was computed on."))
      W[rws, ] <- cw$W[kk, , drop = FALSE]
    }
  }
  imp <- .gloc_impute(X, Ud, fit$B, fit$Sigma, M, W = W, w_min = peer_w_min,
                      band = peer_band, cov = noise)
  Xi <- if (noise) imp$X else imp
  if (noise) for (nm in names(imp$cond_cov)) {
    i <- as.integer(nm); miss <- which(M[i, ])
    Xi[i, miss] <- Xi[i, miss] +
      drop(stats::rnorm(length(miss)) %*% .gloc_chol_psd(imp$cond_cov[[nm]]))
  }
  # The point imputation is the fit's, so it takes the fit's mixture as well.
  if (!noise && !is.null(cw))
    Xi <- .gloc_impute_mix(Xi, X, M, fit$B, fit$Sigma, cw, design, catp$levels,
                           w_min = peer_w_min, band = peer_band)
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
#' Under per-level detection (soft corner, since 7.5.1) the posteriors are a
#' function of the \emph{candidates'} own weight rows, not of \code{W}, so
#' reproducing them needs \code{cat_weights} as well: the E-step then scores each
#' candidate with \code{.gloc_cat_score} under those weights, with \eqn{\lambda}
#' from \code{fit$Sigma} (Ruling R74). The original fit's \code{cat_weights} are
#' returned with their \code{prob} recomputed under this fit, so that
#' \code{.gloc_draw_mi} can impute a drawn row with its candidate's weights.
#' Without them the 7.5.0 E-step runs, which is what the binary corner needs.
#' @param fit an \code{imputeCellGLoc} result with \code{cat_priors}.
#' @param data the rows to impute.
#' @param W cell weights for \code{data}; \code{NULL} gives 1 on observed cells.
#' @param cat_weights the per-candidate cell weights of a fit on \emph{these}
#'   rows (its \code{cat_weights}), or \code{NULL}. Its \code{row} and
#'   \code{levels} must match the candidate table of \code{data}, or this stops.
#' @param design,peer_w_min,peer_band as for the fit.
#' @return \code{list(B, Sigma, W, U, cat_posterior, cat_weights)};
#'   \code{cat_weights} is \code{NULL} unless it was passed in.
#' @keywords internal
.gloc_cat_posterior_for <- function(fit, data, W = NULL, cat_weights = NULL,
                                    design = ~ .,
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
  out <- list(B = B, Sigma = fit$Sigma, W = W, U = U0, cat_posterior = list(),
              cat_weights = NULL)
  if (em) {
    cand <- .gloc_cat_candidates(catp, data, design)
    if (!identical(colnames(cand$Up), cols))
      stop(paste(".gloc_cat_posterior_for(): the candidate design columns do not match",
                 "the data's design."))
    inc <- which(rowSums(catp$Mc)[cand$pr_row] > 0L)
    Wc <- NULL
    if (!is.null(cat_weights)) {
      lv <- cand$Fp[inc, , drop = FALSE]
      ok <- identical(as.integer(cat_weights$row), as.integer(cand$pr_row[inc])) &&
        identical(dim(cat_weights$W), c(length(inc), ncol(W))) &&
        identical(names(cat_weights$levels), names(lv)) &&
        all(vapply(names(lv), function(v)
          identical(as.character(cat_weights$levels[[v]]), as.character(lv[[v]])),
          NA))
      if (!ok)
        stop(paste(".gloc_cat_posterior_for(): cat_weights does not match the candidate table",
                   "of these rows; pass the cat_weights of a fit on this data."))
      # Only the candidates of rows with a missing categorical cell are read; the
      # rest carry their row's weights so that the matrix means what it says.
      Wc <- W[cand$pr_row, , drop = FALSE]
      Wc[inc, ] <- cat_weights$W
    }
    es <- .gloc_cat_estep(X, M, W, B, fit$Sigma, catp, cand,
                          .gloc_cat_align_priors(fit$cat_priors, catp$levels),
                          w_min = peer_w_min, band = peer_band, Wc = Wc)
    out$U <- es$Ubar
    out$cat_posterior <- es$post
    if (!is.null(cat_weights)) {
      cat_weights$prob <- es$pr_w[inc]
      out$cat_weights <- cat_weights
    }
  }
  attr(out, "dropped") <- length(cols) - length(common)
  out
}

#' Rows of the design of \code{categorical = "level"} at known categorical values
#'
#' The design on the data with a missing categorical value as a level of its own
#' (\code{.gloc_design}), evaluated at the rows of \code{Fr}, whose categorical
#' values are all known. The rows of \code{Fr} are appended to the data's own
#' categorical columns before the design is built, so every variable keeps its
#' levels, its missing-value level, its orderedness and its contrasts exactly as
#' in the design on the data alone. The data's own rows must come out unchanged;
#' otherwise (for example a design term whose levels depend on which rows are
#' present) this stops.
#' @param data the data frame of the fit.
#' @param Fr data frame of the categorical columns without \code{NA}, such as the
#'   pseudo-row table of \code{.gloc_cat_candidates}.
#' @param design one-sided formula.
#' @param cat_vars names of the categorical columns.
#' @param U the design on \code{data}, if already built.
#' @return an \code{nrow(Fr) x ncol(U)} matrix with the columns of \code{U}.
#' @keywords internal
.gloc_level_rows <- function(data, Fr, design, cat_vars,
                             U = .gloc_design(data, design, cat_vars)) {
  n <- nrow(data); m <- nrow(Fr)
  cols <- lapply(stats::setNames(cat_vars, cat_vars), function(v) {
    x <- data[[v]]
    y <- as.character(Fr[[v]])
    if (is.factor(x))
      .gloc_keep_contrasts(factor(c(as.character(x), y), levels = levels(x),
                                  ordered = is.ordered(x)), x)
    else if (is.logical(x)) c(x, as.logical(y))
    else c(as.character(x), y)
  })
  both <- structure(cols, class = "data.frame", row.names = seq_len(n + m))
  Ub <- .gloc_design(both, design, cat_vars)
  if (!identical(colnames(Ub), colnames(U)) ||
      !isTRUE(all(Ub[seq_len(n), , drop = FALSE] == U)))
    stop(paste(".gloc_level_rows(): the design at the appended rows does not reproduce",
               "the design on the data."))
  Ub[n + seq_len(m), , drop = FALSE]
}

#' Coefficients of the \code{categorical = "level"} start in the EM's design
#'
#' The categorical EM's second start is the robust start of
#' \code{categorical = "level"}, whose design has a level for the missing value;
#' the EM's design has none. Under treatment coding, the default for an unordered
#' factor, that level adds its own columns, which are 0 on every row whose value
#' is known, and leaves the others unchanged: the rows of \code{B} for those
#' columns are dropped. That holds exactly when the design of \code{"level"},
#' evaluated at the EM's pseudo-rows, equals the EM's pseudo-row design in the
#' EM's columns and is 0 in the others, which is what is checked. Otherwise --
#' an ordered factor, whose polynomial coding changes with the number of levels,
#' or sum contrasts -- the start's fitted means are carried over: the coefficients
#' are the least-squares fit of the EM's pseudo-row design to the fitted means
#' the \code{"level"} start gives the same level combinations, with the
#' coefficients of aliased columns at 0. Both routes give the pseudo-rows the
#' start's fitted means, which is all the iteration uses of a start's
#' coefficients; where both apply they agree up to rounding.
#' @param B coefficients of the \code{"level"} design, one row per column of
#'   \code{Ulv}.
#' @param Ulv that design at the pseudo-rows (\code{.gloc_level_rows}).
#' @param Up the EM's pseudo-row design.
#' @param lsq \code{TRUE} takes the least-squares route even where the rows can
#'   be dropped; for testing.
#' @return a matrix with one row per column of \code{Up}.
#' @keywords internal
.gloc_level_B <- function(B, Ulv, Up, lsq = FALSE) {
  nm <- colnames(Up)
  if (!lsq && all(nm %in% colnames(Ulv)) &&
      isTRUE(all(Ulv[, nm, drop = FALSE] == Up)) &&
      isTRUE(all(Ulv[, setdiff(colnames(Ulv), nm), drop = FALSE] == 0)))
    return(B[nm, , drop = FALSE])
  Bm <- qr.coef(qr(Up), Ulv %*% B)
  Bm[is.na(Bm)] <- 0
  dimnames(Bm) <- list(nm, colnames(B))
  Bm
}

#' The categorical EM's second start
#'
#' The robust start of \code{categorical = "level"}: a missing categorical value
#' is a level of its own and every row is fitted (\code{.gloc_design_setup},
#' \code{.gloc_start_robust} without \code{fit_rows}). Its flags are kept, and its
#' coefficients are carried into the EM's design by \code{.gloc_level_B}. The
#' level posteriors are left to the caller, which starts them from the priors
#' alone, as for the first start.
#' @param X,M the continuous data and their missing mask.
#' @param data,design,cat_vars as in \code{imputeCellGLoc}.
#' @param cand the EM's pseudo-row table, from \code{.gloc_cat_candidates}.
#' @return \code{list(W, B)}.
#' @keywords internal
.gloc_na_level_start <- function(X, M, data, design, cat_vars, cand) {
  # Base R's "contrasts dropped" warnings of these designs would repeat those of
  # the EM's own design; see .gloc_no_contrasts_warning.
  ds <- .gloc_no_contrasts_warning(.gloc_design_setup(data, design, cat_vars))
  st <- .gloc_start_robust(X, ds$U, M, warn_design = FALSE, U_main = ds$U_main,
                           patterns = ds$patterns)
  Ulv <- .gloc_no_contrasts_warning(
    .gloc_level_rows(data, cand$Fp, design, cat_vars, U = ds$U))
  list(W = st$W, B = .gloc_level_B(st$B, Ulv, cand$Up))
}
