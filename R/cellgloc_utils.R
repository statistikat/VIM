#' Build the categorical design matrix for cellGLoc
#'
#' Missing factor values become an explicit level rather than dropped rows, so
#' every observation keeps a design row.
#'
#' @param data a data.frame containing the categorical variables.
#' @param design a one-sided formula, e.g. \code{~ .} or \code{~ 1}.
#' @param cat_vars character vector naming the categorical columns.
#' @return an \code{n x q} numeric matrix whose first column is the intercept.
#' @keywords internal
.gloc_design <- function(data, design, cat_vars) {
  if (is.null(design)) design <- ~ 1
  if (!length(cat_vars) || identical(all.vars(design), character(0))) {
    return(matrix(1, nrow = nrow(data), ncol = 1,
                  dimnames = list(NULL, "(Intercept)")))
  }
  df <- data[, cat_vars, drop = FALSE]
  for (v in names(df)) {
    df[[v]] <- as.factor(df[[v]])
    if (anyNA(df[[v]])) df[[v]] <- addNA(df[[v]], ifany = TRUE)
  }
  mf <- stats::model.frame(design, data = df, na.action = stats::na.pass,
                           drop.unused.levels = TRUE)
  stats::model.matrix(design, mf)
}

#' Weighted least-squares update of the cellGLoc mean structure
#'
#' Column \code{j} of \code{B} is fitted on the design using that column's cell
#' weights as observation weights. Cross-response dependence is handled by the
#' scatter step, not here.
#'
#' A column left with no cell of positive weight (e.g. every weight is zero)
#' cannot be fit at all; rather than silently returning \code{NA} into
#' downstream matrices, this case warns (naming the offending column) and
#' falls back to the unweighted median of that column's finite values, or to
#' \code{0} if none exists.
#'
#' @param X \eqn{n x p} numeric matrix of continuous variables, may contain NA.
#' @param U \eqn{n x q} design matrix from \code{.gloc_design}.
#' @param W \eqn{n x p} matrix of cell weights in \[0, 1\].
#' @return a \eqn{q x p} matrix of coefficients.
#' @keywords internal
.gloc_update_B <- function(X, U, W) {
  p <- ncol(X); q <- ncol(U)
  B <- matrix(0, q, p, dimnames = list(colnames(U), colnames(X)))
  for (j in seq_len(p)) {
    w  <- W[, j]
    ok <- is.finite(X[, j]) & is.finite(w) & w > 0
    if (sum(ok) <= q) {
      if (sum(ok) == 0L) {
        cn <- colnames(X)[j]
        if (is.null(cn) || is.na(cn) || !nzchar(cn)) cn <- as.character(j)
        warning(sprintf(
          "cellGLoc: column '%s' has no cell with positive weight; using the unweighted median as a deterministic fallback (0 if no finite value exists)",
          cn), call. = FALSE)
        finite_x <- X[is.finite(X[, j]), j]
        B[1L, j] <- if (length(finite_x)) stats::median(finite_x) else 0
      } else {
        B[1L, j] <- stats::median(X[ok, j])
      }
      next
    }
    sw <- sqrt(w[ok])
    fit <- tryCatch(qr.solve(U[ok, , drop = FALSE] * sw, X[ok, j] * sw),
                    error = function(e) NULL)
    if (is.null(fit)) {                       # rank-deficient design: intercept only
      B[1L, j] <- stats::weighted.mean(X[ok, j], w[ok])
    } else B[, j] <- fit
  }
  B
}

#' Half-width of the peer-reliability band in \code{.gloc_cond_resid}
#'
#' The peer-inclusion rule \eqn{w > w_{\min}} is applied over a band
#' \eqn{[w_{\min} - h, w_{\min} + h]} rather than at a point, which is what
#' makes the soft corner's weight map continuous; see the "peer band" section
#' of \code{.gloc_cond_resid} for the argument and the construction.
#'
#' The value is a compromise between two measurable things. Too narrow and the
#' map is continuous but so steep that the relaxed iteration still overshoots:
#' the damped update \eqn{w \mapsto (1 - d) w + d f(w)} is locally contracting
#' only while \eqn{f' > 1 - 2/d}, and \eqn{f'} across the band is about
#' \eqn{-\Delta / 2h} where \eqn{\Delta \approx 0.2} is the measured jump a
#' single threshold crossing used to produce, so \eqn{h} must exceed about
#' \eqn{\Delta d / 2} -- 0.025 at an unrelaxed step and less when relaxed. Too
#' wide and the interpolation stops being a fringe: bisquare weights sit at
#' 0.5 at \eqn{|z| = 2.55}, so a band of \eqn{\pm h} covers the standardised
#' residuals in roughly \eqn{|z| \in (2.55 - 4h, 2.55 + 4h)}, which at
#' \eqn{h = 0.05} is about 1.2\% of clean Gaussian cells and at \eqn{h = 0.25}
#' would be nearer 10\%.
#'
#' 0.05 sits above the steepness bound and below the fringe bound. Measured on
#' a sweep of one cell's weight across the threshold, the largest one-step jump
#' it leaves in a row-mate's standardised residual is 0.0035, against 0.110 for
#' the hard cut, with identical values at both ends of the sweep.
#'
#' @format a length-one numeric.
#' @keywords internal
.gloc_peer_band <- 0.05

#' Reliability of each cell as a peer to condition on
#'
#' The peer rule shared by detection (\code{.gloc_cond_resid}) and imputation
#' (\code{.gloc_impute}), kept in one place so the two cannot drift apart.
#' Without weights every observed cell is fully reliable. With weights, a
#' cell's reliability ramps smoothly (\eqn{3t^2 - 2t^3}) from 0 at
#' \code{w_min - band} to 1 at \code{w_min + band}; \code{band = 0} is the hard
#' cut \eqn{w > w_{\min}}. Unobserved cells are 0. See the "peer band" section
#' of \code{.gloc_cond_resid} for why the rule is a band.
#'
#' @param obs \eqn{n x p} logical matrix, \code{TRUE} where a cell is observed.
#' @param W optional \eqn{n x p} matrix of cell weights.
#' @param w_min,band the peer threshold and the half-width of its band.
#' @return an \eqn{n x p} numeric matrix with values in \[0, 1\].
#' @keywords internal
.gloc_peer_rel <- function(obs, W = NULL, w_min = 0.5, band = .gloc_peer_band) {
  if (is.null(W)) return(obs + 0)
  Wf <- W
  Wf[!is.finite(Wf)] <- 0
  Rel <- if (!is.finite(band) || band <= 0) (Wf > w_min) + 0 else {
    tt <- pmin(pmax((Wf - (w_min - band)) / (2 * band), 0), 1)
    tt * tt * (3 - 2 * tt)
  }
  Rel[!obs] <- 0
  dim(Rel) <- dim(W)
  Rel
}

#' Standardised conditional residuals of each cell given the others in its row
#'
#' For each target column \code{j}, rows are grouped by which peer columns are
#' actually observed (finite) in that row, and the conditional mean and
#' conditional variance are computed from the submatrix of \code{Sigma} for
#' that observed peer set -- one matrix inversion per distinct missingness
#' pattern, not one per row. A row whose peers are all absent falls back to
#' the marginal distribution of column \code{j} (conditional mean 0,
#' conditional variance \code{Sigma[j, j]}), because a mean of 0 is only the
#' right no-information answer when it is paired with the no-information
#' (marginal) variance -- dividing it by the full-peer-set conditional
#' variance instead would understate the true spread and inflate the
#' standardised residual.
#'
#' When \code{W} is supplied, a peer counts as usable only if it is finite
#' \emph{and} its cell weight exceeds \code{w_min}: a downweighted peer is
#' treated exactly like an absent one and simply joins the unobserved set, so
#' the grouping machinery above handles it unchanged. Without this, a single
#' contaminated cell corrupts the conditional mean of every other cell in its
#' row and they are all flagged -- outlier propagation. Measured on data whose
#' only contamination was in \code{x1}, the flag rate for the clean peers
#' \code{x2} and \code{x3} in those same rows was 0.875 and 0.940, against
#' 0.017 in clean rows; excluding downweighted peers brings it back to the
#' clean-row rate. This also aligns the soft corner with
#' \code{cellWise::cellMCD}, which predicts a flagged cell from the clean cells
#' in the same row.
#'
#' @section The peer band:
#' Taken literally, "usable iff \eqn{w > w_{\min}}" makes the weight map
#' \emph{discontinuous}: a cell whose weight sits at the threshold switches its
#' row-mates' conditioning set on and off, and their standardised residuals
#' jump. A discontinuous self-map of \eqn{[0, 1]^{n \times p}} need not have a
#' fixed point at all (Brouwer needs continuity), so an iteration asked to
#' drive \eqn{\max|f(W) - W|} below a tolerance can be asked for something that
#' does not exist -- and in the categorical-mean-structure arm it routinely was:
#' one to twenty cells recrossed the threshold every few iterations forever,
#' each crossing kicking the fixed-point residual back up by 0.1 to 0.2 while
#' the rest of the system contracted geometrically.
#'
#' The threshold is therefore a \emph{band}, not a cut. A peer's reliability
#' \eqn{r} ramps smoothly (\eqn{3t^2 - 2t^3}) from 0 at \eqn{w_{\min} - h} to 1
#' at \eqn{w_{\min} + h}, and a partially reliable peer is conditioned on as
#' though it were observed with independent measurement error of variance
#' \eqn{\sigma_{kk}(1/r - 1)}: exact at \eqn{r = 1}, infinitely noisy and hence
#' absent at \eqn{r = 0}. Both endpoints reproduce the hard rule exactly, so
#' nothing changes for a cell that is confidently clean or confidently flagged;
#' only the ambiguous fringe is interpolated, and it is interpolated the way
#' the rest of the soft corner already treats partial information. The map
#' becomes continuous in \eqn{W}, a fixed point exists, and the convergence
#' test can stay exactly as strict as it was.
#'
#' \strong{What the band does to the estimate.} It is a fix for convergence and
#' nothing else, and it must not be described as an improvement in accuracy.
#' Measured over 520 paired fits with known ground truth -- band against no
#' band within the same build, varying correlation, contamination fraction,
#' shift and contamination type, so that only the band differs -- the scatter
#' moves by up to about 20\% per fit in \emph{either} direction (largest
#' relative changes -19.9\% and +20.7\%), and 22\% of fits move by more than
#' 1\% of their error. The direction is a coin flip: 48.5\% of fits move
#' towards the truth on the mean-structure arm, sign test p = 0.66, and among
#' the fits that move appreciably it is about 2 to 1 \emph{away}. What can be
#' said is that the mean effect is near zero (relative error 0.8401 against
#' 0.8389) and that detection is untouched (F1 0.4837 against 0.4842), so
#' convergence is not being bought at the cost of accuracy. No predictor of the
#' direction was found other than the arm itself. An earlier revision of this
#' page inferred "always towards the truth" from four hand-picked cases; that
#' was four points out of 520, and it was wrong.
#'
#' Implementation notes. (i) The noise-inflated system is solved in the scaled
#' form \eqn{\Sigma_{jS} D G^{-1} D} with \eqn{D = \mathrm{diag}(\sqrt r)} and
#' \eqn{G = D \Sigma_{SS} D + \mathrm{diag}(\sigma_{kk}(1 - r))}, which
#' interpolates between \eqn{\mathrm{diag}(\sigma_{kk})} at \eqn{r = 0} and
#' \eqn{\Sigma_{SS}} at \eqn{r = 1} and is well conditioned throughout -- the
#' unscaled \eqn{\Sigma_{SS} + \mathrm{diag}(\sigma_{kk}(1/r - 1))} blows up as
#' \eqn{r \to 0}. (ii) Rows whose peers are all at \eqn{r \in \{0, 1\}} keep
#' the pattern grouping and one inversion per pattern; only rows holding a peer
#' inside the band pay for an inversion of their own. With \eqn{h} small that
#' is a handful of rows, and the whole function is about 1\% of an iteration.
#' (iii) \code{band = 0} restores the hard cut bit-for-bit, which is how the
#' tests pin the endpoint behaviour.
#'
#' @param R \eqn{n x p} matrix of residuals from the mean structure.
#' @param Sigma \eqn{p x p} scatter matrix.
#' @param W optional \eqn{n x p} matrix of cell weights. \code{NULL} (default)
#'   conditions on every finite peer, which is the behaviour existing callers
#'   rely on.
#' @param w_min weight above which a peer counts as clean enough to condition
#'   on. The default 0.5 is the conventional 1% flagging rule: the Tukey
#'   bisquare weight at \eqn{|z| = 2.576} is 0.487, so "weight below 0.5" and
#'   "flagged at the 99% cut-off" coincide.
#' @param band half-width \eqn{h} of the reliability ramp around \code{w_min};
#'   see the section above. \code{0} is the hard cut. The default
#'   \code{.gloc_peer_band} is deliberately narrow.
#' @return an \eqn{n x p} matrix of standardised conditional residuals.
#' @keywords internal
.gloc_cond_resid <- function(R, Sigma, W = NULL, w_min = 0.5,
                             band = .gloc_peer_band) {
  n <- nrow(R); p <- ncol(R)
  Z <- matrix(NA_real_, n, p, dimnames = dimnames(R))
  if (p == 1L) return(R / sqrt(Sigma[1L, 1L]))
  obs <- is.finite(R)
  Rel <- .gloc_peer_rel(obs, W, w_min = w_min, band = band)
  sdiag <- diag(Sigma)
  for (j in seq_len(p)) {
    mj   <- setdiff(seq_len(p), j)
    Relj <- Rel[, mj, drop = FALSE]
    sharp <- rowSums(Relj > 0 & Relj < 1) == 0L

    # ---- peers are in or out: one inversion per distinct pattern, as before
    idx <- which(sharp)
    if (length(idx)) {
      pat <- Relj[idx, , drop = FALSE] > 0
      # string key rather than a 2^k dot product: downweighting multiplies the
      # number of distinct patterns, and the numeric code overflows past ~53 peers
      patcode <- do.call(paste, c(as.data.frame(pat + 0L), sep = ""))
      for (rows in split(idx, patcode)) {
        obs_idx <- mj[pat[match(rows[1L], idx), ]]
        if (length(obs_idx) == 0L) {              # no peer observed: marginal fallback
          mean_i <- 0
          cvar   <- Sigma[j, j]
        } else {
          Sinv <- tryCatch(chol2inv(chol(Sigma[obs_idx, obs_idx, drop = FALSE])),
                           error = function(e) MASS::ginv(Sigma[obs_idx, obs_idx, drop = FALSE]))
          beta   <- Sigma[j, obs_idx, drop = FALSE] %*% Sinv
          cvar   <- as.numeric(Sigma[j, j] - beta %*% Sigma[obs_idx, j, drop = FALSE])
          mean_i <- as.vector(R[rows, obs_idx, drop = FALSE] %*% t(beta))
        }
        cvar <- max(cvar, .Machine$double.eps)
        Z[rows, j] <- (R[rows, j] - mean_i) / sqrt(cvar)
      }
    }

    # ---- at least one peer inside the band: one solve for that row
    for (i in which(!sharp)) {
      r <- Relj[i, ]
      keep <- which(r > 0)
      if (!length(keep)) {
        Z[i, j] <- R[i, j] / sqrt(max(Sigma[j, j], .Machine$double.eps))
        next
      }
      kk <- mj[keep]; rk <- r[keep]; dd <- sqrt(rk)
      G  <- outer(dd, dd) * Sigma[kk, kk, drop = FALSE] +
              diag(sdiag[kk] * (1 - rk), length(kk))
      Gi <- tryCatch(chol2inv(chol(G)), error = function(e) MASS::ginv(G))
      bt <- as.vector((as.vector(Sigma[j, kk]) * dd) %*% Gi)
      cvar <- max(Sigma[j, j] - sum(bt * (dd * Sigma[kk, j])), .Machine$double.eps)
      Z[i, j] <- (R[i, j] - sum(bt * (dd * R[i, kk]))) / sqrt(cvar)
    }
  }
  Z
}

#' Fixed random-number state for the robust start
#'
#' A valid Mersenne-Twister \code{.Random.seed} vector, built by a linear
#' congruential recursion so that constructing it draws no random numbers.
#' \code{robustbase::lmrob} installs it for the subsampling in its S-step and
#' restores the caller's stream afterwards, which makes the robust start
#' deterministic and keeps it from desynchronising paired simulation arms.
#' @keywords internal
.gloc_start_seed <- local({
  s <- numeric(624L)
  v <- 20260915
  for (i in seq_len(624L)) {
    v <- (v * 69069 + 1) %% 4294967296
    s[i] <- if (v >= 2147483648) v - 4294967296 else v
  }
  s[s == -2147483648] <- 1                  # not representable as an integer
  c(10403L, 624L, as.integer(s))
})

#' cellMCD tolerance used by the robust start
#'
#' \code{cellWise::cellMCD} refuses any column whose marginal outliers plus
#' missing values exceed \eqn{1 - \alpha} of its cells. At the user-facing
#' default \eqn{\alpha = 0.75} that is 25\%, which 20\% missingness plus a few
#' percent of shifted cells already exceeds: in the 7.4.1 pilot (n = 200, six
#' continuous columns, 20\% missing) the robust start fell back to its
#' MAD-threshold flags in 74 of 360 fits, all of them at \eqn{\epsilon \ge 0.10}
#' with shifts of 6 or 10, where the start matters most, and in none at
#' \eqn{\alpha = 0.5}. The start therefore runs cellMCD at this value, and the
#' \code{alpha} argument of \code{imputeCellGLoc} keeps governing the binary
#' corner only.
#' @keywords internal
.gloc_start_alpha <- 0.5

#' Robust starting values for the cellGLoc soft corner
#'
#' Until 7.4.0 the soft corner started from a classical fit: every observed
#' cell at weight 1 and \eqn{B} by ordinary least squares. A redescending weight
#' function started there can settle on a masked solution. This start fits each
#' continuous column by MM regression (bisquare) on the categorical design
#' \emph{alone}. The predictors are dummies, which cannot carry a contaminated
#' continuous cell, so the casewise robustness of MM is exactly what is needed:
#' a contaminated cell is an outlying response. The starting flags are then
#' those of \code{cellWise::cellMCD} on the residuals \eqn{X - U B}, at
#' \code{.gloc_start_alpha}.
#'
#' The MM fit starts from the L1 regression (\code{robustbase::lmrob.lar})
#' followed by the M-step (\code{method = "lM"}). That is exactly what
#' \code{robustbase::lmrob(..., init = "M-S")} does for a design with no
#' continuous predictor: its coefficients agreed to 0 over 960 column fits.
#' \code{lmrob}'s default S-estimator start is not used first because it does
#' not converge on many purely categorical designs ("S refinements did not
#' converge in k.max steps", "initial estim. 'init' not converged"): in the
#' 7.4.1 pilot some column failed in 42 of 90 fits with \code{design = ~ .},
#' as often at \eqn{\epsilon = 0} as under contamination, and on 960 column
#' fits of 200 rows on four factors it failed 54 times, where the L1 start
#' converged every time. The two agree in accuracy (median fitted-mean error
#' against the truth 0.243 and 0.237) and the L1 start needs no subsampling. The
#' S start remains the fallback when the L1-started fit does not converge.
#'
#' Every degraded path warns, once per reason, naming the columns:
#' too few observed rows for the design (fewer than \eqn{2q}, or a design column
#' with fewer than three observed rows), an \code{lmrob} error, or \code{lmrob}
#' not converging from either start each fall back to the column median as
#' intercept with zero contrasts; without \code{cellWise}, or if \code{cellMCD}
#' fails, a cell is flagged when \eqn{|r_{ij}| / \mathrm{MAD}(r_{.j})} exceeds
#' \eqn{\sqrt{\chi^2_{1,0.99}}}.
#'
#' @param X \eqn{n x p} numeric matrix of continuous variables, may contain NA.
#' @param U \eqn{n x q} design matrix from \code{.gloc_design}.
#' @param M \eqn{n x p} logical missingness mask.
#' @param alpha minimum fraction of unflagged cells per column for
#'   \code{cellWise::cellMCD}; see \code{.gloc_start_alpha}.
#' @param have_cw whether \code{cellWise} is available.
#' @param control an \code{robustbase::lmrob.control} list; \code{NULL} uses the
#'   defaults. An empty seed (the default) is replaced by
#'   \code{.gloc_start_seed}. Its \code{method} is set to \code{"lM"} for the
#'   L1-started fit and left as given for the S-started fallback.
#' @return a list with \code{B} (\eqn{q x p}) and \code{W} (\eqn{n x p}, 0 or 1,
#'   0 on missing cells).
#' @keywords internal
.gloc_start_robust <- function(X, U, M, alpha = .gloc_start_alpha,
                               have_cw = requireNamespace("cellWise",
                                                          quietly = TRUE),
                               control = NULL) {
  n <- nrow(X); p <- ncol(X); q <- ncol(U)
  cnames <- if (is.null(colnames(X))) as.character(seq_len(p)) else colnames(X)
  B <- matrix(0, q, p, dimnames = list(colnames(U), colnames(X)))
  if (is.null(control)) control <- robustbase::lmrob.control()
  # lmrob.control() returns seed = integer(0), not NULL, so test the length.
  # Testing is.null() left the seed unset and let lmrob draw from, and advance,
  # the caller's random-number stream.
  if (is.list(control) && !length(control$seed)) control$seed <- .gloc_start_seed
  quiet <- function(expr)
    tryCatch(withCallingHandlers(expr,
                                 warning = function(w) invokeRestart("muffleWarning")),
             error = function(e) NULL)
  usable <- function(f) !is.null(f) && !anyNA(f$coefficients)

  few <- failed <- noconv <- character(0)
  for (j in seq_len(p)) {
    ok <- !M[, j] & is.finite(X[, j])
    Uo <- U[ok, , drop = FALSE]
    thin <- sum(ok) < 2L * q ||
      (q > 1L && min(colSums(Uo[, -1L, drop = FALSE] != 0)) < 3L)
    fit <- NULL
    if (thin) {
      few <- c(few, cnames[j])
    } else {
      # Centre the response by its median first and put the median back into
      # the intercept afterwards (column 1 of U). lmrob's stopping rules are
      # relative to the size of the coefficients, so on data shifted by +1000
      # the uncentred fit stopped earlier: its intercept moved by up to 3.3e-4
      # and the final weights by 8e-6, breaking the shift equivariance the
      # convergence test pins at 1e-6.
      yj <- X[ok, j]
      mj <- stats::median(yj)
      yc <- yj - mj
      # L1 start, then the M-step: robustbase's own M-S path for a design with
      # no continuous predictor. bare.only skips the covariance, which is not
      # used here and whose computation only warns after a non-S start.
      fit <- quiet({
        ctrl_l <- control; ctrl_l$method <- "lM"
        robustbase::lmrob.fit(Uo, yc, control = ctrl_l, bare.only = TRUE,
                              init = robustbase::lmrob.lar(Uo, yc,
                                                           control = ctrl_l))
      })
      if (!(usable(fit) && isTRUE(fit$converged))) {
        # fallback: lmrob's default S-estimator start
        fit_s <- quiet(robustbase::lmrob.fit(Uo, yc, control = control,
                                             bare.only = TRUE))
        if (usable(fit_s) && isTRUE(fit_s$converged)) {
          fit <- fit_s
        } else if (usable(fit) || usable(fit_s)) {
          noconv <- c(noconv, cnames[j]); fit <- NULL
        } else {
          failed <- c(failed, cnames[j]); fit <- NULL
        }
      }
    }
    if (is.null(fit)) {
      B[1L, j] <- if (any(ok)) stats::median(X[ok, j]) else 0
    } else {
      B[, j] <- fit$coefficients
      B[1L, j] <- B[1L, j] + mj          # undo the centring
    }
  }
  fallback_msg <- "using the column median as intercept with zero contrasts."
  if (length(few))
    warning(sprintf(paste("cellGLoc: robust start: too few observed rows for",
                          "the design in column(s) %s (fewer than %d rows, or a",
                          "design column with fewer than 3); %s"),
                    paste(few, collapse = ", "), 2L * q, fallback_msg),
            call. = FALSE)
  if (length(failed))
    warning(sprintf("cellGLoc: robust start: robustbase::lmrob failed for column(s) %s; %s",
                    paste(failed, collapse = ", "), fallback_msg), call. = FALSE)
  if (length(noconv))
    warning(sprintf(paste("cellGLoc: robust start: robustbase::lmrob did not",
                          "converge for column(s) %s; %s"),
                    paste(noconv, collapse = ", "), fallback_msg), call. = FALSE)

  R <- X - U %*% B
  R[M | !is.finite(X)] <- NA_real_
  W <- NULL
  if (have_cw) {
    cm_err <- NULL
    # cellMCD stops with "mean(): object has no elements" when any row has no
    # observed cell, at every alpha. Such a row has nothing to flag (its cells
    # get weight 0 below), so it is left out of the call.
    has_obs <- rowSums(is.finite(R)) > 0
    cm <- tryCatch(cellWise::cellMCD(R[has_obs, , drop = FALSE], alpha = alpha,
                                     checkPars = list(coreOnly = TRUE,
                                                      silent = TRUE)),
                   error = function(e) { cm_err <<- conditionMessage(e); NULL })
    if (is.null(cm)) {
      warning(sprintf(paste("cellGLoc: robust start: cellWise::cellMCD() failed",
                            "(%s); the starting flags use a hard threshold on",
                            "|residual| / MAD instead."),
                      gsub("\\s+", " ", trimws(cm_err))), call. = FALSE)
    } else {
      W <- matrix(1, n, p)
      W[has_obs, ] <- as.numeric(cm$W)
    }
  } else {
    warning(paste("cellGLoc: robust start: the cellWise package is not",
                  "installed, so the starting flags use a hard threshold on",
                  "|residual| / MAD instead of cellMCD."), call. = FALSE)
  }
  if (is.null(W)) {
    thr <- sqrt(stats::qchisq(0.99, df = 1))
    W <- matrix(1, n, p)
    for (j in seq_len(p)) {
      s <- stats::mad(R[, j], na.rm = TRUE)
      if (is.finite(s) && s > 0)
        W[, j] <- as.numeric(!(is.finite(R[, j]) & abs(R[, j]) / s > thr))
    }
  }
  W[M | !is.finite(X)] <- 0
  dimnames(W) <- dimnames(X)
  list(B = B, W = W)
}
