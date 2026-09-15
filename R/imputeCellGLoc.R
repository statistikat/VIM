#' Cellwise-robust estimation on a categorical mean structure
#'
#' Estimates the mean structure \eqn{B}, scatter \eqn{\Sigma} and cell weights
#' \eqn{W} of the model \eqn{x_i | u_i \sim N(B' u_i, \Sigma)}, where \eqn{u_i}
#' is a design row built from the categorical variables. Detection uses the
#' conditional residual of a cell given the other continuous cells in its row
#' \emph{and} the row's categorical pattern.
#'
#' With \code{design = ~ 1} the estimator reduces to the published continuous-only
#' estimators: to the cellwise MCD of Raymaekers and Rousseeuw (2024) with
#' \code{weights = "binary"}, and to the cellwise weighted maximum likelihood
#' estimator of Rousseeuw (2026) with \code{weights = "soft"}.
#'
#' The soft corner rescales the weighted scatter so that it is
#' Fisher-consistent at the Gaussian model. Without it the bisquare
#' downweighting deflates the scatter by about 21% at the default tuning,
#' which would inflate the standardised residuals and make the estimator
#' over-flag. The correction is per column and depends on \eqn{\Sigma}, not a
#' single constant: the weights act on \emph{conditional} residuals, so they
#' shrink only the unpredictable part of each cell. See
#' \code{.gloc_correct_scatter}. It is exact for the scale at any correlation
#' and vanishes at \code{psi_c = Inf}. The correlations themselves stay mildly
#' biased upward; see that function's note.
#'
#' The estimator is the triple \eqn{(B, \Sigma, W)} and iteration stops only
#' when all three have settled:
#' \eqn{\max|U(B - B_{old})| / \sqrt{\max \mathrm{diag}(\Sigma)} < eps},
#' \eqn{\max|\Sigma - \Sigma_{old}| / \max \mathrm{diag}(\Sigma) < eps} and
#' \eqn{\max|W - W_{old}| / damp < eps}. The first term is a change in the
#' fitted values measured in units of the scatter, not a change in a
#' coefficient measured against a location, so the criterion is invariant to
#' shifting the data; the second is relative, so it is invariant to rescaling
#' it; the third is the weight map's fixed-point residual, relaxation divided
#' out. \code{converged = TRUE} therefore means that nothing this function
#' returns is still moving, which is the only reading of convergence a
#' detection method can defend: \eqn{W} is not a nuisance quantity here, it is
#' the flag set the caller uses.
#'
#' The scatter condition is a formal guard rather than an active one. Across
#' every fit measured so far it has never been the binding constraint: at the
#' iteration where the other two conditions are first met it stands at most a
#' seventh of its own tolerance. It is there because \eqn{\Sigma} is returned
#' and was the one returned quantity the rule did not test, and because it
#' costs nothing; it is not there because it was observed to catch anything.
#'
#' The soft corner's weight update is relaxed (see \code{.gloc_damp}) to keep
#' the fixed-point iteration inside its contraction radius, and the factor is
#' adapted rather than fixed; see \code{damp}. Relaxation alone does not make
#' the criterion attainable: conditioning on a peer only when its weight
#' exceeds a threshold makes the weight map discontinuous, and a discontinuous
#' self-map of \eqn{[0,1]^{n \times p}} need not have a fixed point at all, so
#' before 7.4.0 the iteration could be asked to reach a state that did not
#' exist -- and in the \code{design = ~ .} arm it usually was. The peer band
#' (see \code{.gloc_peer_band}) removes that, and the two work together rather
#' than one of them doing the work: over 260 fits of the
#' categorical-mean-structure arm, \code{converged} is 131 under the 7.3.1
#' schedule and hard cut, 181 with the relaxation floor at 0.5 and the scatter
#' condition but the cut still hard, and 245 with the band as well. About 40%
#' of the gain is the floor, the rest the band.
#'
#' Convergence is not universal and should not be assumed. On that 260-fit grid
#' it is 245, or 94%, with the failures concentrated at correlations of 0.6
#' and above combined with 20% of cells contaminated.
#'
#' All convergence figures in this section were measured before 7.4.1, with
#' the classical cold start (\code{start = "classical"}); the robust start has
#' not been measured on that grid.
#'
#' Continuous columns that are \code{integer} in \code{data} stay
#' \code{integer} in \code{$imputed}; their conditional expectations are
#' rounded.
#'
#' A missing continuous cell is imputed by its conditional expectation given
#' the unflagged cells in its row, selected by the same peer rule detection
#' uses (\code{peer_w_min} with the peer band), in both weight corners. Until
#' 7.4.0 the imputation conditioned on every observed cell, flagged ones
#' included, so a grossly contaminated cell was carried into the imputation of
#' its row-mates; \code{B}, \code{Sigma} and \code{W} were not affected. See
#' \code{.gloc_impute}.
#'
#' @param data a \code{data.frame} with continuous and categorical columns.
#' @param design one-sided formula for the categorical mean structure.
#'   \code{~ .} (default) is main effects over all categorical columns,
#'   \code{~ .^2} adds interactions, \code{~ 1} is intercept only.
#' @param weights \code{"soft"} for redescending weights in \[0, 1\],
#'   \code{"binary"} for the penalised cellwise MCD objective.
#' @param maxit maximum number of outer iterations. \code{0} returns the
#'   starting fit. The default is 200 rather than 50 because the relaxed
#'   iteration needed up to 51 steps across the sweep in \code{.gloc_damp};
#'   converged fits leave the loop early, so the cap costs nothing. Failing to
#'   converge within \code{maxit} warns, and the warning says whether the
#'   limit was merely exhausted or the iteration is cycling. Note that
#'   \code{maxit} changes the \emph{trajectory} and not only the stopping
#'   point, because the fallback in \code{.gloc_damp} fires on a stall counter
#'   and what is left of the budget then decides whether the restarted run
#'   finishes: truncating a non-converged fit early is not the same answer
#'   sooner, and measured deviations are non-monotone in \code{maxit}.
#' @param eps convergence tolerance, applied to the scaled change in the
#'   fitted means, to the relative change in the scatter, and to the cell
#'   weights' fixed-point residual \eqn{\max|f(W) - W|}. The weight step is
#'   divided by \code{damp} before the comparison, because the step itself is
#'   \code{damp} times that residual: without the division a relaxed run would
#'   stop at a proportionally looser residual than an unrelaxed one, and
#'   \code{eps} would not mean the same thing at two relaxation factors. The
#'   mean and scatter terms are not divided, since neither is exactly
#'   \code{damp} times a fixed-point residual; the weight term is in practice
#'   the binding one.
#' @param alpha minimum fraction of unflagged cells per column (binary corner).
#'   The robust start of the soft corner runs cellMCD at its own tolerance,
#'   \code{.gloc_start_alpha} = 0.5, and ignores this argument.
#' @param psi_c tuning constant of the Tukey bisquare (soft corner).
#'   \code{Inf} disables downweighting.
#' @param damp relaxation factor for the soft corner's weight update.
#'   \code{NULL} (default) runs the adaptive schedule of
#'   \code{.gloc_damp_start} / \code{.gloc_damp_shrink} / \code{.gloc_damp}:
#'   it starts unrelaxed, strengthens only when the weight change stops
#'   falling, and falls back once to the cold start at the floor if it is
#'   still cycling there. A number in (0, 1] pins the factor instead, with no
#'   backoff and no fallback; \code{damp = 0.25} is the relaxation factor used
#'   by releases before 7.4.0 and is what the tests compare the schedule
#'   against, the floor having since risen to 0.5 because the band removed the
#'   cycling the low floor was there to suppress.
#'   It is not bit-identical to those releases, because they also
#'   compared the undivided weight step against \code{eps} and so stopped
#'   four times earlier than \code{eps} asked; see \code{eps}. Ignored for
#'   \code{weights = "binary"}, which takes \eqn{W} from
#'   \code{cellWise::cellMCD} and never relaxes it.
#' @param cw_crit convergence tolerance of the EM inside
#'   \code{cellWise::cwLocScat}, the scatter step. That step is about 95% of an
#'   outer iteration, and \code{cwLocScat}'s own default of 1e-12 is seven
#'   orders of magnitude tighter than \code{eps}, so it refines digits this
#'   function immediately discards. See \code{.gloc_scatter_soft}.
#' @param start starting values for the soft corner. \code{"robust"} (default
#'   since 7.4.1) fits each continuous column on the categorical design alone
#'   along \code{robustbase::lmrob}'s M-S path (an L1 fit followed by an M-step
#'   at the L1 residual scale) and takes the starting flags from
#'   \code{cellWise::cellMCD} on those residuals at \code{.gloc_start_alpha};
#'   see \code{.gloc_start_robust}. \code{"classical"} starts with every
#'   observed cell at weight 1 and the mean structure by least squares. A
#'   redescending weight function started from a non-robust fit can settle on
#'   a masked solution, which is why the default changed. Ignored for
#'   \code{weights = "binary"}, whose first step already calls
#'   \code{cellWise::cellMCD}.
#'
#'   \code{start} selects the fixed point, not only the path to it: the
#'   iteration can have several fixed points, and the starting values choose
#'   among them. On clean data (n = 200, six continuous and six categorical
#'   variables, 20% missing) at \code{eps = 1e-8}, the two starts reached
#'   different fixed points in 7 of 10 fits with \code{design = ~ .} (relative
#'   scatter difference 0.009 to 0.032, 2 to 14 cells flagged differently,
#'   unchanged as the tolerance tightens) and in 4 of 9 converged fits with
#'   \code{design = ~ 1}. Which fixed point is reached depends on the starting
#'   mean and the starting weights together, and on how the weights are built:
#'   in the 7 differing \code{~ .} fits, the start's hard flags with a
#'   least-squares starting mean reached the robust start's fixed point in 4;
#'   soft starting weights on the robust residuals reached it in 6 when computed
#'   from conditional residuals and in none when computed from marginal ones.
#'   The conditional construction used the start's hard flags twice: as the
#'   cell weights of the scatter it conditioned with, and as the set of peers
#'   it conditioned on.
#'   (Corrected: an earlier version said the hard starting flags were not the
#'   cause, on the evidence of one construction of soft starting weights.) Under
#'   contamination the classical start can mask: with 20% of cells shifted by 10
#'   its scatter error was 6.45 against 0.14 for the robust start.
#'
#'   With \code{"classical"} the estimates \code{B}, \code{Sigma} and \code{W}
#'   reproduce VIM 7.4.0; the imputations do not, because since 7.4.1 a missing
#'   cell is imputed from unflagged cells only. (Corrected: this page said
#'   \code{"classical"} "reproduces VIM 7.4.0 exactly", which stopped being true
#'   for \code{$imputed} under both starts.)
#' @param peer_w_min a cell is conditioned on only when its weight exceeds
#'   this, so that a downweighted peer is treated as absent rather than as
#'   evidence. The threshold is applied over a narrow band rather than at a
#'   point -- a peer whose weight is within \code{.gloc_peer_band} of it is
#'   conditioned on with its information discounted, which is what keeps the
#'   weight map continuous; see \code{.gloc_cond_resid}. Cells outside the band
#'   are unaffected, so the meaning of this argument is unchanged.
#'   The default 0.5 is the conventional 1% flagging rule. Raising
#'   it discards more peers. A threshold of 0 does \emph{not} disable peer
#'   filtering: the damped weight update multiplies a weight by
#'   \eqn{1 - d} each time the bisquare sends it to zero, so a contaminated
#'   weight decays geometrically towards zero without ever attaining it
#'   (measured around 4e-6 at convergence, with no cell exactly 0), and a
#'   zero threshold readmits those cells at full influence. Use a negative
#'   value to condition on every finite peer, which is useful only for
#'   demonstrating what the filtering buys. The same rule selects the cells a
#'   missing cell is imputed from (since 7.4.1).
#' @param peer_band half-width of the band around \code{peer_w_min} over which
#'   a peer fades out of the conditioning set instead of leaving it at a step;
#'   see \code{.gloc_peer_band} for the value and \code{.gloc_cond_resid} for
#'   the construction. \code{0} restores the hard cut, which is useful only for
#'   demonstrating what the band buys: it makes the weight map discontinuous
#'   again, and with it the limit cycles that made \code{converged = FALSE} the
#'   usual outcome of a \code{design = ~ .} fit. It does \emph{not} reproduce a
#'   release from before 7.4.0, because the relaxation floor moved at the same
#'   time and the two interact: measured on 260 pooled fits, the hard cut
#'   converges 208 times at the current floor of 0.5 against 245 at the old
#'   0.25. A low floor helps under the hard cut and merely costs iterations
#'   under the band.
#'
#'   Setting it changes the estimator and not only the iteration. Measured over
#'   520 paired fits against \code{peer_band = 0} on the same data, the scatter
#'   moves by up to about 20% either way (the largest relative Frobenius
#'   changes seen were -19.9% and +20.7%, and 22% of fits move by more than
#'   1%). The mean effect is near zero -- 0.8401 against 0.8389 in relative
#'   error against a known truth -- the direction is not predictable from
#'   anything but the design arm itself, and detection is unaffected (F1 0.4837
#'   against 0.4842). The band is a fix for convergence, not for accuracy, and
#'   is not claimed to improve the estimate.
#' @param trace print progress.
#' @return a list with \code{B}, \code{Sigma}, \code{W}, \code{U},
#'   \code{imputed}, \code{converged}, \code{iterations} and \code{criterion}.
#'
#'   \code{criterion} is the named vector \code{(means, scatter, weights,
#'   scatter_spread)}: the three stopping residuals as of the last iteration,
#'   compared against \code{eps}, plus the elementwise spread of \eqn{\Sigma}
#'   over the last \code{.gloc_stall_iters} iterations relative to its largest
#'   variance. \code{scatter_spread} is reported only when \code{converged} is
#'   \code{FALSE}, and is \code{NA} otherwise: on a converged fit the window
#'   still holds the last steps of the approach, so a value there would say
#'   nothing about stability. When the fit did not converge it says how much
#'   the returned scatter depends on where \code{maxit} happened to stop -- a
#'   settled cycle reports its amplitude, a run still drifting reports the
#'   drift -- and a caller can test it instead of parsing a warning string.
#'
#'   It does not tell those two apart, and it is not an error estimate. On 10
#'   non-converged \code{weights = "binary"} fits it was positive for all 10,
#'   while raising \code{maxit} left 8 of them bit-identical and moved 2 (by
#'   0.008 and 0.096 relative, the latter matching its reported spread
#'   exactly). That is the intended behaviour -- all 10 return a scatter that
#'   depends on the stopping point -- but the rank correlation with what
#'   raising \code{maxit} actually does is only 0.16, so read it as "this
#'   answer is not settled", never as "it would move by this much".
#'   Every entry is \code{NA} at \code{maxit = 0}.
#' @references
#' Raymaekers, J. and Rousseeuw, P. J. (2024). The cellwise minimum covariance
#' determinant estimator. \emph{JASA} 119(548), 2610-2621.
#' \doi{10.1080/01621459.2023.2267777}
#'
#' Rousseeuw, P. J. (2026). Analyzing cellwise weighted data.
#' \emph{Econometrics and Statistics} 38, 31-41.
#' \doi{10.1016/j.ecosta.2023.01.007}
#' @export
imputeCellGLoc <- function(data, design = ~ ., weights = c("soft", "binary"),
                           maxit = 200, eps = 5e-3, alpha = 0.75,
                           psi_c = 4.685, peer_w_min = 0.5,
                           peer_band = .gloc_peer_band, damp = NULL,
                           cw_crit = 1e-8, trace = FALSE,
                           start = c("robust", "classical"),
                           categorical = c("em", "level")) {
  weights <- match.arg(weights)
  start <- match.arg(start)
  categorical <- match.arg(categorical)
  # cellWise::cellMCD (with DDC and estLocScale inside it) and robustbase's
  # S-estimator create .Random.seed in a session that has none. A session with
  # none must still have none afterwards, or two fresh sessions draw the same
  # "random" numbers after one call. An existing stream is left alone. The
  # data promise is forced first: a seed that evaluating the caller's own
  # argument creates, as in imputeCellGLoc({ set.seed(1); d }), is the
  # caller's and must survive.
  force(data)
  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    on.exit(if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
              rm(".Random.seed", envir = globalenv()), add = TRUE)
  stopifnot(is.data.frame(data))
  adaptive <- is.null(damp)
  if (adaptive) damp <- .gloc_damp_start
  # Only the soft corner relaxes its weight update. The binary corner takes W
  # straight from cellMCD, so there is no relaxation factor to adapt and none
  # to divide out of the convergence test below; pinning it at 1 keeps both
  # off.
  relax <- identical(weights, "soft")
  stopifnot(is.numeric(damp), length(damp) == 1L, is.finite(damp),
            damp > 0, damp <= 1,
            is.numeric(cw_crit), length(cw_crit) == 1L, cw_crit > 0,
            is.numeric(peer_band), length(peer_band) == 1L,
            is.finite(peer_band), peer_band >= 0)
  if (!relax) { adaptive <- FALSE; damp <- 1 }
  sv <- .gloc_split_vars(data)
  cont_vars <- sv$cont
  cat_vars  <- sv$cat
  if (!length(cont_vars))
    stop("imputeCellGLoc() needs at least one continuous variable.")
  # The categorical EM runs only when a categorical cell is missing. Without
  # one this function takes the 7.4.1 path unchanged, which is what keeps
  # categorical = "em" and "level" bit-identical there. Under "level" the
  # preparation does not run at all, so that path does no work the EM added:
  # every later read of catp is under categorical = "em", directly or through em.
  catp <- if (identical(categorical, "em")) .gloc_cat_prepare(data, cat_vars) else NULL
  em <- !is.null(catp) && any(catp$Mc)

  X <- as.matrix(data[, cont_vars, drop = FALSE])
  storage.mode(X) <- "double"
  if (!em) {
    U <- .gloc_design(data, design, cat_vars)
    # The main-effects design and the level combinations, used only for
    # combinations that the rows a variable is fitted from do not identify.
    aux <- .gloc_design_aux(data, design, cat_vars)
    U_main <- if (is.null(aux$U_main)) U else aux$U_main
    pats <- aux$patterns
    if (!is.null(pats) && !identical(colnames(pats$P), colnames(U))) pats <- NULL
    if (!is.null(pats$P_main) && !identical(colnames(pats$P_main), colnames(U_main)))
      pats$P_main <- NULL
  }
  n <- nrow(X); p <- ncol(X)

  # Inf / NaN are treated as missing and imputed, which is a real decision
  # about the user's data, so say so rather than doing it silently.
  odd <- !is.finite(X) & !is.na(X)
  if (any(odd)) {
    warning(sprintf(paste("cellGLoc: %d non-finite value(s) that are not NA",
                          "(Inf, -Inf or NaN) were treated as missing and will",
                          "be imputed; check whether that is intended."),
                    sum(odd)), call. = FALSE)
    X[odd] <- NA_real_
  }

  # A degraded path must never be taken silently, but neither should it shout
  # once per iteration: report each distinct reason exactly once per call. The
  # mean step warns about a rank-deficient design on every call, from the
  # classical start below onwards, so the handler is set up before it.
  seen <- character(0)
  dedup <- function(w) {
    m <- conditionMessage(w)
    if (startsWith(m, "cellGLoc: ")) {
      if (m %in% seen) invokeRestart("muffleWarning") else seen <<- c(seen, m)
    }
  }

  M <- !is.finite(X)                       # missing mask
  if (em) {
    cc <- rowSums(catp$Mc) == 0L
    if (!any(cc))
      stop(paste("imputeCellGLoc(): categorical = \"em\" needs at least one row",
                 "with every categorical variable observed; use",
                 "categorical = \"level\"."))
    withCallingHandlers({
      cand   <- .gloc_cat_candidates(catp, data, design)
      priors <- .gloc_cat_fit_priors(catp$F[cc, , drop = FALSE], rep(1, sum(cc)),
                                     catp$levels)
      es     <- .gloc_cat_estep(X, M, NULL, NULL, NULL, catp, cand, priors,
                                w_min = peer_w_min, band = peer_band)
    }, warning = dedup)
    U <- es$Ubar                                     # expected design rows
    U_main <- if (is.null(es$Umain_bar)) U else es$Umain_bar
    pats <- cand$pats_rows                           # NA id where a category is unknown
    priors0 <- priors; es0 <- es
  }
  post_old <- NULL
  W <- matrix(1, n, p, dimnames = dimnames(X))
  W[M] <- 0
  # Design warnings are collected rather than raised, and reported once from
  # the final iterate: the unidentified set can change between iterations.
  W_fit <- W
  if (em) W_fit[!cc, ] <- 0                        # fit on complete-category rows
  B <- withCallingHandlers(.gloc_update_B(X, U, W_fit, U_main, pats, warn = FALSE),
                           warning = dedup)
  design_diag <- attr(B, "gloc_design")
  attr(B, "gloc_design") <- NULL
  Sigma <- NULL
  converged <- FALSE
  iter_count <- 0L
  dW_prev <- Inf          # previous iteration's weight change, for the backoff
  dW_best <- Inf          # best so far, for stall detection
  stall   <- 0L           # iterations since the weight change last improved
  restarted <- FALSE      # has the floor-schedule fallback already been used?
  W0 <- W; B0 <- B        # the cold start, kept for that fallback
  dB <- dS <- dW <- NA_real_       # stopping residuals, reported in $criterion
  dR <- 0      # categorical posterior change; stays 0 without the EM
  # Ring buffer of the last .gloc_stall_iters scatters. A non-converged fit
  # returns whatever the last iteration produced, and from the returned Sigma
  # alone the caller cannot tell a point on a settled cycle -- where the answer
  # depends on which phase maxit stopped in -- from a value still drifting.
  # Measured on the binary corner, 8 of 10 non-converged fits are bit-identical
  # at a raised maxit and 2 move, by 0.008 and 0.096 relative (the first was
  # written as 0.011 here until 2026-09-12; the measurement is 0.008, as the
  # roxygen and NEWS both say). The trailing
  # spread measures that dependence directly and is returned, so it can be
  # tested by a caller rather than only read out of a warning string.
  S_hist <- vector("list", .gloc_stall_iters)

  kappa_soft <- .gloc_consistency(psi_c, "bisquare")
  hard_q     <- sqrt(stats::qchisq(0.99, df = 1))
  kappa_hard <- .gloc_consistency(hard_q, "hard")

  # The robust start replaces the classical one computed above, and it also
  # becomes the cold start the relaxation schedule falls back to. The first
  # iteration recomputes Sigma from (X - U B, W), so no starting scatter is
  # needed. The binary corner keeps its start: its first step already calls
  # cellWise::cellMCD. With start = "classical" the estimates B, Sigma and W
  # reproduce 7.4.0; $imputed does not, under either start, because a missing
  # cell is now imputed from unflagged cells only. (Corrected: this comment said
  # start = "classical" "reproduces 7.4.0 exactly".)
  # The start's cellMCD runs at .gloc_start_alpha, not at the user's alpha,
  # which governs the binary corner only.
  # warn_design = FALSE: a rank-deficient design is reported once, by the mean
  # step, which also says what the returned fit does with it.
  if (relax && start == "robust") {
    st <- withCallingHandlers(.gloc_start_robust(X, U, M, warn_design = FALSE,
                                                 U_main = U_main, patterns = pats,
                                                 fit_rows = if (em) cc else NULL),
                              warning = dedup)
    W <- st$W; B <- st$B
    W0 <- W; B0 <- B
  }

  withCallingHandlers({
    for (it in seq_len(maxit)) {
      iter_count <- it
      B_old <- B; W_old <- W; Sigma_old <- Sigma
      R <- X - U %*% B

      if (weights == "binary") {
        if (!requireNamespace("cellWise", quietly = TRUE))
          stop('weights = "binary" requires the cellWise package.')
        Rf <- R; Rf[M] <- NA_real_
        # cellMCD keeps its own (free) centre here on purpose. That centre is a
        # nuisance parameter, profiled out inside cellMCD and discarded: B alone
        # carries the mean structure. Passing fixedCenter = TRUE would instead
        # switch cellMCD's *preliminary* standardisation to
        # estLocScale(., center = FALSE), and since cellMCD rebuilds its scatter
        # as diag(rscales) %*% cov2cor(.) %*% diag(rscales), that preliminary
        # scale becomes sqrt(diag(S)) verbatim. Residuals that are not exactly
        # centred would then inflate the scale, and the reduction to cellMCD
        # would only hold to about 6% instead of to machine precision.
        fit <- tryCatch(cellWise::cellMCD(Rf, alpha = alpha,
                                          checkPars = list(coreOnly = TRUE,
                                                           silent = TRUE)),
                        error = function(e) NULL)
        if (is.null(fit)) {
          warning(paste("cellGLoc: cellWise::cellMCD() failed; falling back to",
                        "a weighted covariance with a hard threshold on the",
                        "conditional residuals. This is a cruder estimator than",
                        "the cellwise MCD and the result is not the published",
                        "cellMCD."), call. = FALSE)
          Sigma <- .gloc_scatter_soft(R, W, M, crit = cw_crit)  # working scatter
          Z <- .gloc_cond_resid(R, Sigma, W = W, w_min = peer_w_min,
                                band = peer_band)
          # recompute W rather than carrying the previous iteration's stale one
          W <- matrix(as.numeric(is.finite(Z) & abs(Z) <= hard_q), n, p,
                      dimnames = dimnames(X))
          W[M] <- 0
          Sigma <- .gloc_scatter_soft(R, W, M, kappa = kappa_hard,
                                      crit = cw_crit)
        } else {
          Sigma <- fit$S; W <- fit$W
        }
      } else {
        # Under the EM the scatter is taken over the pseudo-rows: a row with a
        # missing category enters once per level, its cell weights scaled by the
        # level's posterior, which cwLocScat treats as a case weight.
        Sigma <- if (em)
          .gloc_scatter_soft(X[es$pr_row, , drop = FALSE] - es$Up %*% B,
                             W[es$pr_row, , drop = FALSE] * es$pr_w,
                             M[es$pr_row, , drop = FALSE],
                             kappa = kappa_soft, crit = cw_crit)
        else .gloc_scatter_soft(R, W, M, kappa = kappa_soft, crit = cw_crit)
        # Condition each cell only on peers that are themselves still clean.
        # Conditioning on every finite peer propagates a single bad cell to its
        # whole row: contaminating only x1 flagged 88-94% of the clean x2 and
        # x3 cells in those same rows, against 1.7% in clean rows. cellMCD does
        # not have this problem because a flagged cell leaves the conditioning
        # set, and the reduction claim requires the soft corner to match.
        Z <- .gloc_cond_resid(R, Sigma, W = W, w_min = peer_w_min,
                              band = peer_band)
        # Relaxed weight update. Relaxation leaves every genuine fixed point
        # untouched (W = f(W) implies W = (1 - d) W + d f(W)), which is why
        # `damp` may be adapted freely from one iteration to the next without
        # moving where the iteration lands. What it does NOT do is manufacture
        # a fixed point where none exists: while peer inclusion was a discrete
        # decision this map was discontinuous, cells near w_min recrossed the
        # threshold forever, and no relaxation factor made the stopping rule
        # attainable -- measured on design = ~ . at n = 200, 1 to 21 cells
        # recrossed every few iterations while the rest of the system
        # contracted geometrically, each crossing kicking max|f(W) - W| back up
        # to 0.1-0.2 against a tolerance of 5e-3. The band in
        # .gloc_cond_resid() is what makes a fixed point exist; relaxation is
        # here for the ordinary reason, to keep the iteration inside its
        # contraction radius. Neither alone accounts for the convergence gain
        # -- raising the floor is worth about 40% of it and the band the rest
        # -- and neither is worth much without the other; see
        # .gloc_damp_schedule.
        W <- (1 - damp) * W + damp * .gloc_bisquare(Z, psi_c)
      }
      W[M] <- 0
      if (em) {
        # E-step, then the M-step on the pseudo-rows. The W-step above used the
        # expected design rows, and so does the next one.
        es <- .gloc_cat_estep(X, M, W, B, Sigma, catp, cand, priors,
                              w_min = peer_w_min, band = peer_band)
        dR <- .gloc_cat_change(es$post, post_old)
        post_old <- es$post
        U <- es$Ubar
        B <- .gloc_update_B(X[es$pr_row, , drop = FALSE], cand$Up,
                            W[es$pr_row, , drop = FALSE] * es$pr_w,
                            if (is.null(cand$Up_main)) cand$Up else cand$Up_main,
                            cand$pat_pr, warn = FALSE)
        priors <- .gloc_cat_fit_priors(es$Fp, es$pr_w, catp$levels)
      } else {
        B <- .gloc_update_B(X, U, W, U_main, pats, warn = FALSE)
      }
      design_diag <- attr(B, "gloc_design")
      attr(B, "gloc_design") <- NULL

      # Scale the change in the FITTED MEANS by the scatter. Normalising a
      # coefficient change by a location (max|B_old|) would make the tolerance
      # scale with the data's offset: shifting the data by +1000 loosened it
      # 200,000-fold and stopped the iteration with the weights still moving.
      sd_ref <- sqrt(max(diag(Sigma)))
      if (!is.finite(sd_ref) || sd_ref <= 0) sd_ref <- 1
      dB <- max(abs(U %*% (B - B_old))) / sd_ref
      # Divide the weight step by the relaxation factor. The update is
      # W_new - W = damp * (f(W) - W), so testing max|W_new - W| would test
      # DAMP TIMES the fixed-point residual and a relaxed run would stop at a
      # proportionally looser residual: at the 0.25 floor, four times looser
      # than an unrelaxed one. That is not a harmless difference in a
      # detection method. Before this division, the same data fitted at 0.25
      # and adaptively disagreed by a relative 1.3e-2 in Sigma and on 4 cell
      # flags; tightening eps from 5e-3 to 1e-5 collapsed both (3.3e-6 and 0
      # cells), which is how we know it was slack and not, as first supposed,
      # a discontinuous map with several fixed points -- distinct fixed points
      # do not merge under a tighter tolerance. The slack was systematic
      # rather than random: in the unsaturated detection regime (shift 2 to 4)
      # the looser run had the lower recall every time it disagreed. Dividing
      # here makes eps mean the same thing at every relaxation factor, and
      # incidentally makes the old fixed-0.25 runs the sloppy ones.
      dW <- max(abs(W - W_old)) / damp
      # The estimator is the TRIPLE (B, Sigma, W), so all three are tested.
      # Sigma used to be the one reported quantity with no stopping test of its
      # own, which left the criterion asserting less than the function returns:
      # it is perfectly possible for the fitted means and the weights to have
      # settled while the scatter is still drifting, since Sigma is a different
      # functional of W (a sum over all cells rather than a per-cell value) and
      # the relaxation damps the two at different rates. The change is measured
      # relative to the largest variance, which makes it scale-equivariant and,
      # because Sigma is a scatter of residuals, shift-invariant. Adding a
      # condition can only make convergence harder, never easier -- it is a
      # strengthening of the rule, not a loosening of it.
      dS <- if (is.null(Sigma_old)) Inf else
        max(abs(Sigma - Sigma_old)) / max(max(diag(Sigma)), .Machine$double.eps)
      S_hist[[(it - 1L) %% .gloc_stall_iters + 1L]] <- Sigma
      if (trace) message(sprintf(paste("  iter %d: scaled change in fitted",
                                       "means = %.3g, in scatter = %.3g,",
                                       "fixed-point residual in W = %.3g,",
                                       "damping = %.3g"),
                                 it, dB, dS, dW, damp))
      if (dW < 0.99 * dW_best) { dW_best <- dW; stall <- 0L } else
        stall <- stall + 1L
      if (dB < eps && dS < eps && dW < eps && dR < eps) { converged <- TRUE; break }

      # Strengthen the relaxation only when the iteration stops contracting.
      # Cycling is correlation-driven, and at low correlation dropping a peer
      # barely moves the conditional variance, so nothing flips the inclusion
      # decision back and no relaxation is needed at all: starting at
      # .gloc_damp_start and backing off on demand pays the cost of damping
      # only where it buys something. The factor never rises again, so a
      # spurious trigger costs iterations, never correctness.
      if (adaptive && relax && dW >= dW_prev)
        damp <- max(damp * .gloc_damp_shrink, .gloc_damp)
      dW_prev <- dW

      # Backing off is not enough on its own: arriving at the floor along a
      # weakly relaxed path can land in a limit cycle that starting at the
      # floor avoids. Measured at rho = 0.8, two configurations of 36 reached
      # the floor by iteration 7 and then cycled with period 3 for the
      # remaining 193 iterations, while a run pinned at the floor from the
      # start converged in 39 and 50. So when the floor has been reached and
      # the iteration has still stopped improving, fall back once to exactly
      # that run: the cold start, at the floor.
      #
      # This does NOT make non-regression structural, and an earlier comment
      # here wrongly claimed it did. The fallback reproduces the fixed-floor
      # run only if enough of maxit is left when it fires: it costs the 20
      # stalled iterations plus everything spent reaching the floor, and the
      # fixed-floor run then needs its own 39 to 50. Measured on the same 36
      # configurations at maxit = 60, the adaptive schedule converges on 30
      # against the fixed floor's 32 -- two regressions, in configurations
      # where this fallback fired and then ran out of budget. Non-regression
      # is an EMPIRICAL property at the default maxit = 200 (34 against 34
      # there), not a structural one, and an earlier version of this comment
      # was wrong to claim otherwise. It is a further reason the default is
      # 200 and not 50.
      #
      # It is also why this is worth re-measuring whenever the floor moves. At
      # the old 0.25 floor the fallback was actively harmful on the
      # mean-structure arm -- it fired in every non-converged fit, threw away
      # 30 to 98 iterations, and cost four of forty configurations. At the
      # current 0.5 floor it does not fire there at all, and still rescues one
      # of the 36 in the p = 4 sweep. See .gloc_damp_schedule.
      if (adaptive && relax && !restarted && damp <= .gloc_damp &&
          stall >= .gloc_stall_iters) {
        restarted <- TRUE
        W <- W0; B <- B0
        if (em) {
          es <- es0; U <- es0$Ubar
          priors <- priors0; post_old <- NULL
        }
        dW_prev <- Inf; dW_best <- Inf; stall <- 0L
        if (trace) message(sprintf(paste("  iter %d: still cycling at the",
                                         "relaxation floor; restarting from",
                                         "the cold start at damping %.3g"),
                                   it, damp))
      }
    }

    # The design warnings of the final iterate, once. (Corrected: they used to
    # be raised on every mean step, so a set of unidentified combinations that
    # grew between iterations warned twice, the first time with a stale set.)
    if (!is.null(design_diag)) .gloc_warn_design(design_diag)

    # Non-convergence is now the likeliest degraded path, and it was the only
    # silent one: every other degraded path in this function warns. The two
    # ways it fails need different actions, so they are reported apart: an
    # exhausted iteration limit is cured by raising maxit, a stalled cycle is
    # not cured by raising anything and needs weaker peer filtering or a
    # smaller relaxation floor. Relaxation is NOT a cure for cycling in
    # general: one configuration at rho = 0.5 on clean data still cycles
    # forever at 0.25, which is why this warning exists at all.
    # How much the returned scatter depends on where maxit stopped: the
    # elementwise spread of Sigma over the last .gloc_stall_iters iterations,
    # relative to its largest variance. Zero for a converged fit, the cycle
    # amplitude for a settled cycle, the drift for a run still moving.
    # Only for a fit that did NOT converge: on a converged one the window still
    # holds the last steps of the approach, so a non-zero value there would say
    # nothing about stability and would invite exactly the wrong reading.
    S_keep <- Filter(Negate(is.null), S_hist)
    S_spread <- if (converged || length(S_keep) < 2L || is.null(Sigma))
      NA_real_ else {
      A <- simplify2array(S_keep)
      max(apply(A, seq_len(2L), max) - apply(A, seq_len(2L), min)) /
        max(max(diag(Sigma)), .Machine$double.eps)
    }

    if (maxit >= 1L && !converged) {
      cycling <- stall >= .gloc_stall_iters
      diagnosis <- if (cycling)
        sprintf(paste("max |dW| has not improved for %d iteration(s), so the",
                      "iteration is cycling rather than converging slowly and",
                      "raising maxit will NOT help. The relaxation factor is",
                      "%.3g%s. Widen the conditioning set with a SMALLER",
                      "peer_w_min (larger values discard more peers), or pass",
                      "a smaller fixed damp."),
                stall, damp,
                if (adaptive && damp <= .gloc_damp)
                  sprintf(", already at the schedule's floor of %.3g",
                          .gloc_damp) else "")
      else
        paste("max |dW| is still improving, so this is the iteration limit",
              "and not a cycle: raise maxit.")
      drift <- if (is.na(S_spread)) "" else sprintf(paste(
        "Over the last %d iteration(s) the scatter moved by a relative %.3g,",
        "which is how much the returned Sigma depends on where maxit stopped."),
        min(maxit, .gloc_stall_iters), S_spread)
      warning(sprintf(paste("cellGLoc: did not converge in %d iteration(s)",
                            "(scaled change in fitted means %s, in scatter %s,",
                            "max |dW| %s%s, tolerance %.3g). The estimates are",
                            "still moving, so B, Sigma and W are only whatever",
                            "the last iteration produced. %s%s $criterion",
                            "carries all %s numbers."),
                      maxit, .gloc_fmt(dB), .gloc_fmt(dS), .gloc_fmt(dW),
                      if (em) paste0(", largest change in a categorical posterior ",
                                     .gloc_fmt(dR)) else "",
                      eps, drift,
                      if (nzchar(drift)) paste0(" ", diagnosis) else diagnosis,
                      if (em) "five" else "four"),
              call. = FALSE)
    }

    if (is.null(Sigma)) {                                # maxit = 0
      kap <- if (weights == "binary") 1 else kappa_soft
      Sigma <- if (em)
        .gloc_scatter_soft(X[es$pr_row, , drop = FALSE] - es$Up %*% B,
                           W[es$pr_row, , drop = FALSE] * es$pr_w,
                           M[es$pr_row, , drop = FALSE], kappa = kap, crit = cw_crit)
      else .gloc_scatter_soft(X - U %*% B, W, M, kappa = kap, crit = cw_crit)
    }
  }, warning = dedup)

  # One last E-step at the returned (B, Sigma, W), so the posteriors, the
  # expected design and the imputations all belong to the returned fit.
  cat_post <- cat_pobs <- cat_multi <- cat_pri <- NULL
  if (identical(categorical, "em") && length(cat_vars)) {
    if (em) {
      es <- withCallingHandlers(
        .gloc_cat_estep(X, M, W, B, Sigma, catp, cand, priors,
                        w_min = peer_w_min, band = peer_band),
        warning = dedup)
      U <- es$Ubar
    }
    cat_post  <- if (em) es$post else list()
    cat_multi <- mean(rowSums(catp$Mc) >= 2L)
    cat_pobs <- withCallingHandlers({
      if (!em) priors <- .gloc_cat_fit_priors(catp$F, rep(1, n), catp$levels)
      .gloc_cat_prob_observed(X, M, W, B, Sigma, catp, priors, design,
                              if (em) es else .gloc_cat_identity(catp, U),
                              w_min = peer_w_min, band = peer_band)
    }, warning = dedup)
    cat_pri <- priors
  }

  # Impute from the unflagged cells only, by the peer rule detection uses. Both
  # corners: a binary flag is a weight of exactly 0 or 1, where the band is inert.
  Ximp <- .gloc_impute(X, U, B, Sigma, M, W = W, w_min = peer_w_min,
                       band = peer_band)
  out <- data
  for (v in cont_vars) out[[v]] <- .gloc_restore_class(Ximp[, v], data[[v]], v)
  if (em) for (v in names(cat_post)) {
    P <- cat_post[[v]]
    val <- catp$F[[v]]
    val[as.integer(rownames(P))] <- colnames(P)[max.col(P, ties.method = "first")]
    out[[v]] <- .gloc_cat_restore(val, data[[v]])
  }

  list(B = B, Sigma = Sigma, W = W, U = U, imputed = out,
       converged = converged, iterations = iter_count,
       criterion = c(means = dB, scatter = dS, weights = dW,
                     scatter_spread = S_spread,
                     categorical = if (iter_count == 0L) NA_real_ else dR),
       cat_posterior = cat_post, cat_prob_observed = cat_pobs,
       cat_multi_missing = cat_multi, cat_priors = cat_pri)
}

#' Relaxation schedule for the soft corner's weight update
#'
#' The weight update is relaxed, \eqn{W \mapsto (1 - d) W + d f(W)}. Relaxation
#' moves no fixed point, since \eqn{W = f(W)} implies
#' \eqn{W = (1 - d) W + d f(W)}; the same argument licenses changing \eqn{d}
#' between iterations, so the factor can be adapted freely.
#'
#' The update has the same form as the damping in \code{imputeCellEM} and
#' \code{imputeCellwise}, which ramp \eqn{\lambda} adaptively rather than
#' holding it fixed. \code{imputeCellGLoc} does the same: it starts at
#' \code{.gloc_damp_start} (no relaxation at all) and multiplies the factor by
#' \code{.gloc_damp_shrink} whenever \code{max |dW|} fails to fall from one
#' iteration to the next, never going below \code{.gloc_damp}. The factor
#' never rises again, so a spurious trigger costs iterations, never
#' correctness.
#'
#' \strong{What relaxation is and is not for.} Until 7.4.0 the peer-inclusion
#' rule was a hard cut, which made the weight map discontinuous, and the floor
#' was set low (0.25) to suppress the resulting limit cycles. That was treating
#' the symptom: a discontinuous self-map need not have a fixed point at all, so
#' no amount of relaxation could make the stopping rule attainable, and the
#' documentation said as much -- configurations existed that cycled forever at
#' the floor. Since the threshold became a band (see
#' \code{.gloc_peer_band}) the map is continuous, cycling in the
#' peer-inclusion decision is largely gone, and relaxation is back to its
#' ordinary job: keeping the fixed-point iteration inside its contraction
#' radius. The floor is therefore set by how fast the iteration converges, not
#' by how badly it cycles, and it rises from 0.25 to 0.50.
#'
#' The floor and the band are a package and must not be quoted apart. The floor
#' is worth about 40% of the convergence gain on the mean-structure arm (131,
#' then 181, then 245 of 260 fits, adding the floor and the scatter condition
#' first and the band second), and it is worth that only \emph{with} the band:
#' under the hard cut a low floor is the better setting, and raising it costs
#' convergence rather than buying it -- measured on 260 pooled fits, the hard
#' cut converges 208 times at 0.5 against 245 at 0.25.
#'
#' The table below is the 36-configuration sweep, re-measured under the band
#' and under the current convergence rule, from a generator stated in full so
#' that it can be checked -- 6 seeds, \eqn{\rho \in \{0, 0.5, 0.8\}}, clean and
#' 5% of cells shifted by +8, \eqn{n = 800}, \eqn{p = 4}, \code{design = ~ 1},
#' \code{weights = "soft"}, \code{maxit = 200}, data from
#' \code{MASS::mvrnorm} with \eqn{\Sigma = (1 - \rho) I + \rho}:
#'
#' \tabular{lrrrr}{
#'   damping \tab hard cut \tab median iters \tab band \tab median iters \cr
#'   1.00 (none) \tab 20/36 \tab   8 \tab 19/36 \tab  9 \cr
#'   0.75        \tab 26/36 \tab  13 \tab 29/36 \tab 11 \cr
#'   0.50        \tab 30/36 \tab  21 \tab \strong{36/36} \tab 17 \cr
#'   0.25        \tab 34/36 \tab  44 \tab \strong{36/36} \tab 36 \cr
#'   0.10        \tab 35/36 \tab 113 \tab \strong{36/36} \tab 92 \cr
#'   adaptive, floor 0.25 \tab 34/36 \tab 14 \tab \strong{36/36} \tab 12 \cr
#'   adaptive, floor 0.50 \tab -- \tab -- \tab \strong{36/36} \tab 11.5
#' }
#'
#' Under the hard cut the floor bought convergence monotonically all the way
#' down and never reached 36/36. Under the band, 0.50 already reaches 36/36 and
#' everything below it only costs iterations. The floor is set there.
#'
#' It is not only the \eqn{p = 4} sweep that prefers 0.50. On the mean-structure
#' arm this function exists for -- \code{design = ~ .}, \eqn{n = 200},
#' 6 continuous and 6 categorical variables, so 19 design columns from 200 rows
#' -- the smooth part of the map has a contraction factor near 0.92, and
#' relaxing to 0.25 slows that to 0.98 per iteration, which does not fit inside
#' \code{maxit}. Across 40 such fits: floor 0.25 converges 35/40 in a median
#' 89 iterations, floor 0.50 converges 40/40 in a median 47. Every floor-0.25
#' failure there is an exhausted budget, not a cycle.
#'
#' Every failure in the hard-cut column of the table, at every damping level,
#' is at \eqn{\rho \ge 0.5}; nothing ever fails at \eqn{\rho = 0}, even
#' undamped. That is the shape of the boundary: threshold crossings are driven
#' by correlation, because when the columns are strongly correlated dropping
#' one peer moves the conditional variance a long way. It is also what makes
#' the adaptive schedule pay: a fixed floor spends several times the iterations
#' it needs on uncorrelated data, where no relaxation is called for at all.
#'
#' \strong{The cold-start fallback.} The hard-cut map was discontinuous, so by
#' Brouwer it need not have possessed a fixed point at all and the iteration
#' could sit in a limit cycle indefinitely; which cycle it entered depended on
#' the path taken, so reaching the floor along a weakly relaxed path could land
#' in one that starting at the floor avoided.
#'
#' \strong{Correction, recorded rather than deleted.} An earlier version of
#' this paragraph said the discontinuous map "had several fixed points" and
#' that the iteration selected among them, and gave that as the reason the
#' relaxed and adaptive schedules disagreed. \strong{That explanation is
#' false.} It was falsified by tightening \code{eps} from 5e-3 to 1e-5, which
#' collapsed the relative scatter difference between the two schedules from
#' 1.28e-2 to 3.3e-6 and the flag disagreement from 4 cells to zero -- distinct
#' fixed points do not vanish under a tighter tolerance. The real cause was the
#' convergence rule: it compared a \emph{relaxed} step against \code{eps} and so
#' stopped at a fixed-point residual \code{1/damp} times looser, which made the
#' stopping point depend on the relaxation factor. That is fixed (see
#' \code{eps}), and the fallback is retained for the path effect described
#' above, which is about which cycle is entered, not about which fixed point is
#' selected. When the
#' factor is at the floor and \code{max |dW|} has not improved for
#' \code{.gloc_stall_iters} iterations, the iteration therefore falls back once
#' to the cold start at the floor, which is precisely the fixed-\code{.gloc_damp}
#' run.
#'
#' The fallback is kept, but it is worth being precise about what it is now
#' worth, because at the old floor it was doing real damage. Measured on the
#' 40 mean-structure fits at floor 0.25 \emph{with} the band, it fires in every
#' non-converged run, discards between 30 and 98 iterations of progress -- in
#' one case a state whose fixed-point residual was within 26% of the tolerance
#' -- and costs four configurations: 35/40 with it against 39/40 without. At
#' the 0.50 floor it never fires there at all (40/40 either way, to the
#' iteration). On the \eqn{p = 4} sweep at floor 0.50 it is still worth exactly
#' one configuration, 36/36 with against 35/36 without. So it is retained: at
#' the current floor it is free where it used to be harmful, and it still
#' rescues a case.
#'
#' @format length-one numerics.
#'
#' @keywords internal
#' @name dot-gloc_damp_schedule
#' @aliases .gloc_damp .gloc_damp_start .gloc_damp_shrink
NULL

#' @rdname dot-gloc_damp_schedule
.gloc_damp <- 0.5

#' @rdname dot-gloc_damp_schedule
.gloc_damp_start <- 1

#' @rdname dot-gloc_damp_schedule
.gloc_damp_shrink <- 0.5

#' Iterations without an improvement in max |dW| that count as a stalled cycle
#'
#' Two uses: it arms the cold-start fallback in \code{.gloc_damp_schedule},
#' and it phrases the non-convergence warning, which must tell an exhausted
#' iteration limit (raise \code{maxit}) apart from a limit cycle (raising
#' \code{maxit} cannot help). A running best is compared rather than
#' consecutive values, so a cycle of any period is caught, not just period 2.
#' The warning's two labels are a heuristic and should be read as such: a run
#' contracting at a rate near 0.999 improves \code{max |dW|} by only 2% over
#' 20 iterations and so is reported as cycling although it is merely slow.
#'
#' @format a length-one integer.
#' @keywords internal
.gloc_stall_iters <- 20L

#' Gaussian consistency factor of a cell-weight function
#'
#' Returns \eqn{E[w(Z) Z^2] / E[w(Z)]} for \eqn{Z \sim N(0, 1)}, the factor by
#' which a \eqn{\sum w}-normalised weighted covariance under-states the scatter
#' at the Gaussian model. Dividing by it makes the weighted scatter
#' Fisher-consistent: at \eqn{\Sigma = \Sigma_0} the standardised conditional
#' residuals are exactly standard normal, the weighted scatter has expectation
#' \eqn{\kappa \Sigma_0}, and the corrected map therefore has \eqn{\Sigma_0} as
#' its fixed point.
#'
#' @param c tuning constant; \code{Inf} returns 1 (no downweighting).
#' @param type \code{"bisquare"} for Tukey weights, \code{"hard"} for a 0/1
#'   cut-off at \code{c}.
#' @return a scalar in (0, 1].
#' @seealso \code{.gloc_correct_scatter}, which is what applies it, and which
#'   must account for correlation between the columns.
#' @keywords internal
.gloc_consistency <- function(c, type = c("bisquare", "hard")) {
  type <- match.arg(type)
  if (!is.finite(c) || c <= 0) return(1)
  wf <- if (type == "bisquare") function(z) (1 - (z / c)^2)^2
        else function(z) rep(1, length(z))
  num <- try(stats::integrate(function(z) wf(z) * z^2 * stats::dnorm(z),
                              -c, c)$value, silent = TRUE)
  den <- try(stats::integrate(function(z) wf(z) * stats::dnorm(z),
                              -c, c)$value, silent = TRUE)
  if (inherits(num, "try-error") || inherits(den, "try-error") ||
      !is.finite(num) || !is.finite(den) || den <= 0) return(1)
  num / den
}

#' Make a weighted scatter Fisher-consistent at the Gaussian model
#'
#' Dividing the whole matrix by \code{kappa} is correct only when the columns
#' are independent. The weights are functions of the \emph{conditional}
#' residual, and a residual splits as \eqn{R_j = m_j + s_j Z_j} with the
#' predictable part \eqn{m_j} independent of \eqn{Z_j}. A weight
#' \eqn{w(Z_j)} therefore downweights only the \eqn{s_j Z_j} part, so
#'
#' \deqn{E[w R_j^2] / E[w] = (\sigma_j^2 - s_j^2) + \kappa s_j^2
#'       = \sigma_j^2 \{1 - (s_j^2/\sigma_j^2)(1 - \kappa)\},}
#'
#' where \eqn{s_j^2 = 1 / (\Sigma^{-1})_{jj}} is the conditional variance. The
#' per-column factor \eqn{\kappa_j = 1 - (s_j^2/\sigma_j^2)(1 - \kappa)}
#' reduces to \eqn{\kappa} under independence (\eqn{s_j^2 = \sigma_j^2}) and to
#' 1 when there is no downweighting. Because \eqn{\kappa_j} depends on
#' \eqn{\Sigma}, it is solved for by a short fixed-point iteration.
#'
#' The scaling is symmetric, \eqn{\Sigma \mapsto D \Sigma D} with
#' \eqn{D = \mathrm{diag}(\kappa_j^{-1/2})}, so it fixes the \emph{scale} and
#' leaves the correlation matrix alone. The correlations are themselves mildly
#' biased upward by conditional-residual downweighting (cells inconsistent with
#' their peers are exactly the ones removed); that bias is not corrected here.
#'
#' @param S_raw the uncorrected weighted scatter.
#' @param kappa the scalar consistency factor from \code{.gloc_consistency}.
#' @return the corrected scatter.
#' @keywords internal
.gloc_correct_scatter <- function(S_raw, kappa) {
  if (!is.finite(kappa) || kappa >= 1) return(S_raw)
  p <- ncol(S_raw)
  if (p == 1L) return(S_raw / kappa)
  S <- S_raw / kappa                       # independence-case starting value
  for (i in seq_len(50L)) {
    Sinv <- tryCatch(chol2inv(chol(S)), error = function(e) NULL)
    if (is.null(Sinv)) return(S_raw / kappa)
    ratio <- 1 / (diag(Sinv) * diag(S))    # s_j^2 / sigma_j^2, in [0, 1]
    ratio[!is.finite(ratio)] <- 1
    ratio <- pmin(pmax(ratio, 0), 1)
    d <- sqrt(1 / (1 - ratio * (1 - kappa)))
    Snew <- S_raw * outer(d, d)
    done <- max(abs(Snew - S)) < 1e-12 * max(abs(S))
    S <- Snew
    if (done) break
  }
  dimnames(S) <- dimnames(S_raw)
  S
}

#' Format a stopping residual for the non-convergence warning
#'
#' The scatter residual is \code{Inf} on the first iteration, where there is no
#' previous scatter to compare against, and every residual is \code{NA} at
#' \code{maxit = 0}. Printing those verbatim reads as a numerical failure
#' rather than as "not defined yet", so they are spelled out.
#'
#' @param x a length-one numeric.
#' @return a length-one character string.
#' @keywords internal
.gloc_fmt <- function(x) {
  if (length(x) != 1L || is.na(x)) "not available"
  else if (!is.finite(x)) "not defined on the first iteration"
  else sprintf("%.3g", x)
}

#' Tukey bisquare weights for standardised conditional residuals
#'
#' @param Z matrix of standardised conditional residuals.
#' @param c tuning constant; \code{Inf} disables downweighting.
#' @return a matrix of weights in \[0, 1\] with the dimensions of \code{Z}.
#' @keywords internal
.gloc_bisquare <- function(Z, c = 4.685) {
  if (!is.finite(c)) return(matrix(1, nrow(Z), ncol(Z), dimnames = dimnames(Z)))
  u <- abs(Z) / c
  w <- (1 - u^2)^2
  w[!is.finite(u) | u > 1] <- 0
  w[!is.finite(Z)] <- 0
  matrix(w, nrow(Z), ncol(Z), dimnames = dimnames(Z))
}

#' Cellwise weighted maximum likelihood scatter of the residuals
#'
#' Uses \code{cellWise::cwLocScat()} when available. \pkg{cellWise} is in
#' Suggests, so its absence is a supported configuration rather than an edge
#' case; the fallback is a weighted pairwise covariance, which still honours
#' the cell weights, and it is never taken silently.
#'
#' @param R \eqn{n x p} matrix of residuals from the mean structure.
#' @param W \eqn{n x p} matrix of cell weights in \[0, 1\].
#' @param M \eqn{n x p} logical mask of missing cells.
#' @param kappa Gaussian consistency factor to divide by; see
#'   \code{.gloc_consistency}.
#' @param crit convergence tolerance handed to \code{cellWise::cwLocScat}'s
#'   EM. This is an inner loop inside the outer cellGLoc iteration, and it is
#'   the whole cost of a cellGLoc step: about 95% of an iteration against
#'   about 5% for the conditional residuals. Measured by \code{Rprof} over
#'   five draws at \eqn{n = 1000}, \eqn{p = 10}, \eqn{\rho = 0.5}, 5% of cells
#'   shifted by +8, \code{weights = "soft"}, across \code{design = ~ 1} and
#'   \code{design = ~ .}: the share runs 94.3 to 95.6%. Earlier releases
#'   quoted 98.8% here and 97.5% elsewhere for the same quantity; neither
#'   reproduced, and 95% replaces both.
#'   \code{cwLocScat}'s own default is 1e-12, seven orders of magnitude
#'   tighter than the outer loop's \code{eps} of 5e-3 can resolve, so the EM
#'   spends most of its steps refining digits the caller discards. The default
#'   1e-8 is still five orders tighter than the outer tolerance; measured on
#'   \eqn{n = 1000, p = 10} it changed the scatter by 1.5e-9 in absolute value
#'   and cut the scatter step's time by about 30%.
#' @param have_cw whether \pkg{cellWise} may be used; exposed so the fallback
#'   path is directly testable.
#' @return a \eqn{p x p} scatter matrix.
#' @keywords internal
.gloc_scatter_soft <- function(R, W, M, kappa = 1, crit = 1e-8,
                               have_cw = requireNamespace("cellWise",
                                                          quietly = TRUE)) {
  Rna <- R; Rna[M] <- NA_real_
  if (have_cw) {
    Wc <- W; Wc[!is.finite(Wc)] <- 0
    # cellWise's internal unpack() drops rows whose weights are all zero and
    # warns "There were rows with only zero weights, we dropped them from both
    # X and W". Such a row (every cell missing or flagged) has no weight in the
    # likelihood, so dropping it leaves the estimate unchanged (checked: the
    # location and scatter agree exactly with the row removed). Under the
    # robust start, whose flags are binary, it happens routinely, and the
    # message gives a user nothing to act on, so it is muffled here and only
    # here.
    fit <- tryCatch(
      withCallingHandlers(
        cellWise::cwLocScat(Rna, W = Wc, methods = "all", crit = crit),
        warning = function(w) {
          if (grepl("only zero weights", conditionMessage(w), fixed = TRUE))
            invokeRestart("muffleWarning")
        }),
      error = function(e) NULL)
    if (!is.null(fit) && all(is.finite(fit$cwMLEsigma)))
      return(.gloc_correct_scatter(fit$cwMLEsigma, kappa))
    warning(paste("cellGLoc: cellWise::cwLocScat() failed or returned a",
                  "non-finite scatter; falling back to a weighted pairwise",
                  "covariance, which is a cruder estimator than the cellwise",
                  "weighted MLE."), call. = FALSE)
  } else {
    warning(paste("cellGLoc: the cellWise package is not installed, so the",
                  "cellwise weighted MLE scatter is unavailable; falling back",
                  "to a weighted pairwise covariance, which is a cruder",
                  "estimator. Install cellWise to get the published",
                  "estimator."), call. = FALSE)
  }
  .gloc_correct_scatter(.gloc_wcov(Rna, W), kappa)
}

#' Weighted pairwise covariance of residuals with cell weights
#'
#' Entry \eqn{(j, k)} uses the weights of both cells, \eqn{w_{ij} w_{ik}}, so a
#' downweighted cell is excluded from every covariance it takes part in. The
#' pairwise construction is not guaranteed non-negative definite, so the result
#' is repaired by flooring its eigenvalues.
#'
#' @param R \eqn{n x p} residual matrix, missing cells as \code{NA}.
#' @param W \eqn{n x p} matrix of cell weights.
#' @return a \eqn{p x p} non-negative definite matrix.
#' @keywords internal
.gloc_wcov <- function(R, W) {
  p <- ncol(R)
  nm <- list(colnames(R), colnames(R))
  obs <- is.finite(R)
  Wc <- W; Wc[!is.finite(Wc)] <- 0; Wc[!obs] <- 0
  Rz <- R; Rz[!obs] <- 0
  cw <- colSums(Wc)
  mu <- ifelse(cw > 0, colSums(Wc * Rz) / pmax(cw, .Machine$double.eps), 0)
  C <- sweep(Rz, 2, mu) * obs
  S <- matrix(0, p, p, dimnames = nm)
  for (j in seq_len(p)) for (k in j:p) {
    wjk <- Wc[, j] * Wc[, k]
    den <- sum(wjk)
    v <- if (den > 0) sum(wjk * C[, j] * C[, k]) / den else 0
    S[j, k] <- v; S[k, j] <- v
  }
  .gloc_psd(S)
}

#' Nearest non-negative definite repair of a symmetric matrix
#'
#' @param S a symmetric matrix.
#' @return \code{S} with its eigenvalues floored at a small positive value.
#' @keywords internal
.gloc_psd <- function(S) {
  ev <- tryCatch(eigen(S, symmetric = TRUE), error = function(e) NULL)
  if (is.null(ev)) return(S)
  top <- max(ev$values)
  if (!is.finite(top) || top <= 0) {
    out <- diag(max(.Machine$double.eps, mean(abs(diag(S)))), nrow(S))
    dimnames(out) <- dimnames(S)
    return(out)
  }
  floor_ev <- top * 1e-8
  if (min(ev$values) >= floor_ev) return(S)
  ev$values[ev$values < floor_ev] <- floor_ev
  out <- ev$vectors %*% diag(ev$values, nrow(S)) %*% t(ev$vectors)
  out <- (out + t(out)) / 2
  dimnames(out) <- dimnames(S)
  out
}

#' Fill missing continuous cells by their conditional expectation
#'
#' A missing cell is imputed by its conditional expectation given the
#' \emph{usable} peers in its row: the observed cells that pass the detection
#' peer rule (\code{.gloc_peer_rel}, i.e. \code{peer_w_min} with the peer
#' band). A flagged cell is therefore treated exactly like a missing one, and a
#' peer inside the band is conditioned on with its information discounted, as
#' in \code{.gloc_cond_resid}. Until 7.4.0 every observed peer was used,
#' flagged ones included, so a grossly contaminated cell was carried into the
#' imputation of its row-mates; \code{W = NULL} still gives that rule.
#'
#' Rows whose peers are all fully in or fully out are grouped by pattern, one
#' matrix inversion per pattern; a row holding a peer inside the band gets a
#' solve of its own. A row with no usable peer falls back to its fitted mean,
#' with the marginal covariance given the design.
#'
#' @param X \eqn{n x p} numeric matrix of continuous variables.
#' @param U \eqn{n x q} design matrix from \code{.gloc_design}.
#' @param B \eqn{q x p} matrix of mean-structure coefficients.
#' @param Sigma \eqn{p x p} scatter matrix.
#' @param M \eqn{n x p} logical mask of missing cells.
#' @param W optional \eqn{n x p} matrix of cell weights. \code{NULL} conditions
#'   on every observed peer, the 7.4.0 rule.
#' @param w_min,band the peer rule; see \code{.gloc_peer_rel}.
#' @param cov if \code{TRUE}, also return the conditional covariance of the
#'   missing cells given the same peers, which is what imputation noise must be
#'   drawn from.
#' @return \code{X} with its missing cells replaced. With \code{cov = TRUE}, a
#'   list with that matrix as \code{X} and \code{cond_cov}, a list named by row
#'   index that holds, for each row with a missing cell, the conditional
#'   covariance matrix of its missing cells.
#' @keywords internal
.gloc_impute <- function(X, U, B, Sigma, M, W = NULL, w_min = 0.5,
                         band = .gloc_peer_band, cov = FALSE) {
  Xi <- X
  cc <- list()
  done <- function() if (cov) list(X = Xi, cond_cov = cc) else Xi
  if (!any(M)) return(done())
  Mu  <- U %*% B
  R   <- X - Mu
  Rel <- .gloc_peer_rel(!M, W, w_min = w_min, band = band)
  sdiag <- diag(Sigma)
  rows  <- which(rowSums(M) > 0)
  Rr    <- Rel[rows, , drop = FALSE]
  sharp <- rowSums(Rr > 0 & Rr < 1) == 0L
  bits  <- function(A) apply(A, 1L, function(r) paste0(as.integer(r), collapse = ""))

  # ---- every peer fully in or fully out: one inversion per pattern. Without
  # weights the key partitions rows exactly as the 7.4.0 code did.
  idx <- rows[sharp]
  if (length(idx)) {
    key <- paste(bits(M[idx, , drop = FALSE]), bits(Rel[idx, , drop = FALSE] > 0))
    for (g in split(idx, key)) {
      i0   <- g[1L]
      miss <- which(M[i0, ]); obs <- which(Rel[i0, ] > 0)
      if (!length(obs)) {
        Xi[g, miss] <- Mu[g, miss, drop = FALSE]
        if (cov) C <- Sigma[miss, miss, drop = FALSE]
      } else {
        Soo  <- Sigma[obs, obs, drop = FALSE]
        Sinv <- tryCatch(chol2inv(chol(Soo)), error = function(e) MASS::ginv(Soo))
        Beta <- Sigma[miss, obs, drop = FALSE] %*% Sinv          # |miss| x |obs|
        Xi[g, miss] <- Mu[g, miss, drop = FALSE] +
          R[g, obs, drop = FALSE] %*% t(Beta)
        if (cov)
          C <- Sigma[miss, miss, drop = FALSE] - Beta %*% Sigma[obs, miss, drop = FALSE]
      }
      if (cov) for (i in g) cc[[as.character(i)]] <- C
    }
  }

  # ---- a peer inside the band: one solve for that row, with the same
  # noise-inflated construction as .gloc_cond_resid
  for (i in rows[!sharp]) {
    miss <- which(M[i, ])
    kk <- which(Rel[i, ] > 0); rk <- Rel[i, kk]; dd <- sqrt(rk)
    G  <- outer(dd, dd) * Sigma[kk, kk, drop = FALSE] +
            diag(sdiag[kk] * (1 - rk), length(kk))
    Gi <- tryCatch(chol2inv(chol(G)), error = function(e) MASS::ginv(G))
    Bt <- (Sigma[miss, kk, drop = FALSE] %*% diag(dd, length(kk))) %*% Gi
    Xi[i, miss] <- Mu[i, miss] + as.vector(Bt %*% (dd * R[i, kk]))
    if (cov)
      cc[[as.character(i)]] <- Sigma[miss, miss, drop = FALSE] -
        Bt %*% (dd * Sigma[kk, miss, drop = FALSE])
  }
  done()
}

#' Give an imputed column back the class of the column it came from
#'
#' @param x numeric vector of imputed values.
#' @param orig the original column.
#' @param nm the column name, for the warning.
#' @return \code{x} coerced back to \code{orig}'s class where that is safe.
#' @keywords internal
.gloc_restore_class <- function(x, orig, nm) {
  if (!is.integer(orig)) return(x)
  r <- round(x)
  if (all(is.finite(r)) && max(abs(r)) <= .Machine$integer.max)
    return(as.integer(r))
  warning(sprintf(paste("cellGLoc: column '%s' is integer but its imputed",
                        "values do not fit in an integer; returning double."),
                  nm), call. = FALSE)
  x
}
