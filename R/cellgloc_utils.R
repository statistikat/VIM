#' Build the categorical design matrix for cellGLoc
#'
#' Missing factor values become an explicit level rather than dropped rows, so
#' every observation keeps a design row.
#'
#' @param data a data.frame containing the categorical variables.
#' @param design a one-sided formula, e.g. \code{~ .} or \code{~ 1}.
#' @param cat_vars character vector naming the categorical columns.
#' @return an \code{n x q} numeric matrix from \code{model.matrix}. Its first
#'   column is the intercept only when the design has one; under
#'   \code{~ f - 1} it is the first level's indicator. (Corrected: this page
#'   said the first column is always the intercept.)
#' @keywords internal
.gloc_design <- function(data, design, cat_vars) {
  if (is.null(design)) design <- ~ 1
  if (!length(cat_vars) || identical(all.vars(design), character(0))) {
    return(matrix(1, nrow = nrow(data), ncol = 1,
                  dimnames = list(NULL, "(Intercept)")))
  }
  stats::model.matrix(design, .gloc_model_frame(data, design, cat_vars))
}

#' The model frame behind \code{.gloc_design}
#'
#' @inheritParams .gloc_design
#' @return the \code{model.frame} of \code{design} on the categorical columns,
#'   with a missing factor value as a level of its own.
#' @keywords internal
.gloc_model_frame <- function(data, design, cat_vars) {
  df <- data[, cat_vars, drop = FALSE]
  for (v in names(df)) {
    df[[v]] <- as.factor(df[[v]])
    if (anyNA(df[[v]])) df[[v]] <- addNA(df[[v]], ifany = TRUE)
  }
  stats::model.frame(design, data = df, na.action = stats::na.pass,
                     drop.unused.levels = TRUE)
}

#' Main-effects design and level combinations for a cellGLoc design
#'
#' What the mean step and the robust start need when a level combination has
#' no fitted row (see \code{.gloc_update_B}).
#'
#' The main-effects design has the variables of \code{design} and none of its
#' terms of order greater than one. The variables are read from the terms
#' object with \code{deparse1}, so a term such as \code{relevel(g2, ref = "B")}
#' or \code{C(g1, contr.sum)} is kept as written, and the matrix is built on
#' the model frame that \code{.gloc_design} evaluates; the names are deparsed
#' with backticks for \code{reformulate()}, so a factor named with a space
#' parses. If that fails for any reason, \code{U_main} is \code{NULL}, a
#' warning says so, and the fill uses the weighted mean only. (Corrected: the first 7.4.1 fix rebuilt the formula from backticked
#' model-frame names, and a function call in an interaction term made
#' \code{imputeCellGLoc} stop with "object not found".)
#'
#' The level combinations are all combinations of the levels of the design's
#' variables when there are at most \code{.gloc_max_patterns} of them, and
#' otherwise those that occur in the data.
#'
#' @inheritParams .gloc_design
#' @param max_patterns enumerate every level combination up to this many,
#'   otherwise only those in the data; see \code{.gloc_max_patterns}.
#' @param main_terms a function of the backticked variable names returning the
#'   terms of the main-effects design; an argument so the failure path can be
#'   tested.
#' @return a list with \code{U_main}, the main-effects design matrix, or
#'   \code{NULL} when \code{design} has no interaction term, and
#'   \code{patterns}, or \code{NULL} when \code{design} uses no categorical
#'   variable or a variable of the model frame is not a factor.
#'   \code{patterns} is a list: \code{id}, the index of each row's level
#'   combination, where a caller marks a row whose categorical values are
#'   unknown with \code{NA}; \code{P}, the pure design row of each combination,
#'   with the columns of the design; \code{P_main}, the same for \code{U_main}
#'   (or \code{NULL}); and \code{labels}, such as \code{"g1=c, g2=C"}.
#' @keywords internal
.gloc_design_aux <- function(data, design, cat_vars,
                             max_patterns = .gloc_max_patterns,
                             main_terms = function(vars)
                               stats::terms(stats::reformulate(vars))) {
  if (is.null(design)) design <- ~ 1
  none <- list(U_main = NULL, patterns = NULL)
  if (!length(cat_vars) || identical(all.vars(design), character(0))) return(none)
  mf <- .gloc_model_frame(data, design, cat_vars)
  tt <- attr(mf, "terms")
  var_exprs <- as.list(attr(tt, "variables"))[-1L]
  if (!length(var_exprs)) return(none)
  # reformulate() gets the names with backticks, so that `g 1` parses; the
  # labels keep them as they read. (Corrected: without backticks a name such as
  # "g 1" made reformulate() fail inside tryCatch, U_main became NULL without a
  # word, and (c, C) was imputed at 13.98 against a truth of 30.)
  vars <- vapply(var_exprs, deparse1, "")
  vars_bt <- vapply(var_exprs, deparse1, "", backtick = TRUE)
  main_tt <- U_main <- NULL
  if (any(attr(tt, "order") > 1L)) {
    U_main <- tryCatch({
      main_tt <- main_terms(vars_bt)
      mfm <- mf
      attr(mfm, "terms") <- main_tt
      stats::model.matrix(main_tt, mfm)
    }, error = function(e) {
      warning(sprintf(paste("cellGLoc: the main-effects version of the design could",
                            "not be built (%s); a level combination that the data do",
                            "not identify is filled towards the variable's weighted",
                            "mean only."), conditionMessage(e)), call. = FALSE)
      NULL
    })
    if (is.null(U_main)) main_tt <- NULL
  }
  list(U_main = U_main,
       patterns = tryCatch(.gloc_patterns(mf, tt, main_tt, vars, max_patterns),
                           error = function(e) NULL))
}

#' The design a fit with a missing categorical value as a level works on
#'
#' The design (\code{.gloc_design}), its main-effects version and its level
#' combinations (\code{.gloc_design_aux}), with a combination table dropped when
#' its columns do not match. \code{imputeCellGLoc} uses it whenever no
#' categorical EM runs, and for the EM's second start, which is the robust start
#' of \code{categorical = "level"}; one function keeps the two identical.
#' @inheritParams .gloc_design
#' @return \code{list(U, U_main, patterns)}; \code{U_main} is \code{U} for a
#'   design without interaction terms, and \code{patterns} may be \code{NULL}.
#' @keywords internal
.gloc_design_setup <- function(data, design, cat_vars) {
  U <- .gloc_design(data, design, cat_vars)
  # The main-effects design and the level combinations, used only for
  # combinations that the rows a variable is fitted from do not identify.
  aux <- .gloc_design_aux(data, design, cat_vars)
  U_main <- if (is.null(aux$U_main)) U else aux$U_main
  pats <- aux$patterns
  if (!is.null(pats) && !identical(colnames(pats$P), colnames(U))) pats <- NULL
  if (!is.null(pats$P_main) && !identical(colnames(pats$P_main), colnames(U_main)))
    pats$P_main <- NULL
  list(U = U, U_main = U_main, patterns = pats)
}

#' Largest number of level combinations enumerated in full
#'
#' Up to this many, \code{.gloc_design_aux} enumerates every combination of the
#' design's levels, so that a combination absent from the data is filled like
#' any other; above it, only the combinations that occur in the data.
#' @format a length-one integer.
#' @keywords internal
.gloc_max_patterns <- 4096L

#' Level combinations, their pure design rows and each row's combination
#'
#' @param mf the model frame of the design.
#' @param tt,main_tt the terms of the design and of its main-effects version
#'   (\code{NULL} if none).
#' @param vars the deparsed variables of \code{tt}, for the labels.
#' @param max_patterns enumerate every combination up to this many, otherwise
#'   only those in the data.
#' @return the \code{patterns} list described in \code{.gloc_design_aux}, or
#'   \code{NULL} when a variable is not a factor.
#' @keywords internal
.gloc_patterns <- function(mf, tt, main_tt, vars, max_patterns = .gloc_max_patterns) {
  if (!all(vapply(mf, is.factor, TRUE))) return(NULL)
  k <- length(vars)
  nl <- vapply(mf, nlevels, 1L)
  codes <- matrix(unlist(lapply(mf, as.integer), use.names = FALSE), nrow(mf), k)
  if (prod(as.numeric(nl)) <= max_patterns) {
    grid <- as.matrix(expand.grid(lapply(nl, seq_len), KEEP.OUT.ATTRS = FALSE))
    id <- as.integer(drop((codes - 1L) %*% cumprod(c(1, nl[-k]))) + 1L)
    rep_row <- lapply(seq_len(k), function(v) match(seq_len(nl[v]), codes[, v]))
    cols <- lapply(seq_len(k), function(v) mf[[v]][rep_row[[v]][grid[, v]]])
  } else {
    key <- do.call(paste, c(unname(split(codes, col(codes))), sep = "\r"))
    id <- match(key, unique(key))
    first <- which(!duplicated(key))
    grid <- codes[first, , drop = FALSE]
    cols <- lapply(seq_len(k), function(v) mf[[v]][first])
  }
  combos <- structure(cols, names = names(mf), class = "data.frame",
                      row.names = seq_len(nrow(grid)))
  attr(combos, "terms") <- tt
  P <- stats::model.matrix(tt, combos)
  P_main <- NULL
  if (!is.null(main_tt)) {
    attr(combos, "terms") <- main_tt
    P_main <- stats::model.matrix(main_tt, combos)
    rownames(P_main) <- NULL
  }
  rownames(P) <- NULL
  labels <- do.call(paste, c(lapply(seq_len(k), function(v)
    paste0(vars[v], "=", levels(mf[[v]])[grid[, v]])), sep = ", "))
  list(id = id, P = P, P_main = P_main, labels = labels)
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
#' \code{0} if none exists. A column with fewer usable cells than design
#' columns warns too and uses the median of those cells. Either constant is put
#' through the design (\code{.gloc_const_coef}), so every row gets it whether
#' or not the design has an intercept column.
#'
#' A design that is rank deficient on the rows a column is fitted from is
#' fitted on a maximal set of linearly independent design columns, chosen by
#' the same pivoted QR decomposition \code{qr.solve()} uses; a full-rank design
#' takes exactly the arithmetic of \code{qr.solve()}, so its result is
#' unchanged bit for bit. Identification is decided on the cells whose weight
#' exceeds \code{.gloc_w_floor} times the column's largest: a fit that is full
#' rank on them keeps that full-rank result, tiny cells included, and a fit
#' that is rank deficient on them takes the path below on them alone.
#' The data then determine the fitted mean only of the level combinations that
#' a fitted row has or whose pure design row lies in the row space of the
#' fitted rows, and those keep it exactly. Every other combination -- a level
#' for which the variable has no cell with positive weight, under any coding
#' of the factors, an unseen combination of partly aliased factors, or a
#' combination absent from the data -- is filled in the null space of the
#' fitted rows' design (\code{.gloc_estimable}, \code{.gloc_fill_null}):
#' first the combinations that occur in the data, by one least-squares step,
#' then the absent ones, only in the null-space directions that step leaves
#' free, so an absent combination never moves a row that exists. Identification and targets are decided on level
#' combinations (\code{patterns}), not on rows: a probability-weighted row
#' whose combination is unknown (an \code{NA} id) is never a combination
#' itself, and its fitted mean follows from the filled coefficients. The
#' target is the variable's weighted mean over the fitted rows, except that
#' under a design with interaction terms a combination that the main-effects
#' design \code{U_main} identifies on the same rows takes that design's fitted
#' mean. Where several combinations share one unidentified direction, it is
#' their average over the combinations that meets the target: under
#' \code{~ g1 + g2} with a level of \code{g1} never observed, that level's
#' combinations keep the \code{g2} effects between them. The fitted means this
#' gives do not depend on how the factors are coded or ordered, nor on whether
#' combinations absent from the data are enumerated. Aliased columns that change no fitted
#' mean (two identical factors) warn as duplicates, naming neither a column
#' that is zero on every fitted row nor one of an unidentified combination.
#' Unidentified combinations that occur in the data (all of them, when some
#' row's combination is unknown) warn once per reason, naming the variables,
#' the combinations and what the fit does with them. With \code{warn = FALSE}
#' the diagnostics are returned instead, and \code{imputeCellGLoc} raises them
#' once, from its final iterate.
#'
#' Correction, recorded rather than deleted: until 7.4.1 a rank-deficient
#' design silently kept only a weighted mean in \code{B[1, j]}, assuming column
#' 1 is the intercept. With a factor duplicated, the x1 group means came back
#' flat at 0.39 against 0.17, 3.92 and -3.15, and \code{diag(Sigma)} was 8.5
#' against 0.87; with a level that never records x1 they came back flat at
#' 15.44 under \code{~ f}, and 14.97, 0 and 0 under \code{~ f - 1}. The first
#' 7.4.1 fix filled design columns one at a time and recognised an unobserved
#' level only as an all-zero column. It missed a never-observed reference level
#' under treatment coding (level a came back 30.72 against a truth of 10),
#' divided by zero under sum contrasts, and sent a combination missing only
#' from an interaction to the grand mean (14.03 against 30).
#'
#' @param X \eqn{n x p} numeric matrix of continuous variables, may contain NA.
#' @param U \eqn{n x q} design matrix from \code{.gloc_design}.
#' @param W \eqn{n x p} matrix of cell weights in \[0, 1\].
#' @param U_main the main-effects version of \code{U}, from
#'   \code{.gloc_design_aux}; the default, \code{U} itself, stands for a design
#'   without interaction terms.
#' @param patterns the level combinations, from \code{.gloc_design_aux}: each
#'   row's combination (\code{id}, \code{NA} where a row's categories are
#'   unknown), the pure design row of each combination (\code{P}, and
#'   \code{P_main} for \code{U_main}) and its \code{labels}. \code{NULL} takes
#'   every distinct row of \code{U} as a combination
#'   (\code{.gloc_row_patterns}).
#' @param warn \code{TRUE} raises the design warnings here; \code{FALSE}
#'   attaches them to the result as the attribute \code{"gloc_design"}, when
#'   there is anything to report, for \code{.gloc_warn_design}.
#' @return a \eqn{q x p} matrix of coefficients.
#' @keywords internal
.gloc_update_B <- function(X, U, W, U_main = U, patterns = NULL, warn = TRUE) {
  p <- ncol(X); q <- ncol(U); n <- nrow(U)
  B <- matrix(0, q, p, dimnames = list(colnames(U), colnames(X)))
  vn <- .gloc_names(colnames(X), p)
  un <- .gloc_names(colnames(U), q)
  use_main <- !is.null(U_main) && !identical(U_main, U)
  aliased <- nonid <- vector("list", p)
  few <- character(0)
  for (j in seq_len(p)) {
    w  <- W[, j]
    ok <- is.finite(X[, j]) & is.finite(w) & w > 0
    if (sum(ok) <= q) {
      if (sum(ok) == 0L) {
        warning(sprintf(
          "cellGLoc: column '%s' has no cell with positive weight; using the unweighted median as a deterministic fallback (0 if no finite value exists)",
          vn[j]), call. = FALSE)
        finite_x <- X[is.finite(X[, j]), j]
        B[, j] <- .gloc_const_coef(U, if (length(finite_x)) stats::median(finite_x) else 0)
      } else {
        # No count in the text: the deduplicating handler in imputeCellGLoc
        # compares whole messages, and a count that moves between iterations
        # made one reason into several warnings.
        few <- c(few, vn[j])
        B[, j] <- .gloc_const_coef(U, stats::median(X[ok, j]))
      }
      next
    }
    sw <- sqrt(w[ok])
    A  <- U[ok, , drop = FALSE] * sw
    qa <- qr(A, tol = 1e-7)                        # what qr.solve() does
    b  <- qr.coef(qa, X[ok, j] * sw)
    # Identification is decided on the cells whose weight exceeds .gloc_w_floor
    # times the column's largest. Full rank on them keeps the full-rank result
    # just computed, tiny cells included, bit for bit; rank deficient on them,
    # the fill path runs on them alone. (Corrected: until the third 7.4.1 fix
    # round the floor acted only when the fit on all cells was already rank
    # deficient, so whether a level at weight 1e-9 was fitted or filled
    # depended on the coding and on whether another level was observed.)
    live <- ok & w > .gloc_w_floor * max(w[ok])
    if (!all(live == ok)) {
      ql <- qr(U[live, , drop = FALSE] * sqrt(w[live]), tol = 1e-7)
      if (ql$rank < q || qa$rank < q) {
        ok <- live
        sw <- sqrt(w[ok])
        qa <- ql
        b  <- qr.coef(qa, X[ok, j] * sw)
      }
    }
    if (qa$rank == q) {                            # full rank: qr.solve() exactly
      B[, j] <- b
      next
    }
    # level combinations, computed once per call (.gloc_row_patterns)
    if (is.null(patterns)) patterns <- .gloc_row_patterns(U, if (use_main) U_main)
    if (is.null(patterns$shown)) patterns$shown <- .gloc_shown(patterns)
    if (is.null(patterns$present)) patterns$present <- .gloc_present(patterns)
    dropped <- which(is.na(b))
    b[dropped] <- 0
    est <- .gloc_estimable(qa, patterns, ok)
    if (length(est$first)) {
      k <- nrow(patterns$P)
      target <- rep(stats::weighted.mean(X[ok, j], w[ok]), k)
      from_main <- logical(k)
      if (use_main && !is.null(patterns$P_main) && sum(ok) > ncol(U_main)) {
        # the main-effects fit on the same rows and weights; where it does not
        # identify a combination either, the target stays the weighted mean
        am <- qr(U_main[ok, , drop = FALSE] * sw, tol = 1e-7)
        bm <- qr.coef(am, X[ok, j] * sw)
        bm[is.na(bm)] <- 0
        from_main <- if (am$rank < ncol(U_main))
          !.gloc_estimable(am, list(id = patterns$id, P = patterns$P_main), ok)$nonest
          else rep(TRUE, k)
        target[from_main] <- drop(patterns$P_main[from_main, , drop = FALSE] %*% bm)
      }
      b <- .gloc_fill_null(b, patterns$P, est, target, patterns$present)
      shown <- est$nonest & patterns$shown
      if (any(shown))
        nonid[[j]] <- list(main = sort(patterns$labels[shown & from_main]),
                           mean = sort(patterns$labels[shown & !from_main]))
    }
    # Duplicate columns. Never one that is zero on every fitted row. When every
    # unidentified combination is absent from the data (two identical factors,
    # say), the rank deficiency the data show is pure aliasing. Otherwise the
    # not-identifiable warning covers it, and a duplicate is reported only for
    # a null direction that no combination shows, and never as a column of an
    # unidentified combination that occurs in the data.
    shown_ne <- est$nonest & patterns$shown
    cand <- dropped[colSums(U[ok, dropped, drop = FALSE] != 0) > 0]
    if (any(shown_ne)) {
      cand <- if (est$pure)
        cand[colSums(abs(patterns$P[shown_ne, cand, drop = FALSE])) == 0] else integer(0)
    }
    if (length(cand)) aliased[[j]] <- cand
    B[, j] <- b
  }
  if (length(few))
    warning(sprintf(paste("cellGLoc: fewer cells with positive weight than design",
                          "columns (%d) for %s; each such variable's fitted mean is",
                          "the median of those cells for every row, without group",
                          "effects."), q, paste(few, collapse = ", ")), call. = FALSE)
  diag <- list(aliased = aliased, nonid = nonid, vn = vn, un = un)
  if (warn) {
    .gloc_warn_design(diag)
  } else if (!all(vapply(c(aliased, nonid), is.null, TRUE))) {
    attr(B, "gloc_design") <- diag
  }
  B
}

#' Name vectors that are never empty
#'
#' @param nm a character vector or \code{NULL}.
#' @param k the length it should have.
#' @return \code{nm} with missing or empty entries replaced by their position.
#' @keywords internal
.gloc_names <- function(nm, k) {
  if (is.null(nm)) nm <- rep("", k)
  bad <- is.na(nm) | !nzchar(nm)
  nm[bad] <- as.character(seq_len(k))[bad]
  nm
}

#' Coefficients that put a constant through the design
#'
#' Exactly \code{m} in the intercept column when the design has one, leaving
#' every other coefficient 0; otherwise the least-squares coefficients that
#' reproduce \code{m} on every row of \code{U}, with aliased columns at 0. For
#' a factor coded without an intercept the level dummies sum to one, so the
#' constant is reproduced exactly.
#'
#' @param U \eqn{n x q} design matrix.
#' @param m a number.
#' @return a length-\eqn{q} numeric vector.
#' @keywords internal
.gloc_const_coef <- function(U, m) {
  q <- ncol(U)
  b <- numeric(q)
  icol <- which(colSums(U != 1) == 0L)[1L]
  if (!is.na(icol)) { b[icol] <- m; return(b) }
  if (!nrow(U) || m == 0) return(b)
  b <- qr.coef(qr(U), rep(m, nrow(U)))
  b[is.na(b)] <- 0
  b
}

#' Tolerance for a design row to count as identified by the fitted rows
#'
#' A row \eqn{u} of the design is identified when it lies in the row space of
#' the design rows a variable is fitted from, and it is treated as such when
#' its component in the null space, the Euclidean length of \eqn{N'u} for an
#' orthonormal basis \eqn{N}, is at most this value times the larger of 1 and
#' the length of \eqn{u}. The test is
#' relative to the row's own length, so it does not depend on how the
#' contrasts scale the design columns. An identified row of a factor design
#' lies in that row space exactly, so its component is rounding error (of the
#' order of 1e-15 times the condition of the fitted design); an unidentified
#' row lies at a distance of the order of its own entries. 1e-6 is far from
#' both. The same threshold, relative to the longest such row, decides which
#' singular values count in \code{.gloc_fill_null}.
#' @format a length-one numeric.
#' @keywords internal
.gloc_est_tol <- 1e-6

#' Orthonormal basis of the null space of a rank-deficient QR decomposition
#'
#' From the pivoted decomposition \eqn{A P = Q R} of rank \eqn{r}, the columns
#' of \eqn{P (-R_{11}^{-1} R_{12}, I)'} span the null space of \eqn{A}; they
#' are then orthonormalised. The diagonal of \eqn{R_{11}} has no zero, which is
#' what the rank decision guarantees.
#' @param qa a \code{qr()} object.
#' @param q the number of columns.
#' @return a \eqn{q x (q - r)} matrix.
#' @keywords internal
.gloc_null_basis <- function(qa, q) {
  r <- qa$rank
  if (r >= q) return(matrix(0, q, 0L))
  if (r == 0L) return(diag(q))
  Rm <- qr.R(qa)
  top <- seq_len(r)
  K <- rbind(-backsolve(Rm[top, top, drop = FALSE],
                        Rm[top, r + seq_len(q - r), drop = FALSE]),
             diag(q - r))
  N <- matrix(0, q, q - r)
  N[qa$pivot, ] <- K
  qr.Q(qr(N))
}

#' Level combinations taken from the distinct rows of a design
#'
#' The fallback when a caller passes no \code{patterns} (direct calls of the
#' internal functions): every distinct row of \code{U} counts as a level
#' combination. A probability-weighted row would then count as a combination
#' of its own, so a caller with such rows must pass \code{patterns} with an
#' \code{NA} id for them. The row keys are built once per call, not once per
#' column.
#' @param U \eqn{n x q} design matrix.
#' @param U_main the main-effects design, or \code{NULL}.
#' @return a \code{patterns} list as described in \code{.gloc_design_aux}.
#' @keywords internal
.gloc_row_patterns <- function(U, U_main = NULL) {
  key <- do.call(paste, c(unname(split(U, col(U))), sep = "\r"))
  first <- which(!duplicated(key))
  P <- U[first, , drop = FALSE]
  rownames(P) <- NULL
  P_main <- NULL
  if (!is.null(U_main)) {
    P_main <- U_main[first, , drop = FALSE]
    rownames(P_main) <- NULL
  }
  list(id = match(key, key[first]), P = P, P_main = P_main,
       labels = .gloc_pattern_names(P, .gloc_names(colnames(U), ncol(U))),
       shown = rep(TRUE, length(first)))
}

#' Which level combinations a warning may name
#'
#' Those that occur in the data, or all of them when some row's categories
#' are unknown (an \code{NA} id), since such a row may belong to any. A
#' combination absent from the data is filled like any other but not named.
#' @param pat a \code{patterns} list.
#' @return a logical vector, one element per combination.
#' @keywords internal
.gloc_shown <- function(pat) {
  k <- nrow(pat$P)
  if (anyNA(pat$id)) return(rep(TRUE, k))
  tabulate(pat$id, nbins = k) > 0L
}

#' Which level combinations occur in the data
#'
#' A combination is present when some row with a known combination (an id
#' that is not \code{NA}) has it. \code{.gloc_fill_null} fills present
#' combinations first and absent ones only in what that leaves free.
#' @param pat a \code{patterns} list.
#' @return a logical vector, one element per combination.
#' @keywords internal
.gloc_present <- function(pat) {
  tabulate(pat$id[!is.na(pat$id)], nbins = nrow(pat$P)) > 0L
}

#' Which level combinations the fitted rows identify
#'
#' A combination is identified when a fitted row has it, or when its pure
#' design row lies in the row space of the fitted rows (see
#' \code{.gloc_est_tol}). This is decided on combinations, not on rows: a
#' probability-weighted row with an \code{NA} id adds to the row space when it
#' is fitted, but is never a combination itself. (Corrected: the first 7.4.1
#' fix decided on the rows of the design, so 50 rows (0.1, 0.9, 0) sent the
#' unobserved level a to 71.02 against a target of 25.11, and rows
#' (5e-7, 0.5, 0.5) counted as identified.)
#' @param qa the \code{qr()} of the (weighted) design rows a fit used.
#' @param pat a list with \code{id}, each row's combination (\code{NA} if
#'   unknown), and \code{P}, the pure design row of each combination.
#' @param fitted logical, the rows the fit used.
#' @return a list: \code{N}, the null-space basis; \code{nonest}, logical, the
#'   combinations not identified; \code{first}, their indices; and \code{pure},
#'   whether some null direction is invisible on every such combination
#'   (singular values counted as in \code{.gloc_fill_null}).
#' @keywords internal
.gloc_estimable <- function(qa, pat, fitted) {
  P  <- pat$P
  N  <- .gloc_null_basis(qa, ncol(P))
  PN <- P %*% N
  rn <- sqrt(rowSums(P^2))
  far <- sqrt(rowSums(PN^2)) > .gloc_est_tol * pmax(1, rn)
  seen <- logical(nrow(P))
  ids <- pat$id[fitted]
  seen[ids[!is.na(ids)]] <- TRUE
  nonest <- far & !seen
  first <- which(nonest)
  d <- if (length(first)) svd(PN[first, , drop = FALSE], nu = 0L, nv = 0L)$d else numeric(0)
  list(N = N, nonest = nonest, first = first,
       pure = sum(d > .gloc_est_tol * max(1, rn[first])) < ncol(N))
}

#' Fill the level combinations that the fitted rows do not identify
#'
#' Returns \eqn{b = b_0 + N c}. Identified combinations keep their fitted
#' means, because \eqn{p N = 0} for their pure design rows \eqn{p}; a row whose
#' categories are unknown gets whatever its design row gives with the filled
#' coefficients. The fill runs in two stages. First, \eqn{c} is the
#' minimum-norm least-squares solution of \eqn{p (b_0 + N c) = t_p} over the
#' unidentified combinations that occur in the data. Second, the unidentified
#' combinations absent from the data are fitted the same way, but only in the
#' directions of the null space that the first stage leaves free, which are
#' invisible on every combination of the first stage. An absent combination
#' therefore never changes the fitted mean of a row that exists, and the
#' same data give the same fitted means whether or not absent combinations
#' are enumerated (\code{.gloc_max_patterns}). (Corrected: until the third
#' 7.4.1 fix round both kinds entered one least-squares step, and absent
#' combinations moved present ones: under partial aliasing imputed values
#' changed by up to 1.80.) Each stage divides only by singular values above
#' \code{.gloc_est_tol} times the length of the longest pure row involved, so
#' finite input gives finite coefficients under any contrasts. With no
#' unidentified combination, \eqn{b_0} is returned as it is.
#' @param b0 length-\eqn{q} coefficients of the fit, 0 in the aliased columns.
#' @param P the pure design rows of the combinations.
#' @param est the result of \code{.gloc_estimable}.
#' @param target one target value per combination; only unidentified ones are
#'   read.
#' @param present logical, one element per combination, from
#'   \code{.gloc_present}.
#' @return length-\eqn{q} coefficients.
#' @keywords internal
.gloc_fill_null <- function(b0, P, est, target, present = rep(TRUE, nrow(P))) {
  if (!length(est$first)) return(b0)
  rn <- sqrt(rowSums(P^2))
  # one minimum-norm least-squares step in the directions Nd; also returns the
  # directions of Nd that the combinations in rows leave free
  stage <- function(b, Nd, rows) {
    PN <- P[rows, , drop = FALSE] %*% Nd
    s <- svd(PN, nv = ncol(Nd))
    kk <- which(s$d > .gloc_est_tol * max(1, rn[rows]))
    rhs <- target[rows] - drop(P[rows, , drop = FALSE] %*% b)
    cc <- s$v[, kk, drop = FALSE] %*% (crossprod(s$u[, kk, drop = FALSE], rhs) / s$d[kk])
    list(b = b + drop(Nd %*% cc),
         free = Nd %*% s$v[, setdiff(seq_len(ncol(Nd)), kk), drop = FALSE])
  }
  pres <- est$first[present[est$first]]
  abs_ <- est$first[!present[est$first]]
  b <- b0
  Nd <- est$N
  if (length(pres)) {
    st1 <- stage(b, Nd, pres)
    b <- st1$b
    Nd <- st1$free
  }
  if (length(abs_) && ncol(Nd)) b <- stage(b, Nd, abs_)$b
  b
}

#' Name design rows by their non-zero columns
#'
#' Used in warnings when no level labels are available.
#' @param Urows rows of a design matrix.
#' @param un design-column names.
#' @return one string per row.
#' @keywords internal
.gloc_pattern_names <- function(Urows, un) {
  apply(Urows, 1L, function(u) {
    nz <- which(u != 0)
    paste(paste0(un[nz], "=", signif(u[nz], 4)), collapse = ", ")
  })
}

#' One warning per kind of rank deficiency in the mean structure
#'
#' @param d the diagnostics of \code{.gloc_update_B}: \code{aliased}, one
#'   element per variable, the aliased design columns that are duplicates
#'   (neither all-zero on the fitted rows nor part of an unidentified
#'   combination); \code{nonid}, one element per variable, \code{NULL} or a
#'   list with the unidentified level combinations filled from the
#'   main-effects fit (\code{main}) and those filled towards the weighted mean
#'   (\code{mean}); and \code{vn}, \code{un}, the variable and design-column
#'   names. \code{imputeCellGLoc} raises them once, from the final iterate.
#' @keywords internal
.gloc_warn_design <- function(d) {
  aliased <- d$aliased; nonid <- d$nonid; vn <- d$vn; un <- d$un
  keys <- vapply(aliased, function(k) paste(un[sort(k)], collapse = ", "), "")
  for (key in setdiff(unique(keys), "")) {
    warning(sprintf(paste("cellGLoc: design column(s) %s duplicate other design",
                          "columns on the rows used to fit %s, so their",
                          "coefficients are not identifiable; they are left out,",
                          "which leaves the fitted mean unchanged for every row",
                          "whose design pattern occurs among those rows."),
                    key, paste(vn[keys == key], collapse = ", ")), call. = FALSE)
  }
  combos <- function(x, k = 6L) {
    s <- sprintf("(%s)", x)
    if (length(s) > k) s <- c(s[seq_len(k)], sprintf("and %d more", length(s) - k))
    paste(s, collapse = "; ")
  }
  nk <- vapply(nonid, function(z) if (is.null(z)) "" else
    paste(c(z$main, "\n", z$mean), collapse = "\r"), "")
  for (key in setdiff(unique(nk), "")) {
    z <- nonid[[which(nk == key)[1L]]]
    what <- c(
      if (length(z$main))
        sprintf(paste("%s take the fitted mean of the main-effects design (the",
                      "design without its interaction terms) on the same rows"),
                combos(z$main)),
      if (length(z$mean))
        sprintf(paste("%s take, on average, the variable's weighted mean over",
                      "the rows it was estimated from, with the identified",
                      "effects kept between them"), combos(z$mean)))
    warning(sprintf(paste("cellGLoc: the mean of %s is not identifiable in level",
                          "combination(s) with no cell of positive weight: %s."),
                    paste(vn[nk == key], collapse = ", "), paste(what, collapse = "; ")),
            call. = FALSE)
  }
  invisible(NULL)
}

#' Relative weight below which a cell does not decide identification
#'
#' In every fit, \code{.gloc_update_B} decides which level combinations are
#' identified on the cells whose weight exceeds this fraction of the column's
#' largest. When the design is full rank on those cells, the fit on all cells
#' with positive weight is kept bit for bit, tiny cells included; when it is
#' rank deficient on them, the fill path runs on them alone. A level whose
#' weights are all at or below the floor is therefore filled, whatever the
#' coding and whether or not other levels are observed. (Corrected, twice:
#' with level b at weight 1e-16 the pivoted QR kept b under treatment coding
#' and dropped it under sum and Helmert coding, and the codings returned
#' 20.07, 39.52 and 80.27 for b; after a first fix the floor acted only in fits
#' that were already rank deficient, so b at 1e-9 was fitted, 20.05, when
#' level a was observed and filled, 35.06, when it was not. The soft corner
#' reaches weights of 5.4e-29 in the simulation designs.)
#' @format a length-one numeric.
#' @keywords internal
.gloc_w_floor <- 1e-8

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
#' \eqn{h = 0.05} is about 1.2% of clean Gaussian cells and at \eqn{h = 0.25}
#' would be nearer 10%.
#'
#' 0.05 sits above the steepness bound and below the fringe bound. Measured on
#' a sweep of one cell's weight across the threshold (the portable test data,
#' drawn as \code{Z %*% chol(S)}), the largest one-step jump it leaves in a
#' row-mate's standardised residual is 0.0071, against 0.225 for the hard cut,
#' a factor of 32, with identical values outside the band. (Corrected: this
#' page quoted 0.0035 against 0.110, figures that matched neither the earlier
#' \code{MASS::mvrnorm} test data nor the current ones.)
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
#' moves by up to about 20% per fit in \emph{either} direction (largest
#' relative changes -19.9% and +20.7%), and 22% of fits move by more than
#' 1% of their error. The direction is a coin flip: 48.5% of fits move
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
#' is a handful of rows, and the whole function is about 1% of an iteration.
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

#' Fixed random-number state for the robust start's S-estimator fallback
#'
#' A valid Mersenne-Twister \code{.Random.seed} vector, built by a linear
#' congruential recursion so that constructing it draws no random numbers.
#' The start's main path, an L1 fit followed by an M-step, draws none either.
#' Only its fallback, \code{robustbase::lmrob}'s S-estimator, subsamples. It
#' installs this vector for that and restores an existing stream afterwards,
#' so the fallback is deterministic and does not desynchronise paired
#' simulation arms.
#'
#' Correction, recorded rather than deleted: this page used to present the S
#' step as the start's normal path and to say the caller's stream was left
#' untouched. The S step is the fallback, and "untouched" held only in a
#' session that already had a stream: robustbase's S path and
#' \code{cellWise::cellMCD} create \code{.Random.seed} when there is none.
#' \code{imputeCellGLoc} now removes a \code{.Random.seed} that it created.
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
#' default \eqn{\alpha = 0.75} that is 25%, which 20% missingness plus a few
#' percent of shifted cells already exceeds: in the 7.4.1 pilot (n = 200, six
#' continuous columns, 20% missing) the robust start fell back to its
#' MAD-threshold flags in 74 of its 180 fits, all of them at
#' \eqn{\epsilon \ge 0.10}, 67 with shifts of 6 or 10 and 7 with a shift of 3,
#' and in none at \eqn{\alpha = 0.5}. (Corrected: this page first said 74 of
#' 360 fits, all with shifts of 6 or 10.) The start therefore runs cellMCD at
#' this value, and the \code{alpha} argument of \code{imputeCellGLoc} keeps
#' governing the binary corner only.
#' @keywords internal
.gloc_start_alpha <- 0.5

#' Robust starting values for the cellGLoc soft corner
#'
#' Until 7.4.0 the soft corner started from a classical fit: every observed
#' cell at weight 1 and \eqn{B} by ordinary least squares. A redescending weight
#' function started there can settle on a masked solution. This start fits each
#' continuous column on the categorical design \emph{alone}, along
#' \code{robustbase::lmrob}'s M-S path: an L1 regression
#' (\code{robustbase::lmrob.lar}) followed by an M-step with the bisquare at the
#' L1 fit's residual scale (\code{method = "lM"}). The predictors are dummies,
#' which cannot carry a contaminated continuous cell, so casewise robustness is
#' exactly what is needed: a contaminated cell is an outlying response. The
#' starting flags are then those of \code{cellWise::cellMCD} on the residuals
#' \eqn{X - U B}, at \code{.gloc_start_alpha}. (Corrected: this page first called
#' the fit "MM regression". There is no S step on this path, so it is not the
#' MM estimator.)
#'
#' The path is exactly what
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
#' Each response is centred by its median before the fit, because lmrob's
#' stopping rules are relative to the size of the coefficients, and the median
#' is put back through the design: into the intercept column when the design
#' has one, which leaves every other coefficient bit for bit as fitted, and
#' otherwise as the coefficients that reproduce the constant on the observed
#' rows. (Corrected: it used to be added to the first design column, assumed to
#' be the intercept. With \code{design = ~ f - 1} that column is a level's
#' dummy, and the start's fitted means came out 10.25, 0.37 and 10.16 against a
#' truth of 10, 20 and 30, after which the fit flagged 210 cells, 172 of them
#' clean, without a warning.)
#'
#' A design that is rank deficient on a column's observed rows, an aliased
#' factor say, is fitted on its non-aliased columns as chosen by \code{qr()}'s
#' pivoting, and the aliased columns get coefficient 0. (Corrected: this used to
#' surface as "robustbase::lmrob did not converge", naming the symptom.) Level
#' combinations that no observed row identifies are then filled by the rule of
#' the mean step (\code{.gloc_update_B}): towards the column median, or, under a
#' design with interaction terms, towards the robust main-effects start
#' wherever that identifies the combination. (Corrected: the first 7.4.1 fix
#' filled design columns one at a time, which missed a never-observed reference
#' level under treatment coding and divided by zero under sum contrasts.)
#'
#' Every degraded path warns, once per reason, naming the columns: too few
#' observed rows for the design (fewer than \eqn{2q}, or a non-constant design
#' column with fewer than three observed rows), a rank-deficient design, an
#' \code{lmrob} error, or \code{lmrob} not converging from either start. The
#' first and the last two fall back to the column median as the fitted mean,
#' without group effects. Without \code{cellWise}, or if \code{cellMCD} fails, a
#' cell is flagged when \eqn{|r_{ij}| / \mathrm{MAD}(r_{.j})} exceeds
#' \eqn{\sqrt{\chi^2_{1,0.99}}}. What \code{cellMCD} prints when it refuses is
#' captured, so a failure is reported once, as a warning, and not also as six
#' lines on the console.
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
#' @param warn_design whether to warn here about a design that is rank deficient
#'   on a column's observed rows. \code{imputeCellGLoc} passes \code{FALSE}:
#'   its mean step (\code{.gloc_update_B}) warns about the same design once, and
#'   says what the returned fit does.
#' @param U_main the main-effects version of \code{U} (see
#'   \code{.gloc_update_B}); the default, \code{U} itself, stands for a design
#'   without interaction terms.
#' @param patterns the level combinations; see \code{.gloc_update_B}.
#' @param fit_rows optional logical vector over the rows: the per-column fits
#'   use only these rows, while the residuals flagged afterwards cover every
#'   row. \code{imputeCellGLoc} passes the rows without a missing categorical
#'   cell when \code{categorical = "em"}.
#' @return a list with \code{B} (\eqn{q x p}), \code{W} (\eqn{n x p}, 0 or 1,
#'   0 on missing cells) and \code{S}, the scatter \code{cellWise::cellMCD}
#'   returned for the residuals (\eqn{p x p}), or \code{NULL} when the flags did
#'   not come from \code{cellMCD}. The categorical EM takes the penalty of its
#'   two-start selection from \code{S}; see \code{.gloc_lambda}.
#' @keywords internal
.gloc_start_robust <- function(X, U, M, alpha = .gloc_start_alpha,
                               have_cw = requireNamespace("cellWise",
                                                          quietly = TRUE),
                               control = NULL, warn_design = TRUE, U_main = U,
                               patterns = NULL, fit_rows = NULL) {
  n <- nrow(X); p <- ncol(X); q <- ncol(U)
  use_main <- !is.null(U_main) && !identical(U_main, U)
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
  # Coefficients that reproduce the constant m on the rows of Uo: exactly
  # m in the intercept column icol when there is one, otherwise least squares
  # on the design, with aliased columns at 0.
  const_coef <- function(Uo, m, icol) {
    b <- numeric(ncol(Uo))
    if (!is.na(icol)) { b[icol] <- m; return(b) }
    if (!nrow(Uo) || m == 0) return(b)
    b <- qr.coef(qr(Uo), rep(m, nrow(Uo)))
    b[is.na(b)] <- 0
    b
  }
  # One robust fit of the response y on the design Ud over the rows ok: the
  # coefficients, the median m0 that a failed fit falls back to, the QR of the
  # observed design rows (NULL when too thin to fit), whether those are rank
  # deficient, and a status "thin", "failed", "noconv" or "" (fitted). Used for
  # the design itself and, under an interaction design, for the main-effects
  # start that unidentified combinations are filled from.
  col_fit <- function(Ud, ok, y) {
    qd <- ncol(Ud)
    # A column of ones, if the design has one, and the non-constant columns.
    icol  <- which(colSums(Ud != 1) == 0L)[1L]
    dummy <- colSums(Ud != 1) > 0L
    Uo <- Ud[ok, , drop = FALSE]
    # A design column with no observed row (a level for which this variable is
    # never recorded) is not identifiable and is filled afterwards. Only
    # columns observed in one or two rows make the design too thin to fit;
    # until the re-review an empty column counted as thin too, and the whole
    # variable started without group effects.
    nobs_col <- colSums(Uo[, dummy, drop = FALSE] != 0)
    thin <- sum(ok) < 2L * qd || any(nobs_col > 0L & nobs_col < 3L)
    fit <- qr_o <- NULL
    cols <- seq_len(qd)
    status <- ""
    if (thin) {
      status <- "thin"
    } else {
      qr_o <- qr(Uo)
      if (qr_o$rank < qd) cols <- sort(qr_o$pivot[seq_len(qr_o$rank)])
      Uf <- Uo[, cols, drop = FALSE]
      # Centre the response by its median first and put the median back through
      # the design afterwards (see const_coef). lmrob's stopping rules are
      # relative to the size of the coefficients, so on data shifted by +1000
      # the uncentred fit stopped earlier: its intercept moved by up to 3.3e-4
      # and the final weights by 8e-6, breaking the shift equivariance the
      # convergence test pins at 1e-6.
      yj <- y[ok]
      mj <- stats::median(yj)
      yc <- yj - mj
      # L1 start, then the M-step: robustbase's own M-S path for a design with
      # no continuous predictor. bare.only skips the covariance, which is not
      # used here and whose computation only warns after a non-S start.
      fit <- quiet({
        ctrl_l <- control; ctrl_l$method <- "lM"
        robustbase::lmrob.fit(Uf, yc, control = ctrl_l, bare.only = TRUE,
                              init = robustbase::lmrob.lar(Uf, yc,
                                                           control = ctrl_l))
      })
      if (!(usable(fit) && isTRUE(fit$converged))) {
        # fallback: lmrob's default S-estimator start
        fit_s <- quiet(robustbase::lmrob.fit(Uf, yc, control = control,
                                             bare.only = TRUE))
        if (usable(fit_s) && isTRUE(fit_s$converged)) {
          fit <- fit_s
        } else {
          status <- if (usable(fit) || usable(fit_s)) "noconv" else "failed"
          fit <- NULL
        }
      }
    }
    b <- numeric(qd)
    if (is.null(fit)) {
      m0 <- if (any(ok)) stats::median(y[ok]) else 0
      b <- const_coef(Uo, m0, icol)
    } else {
      m0 <- mj
      b[cols] <- fit$coefficients
      b <- b + const_coef(Uo, mj, icol)          # undo the centring
    }
    list(b = b, m0 = m0, qr = qr_o, status = status,
         rankdef = !is.null(qr_o) && qr_o$rank < qd)
  }

  few <- failed <- noconv <- rankdef <- character(0)
  for (j in seq_len(p)) {
    ok <- !M[, j] & is.finite(X[, j])
    if (!is.null(fit_rows)) ok <- ok & fit_rows
    cf <- col_fit(U, ok, X[, j])
    if (cf$rankdef && warn_design) rankdef <- c(rankdef, cnames[j])
    if (cf$status == "thin") few <- c(few, cnames[j])
    if (cf$status == "noconv") noconv <- c(noconv, cnames[j])
    if (cf$status == "failed") failed <- c(failed, cnames[j])
    b <- cf$b
    # A level combination that no observed row identifies -- a level for which
    # this variable is never recorded, under any coding, or an unseen
    # combination of partly aliased factors -- is filled by the mean step's
    # rule (.gloc_update_B): towards the column median, or, under a design
    # with interactions, towards the robust main-effects start wherever that
    # identifies the combination.
    qr_o <- if (!any(ok)) NULL else if (is.null(cf$qr)) qr(U[ok, , drop = FALSE]) else cf$qr
    if (!is.null(qr_o) && qr_o$rank < q) {
      if (is.null(patterns)) patterns <- .gloc_row_patterns(U, if (use_main) U_main)
      if (is.null(patterns$present)) patterns$present <- .gloc_present(patterns)
      est <- .gloc_estimable(qr_o, patterns, ok)
      if (length(est$first)) {
        k <- nrow(patterns$P)
        target <- rep(cf$m0, k)
        # Main-effects targets only for a start that fitted its group effects:
        # a thin, failed or non-converged fit falls back to the median without
        # group effects, as its warning says. (Corrected: the first 7.4.1 fix
        # applied them anyway, and a thin start gave (c, C) 29.96 against the
        # median 10.87 everywhere else.)
        if (use_main && !nzchar(cf$status) && !is.null(patterns$P_main)) {
          cm <- col_fit(U_main, ok, X[, j])
          if (!nzchar(cm$status)) {
            from_main <- if (cm$rankdef)
              !.gloc_estimable(cm$qr, list(id = patterns$id, P = patterns$P_main),
                               ok)$nonest else rep(TRUE, k)
            target[from_main] <- drop(patterns$P_main[from_main, , drop = FALSE] %*% cm$b)
          }
        }
        b <- .gloc_fill_null(b, patterns$P, est, target, patterns$present)
      }
    }
    B[, j] <- b
  }
  fallback_msg <- "using the column median as the fitted mean, without group effects."
  if (length(few))
    warning(sprintf(paste("cellGLoc: robust start: too few observed rows for",
                          "the design in column(s) %s (fewer than %d rows, or a",
                          "design column with fewer than 3); %s"),
                    paste(few, collapse = ", "), 2L * q, fallback_msg),
            call. = FALSE)
  if (length(rankdef))
    warning(sprintf(paste("cellGLoc: robust start: the design is rank deficient on",
                          "the observed rows of column(s) %s (aliased design",
                          "columns, such as two identical factors, or a level",
                          "with no observed row); fitted on the non-aliased",
                          "columns. A level combination that no observed row",
                          "identifies starts at the column median, or at the",
                          "main-effects start under a design with interactions."),
                    paste(rankdef, collapse = ", ")), call. = FALSE)
  if (length(failed))
    warning(sprintf("cellGLoc: robust start: robustbase::lmrob failed for column(s) %s; %s",
                    paste(failed, collapse = ", "), fallback_msg), call. = FALSE)
  if (length(noconv))
    warning(sprintf(paste("cellGLoc: robust start: robustbase::lmrob did not",
                          "converge for column(s) %s; %s"),
                    paste(noconv, collapse = ", "), fallback_msg), call. = FALSE)

  R <- X - U %*% B
  R[M | !is.finite(X)] <- NA_real_
  W <- S <- NULL
  if (have_cw) {
    cm_err <- NULL
    # cellMCD stops with "mean(): object has no elements" when any row has no
    # observed cell, at every alpha. Such a row has nothing to flag (its cells
    # get weight 0 below), so it is left out of the call. When cellMCD refuses
    # it also prints the per-variable percentages; that output is captured, and
    # the refusal reaches the user once, as the warning below.
    has_obs <- rowSums(is.finite(R)) > 0
    cm <- tryCatch({
      utils::capture.output(
        cm_fit <- cellWise::cellMCD(R[has_obs, , drop = FALSE], alpha = alpha,
                                    checkPars = list(coreOnly = TRUE,
                                                     silent = TRUE)))
      cm_fit
    }, error = function(e) { cm_err <<- conditionMessage(e); NULL })
    if (is.null(cm)) {
      warning(sprintf(paste("cellGLoc: robust start: cellWise::cellMCD() failed",
                            "(%s); the starting flags use a hard threshold on",
                            "|residual| / MAD instead."),
                      gsub("\\s+", " ", trimws(cm_err))), call. = FALSE)
    } else {
      W <- matrix(1, n, p)
      W[has_obs, ] <- as.numeric(cm$W)
      S <- cm$S
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
  list(B = B, W = W, S = S)
}

#' Binary-corner objective of a fit, for choosing between two starts
#'
#' The objective of the binary corner evaluated at a fit: minus twice the
#' Gaussian log-likelihood of the retained residual cells of each row under
#' \code{Sigma}, plus \code{lambda[j]} for every flagged observed cell of column
#' \code{j}. A cell is retained when it is observed and its weight is at least
#' 1/2, so soft weights are dichotomised there, and a row that retains no cell
#' adds only its penalty. This is the objective \code{cellWise::cellMCD()}
#' minimises, with the fitted means \eqn{B' \bar u_i} in place of cellMCD's free
#' centre. cellMCD's penalty also counts missing cells; for two fits of the same
#' data under the same \code{lambda} that part is one constant, so leaving it
#' out changes no comparison.
#'
#' \code{imputeCellGLoc(categorical = "em")} uses it to choose between the fixed
#' points reached from its two starts. It tells a masked fixed point from an
#' unmasked one; it cannot rank two nearly equivalent ones, and it is not a
#' quantity the soft iteration descends.
#'
#' @param R \eqn{n x p} residuals, \code{NA} on missing cells.
#' @param W \eqn{n x p} cell weights.
#' @param M \eqn{n x p} logical mask of missing cells.
#' @param Sigma \eqn{p x p} scatter.
#' @param lambda length-\eqn{p} penalty per flagged cell; see \code{.gloc_lambda}.
#' @return a number, \code{Inf} when \code{Sigma} is not positive definite on
#'   the retained cells of some row.
#' @keywords internal
.gloc_objective <- function(R, W, M, Sigma, lambda) {
  keep <- !M & is.finite(W) & W >= 0.5
  pen <- sum(lambda * colSums(!M & !keep))
  key <- apply(keep, 1L, function(r) paste0(as.integer(r), collapse = ""))
  m2ll <- 0
  for (g in split(seq_len(nrow(R)), key)) {
    obs <- which(keep[g[1L], ])
    if (!length(obs)) next
    L <- tryCatch(chol(Sigma[obs, obs, drop = FALSE]), error = function(e) NULL)
    if (is.null(L)) return(Inf)
    Z <- backsolve(L, t(R[g, obs, drop = FALSE]), transpose = TRUE)
    m2ll <- m2ll + sum(Z^2) +
      length(g) * (2 * sum(log(diag(L))) + length(obs) * log(2 * pi))
  }
  m2ll + pen
}

#' Penalty per flagged cell of the binary-corner objective
#'
#' \eqn{\lambda_j = \chi^2_{1;0.99} + \log 2\pi + \log c_j} with
#' \eqn{c_j = 1 / (S^{-1})_{jj}}, the variance of column \eqn{j} given the
#' others: flagging a cell costs what keeping it at the 99% cut-off would.
#' \code{cellWise::cellMCD()} takes \eqn{c_j} from its initial estimate, a
#' function \pkg{cellWise} does not export. The two-start selection of
#' \code{imputeCellGLoc(categorical = "em")} takes \eqn{S} from the first
#' start's own \code{cellMCD} call (\code{.gloc_start_robust}) and holds it
#' fixed for both candidates. The per-level score of a candidate level
#' (\code{.gloc_cat_score}, since 7.5.1) takes \eqn{S} from the scatter of the
#' current iteration instead, so that the density it trades against the penalty
#' sits on the same scale.
#'
#' A scatter that is singular, or whose inverse has a diagonal entry that is not
#' finite and positive (it is then not positive definite), gives no penalty, and
#' the log is not taken, so no "NaNs produced" warning escapes.
#'
#' @param S a \eqn{p x p} scatter, or \code{NULL}.
#' @return a length-\eqn{p} vector, named when \code{S} has dimnames, or
#'   \code{NULL} when \code{S} is \code{NULL} or singular, when a diagonal entry
#'   of its inverse is not finite and positive, or when a value is not finite.
#' @keywords internal
.gloc_lambda <- function(S) {
  if (is.null(S)) return(NULL)
  Si <- tryCatch(solve(S), error = function(e) NULL)
  if (is.null(Si)) return(NULL)
  dSi <- diag(Si)
  if (!all(is.finite(dSi) & dSi > 0)) return(NULL)
  lam <- stats::qchisq(0.99, df = 1) + log(2 * pi) + log(1 / dSi)
  if (!all(is.finite(lam))) return(NULL)
  lam
}
