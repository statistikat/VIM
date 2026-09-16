# ==========================================================================
# Tests for cellGLoc's categorical EM step (VIM 7.5.0). Spec: design doc §12.
# ==========================================================================

collect_warnings <- function(expr) {
  w <- character(0)
  withCallingHandlers(expr, warning = function(cond) {
    w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning")
  })
  w
}

# Data from the model itself: f shifts x1..x3, g shifts x2 and x3, and g
# depends on f, so the prior for f carries information as well.
gen_cat <- function(n, seed, miss_f = 0.2, miss_g = 0, shift = c(0, 4, -3)) {
  set.seed(seed)
  f <- factor(sample(c("a", "b", "c"), n, TRUE))
  g <- factor(ifelse(runif(n) < ifelse(f == "a", 0.8, 0.3), "u", "v"))
  X <- matrix(rnorm(n * 3), n) %*% chol(0.5 * diag(3) + 0.5)
  X <- X + outer(shift[as.integer(f)], c(1, 1, 0.5)) + outer(2 * (g == "v"), c(0, 1, 1))
  colnames(X) <- paste0("x", 1:3)
  truth <- data.frame(X, f = f, g = g)
  d <- truth
  d$f[runif(n) < miss_f] <- NA
  if (miss_g > 0) d$g[runif(n) < miss_g] <- NA
  list(d = d, truth = truth)
}

# --- preparation ---
sim <- gen_cat(120, 1)
sv <- VIM:::.gloc_split_vars(sim$d)
expect_identical(sv, list(cont = c("x1", "x2", "x3"), cat = c("f", "g")))
cp <- VIM:::.gloc_cat_prepare(sim$d, sv$cat)
expect_identical(dim(cp$Mc), c(120L, 2L))
expect_identical(colnames(cp$Mc), c("f", "g"))
expect_identical(unname(cp$Mc[, "f"]), is.na(sim$d$f))
expect_identical(cp$levels, list(f = c("a", "b", "c"), g = c("u", "v")))
dch <- data.frame(x = c(1.5, 2.5, 3.5, 4.5), s = c("q", NA, "p", "q"),
                  b = c(TRUE, FALSE, NA, TRUE), stringsAsFactors = FALSE)
cpc <- VIM:::.gloc_cat_prepare(dch, c("s", "b"))
expect_identical(cpc$levels, list(s = c("p", "q"), b = c("FALSE", "TRUE")))

# --- on complete data the fixed-level design is .gloc_design, bit for bit ---
cpt <- VIM:::.gloc_cat_prepare(sim$truth, sv$cat)
for (des in list(~ ., ~ 1, ~ f, ~ f - 1, ~ f * g))
  expect_identical(VIM:::.gloc_design_rows(cpt$F, des, cpt$levels),
                   VIM:::.gloc_design(sim$truth, des, sv$cat), info = deparse(des))

# --- a single filled row keeps every level's column ---
one <- cp$F[1, , drop = FALSE]
one$f <- factor("c", levels = cp$levels$f)
u1 <- VIM:::.gloc_design_rows(one, ~ ., cp$levels)
expect_identical(colnames(u1), c("(Intercept)", "fb", "fc", "gv"))
expect_equal(unname(u1[1, "fc"]), 1)

# --- restore keeps the caller's class and declared levels ---
expect_identical(VIM:::.gloc_cat_restore(factor(c("a", "b")),
                                         factor(c("a", NA), levels = c("a", "b", "z"))),
                 factor(c("a", "b"), levels = c("a", "b", "z")))
expect_identical(VIM:::.gloc_cat_restore(factor(c("TRUE", "FALSE")), c(TRUE, NA)),
                 c(TRUE, FALSE))
expect_identical(VIM:::.gloc_cat_restore(factor(c("p", "q")), c("p", NA)), c("p", "q"))

# ==========================================================================
# Task 2: priors
# ==========================================================================
big <- gen_cat(2000, 2, miss_f = 0)
cpb <- VIM:::.gloc_cat_prepare(big$truth, c("f", "g"))
pri <- VIM:::.gloc_cat_fit_priors(cpb$F, rep(1, 2000), cpb$levels)
expect_identical(pri$g$type, "multinom")
expect_identical(pri$g$preds, "f")
# g depends on f: P(g = u | f = a) = 0.8, P(g = u | f = b) = 0.3
nd <- data.frame(f = factor(c("a", "b"), levels = cpb$levels$f),
                 g = factor(c("u", "u"), levels = cpb$levels$g))
Pg <- VIM:::.gloc_cat_prior(pri$g, nd)
expect_identical(dim(Pg), c(2L, 2L))
expect_true(abs(Pg[1, 1] - 0.8) < 0.05 && abs(Pg[2, 1] - 0.3) < 0.05)
Pf <- VIM:::.gloc_cat_prior(pri$f, nd[1, , drop = FALSE])  # three levels, one row
expect_identical(dim(Pf), c(1L, 3L))
expect_equal(sum(Pf), 1, tolerance = 1e-12)
expect_true(all(Pf >= VIM:::.gloc_cat_floor))
# a predictor level the fit never saw: the marginal frequencies, no error
nd2 <- data.frame(f = factor("z", levels = c(cpb$levels$f, "z")),
                  g = factor("u", levels = cpb$levels$g))
expect_equal(as.vector(VIM:::.gloc_cat_prior(pri$g, nd2)), pri$g$probs, tolerance = 1e-8)

# case weights: weight 2 on the first 30 rows equals duplicating them
w2 <- rep(1, 2000); w2[1:30] <- 2
pri_w <- VIM:::.gloc_cat_fit_priors(cpb$F, w2, cpb$levels)
pri_d <- VIM:::.gloc_cat_fit_priors(cpb$F[c(1:2000, 1:30), ], rep(1, 2030), cpb$levels)
expect_equal(VIM:::.gloc_cat_prior(pri_w$f, cpb$F[1:50, ]),
             VIM:::.gloc_cat_prior(pri_d$f, cpb$F[1:50, ]), tolerance = 1e-4)

# a single categorical variable, or a response with one level: marginal, silent
cp1 <- VIM:::.gloc_cat_prepare(big$truth, "f")
expect_silent(pr1 <- VIM:::.gloc_cat_fit_priors(cp1$F, rep(1, 2000), cp1$levels))
expect_identical(pr1$f$type, "marginal")
expect_equal(sum(pr1$f$probs), 1, tolerance = 1e-12)

# a failing fit warns once, naming the variable, and uses weighted frequencies
boom <- function(y, Xdf, w) stop("boom")
wb <- collect_warnings(prb <- VIM:::.gloc_cat_fit_priors(cpb$F, w2, cpb$levels, fit = boom))
expect_equal(length(wb), 1L)
expect_true(grepl("^cellGLoc: ", wb) && grepl("f, g", wb, fixed = TRUE))
expect_identical(prb$f$type, "marginal")
expect_equal(unname(prb$f$probs),
             as.vector(tapply(w2, cpb$F$f, sum)) / sum(w2), tolerance = 1e-12)

# R46 item 2 -- a predictor that carries a contrasts attribute can make
# predict()'s own model.matrix() call warn "contrasts dropped from factor ...
# due to missing levels" when the newdata it predicts on does not itself
# realise every one of that predictor's levels (a "keep" or candidate subset
# of the pseudo-row table routinely does not). The warning is unrelated to
# whether the returned probabilities are right -- predict.multinom() forces
# its own xlevels and contrasts regardless -- and it carries no "cellGLoc: "
# prefix, so it must not reach the user at all.
gp2 <- data.frame(f = factor(rep(c("a", "b", "c"), each = 20)),
                  g = factor(rep(c("u", "v"), 30)))
contrasts(gp2$g) <- contr.sum(2)
prg2 <- VIM:::.gloc_cat_fit_priors(gp2, rep(1, nrow(gp2)),
                                   list(f = c("a", "b", "c"), g = c("u", "v")))
expect_identical(prg2$f$type, "multinom")
newd2 <- gp2[gp2$g == "u", , drop = FALSE][1:5, ]     # realises only one level of g
wp2 <- collect_warnings(Pp2 <- VIM:::.gloc_cat_prior(prg2$f, newd2))
expect_false(any(grepl("contrasts dropped", wp2, fixed = TRUE)))
expect_identical(dim(Pp2), c(5L, 3L))
expect_equal(unname(rowSums(Pp2)), rep(1, 5), tolerance = 1e-10)
# a plain (no contrasts attribute) predictor never had this warning to begin
# with (the hard constraint: this fix must not change that path at all)
gp2b <- gp2; attr(gp2b$g, "contrasts") <- NULL
prg2b <- VIM:::.gloc_cat_fit_priors(gp2b, rep(1, nrow(gp2b)),
                                    list(f = c("a", "b", "c"), g = c("u", "v")))
wp2b <- collect_warnings(Pp2b <- VIM:::.gloc_cat_prior(prg2b$f, gp2b[gp2b$g == "u", ][1:5, ]))
expect_equal(length(wp2b), 0L)
expect_identical(dim(Pp2b), c(5L, 3L))
expect_equal(unname(rowSums(Pp2b)), rep(1, 5), tolerance = 1e-10)

# ==========================================================================
# Task 3: level log-likelihood
# ==========================================================================
S3 <- 0.5 * diag(3) + 0.5
B3 <- rbind(c(0, 0, 0), c(4, 4, 2))               # intercept, one dummy
X3 <- rbind(c(3.5, 4.2, 1.1), c(0.2, -0.1, 0.3))
M3 <- matrix(FALSE, 2, 3)
W3 <- matrix(1, 2, 3)
Uc <- rbind(c(1, 0), c(1, 1), c(1, 0), c(1, 1))   # two candidates per row
ro <- c(1L, 1L, 2L, 2L)
ll <- VIM:::.gloc_cat_loglik(X3, M3, W3, B3, S3, Uc, ro)
q <- function(x, mu, S) -0.5 * drop(t(x - mu) %*% solve(S) %*% (x - mu))
expect_equal(ll[2] - ll[1], q(X3[1, ], B3[2, ] + B3[1, ], S3) - q(X3[1, ], B3[1, ], S3),
             tolerance = 1e-10)

# a flagged peer drops out: its value no longer matters
W3f <- W3; W3f[1, 1] <- 0
X3f <- X3; X3f[1, 1] <- 99
expect_identical(VIM:::.gloc_cat_loglik(X3, M3, W3f, B3, S3, Uc, ro),
                 VIM:::.gloc_cat_loglik(X3f, M3, W3f, B3, S3, Uc, ro))
expect_equal(VIM:::.gloc_cat_loglik(X3, M3, W3f, B3, S3, Uc, ro)[1:2],
             c(q(X3[1, 2:3], B3[1, 2:3], S3[2:3, 2:3]),
               q(X3[1, 2:3], colSums(B3)[2:3], S3[2:3, 2:3])), tolerance = 1e-10)

# a peer inside the band enters with the noise-inflated covariance
W3b <- W3; W3b[1, 2] <- 0.5                        # reliability 0.5 at the band centre
rel <- VIM:::.gloc_peer_rel(!M3, W3b)[1, ]
dd <- sqrt(rel)
G <- outer(dd, dd) * S3 + diag(diag(S3) * (1 - rel))
z <- dd * (X3[1, ] - B3[1, ])
expect_equal(VIM:::.gloc_cat_loglik(X3, M3, W3b, B3, S3, Uc, ro)[1],
             -0.5 * drop(t(z) %*% solve(G) %*% z), tolerance = 1e-10)

# no peer at all: 0; identical candidate rows: identical values
M3n <- M3; M3n[2, ] <- TRUE
expect_identical(VIM:::.gloc_cat_loglik(X3, M3n, W3, B3, S3, Uc, ro)[3:4], c(0, 0))
Ueq <- rbind(c(1, 0), c(1, 0))
lleq <- VIM:::.gloc_cat_loglik(X3, M3, W3, B3, S3, Ueq, c(1L, 1L))
expect_identical(lleq[1], lleq[2])

# ==========================================================================
# Task 4: candidate table and E-step
# ==========================================================================
# Spec §12.3 test 3, the M-step's precondition: scaling a row's cell weights
# is a case weight in cwLocScat (checked 2026-09-15, cellWise 2.5.7).
if (requireNamespace("cellWise", quietly = TRUE)) {
  set.seed(4)
  Xp <- matrix(rnorm(80 * 4), 80) %*% chol(0.5 * diag(4) + 0.5)
  Xp[sample(length(Xp), 30)] <- NA
  Wp <- matrix(runif(80 * 4), 80); Wp[is.na(Xp)] <- 0
  W2 <- Wp; W2[1:15, ] <- 2 * Wp[1:15, ]
  a <- cellWise::cwLocScat(rbind(Xp, Xp[1:15, ]), rbind(Wp, Wp[1:15, ]), crit = 1e-14, maxiter = 10000, lmin = NULL)
  b <- cellWise::cwLocScat(Xp, W2, crit = 1e-14, maxiter = 10000, lmin = NULL)
  expect_equal(a$cwMLEsigma, b$cwMLEsigma, tolerance = 1e-10)
  expect_equal(a$cwMLEmu, b$cwMLEmu, tolerance = 1e-10)
  # ... and again at the settings the estimator itself uses (R43): cw_crit's
  # default 1e-8 with cwLocScat's own maxiter and lmin. lmin is an eigenvalue
  # floor, a nonlinear step that need not commute with case weights, so the
  # property is pinned where it is relied on and not only in a tighter
  # neighbouring configuration. Tolerance fixed before the first run.
  ap <- cellWise::cwLocScat(rbind(Xp, Xp[1:15, ]), rbind(Wp, Wp[1:15, ]),
                            methods = "all", crit = 1e-8)
  bp <- cellWise::cwLocScat(Xp, W2, methods = "all", crit = 1e-8)
  expect_equal(ap$cwMLEsigma, bp$cwMLEsigma, tolerance = 1e-6)
  expect_equal(ap$cwMLEmu, bp$cwMLEmu, tolerance = 1e-6)
}

sim4 <- gen_cat(90, 4, miss_f = 0.2, miss_g = 0.15)
sim4$d$f[1:3] <- NA; sim4$d$g[1:3] <- NA                  # guarantee rows missing both
cp4 <- VIM:::.gloc_cat_prepare(sim4$d, c("f", "g"))
cand4 <- VIM:::.gloc_cat_candidates(cp4, sim4$d, ~ .)
only_f <- cp4$Mc[, "f"] & !cp4$Mc[, "g"]; only_g <- !cp4$Mc[, "f"] & cp4$Mc[, "g"]
both <- cp4$Mc[, "f"] & cp4$Mc[, "g"]; none <- !cp4$Mc[, "f"] & !cp4$Mc[, "g"]
expect_identical(nrow(cand4$Fp),
                 as.integer(sum(none) + 3 * sum(only_f) + 2 * sum(only_g) + 6 * sum(both)))
expect_false(anyNA(cand4$Fp$f) || anyNA(cand4$Fp$g))
expect_identical(cand4$need$f, which(cp4$Mc[cand4$pr_row, "f"]))

# prior-only E-step: posteriors are the priors, weights sum to 1 per row, the
# expected design is the design on complete rows and 1 in the intercept column
pri4 <- VIM:::.gloc_cat_fit_priors(cp4$F[none, ], rep(1, sum(none)), cp4$levels)
X4 <- as.matrix(sim4$d[, 1:3]); M4 <- is.na(X4)
es0 <- VIM:::.gloc_cat_estep(X4, M4, NULL, NULL, NULL, cp4, cand4, pri4)
rf <- which(only_f)
expect_equal(unname(es0$post$f[as.character(rf), ]),
             VIM:::.gloc_cat_prior(pri4$f, cp4$F[rf, ]), tolerance = 1e-12)
expect_equal(as.vector(tapply(es0$pr_w, es0$pr_row, sum)), rep(1, 90), tolerance = 1e-12)
expect_true(all(es0$Ubar[none, ] == VIM:::.gloc_design_rows(cp4$F[none, ], ~ ., cp4$levels)))
expect_true(all(es0$Ubar[, "(Intercept)"] == 1))
expect_identical(rownames(es0$post$f), as.character(which(cp4$Mc[, "f"])))

# pseudo-rows come from the fill's combination table, with the design's columns
expect_identical(colnames(cand4$Up), colnames(VIM:::.gloc_design(sim4$truth, ~ ., c("f", "g"))))
expect_false(anyNA(cand4$pr_c))
expect_true(all(cand4$Up == VIM:::.gloc_design_rows(cand4$Fp, ~ ., cp4$levels)))
inc4 <- rowSums(cp4$Mc) > 0
expect_true(all(is.na(cand4$pats_rows$id[inc4])))
expect_false(anyNA(cand4$pats_rows$id[!inc4]))
# an interaction design carries main-effects rows for the pseudo-rows and the E-step
cand4i <- VIM:::.gloc_cat_candidates(cp4, sim4$d, ~ f * g)
expect_false(is.null(cand4i$Up_main))
es4i <- VIM:::.gloc_cat_estep(X4, M4, NULL, NULL, NULL, cp4, cand4i, pri4)
expect_identical(dim(es4i$Umain_bar), c(90L, ncol(cand4i$Up_main)))
expect_true(all(es4i$Umain_bar[, "(Intercept)"] == 1))

# a design without categorical variables needs no combination table
cand4o <- VIM:::.gloc_cat_candidates(cp4, sim4$d, ~ 1)
expect_true(is.null(cand4o$pat_pr) && is.null(cand4o$pats_rows) &&
              is.null(cand4o$Up_main) && is.null(cand4o$pr_c))
expect_identical(colnames(cand4o$Up), "(Intercept)")
expect_true(all(cand4o$Up == 1))
es4o <- VIM:::.gloc_cat_estep(X4, M4, NULL, NULL, NULL, cp4, cand4o, pri4)
expect_true(all(es4o$Ubar == 1) && is.null(es4o$Umain_bar))

# an ordered factor keeps its contrasts in the pseudo-rows (rows missing f alone and f and g)
d4ord <- sim4$d
d4ord$f <- factor(d4ord$f, levels = c("a", "b", "c"), ordered = TRUE)
t4ord <- sim4$truth
t4ord$f <- factor(t4ord$f, levels = c("a", "b", "c"), ordered = TRUE)
cp4ord <- VIM:::.gloc_cat_prepare(d4ord, c("f", "g"))
cand4ord <- VIM:::.gloc_cat_candidates(cp4ord, d4ord, ~ .)
expect_true(is.ordered(cand4ord$Fp$f))
expect_identical(colnames(cand4ord$Up), colnames(VIM:::.gloc_design(t4ord, ~ ., c("f", "g"))))
expect_true(all(cand4ord$Up == VIM:::.gloc_design_rows(cand4ord$Fp, ~ ., cp4ord$levels)))

# two missing cells whose evidence factorises (diagonal Sigma, f moves x1 only,
# g moves x2 only, marginal priors): the mean-field sweeps are exact
d5 <- data.frame(x1 = c(1.7, 0.1, 2.0, -1.9, 0.3, 2.2, -2.1),
                 x2 = c(2.4, 0.0, 3.1, 0.2, -0.3, 2.9, 0.1),
                 f = factor(c(NA, "a", "b", "c", "a", "b", "c"), levels = c("a", "b", "c")),
                 g = factor(c(NA, "u", "v", "u", "u", "v", "u"), levels = c("u", "v")))
cp5 <- VIM:::.gloc_cat_prepare(d5, c("f", "g"))
cand5 <- VIM:::.gloc_cat_candidates(cp5, d5, ~ .)
pri5 <- list(f = list(type = "marginal", probs = c(0.5, 0.3, 0.2), levels = c("a", "b", "c")),
             g = list(type = "marginal", probs = c(0.6, 0.4), levels = c("u", "v")))
B5 <- rbind(c(0, 0), c(2, 0), c(-2, 0), c(0, 3))        # (Intercept), fb, fc, gv
X5 <- as.matrix(d5[, 1:2]); M5 <- is.na(X5)
es5 <- VIM:::.gloc_cat_estep(X5, M5, matrix(1, 7, 2), B5, diag(2), cp5, cand5, pri5)
exact_f <- c(0.5, 0.3, 0.2) * dnorm(1.7, c(0, 2, -2))
expect_equal(unname(es5$post$f["1", ]), exact_f / sum(exact_f), tolerance = 1e-8)
exact_g <- c(0.6, 0.4) * dnorm(2.4, c(0, 3))
expect_equal(unname(es5$post$g["1", ]), exact_g / sum(exact_g), tolerance = 1e-8)

# capped rows take their mode rows in both expected designs (R24)
cand5ci <- suppressWarnings(VIM:::.gloc_cat_candidates(cp5, d5, ~ f * g, max_combos = 2L))
es5ci <- VIM:::.gloc_cat_estep(X5, M5, NULL, NULL, NULL, cp5, cand5ci, pri5)
expect_true(all(es5ci$Ubar[1, ] == cand5ci$many$Umode[1, ]))
expect_true(all(es5ci$Umain_bar[1, ] == cand5ci$many$Umode_main[1, ]))
# a double-missing row's pseudo-row weights are the products of its marginal posteriors (R24)
mr5 <- cand5$multi[[1]]
expect_equal(es5$pr_w[mr5$pos],
             unname(es5$post$f["1", mr5$lvl[, 1]] * es5$post$g["1", mr5$lvl[, 2]]),
             tolerance = 1e-12)
# a combination is present only when a row with known categories has it (R25)
expect_identical(which(cand5$pat_pr$present),
                 sort(unique(cand5$pats_rows$id[!is.na(cand5$pats_rows$id)])))
expect_false(all(cand5$pat_pr$present[cand5$pr_c[cand5$pr_row == 1L]]))
# a factor with its own contrasts attribute is coded like the design, so the
# candidate columns match it and the fit runs (R42). droplevels(), factor() and
# rbind() each drop the attribute while .gloc_design() and .gloc_patterns() keep
# it; 7.4.1 fitted such data, so the new default must too. The reference is the
# design on the COMPLETED copy, which is what the EM codes against -- on the raw
# data .gloc_design() adds an NA level and loses the attribute with it.
d5s <- d5; contrasts(d5s$f) <- contr.sum(3)
cp5s <- VIM:::.gloc_cat_prepare(d5s, c("f", "g"))
cand5s <- VIM:::.gloc_cat_candidates(cp5s, d5s, ~ .)
d5sf <- d5s; d5sf$f[1] <- "a"; d5sf$g[1] <- "u"
expect_identical(colnames(cand5s$Up),
                 colnames(VIM:::.gloc_design(d5sf, ~ ., c("f", "g"))))
expect_identical(colnames(cand5s$Up), c("(Intercept)", "f1", "f2", "gv"))
expect_true(all(cand5s$Up == VIM:::.gloc_design_rows(cand5s$Fp, ~ ., cp5s$levels)))

# a row with more combinations than the cap stays out of the table, once warned
wc <- collect_warnings(cand5c <- VIM:::.gloc_cat_candidates(cp5, d5, ~ ., max_combos = 2L))
expect_equal(length(wc), 1L)
expect_true(grepl("^cellGLoc: ", wc))
expect_false(1L %in% cand5c$pr_row)
es5c <- VIM:::.gloc_cat_estep(X5, M5, matrix(1, 7, 2), B5, diag(2), cp5, cand5c, pri5)
expect_equal(unname(es5c$post$f["1", ]), c(2, 2, 2) / 6, tolerance = 1e-12)   # complete-row frequencies
expect_identical(VIM:::.gloc_cat_change(es5$post, NULL), Inf)
expect_identical(VIM:::.gloc_cat_change(es5$post, es5$post), 0)

# R46 item 6 / R41 -- under the categorical EM the pseudo-row scatter stops
# rather than silently falling back to the weighted pairwise covariance,
# which would square a pseudo-row's posterior weight and so under-weight the
# rows whose level is uncertain. have_cw is exposed exactly so this is
# testable without installing or removing cellWise, and no namespace
# patching is used: have_cw = FALSE reaches the same em_stop() as an actual
# cellWise failure would, on the "package is not installed" branch.
Rsc <- matrix(rnorm(20 * 3), 20); Wsc <- matrix(runif(20 * 3), 20); Msc <- matrix(FALSE, 20, 3)
e6 <- tryCatch(VIM:::.gloc_scatter_soft(Rsc, Wsc, Msc, have_cw = FALSE, em = TRUE),
               error = function(e) conditionMessage(e))
expect_true(is.character(e6))
expect_true(grepl("cellWise", e6, fixed = TRUE))
expect_true(grepl('categorical = "level"', e6, fixed = TRUE))
# em = FALSE takes the fallback instead (a warning, not a stop) on the same input
w6 <- collect_warnings(S6 <- VIM:::.gloc_scatter_soft(Rsc, Wsc, Msc, have_cw = FALSE, em = FALSE))
expect_true(any(grepl("^cellGLoc: ", w6)))
expect_identical(dim(S6), c(3L, 3L))

# ==========================================================================
# Task 5: the EM inside imputeCellGLoc()
# ==========================================================================
if (requireNamespace("cellWise", quietly = TRUE)) {
  # spec test 1 -- no missing categorical cell: "em" is "level", bit for bit
  s1 <- gen_cat(150, 11, miss_f = 0)
  d1c <- s1$truth; d1c$x2[c(3, 9, 40)] <- NA            # continuous gaps only
  for (cs in list(c("soft", "robust"), c("soft", "classical"), c("binary", "classical"))) {
    lab <- paste(cs, collapse = "/")
    fe <- suppressWarnings(VIM::imputeCellGLoc(d1c, weights = cs[1], start = cs[2]))
    fl <- suppressWarnings(VIM::imputeCellGLoc(d1c, weights = cs[1], start = cs[2],
                                               categorical = "level"))
    keep <- c("B", "Sigma", "W", "U", "imputed", "converged", "iterations", "criterion")
    expect_identical(fe[keep], fl[keep], info = lab)
    expect_identical(fe$cat_posterior, list(), info = lab)
    expect_identical(fe$cat_multi_missing, 0, info = lab)
    expect_true(is.null(fl$cat_posterior) && is.null(fl$cat_multi_missing) &&
                  is.null(fl$cat_priors), info = lab)
  }

  # no categorical column at all: "em" is "level", bit for bit
  d0 <- s1$truth[, c("x1", "x2", "x3")]; d0$x2[c(3, 9, 40)] <- NA
  f0e <- suppressWarnings(VIM::imputeCellGLoc(d0))
  f0l <- suppressWarnings(VIM::imputeCellGLoc(d0, categorical = "level"))
  expect_identical(f0e[keep], f0l[keep])
  expect_true(is.null(f0e$cat_posterior) && is.null(f0e$cat_priors))

  # spec test 4 -- recovery on data from the model, 20% of f missing
  s5 <- gen_cat(400, 5, miss_f = 0.2)
  miss5 <- is.na(s5$d$f)
  fit5 <- suppressWarnings(VIM::imputeCellGLoc(s5$d))
  expect_true(fit5$converged)
  expect_identical(names(fit5$criterion),
                   c("means", "scatter", "weights", "scatter_spread", "categorical"))
  expect_true(fit5$criterion[["categorical"]] < 5e-3)
  expect_false(anyNA(fit5$imputed$f))
  expect_identical(levels(fit5$imputed$f), levels(s5$d$f))
  expect_equal(unname(rowSums(fit5$cat_posterior$f)), rep(1, sum(miss5)), tolerance = 1e-12)
  hit_em <- mean(fit5$imputed$f[miss5] == s5$truth$f[miss5])
  cp5b <- VIM:::.gloc_cat_prepare(s5$d, c("f", "g"))
  pr5 <- VIM:::.gloc_cat_fit_priors(cp5b$F[!miss5, ], rep(1, sum(!miss5)), cp5b$levels)
  P5 <- VIM:::.gloc_cat_prior(pr5$f, cp5b$F[miss5, ])
  hit_prior <- mean(cp5b$levels$f[max.col(P5, ties.method = "first")] ==
                      as.character(s5$truth$f[miss5]))
  expect_true(hit_em >= 0.85)
  expect_true(hit_em >= hit_prior + 0.2)
  full5 <- suppressWarnings(VIM::imputeCellGLoc(s5$truth))
  expect_true(max(abs(fit5$U %*% fit5$B - full5$U %*% full5$B)[!miss5, ]) < 0.3)

  # spec test 5 -- a gross continuous value in a row with a missing category
  # barely moves that row's posterior, compared with the same cell set to NA
  r0 <- which(miss5)[1]
  dA <- s5$d; dA$x1[r0] <- s5$truth$x1[r0] + 40
  dB <- s5$d; dB$x1[r0] <- NA
  fA <- suppressWarnings(VIM::imputeCellGLoc(dA))
  fB <- suppressWarnings(VIM::imputeCellGLoc(dB))
  expect_true(fA$W[r0, "x1"] < 0.5)
  expect_true(max(abs(fA$cat_posterior$f[as.character(r0), ] -
                        fB$cat_posterior$f[as.character(r0), ])) < 0.05)

  # spec test 7 -- rows with two missing categorical cells
  s7 <- gen_cat(300, 7, miss_f = 0.2, miss_g = 0.2)
  fit7 <- suppressWarnings(VIM::imputeCellGLoc(s7$d))
  both7 <- is.na(s7$d$f) & is.na(s7$d$g)
  expect_true(sum(both7) > 0)
  expect_true(fit7$converged)
  expect_identical(fit7$cat_multi_missing, mean(both7))
  for (v in c("f", "g"))
    expect_equal(unname(rowSums(fit7$cat_posterior[[v]])), rep(1, sum(is.na(s7$d[[v]]))),
                 tolerance = 1e-12, info = v)
  expect_false(anyNA(fit7$imputed$f) || anyNA(fit7$imputed$g))
  expect_true(all(is.finite(fit7$U %*% fit7$B)))

  # design = ~ 1: the categorical step cannot move the continuous fit much
  one_em <- suppressWarnings(VIM::imputeCellGLoc(s5$d, design = ~ 1, start = "classical"))
  one_lv <- suppressWarnings(VIM::imputeCellGLoc(s5$d, design = ~ 1, start = "classical",
                                                 categorical = "level"))
  expect_true(norm(one_em$Sigma - one_lv$Sigma, "F") / norm(one_lv$Sigma, "F") < 0.02)
  expect_true(mean((one_em$W < 0.5) == (one_lv$W < 0.5)) >= 0.99)

  # the caller's random-number stream is untouched on the EM path too
  set.seed(99); before <- .Random.seed
  invisible(suppressWarnings(VIM::imputeCellGLoc(s7$d)))
  expect_identical(.Random.seed, before)

  # an interaction design with a missing category runs and gives finite fitted means
  fit_i <- suppressWarnings(VIM::imputeCellGLoc(s7$d, design = ~ f * g))
  expect_true(all(is.finite(fit_i$U %*% fit_i$B)))
  expect_false(anyNA(fit_i$imputed$f))

  # R40 -- at a tight tolerance the categorical residual can be the last of the
  # four to settle, so the stall detector has to count its improvements as
  # progress; keyed on the weight change alone, the cold restart could fire and
  # discard every categorical iteration. The fit converges and each stopping
  # residual is below eps. scatter_spread is NA on a converged fit by design
  # (it would describe the approach, not the answer), so it is checked apart.
  tight <- suppressWarnings(VIM::imputeCellGLoc(s5$d, eps = 1e-5))
  expect_true(tight$converged)
  expect_true(all(tight$criterion[c("means", "scatter", "weights",
                                    "categorical")] < 1e-5))
  expect_true(is.na(tight$criterion[["scatter_spread"]]))

  # R46 item 7 -- pin the R40 stall-reset rule itself, not only that a fit at
  # tight eps happens to converge. On this dataset dW alone (the pre-R40 rule)
  # plateaus for .gloc_stall_iters iterations while dR is still improving, so
  # the pre-R40 rule fires one cold restart (confirmed by temporarily
  # reverting the rule locally: same run, 59 iterations and one "restarting
  # from" message, against 38 iterations and none under the current rule).
  # trace = TRUE's "restarting from" message is the observable signal.
  s7t <- gen_cat(300, 5, miss_f = 0.25)
  msg_tight <- capture.output(
    tight_tr <- suppressWarnings(VIM::imputeCellGLoc(s7t$d, eps = 1e-5, trace = TRUE)),
    type = "message")
  expect_true(tight_tr$converged)
  expect_true(all(tight_tr$criterion[c("means", "scatter", "weights",
                                       "categorical")] < 1e-5))
  expect_false(any(grepl("restarting from", msg_tight, fixed = TRUE)))

  # R42 -- a factor carrying its own contrasts attribute fits under the default,
  # as it did in 7.4.1, and the diagnostic runs on it too
  sc <- gen_cat(200, 12, miss_f = 0.2)
  contrasts(sc$d$f) <- contr.sum(3)
  fitc <- suppressWarnings(VIM::imputeCellGLoc(sc$d))
  expect_identical(rownames(fitc$B), c("(Intercept)", "f1", "f2", "gv"))
  expect_false(anyNA(fitc$imputed$f))
  expect_false(anyNA(fitc$cat_prob_observed[!is.na(sc$d$f), "f"]))

  # R46 item 1 -- a stale contrasts matrix (set while a factor had fewer
  # levels than it now declares -- levels(f) <- c(levels(f), "z") adds a level
  # without going through factor(), so it leaves the OLD, now too-small
  # contrasts matrix attached) must not be re-attached just because its row
  # count happens to match the reduced level count droplevels() produces
  # elsewhere: the reference design (built on the completed copy, which
  # model.frame(drop.unused.levels = TRUE) strips of the very same attribute)
  # would then use different columns and the fit would stop with "candidate
  # design columns do not match the design".
  s1u <- gen_cat(200, 21, miss_f = 0)
  d1u <- s1u$truth
  contrasts(d1u$f) <- contr.sum(3)
  levels(d1u$f) <- c(levels(d1u$f), "z")        # an unused 4th level, contrasts left stale
  d1u$f[5] <- NA                                # one missing cell
  fit1u <- suppressWarnings(VIM::imputeCellGLoc(d1u))
  d1uf <- d1u; d1uf$f[5] <- "a"
  # The reference design is built on a factor that still carries the stale
  # matrix, so model.matrix() drops it with base R's "contrasts dropped from
  # factor f due to missing levels" -- the very loss this test is about. The
  # warning is silenced rather than matched: its text is base R's and may be
  # translated (R54, m1).
  expect_identical(colnames(fit1u$U),
                   colnames(suppressWarnings(VIM:::.gloc_design(d1uf, ~ ., c("f", "g")))))

  # R54 m3 -- the same with a character-valued contrasts attribute set
  # directly, together with an unused level. Before R46 the guard re-attached
  # any non-matrix attribute after droplevels(), so the EM design used
  # contr.sum while the reference design, which loses the attribute with the
  # unused level, used treatment coding, and the fit stopped with "candidate
  # design columns do not match the design".
  s1k <- gen_cat(200, 23, miss_f = 0)
  d1k <- s1k$truth
  levels(d1k$f) <- c(levels(d1k$f), "z")        # an unused 4th level
  attr(d1k$f, "contrasts") <- "contr.sum"       # a character, set directly
  d1k$f[5] <- NA
  fit1k <- suppressWarnings(VIM::imputeCellGLoc(d1k))
  d1kf <- d1k; d1kf$f[5] <- "a"
  expect_identical(colnames(fit1k$U),
                   colnames(suppressWarnings(VIM:::.gloc_design(d1kf, ~ ., c("f", "g")))))
  expect_identical(colnames(fit1k$U), c("(Intercept)", "fb", "fc", "gv"))
  expect_false(anyNA(fit1k$imputed$f))

  # R46 item 5 -- an EM-imputed factor's restored column keeps the caller's
  # own contrasts attribute (.gloc_cat_restore() used to rebuild it with a
  # plain factor(), which drops one just like droplevels() does)
  s1r <- gen_cat(200, 22, miss_f = 0.2)
  d1r <- s1r$d
  contrasts(d1r$f) <- contr.sum(3)
  fit1r <- suppressWarnings(VIM::imputeCellGLoc(d1r))
  expect_false(anyNA(fit1r$imputed$f))
  expect_identical(attr(fit1r$imputed$f, "contrasts"), attr(d1r$f, "contrasts"))

  # a fit stopped by maxit names the categorical change in its warning
  w7 <- collect_warnings(VIM::imputeCellGLoc(s7$d, maxit = 1))
  expect_true(any(grepl("largest change in a categorical posterior", w7)))
}

# spec test 2 -- categorical = "level" reproduces VIM 7.4.1
if (at_home() && requireNamespace("cellWise", quietly = TRUE)) {
  ref741 <- readRDS("gloc_level_ref_741.rds")
  bitref <- identical(Sys.getenv("VIM_BITREF"), "true")
  cases <- list(soft_dot = list(design = ~ ., weights = "soft"),
                soft_one = list(design = ~ 1, weights = "soft"),
                bin_dot  = list(design = ~ ., weights = "binary"))
  for (nm in names(cases)) {
    ref <- ref741$fits[[nm]]
    fit <- suppressWarnings(VIM::imputeCellGLoc(ref741$data, design = cases[[nm]]$design,
                                                weights = cases[[nm]]$weights,
                                                categorical = "level"))
    expect_identical(fit[c("U", "converged", "iterations")],
                     ref[c("U", "converged", "iterations")], info = nm)
    for (k in c("B", "Sigma", "W"))
      expect_equal(fit[[k]], ref[[k]], tolerance = 1e-10, info = paste(nm, k))
    expect_equal(fit$imputed, ref$imputed, tolerance = 1e-10, info = nm)
    if (bitref)
      expect_identical(c(fit[c("B", "Sigma", "W", "imputed")],
                         list(criterion = fit$criterion[names(ref$criterion)])),
                       c(ref[c("B", "Sigma", "W", "imputed")],
                         list(criterion = ref$criterion)), info = nm)
    expect_true(is.null(fit$cat_posterior), info = nm)
  }
}

# ==========================================================================
# Task 6: cat_prob_observed
# ==========================================================================
# with an intercept-only design the continuous cells carry no level
# information, so the diagnostic is the prior of the observed level
d6s <- d5[-1, ]; rownames(d6s) <- NULL
cp6 <- VIM:::.gloc_cat_prepare(d6s, c("f", "g"))
X6 <- as.matrix(d6s[, 1:2])
U1 <- matrix(1, 6, 1, dimnames = list(NULL, "(Intercept)"))
po1 <- VIM:::.gloc_cat_prob_observed(X6, is.na(X6), matrix(1, 6, 2), matrix(0.5, 1, 2),
                                     diag(2), cp6, pri5, ~ 1, VIM:::.gloc_cat_identity(cp6, U1))
expect_equal(unname(po1[, "f"]), c(0.5, 0.3, 0.2)[as.integer(cp6$F$f)], tolerance = 1e-12)
expect_equal(unname(po1[, "g"]), c(0.6, 0.4)[as.integer(cp6$F$g)], tolerance = 1e-12)

# a B without matching design column names stops with a clear error (R19)
Bbad <- matrix(0.5, 1, 2, dimnames = list("wrong", NULL))
expect_error(VIM:::.gloc_cat_prob_observed(X6, is.na(X6), matrix(1, 6, 2), Bbad, diag(2), cp6,
                                           pri5, ~ 1, VIM:::.gloc_cat_identity(cp6, U1)),
             "do not match B")

if (requireNamespace("cellWise", quietly = TRUE)) {
  # spec test 6 -- 5% of f miscoded to the level whose group mean is farthest
  s6 <- gen_cat(400, 6, miss_f = 0)
  d6 <- s6$truth
  set.seed(66)
  mis6 <- sample(400, 20)
  far <- c(a = "b", b = "c", c = "b")               # shifts 0, 4, -3
  d6$f[mis6] <- far[as.character(d6$f[mis6])]
  fit6 <- suppressWarnings(VIM::imputeCellGLoc(d6))
  po <- fit6$cat_prob_observed[, "f"]
  expect_identical(dim(fit6$cat_prob_observed), c(400L, 2L))
  expect_false(anyNA(po))
  expect_identical(names(fit6$cat_priors), c("f", "g"))
  # Separation, not an absolute level. A miscoded label inflates its own row's
  # residuals, so the fit's weights flag that row's continuous cells and the
  # diagnostic falls back to the prior of the recorded level (design doc §12.2,
  # self-masking). What the diagnostic must do is separate miscoded cells from
  # correct ones.
  auc6 <- mean(outer(po[-mis6], po[mis6], ">")) +
    0.5 * mean(outer(po[-mis6], po[mis6], "=="))
  expect_true(auc6 > 0.9)
  expect_true(mean(po[mis6] < 0.5) > 0.8)
  expect_true(mean(po[-mis6] < 0.5) < 0.05)
  expect_true(mean(po[-mis6]) > 0.7)

  # missing cells are NA in the diagnostic; the posterior carries them instead
  s6m <- gen_cat(200, 8, miss_f = 0.2)
  fit6m <- suppressWarnings(VIM::imputeCellGLoc(s6m$d))
  expect_true(all(is.na(fit6m$cat_prob_observed[is.na(s6m$d$f), "f"])))
  expect_false(anyNA(fit6m$cat_prob_observed[!is.na(s6m$d$f), "f"]))

  # an ordered factor with missing cells runs through the EM and the diagnostic (R23)
  d6o <- s6m$d
  d6o$f <- factor(d6o$f, levels = c("a", "b", "c"), ordered = TRUE)
  fit6o <- suppressWarnings(VIM::imputeCellGLoc(d6o))
  expect_true(is.ordered(fit6o$imputed$f))
  expect_false(anyNA(fit6o$imputed$f))
  expect_false(anyNA(fit6o$cat_prob_observed[!is.na(d6o$f), "f"]))
}

# ==========================================================================
# Task 7: one MI draw
# ==========================================================================
Cp <- matrix(c(1, 1, 1, 1), 2)                       # singular
Rp <- VIM:::.gloc_chol_psd(Cp)
expect_equal(crossprod(Rp), Cp, tolerance = 1e-12)

if (requireNamespace("cellWise", quietly = TRUE)) {
  s8 <- gen_cat(200, 8, miss_f = 0.2)
  r3 <- which(!is.na(s8$d$f))[1:3]
  s8$d$x3[r3] <- NA
  fit8 <- suppressWarnings(VIM::imputeCellGLoc(s8$d))
  expect_identical(VIM:::.gloc_draw_mi(fit8, s8$d, noise = FALSE), fit8$imputed)
  expect_error(VIM:::.gloc_draw_mi(fit8, s8$d, design = ~ f), "do not match")
  if (at_home()) {
    set.seed(808)
    draws <- lapply(1:300, function(i) VIM:::.gloc_draw_mi(fit8, s8$d))
    rf8 <- rownames(fit8$cat_posterior$f)[1]
    freq <- table(factor(vapply(draws, function(z) as.character(z$f[as.integer(rf8)]), ""),
                         levels = levels(s8$d$f))) / 300
    expect_true(max(abs(as.vector(freq) - fit8$cat_posterior$f[rf8, ])) < 0.1)
    x3d <- vapply(draws, function(z) z$x3[r3[1]], 0)
    expect_true(abs(mean(x3d) - fit8$imputed$x3[r3[1]]) < 4 * sd(x3d) / sqrt(300))
    expect_true(sd(x3d) > 0)
  }
}

# ==========================================================================
# Task 7b: posteriors for other rows under a given fit
# ==========================================================================
if (requireNamespace("cellWise", quietly = TRUE)) {
  s9 <- gen_cat(200, 9, miss_f = 0.2)
  fit9 <- suppressWarnings(VIM::imputeCellGLoc(s9$d))
  fo9 <- VIM:::.gloc_cat_posterior_for(fit9, s9$d, W = fit9$W)
  expect_equal(fo9$cat_posterior, fit9$cat_posterior, tolerance = 1e-10)
  expect_equal(unname(fo9$U), unname(fit9$U), tolerance = 1e-10)
  expect_identical(attr(fo9, "dropped"), 0L)
  set.seed(909)
  bi <- sample(200, 200, TRUE)
  fb9 <- suppressWarnings(VIM::imputeCellGLoc(s9$d[bi, ]))
  fo9b <- VIM:::.gloc_cat_posterior_for(fb9, s9$d, W = fit9$W)
  expect_equal(unname(rowSums(fo9b$cat_posterior$f)), rep(1, sum(is.na(s9$d$f))),
               tolerance = 1e-12)
  expect_true(all(is.finite(fo9b$U %*% fo9b$B)))
  expect_false(anyNA(VIM:::.gloc_draw_mi(fo9b, s9$d)[, c("x1", "x2", "x3", "f")]))
}
# a level the prior model never saw gets the floor; the others keep their ratio
pr_m <- list(f = list(type = "marginal", probs = c(0.75, 0.25), levels = c("a", "b")))
al <- VIM:::.gloc_cat_align_priors(pr_m, list(f = c("a", "b", "c")))
Pal <- VIM:::.gloc_cat_prior(al$f, data.frame(f = factor("a", levels = c("a", "b", "c"))))
expect_identical(dim(Pal), c(1L, 3L))
expect_equal(Pal[1, 1] / Pal[1, 2], 3, tolerance = 1e-8)
expect_true(Pal[1, 3] < 1e-9)

# ==========================================================================
# Task 13: two starts under the EM (spec §12.2 "Two starts under the EM",
# §12.3 test 9; Rulings R57 and R60)
# ==========================================================================
# --- the objective: -2 log-likelihood of the retained cells of each row, plus
# lambda_j per flagged observed cell, against a hand computation. A cell is
# retained when its weight is at least 1/2 (row 2's first cell sits exactly
# there). The missing cell carries weight 1 on purpose: the mask, not the
# weight, keeps it out. Row 3 retains nothing and adds only its penalty.
R13 <- rbind(c(0.3, -1.2, 0.8),
             c(2.5, 0.4, -0.6),
             c(-0.7, 0.9, 1.1),
             c(1.4, -0.2, NA))
W13 <- rbind(c(1, 0.9, 0.2),
             c(0.5, 0.4, 1),
             c(0.1, 0.3, 0.45),
             c(1, 1, 1))
M13 <- is.na(R13)
S13 <- matrix(c(2, 0.6, 0.3, 0.6, 1.5, -0.4, 0.3, -0.4, 1), 3)
lam13 <- c(7.1, 6.4, 8.2)
m2ll <- function(r, S) drop(t(r) %*% solve(S) %*% r) + log(det(S)) + length(r) * log(2 * pi)
obj13 <- m2ll(R13[1, 1:2], S13[1:2, 1:2]) + m2ll(R13[2, c(1, 3)], S13[c(1, 3), c(1, 3)]) +
  m2ll(R13[4, 1:2], S13[1:2, 1:2]) +
  lam13[1] * 1 + lam13[2] * 2 + lam13[3] * 2        # flagged observed cells per column: 1, 2, 2
expect_equal(VIM:::.gloc_objective(R13, W13, M13, S13, lam13), obj13, tolerance = 1e-12)
# the penalty from a scatter S: qchisq(0.99, 1) + log(2 pi) + log c_j, c_j = 1 / (S^-1)_jj
expect_equal(VIM:::.gloc_lambda(S13),
             qchisq(0.99, 1) + log(2 * pi) + log(1 / diag(solve(S13))), tolerance = 1e-12)
expect_null(VIM:::.gloc_lambda(NULL))               # no cellMCD scatter (R60 item 1)
expect_null(VIM:::.gloc_lambda(matrix(1, 2, 2)))    # nor a singular one

# --- the second start's design rows and coefficients (R60 item 2). Evaluated at
# the EM's pseudo-rows, the NA-level design reproduces the EM design in the EM's
# columns and is 0 in its NA-level columns under treatment coding, so the
# NA-level rows of B are dropped by name; least squares on the fitted means of
# the same level combinations agrees.
s13 <- gen_cat(150, 13, miss_f = 0.2, miss_g = 0.1)
cp13 <- VIM:::.gloc_cat_prepare(s13$d, c("f", "g"))
cand13 <- VIM:::.gloc_cat_candidates(cp13, s13$d, ~ .)
Ud13 <- VIM:::.gloc_design(s13$d, ~ ., c("f", "g"))     # the design of categorical = "level"
Ul13 <- VIM:::.gloc_level_rows(s13$d, cand13$Fp, ~ ., c("f", "g"))
nm13 <- colnames(cand13$Up)
cc13 <- which(rowSums(cp13$Mc) == 0L)
expect_identical(colnames(Ul13), colnames(Ud13))
expect_identical(dim(Ul13), c(nrow(cand13$Fp), ncol(Ud13)))
expect_true(all(Ul13[seq_along(cc13), ] == Ud13[cc13, ]))   # complete rows: their own rows
expect_true(all(Ul13[, nm13] == cand13$Up))
expect_true(all(Ul13[, setdiff(colnames(Ud13), nm13)] == 0))
set.seed(1301)
Bl13 <- matrix(rnorm(ncol(Ud13) * 3), ncol(Ud13), 3,
               dimnames = list(colnames(Ud13), c("x1", "x2", "x3")))
Bn13 <- VIM:::.gloc_level_B(Bl13, Ul13, cand13$Up)
expect_identical(Bn13, Bl13[nm13, , drop = FALSE])
expect_true(max(abs(VIM:::.gloc_level_B(Bl13, Ul13, cand13$Up, lsq = TRUE) - Bn13)) < 1e-10)
# An ordered factor: the NA level changes the polynomial coding of the observed
# levels, so the names match while the coefficients do not; the fitted means
# are carried over instead.
d13o <- s13$d
d13o$f <- factor(d13o$f, levels = c("a", "b", "c"), ordered = TRUE)
cp13o <- VIM:::.gloc_cat_prepare(d13o, c("f", "g"))
cand13o <- VIM:::.gloc_cat_candidates(cp13o, d13o, ~ .)
Ud13o <- VIM:::.gloc_design(d13o, ~ ., c("f", "g"))
Ul13o <- VIM:::.gloc_level_rows(d13o, cand13o$Fp, ~ ., c("f", "g"))
set.seed(1302)
Bl13o <- matrix(rnorm(ncol(Ud13o) * 3), ncol(Ud13o), 3,
                dimnames = list(colnames(Ud13o), c("x1", "x2", "x3")))
Bm13o <- VIM:::.gloc_level_B(Bl13o, Ul13o, cand13o$Up)
expect_identical(dimnames(Bm13o), list(colnames(cand13o$Up), c("x1", "x2", "x3")))
expect_true(all(colnames(cand13o$Up) %in% colnames(Ud13o)))
expect_true(max(abs(Bm13o - Bl13o[colnames(cand13o$Up), ])) > 0.01)
expect_true(max(abs(cand13o$Up %*% Bm13o - Ul13o %*% Bl13o)) < 1e-10)

if (requireNamespace("cellWise", quietly = TRUE)) {
  cols13 <- c("x1", "x2", "x3")
  obj_of <- function(fit, d) {
    X <- as.matrix(d[, cols13])
    VIM:::.gloc_objective(X - fit$U %*% fit$B, fit$W, is.na(X), fit$Sigma, fit$em_starts$lambda)
  }

  # spec test 9 -- with a missing categorical cell, the soft corner, the robust
  # start and a categorical term, two starts run. em_starts reports both, and
  # the returned fit is the chosen run: it reproduces that run's objective, and
  # its iterations and converged are that run's.
  s5 <- gen_cat(400, 5, miss_f = 0.2)
  f13 <- suppressWarnings(VIM::imputeCellGLoc(s5$d))
  es13 <- f13$em_starts
  expect_false(is.null(es13))
  expect_identical(names(es13), c("chosen", "objective", "converged", "iterations", "lambda"))
  expect_true(es13$chosen %in% c("complete", "na_level"))
  for (k in c("objective", "converged", "iterations"))
    expect_identical(names(es13[[k]]), c("complete", "na_level"), info = k)
  expect_identical(names(es13$lambda), cols13)
  expect_equal(obj_of(f13, s5$d), es13$objective[[es13$chosen]], tolerance = 1e-10)
  expect_identical(f13$iterations, es13$iterations[[es13$chosen]])
  expect_identical(f13$converged, es13$converged[[es13$chosen]])

  # spec test 9 -- only one start runs without a missing categorical cell, under
  # "level", start = "classical" and design = ~ 1, and (R60 items 5 and 6) in the
  # binary corner and at maxit = 0. em_starts is then NULL, but present.
  s13n <- gen_cat(150, 11, miss_f = 0)
  one13 <- list(
    no_missing = suppressWarnings(VIM::imputeCellGLoc(s13n$truth)),
    level      = suppressWarnings(VIM::imputeCellGLoc(s5$d, categorical = "level")),
    classical  = suppressWarnings(VIM::imputeCellGLoc(s5$d, start = "classical")),
    intercept  = suppressWarnings(VIM::imputeCellGLoc(s5$d, design = ~ 1)),
    binary     = suppressWarnings(VIM::imputeCellGLoc(s5$d, weights = "binary")),
    maxit0     = suppressWarnings(VIM::imputeCellGLoc(s5$d, maxit = 0)))
  for (nm in names(one13)) {
    expect_true("em_starts" %in% names(one13[[nm]]), info = nm)
    expect_null(one13[[nm]]$em_starts, info = nm)
  }

  # spec test 9 -- maxit = 0 returns start 1's starting fit: the robust start on
  # the prior-expected design rows, fitted on the complete-category rows
  X13 <- as.matrix(s5$d[, cols13])
  M13s <- is.na(X13)
  cp13s <- VIM:::.gloc_cat_prepare(s5$d, c("f", "g"))
  cand13s <- VIM:::.gloc_cat_candidates(cp13s, s5$d, ~ .)
  cc13s <- rowSums(cp13s$Mc) == 0L
  pri13s <- VIM:::.gloc_cat_fit_priors(cp13s$F[cc13s, , drop = FALSE], rep(1, sum(cc13s)),
                                       cp13s$levels)
  es13s <- VIM:::.gloc_cat_estep(X13, M13s, NULL, NULL, NULL, cp13s, cand13s, pri13s)
  st13s <- VIM:::.gloc_start_robust(X13, es13s$Ubar, M13s, warn_design = FALSE,
                                    patterns = cand13s$pats_rows, fit_rows = cc13s)
  expect_identical(one13$maxit0$W, st13s$W)
  expect_identical(one13$maxit0$B, st13s$B)
  expect_identical(one13$maxit0$iterations, 0L)

  # R60 item 1 -- when start 1's cellMCD refuses (here 55% of x3 is missing),
  # the start flags by MAD and says so, and the fit runs from that start alone
  d13h <- s5$d
  set.seed(1313)
  d13h$x3[sample(400, 220)] <- NA
  w13h <- collect_warnings(f13h <- VIM::imputeCellGLoc(d13h))
  expect_true(any(grepl("robust start: cellWise::cellMCD() failed", w13h, fixed = TRUE)))
  expect_true("em_starts" %in% names(f13h))
  expect_null(f13h$em_starts)

  # R60 item 4 -- an error in the second start never becomes an error of the
  # fit. A term that maps the NA level to NA, factor(f, levels = ...), leaves
  # NA rows in the NA-level design, whose robust start then stops; the EM design
  # has no NA level and fits. One warning, start 1's fit, the error recorded.
  des13 <- ~ factor(f, levels = c("a", "b", "c")) + g
  w13i <- collect_warnings(f13i <- VIM::imputeCellGLoc(s5$d, design = des13))
  expect_equal(sum(grepl(paste0("^cellGLoc: the second start failed \\(.+\\); ",
                                "returning the fit from the first start"), w13i)), 1L)
  es13i <- f13i$em_starts
  expect_identical(names(es13i),
                   c("chosen", "objective", "converged", "iterations", "lambda", "error"))
  expect_identical(es13i$chosen, "complete")
  expect_true(is.na(es13i$objective[["na_level"]]) && is.na(es13i$converged[["na_level"]]) &&
                is.na(es13i$iterations[["na_level"]]))
  expect_true(is.character(es13i$error) && length(es13i$error) == 1L && nzchar(es13i$error))
  expect_true(f13i$converged)
  expect_equal(obj_of(f13i, s5$d), es13i$objective[["complete"]], tolerance = 1e-10)

  # R60 item 3 -- only the returned run's warnings reach the caller. At maxit = 1
  # neither run converges, so start 1 is returned, and exactly one
  # non-convergence warning appears, carrying start 1's numbers.
  s13w <- gen_cat(300, 7, miss_f = 0.2, miss_g = 0.2)
  w13w <- collect_warnings(f13w <- VIM::imputeCellGLoc(s13w$d, maxit = 1))
  expect_false(is.null(f13w$em_starts))
  expect_identical(f13w$em_starts$chosen, "complete")
  nc13 <- grep("did not converge", w13w, value = TRUE)
  expect_equal(length(nc13), 1L)
  expect_true(any(grepl(sprintf("scaled change in fitted means %s,",
                                VIM:::.gloc_fmt(f13w$criterion[["means"]])), nc13, fixed = TRUE)))
  # Two missing cells give the NA-level design a column of two rows, which the
  # second start's robust fit reports as too thin; that warning appears exactly
  # when the second start is the run returned.
  s13k <- gen_cat(300, 41, miss_f = 0)
  d13k <- s13k$d
  d13k$f[c(7, 19)] <- NA
  w13k <- collect_warnings(f13k <- VIM::imputeCellGLoc(d13k))
  expect_false(is.null(f13k$em_starts))
  expect_identical(any(grepl("robust start: too few observed rows", w13k, fixed = TRUE)),
                   identical(f13k$em_starts$chosen, "na_level"))

  # R60 item 2 -- an ordered factor with missing cells fits under two starts
  w13o <- collect_warnings(f13o <- VIM::imputeCellGLoc(d13o))
  expect_false(is.null(f13o$em_starts))
  expect_false(any(grepl("second start failed", w13o, fixed = TRUE)))
  expect_false(anyNA(f13o$imputed$f))
  expect_true(is.ordered(f13o$imputed$f))
}

# spec test 9 -- two starts that end at clearly different fixed points (slow: two
# fits of 200 rows, six continuous and six categorical variables). A compact
# version of the pilot's hard cell: shifts of 6 in 20% of rows of the block
# x1..x5 and of the lone, weakly correlated column x6, 20% of continuous cells
# and 10% of every factor missing. The first start alone masks the contamination
# here; the second does not. The objectives differ by far more than one fixed
# point reached from different starts spreads (at most about 18 in the
# investigation behind the rule), the fit returned is the run with the smaller
# objective, and its scatter is the unmasked one (relative error below 0.6).
if (at_home() && requireNamespace("cellWise", quietly = TRUE)) {
  gen_hard <- function(seed, n = 200) {
    set.seed(seed)
    S <- matrix(0.1, 6, 6); S[1:5, 1:5] <- 0.7; diag(S) <- 1
    X <- matrix(rnorm(n * 6), n) %*% chol(S)
    colnames(X) <- paste0("x", 1:6)
    f <- data.frame(f1 = sample(c("A", "B", "C"), n, TRUE, c(0.5, 0.3, 0.2)),
                    f2 = sample(c("low", "med", "high"), n, TRUE),
                    f3 = sample(c("yes", "no"), n, TRUE),
                    f4 = sample(c("t1", "t2", "t3", "t4"), n, TRUE),
                    f5 = sample(c("N", "S", "E", "W"), n, TRUE),
                    f6 = sample(c("urban", "rural"), n, TRUE, c(0.6, 0.4)),
                    stringsAsFactors = TRUE)
    X[, 1] <- X[, 1] + 3 * (f$f1 == "A") - 3 * (f$f1 == "C")
    X[, 2] <- X[, 2] + 2 * (f$f1 == "A") + 2.5 * (f$f2 == "high") - 2 * (f$f2 == "low")
    X[, 3] <- X[, 3] + 3 * (f$f3 == "yes") + 2 * (f$f6 == "urban")
    X[, 4] <- X[, 4] + 3 * (f$f2 == "high") + 2 * (f$f4 == "t1") - 2 * (f$f4 == "t4")
    X[, 5] <- X[, 5] + 2 * (f$f5 == "N") - 2 * (f$f5 == "S")
    X[, 6] <- X[, 6] + 3 * (f$f6 == "urban") + 1.5 * (f$f3 == "yes")
    trig <- runif(n) < 0.2                          # the block x1..x5
    C <- matrix(FALSE, n, 6)
    C[, 1] <- trig | runif(n) < 0.06
    C[, 2:5] <- (trig & matrix(runif(n * 4) < 0.5, n)) | matrix(runif(n * 4) < 0.06, n)
    C[, 6] <- runif(n) < 0.2 | runif(n) < 0.06      # x6, a block of its own
    X[C] <- X[C] + sample(c(-6, 6), sum(C), TRUE)
    X[matrix(runif(n * 6) < 0.2, n)] <- NA
    d <- data.frame(X, f)
    for (v in names(f)) d[[v]][runif(n) < 0.1] <- NA
    list(d = d, S = S)
  }
  g13 <- gen_hard(14)
  f13g <- suppressWarnings(VIM::imputeCellGLoc(g13$d))
  es13g <- f13g$em_starts
  expect_true(length(es13g$converged) == 2L && isTRUE(all(es13g$converged)))
  expect_true(abs(es13g$objective[["na_level"]] - es13g$objective[["complete"]]) > 100)
  expect_identical(es13g$chosen, c("complete", "na_level")[which.min(es13g$objective)])
  expect_identical(es13g$chosen, "na_level")
  Xg <- as.matrix(g13$d[, paste0("x", 1:6)])
  expect_equal(VIM:::.gloc_objective(Xg - f13g$U %*% f13g$B, f13g$W, is.na(Xg), f13g$Sigma,
                                     es13g$lambda),
               es13g$objective[["na_level"]], tolerance = 1e-10)
  expect_identical(f13g$iterations, es13g$iterations[["na_level"]])
  expect_true(norm(f13g$Sigma - g13$S, "F") / norm(g13$S, "F") < 0.6)
}
