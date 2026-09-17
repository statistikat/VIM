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

  # R46 item 7 -- a fit at a tight tolerance converges without a cold restart in
  # its first start. Since 7.5.1 this test no longer pins the R40 stall-reset
  # rule; the unit test of .gloc_stall_update in the Task 20 section does
  # (Task 20 fix round, I1). It was written to pin it: on this dataset under
  # 7.5.0, dW alone (the pre-R40 rule) plateaued for .gloc_stall_iters iterations
  # while dR was still improving, so the pre-R40 rule fired one cold restart
  # (confirmed by temporarily reverting the rule locally: same run, 59 iterations
  # and one "restarting from" message, against 38 iterations and none under the
  # current rule). trace = TRUE's "restarting from" message is the observable
  # signal. Those counts are the first start's run. The fit now runs from two
  # starts (spec §12.2): the trace holds the first start's iterations, then a
  # "second start" line and the second start's (13 iterations here under 7.5.0,
  # and it is the run returned), so the restart check reads the first start's part
  # only, and a legitimate restart of the second start cannot break it (R64 M5).
  # Under 7.5.1 (per-level detection) the plateau is gone: both rules converge in
  # 25 iterations of the first start without a restart here, and the pre-R40 rule
  # fired no first-start restart on any of 275 datasets searched for a replacement
  # (this call with seeds 1 to 150, and 25 seeds each of five variants: weaker level
  # shifts, more missing cells, both factors missing, n = 150).
  s7t <- gen_cat(300, 5, miss_f = 0.25)
  msg_tight <- capture.output(
    tight_tr <- suppressWarnings(VIM::imputeCellGLoc(s7t$d, eps = 1e-5, trace = TRUE)),
    type = "message")
  expect_true(tight_tr$converged)
  expect_true(all(tight_tr$criterion[c("means", "scatter", "weights",
                                       "categorical")] < 1e-5))
  cut_tight <- grep("second start: the robust start", msg_tight, fixed = TRUE)
  msg_first <- if (length(cut_tight)) msg_tight[seq_len(cut_tight[1] - 1L)] else msg_tight
  expect_false(any(grepl("restarting from", msg_first, fixed = TRUE)))

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
  # The binary corner: its posteriors are a function of the returned W, which is
  # what W = fit9$W reproduces. Under per-level detection (soft corner, 7.5.1) they
  # come from the candidates' own weight rows instead (spec §12.9), which this
  # helper takes only from Task 21 on (its cat_weights argument).
  fit9 <- suppressWarnings(VIM::imputeCellGLoc(s9$d, weights = "binary"))
  fo9 <- VIM:::.gloc_cat_posterior_for(fit9, s9$d, W = fit9$W)
  expect_equal(fo9$cat_posterior, fit9$cat_posterior, tolerance = 1e-10)
  expect_equal(unname(fo9$U), unname(fit9$U), tolerance = 1e-10)
  expect_identical(attr(fo9, "dropped"), 0L)
  set.seed(909)
  bi <- sample(200, 200, TRUE)
  # the bootstrap fit in the same corner, so the pairing mirrors bootstrap-proper MI
  fb9 <- suppressWarnings(VIM::imputeCellGLoc(s9$d[bi, ], weights = "binary"))
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
# nor an invertible one whose inverse has a negative diagonal, and without a
# "NaNs produced" warning from the log (R64 M2)
expect_silent(VIM:::.gloc_lambda(diag(c(1, -1))))
expect_null(suppressWarnings(VIM:::.gloc_lambda(diag(c(1, -1)))))

# --- R64 I1: a warning of the second start reads "cellGLoc: (second start) ...",
# and one reason is reported once per call whether it arrives with the label or
# without; warnings without the prefix pass as before
hd13 <- VIM:::.gloc_dedup_handler()
wd13 <- collect_warnings(withCallingHandlers({
  warning("cellGLoc: A", call. = FALSE)
  warning("cellGLoc: (second start) A", call. = FALSE)
  warning("cellGLoc: (second start) B", call. = FALSE)
  warning("cellGLoc: B", call. = FALSE)
  warning("other", call. = FALSE)
  warning("other", call. = FALSE)
}, warning = hd13))
expect_identical(wd13, c("cellGLoc: A", "cellGLoc: (second start) B", "other", "other"))
expect_identical(conditionMessage(VIM:::.gloc_label_second(simpleWarning("cellGLoc: robust start: x"))),
                 "cellGLoc: (second start) robust start: x")
expect_identical(conditionMessage(VIM:::.gloc_label_second(simpleWarning("other"))), "other")

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
# Sum contrasts on a factor with missing cells (R64 M4): the EM design keeps them
# (f1, f2), while addNA() strips the attribute from the "level" design, which
# is treatment-coded (fb, fc, fNA); the names differ, and the fitted means are
# carried over by least squares.
s13c <- gen_cat(200, 12, miss_f = 0.2)
d13c <- s13c$d
contrasts(d13c$f) <- contr.sum(3)
cp13c <- VIM:::.gloc_cat_prepare(d13c, c("f", "g"))
cand13c <- VIM:::.gloc_cat_candidates(cp13c, d13c, ~ .)
Ul13c <- VIM:::.gloc_level_rows(d13c, cand13c$Fp, ~ ., c("f", "g"))
expect_identical(colnames(Ul13c), c("(Intercept)", "fb", "fc", "fNA", "gv"))
set.seed(1303)
Bl13c <- matrix(rnorm(ncol(Ul13c) * 3), ncol(Ul13c), 3,
                dimnames = list(colnames(Ul13c), c("x1", "x2", "x3")))
Bm13c <- VIM:::.gloc_level_B(Bl13c, Ul13c, cand13c$Up)
expect_identical(rownames(Bm13c), c("(Intercept)", "f1", "f2", "gv"))
expect_true(max(abs(cand13c$Up %*% Bm13c - Ul13c %*% Bl13c)) < 1e-10)

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
  # when the second start is the run returned, and then with the label "(second
  # start)" after the prefix, never without it (R64 I1).
  s13k <- gen_cat(300, 41, miss_f = 0)
  d13k <- s13k$d
  d13k$f[c(7, 19)] <- NA
  w13k <- collect_warnings(f13k <- VIM::imputeCellGLoc(d13k))
  expect_false(is.null(f13k$em_starts))
  expect_identical(any(grepl("robust start: too few observed rows", w13k, fixed = TRUE)),
                   identical(f13k$em_starts$chosen, "na_level"))
  expect_identical(any(grepl("cellGLoc: (second start) robust start: too few observed rows",
                             w13k, fixed = TRUE)),
                   identical(f13k$em_starts$chosen, "na_level"))
  expect_false(any(startsWith(w13k, "cellGLoc: robust start: too few observed rows")))

  # R60 item 2 -- an ordered factor with missing cells fits under two starts
  w13o <- collect_warnings(f13o <- VIM::imputeCellGLoc(d13o))
  expect_false(is.null(f13o$em_starts))
  expect_false(any(grepl("second start failed", w13o, fixed = TRUE)))
  expect_false(anyNA(f13o$imputed$f))
  expect_true(is.ordered(f13o$imputed$f))

  # R64 I2 -- a factor with a contrasts attribute and an unused level: base R
  # drops the attribute with a "contrasts dropped from factor f due to missing
  # levels" warning wherever a design is built from it. The EM builds designs
  # that "level" does not (on the completed copy, and for the second start);
  # under "em" the fit must raise that warning no more often than under "level",
  # both when the factor itself has missing cells (A: "level" strips the
  # attribute with the NA level and is silent) and when another factor has (B).
  set.seed(1)
  n13d <- 300
  d13d <- data.frame(x1 = rnorm(n13d), x2 = rnorm(n13d), x3 = rnorm(n13d),
                     f = factor(sample(c("a", "b", "c"), n13d, TRUE),
                                levels = c("a", "b", "c", "z")),
                     g = factor(sample(c("u", "v"), n13d, TRUE)))
  contrasts(d13d$f) <- contr.sum(4)
  n_dropped <- function(dd, ...) {
    k <- 0L
    withCallingHandlers(VIM::imputeCellGLoc(dd, ...), warning = function(w) {
      if (grepl("contrasts dropped", conditionMessage(w), fixed = TRUE)) k <<- k + 1L
      invokeRestart("muffleWarning")
    })
    k
  }
  d13A <- d13d; d13A$f[1:30] <- NA
  d13B <- d13d; d13B$g[1:30] <- NA
  expect_true(n_dropped(d13A) <= n_dropped(d13A, categorical = "level"))
  expect_true(n_dropped(d13B) <= n_dropped(d13B, categorical = "level"))
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

# ==========================================================================
# Task 20: per-level detection (VIM 7.5.1; spec §12.9, tests 10 to 15)
# ==========================================================================
# --- one hand-built row: p = 4, one missing categorical cell (levels a, b), two
# candidates whose weight rows differ; weights 0 and 0.3 lie below the band
# (flagged), 0.8 and 1 above it (retained)
S11 <- matrix(c(1.0, 0.4, 0.2, 0.1,
                0.4, 1.5, 0.3, -0.2,
                0.2, 0.3, 0.8, 0.25,
                0.1, -0.2, 0.25, 1.2), 4)
B11 <- rbind(c(0.5, -0.3, 1.0, 0.2),                # (Intercept)
             c(2.0, 1.0, -1.5, 0.5))                # fb
X11 <- matrix(c(1.1, 0.2, 2.9, -0.4), 1)
M11 <- matrix(FALSE, 1, 4)
U11 <- rbind(c(1, 0), c(1, 1))                      # candidates a and b of row 1
W11 <- rbind(c(1, 0.8, 0.3, 0),
             c(0, 1, 0.8, 0.3))
lam11 <- VIM:::.gloc_lambda(S11)
half_obj <- function(k, w, M = M11)
  -0.5 * VIM:::.gloc_objective(X11 - U11[k, , drop = FALSE] %*% B11, matrix(w, 1), M,
                               S11, lam11)

# spec test 11 -- at the endpoints the score is -1/2 times the row's term of the
# binary-corner objective, with lambda from the same scatter
sc11 <- VIM:::.gloc_cat_score(X11, M11, W11, B11, S11, U11, c(1L, 1L))
expect_identical(length(sc11), 2L)
for (k in 1:2)
  expect_true(abs(sc11[k] - half_obj(k, W11[k, ])) < 1e-10, info = paste("candidate", k))
# a missing cell is left out whatever its weight: the mask decides, not the weight
M11m <- M11; M11m[1, 3] <- TRUE
W11m <- W11; W11m[1, 3] <- 0.8
sc11m <- VIM:::.gloc_cat_score(X11, M11m, W11m, B11, S11, U11, c(1L, 1L))
for (k in 1:2)
  expect_true(abs(sc11m[k] - half_obj(k, W11m[k, ], M11m)) < 1e-10,
              info = paste("missing cell, candidate", k))

# spec test 12 -- continuity: candidate b's x3 swept across the band in 201
# steps, its other cells retained. The ends are the flagged and the retained
# endpoint values, and no step moves the score by more than 5% of its range.
w12 <- seq(0.5 - VIM:::.gloc_peer_band, 0.5 + VIM:::.gloc_peer_band, length.out = 201)
W12 <- cbind(1, 1, w12, 1)
sc12 <- VIM:::.gloc_cat_score(X11, M11, W12, B11, S11, U11[rep(2L, 201), , drop = FALSE],
                              rep(1L, 201))
expect_true(abs(sc12[1] - half_obj(2, c(1, 1, 0, 1))) < 1e-10)
expect_true(abs(sc12[201] - half_obj(2, c(1, 1, 1, 1))) < 1e-10)
expect_true(max(abs(diff(sc12))) <= 0.05 * diff(range(sc12)))

# --- the score's fallbacks (fix round, M2). A singular scatter gives no penalty
# from .gloc_lambda, so c_j = sigma_jj with one warning, and the Cholesky factor
# of G fails, so log det G and G^-1 come from its eigenvalues floored at 1e-8
# times the largest. Hand computations of both routes.
floor_part <- function(G, y) {
  ev <- eigen(G, symmetric = TRUE)
  vals <- pmax(ev$values, 1e-8 * max(ev$values))
  sum(drop(crossprod(ev$vectors, y))^2 / vals) + sum(log(vals))
}
# matrix(1, 2, 2), all weights 1: both cells retained, so no penalty enters
Sfb2 <- matrix(1, 2, 2)
wfb2 <- collect_warnings(scfb2 <- VIM:::.gloc_cat_score(matrix(c(0.3, -0.2), 1),
                                                        matrix(FALSE, 1, 2), matrix(1, 1, 2),
                                                        matrix(0, 1, 2), Sfb2, matrix(1, 1, 1), 1L))
expect_equal(length(wfb2), 1L)
expect_true(startsWith(wfb2[1], "cellGLoc: "))
expect_true(is.finite(scfb2))
expect_equal(scfb2, -0.5 * (floor_part(Sfb2, c(0.3, -0.2)) + 2 * log(2 * pi)), tolerance = 1e-10)
# matrix(1, 3, 3), x3 flagged: the retained block is singular, and the flagged
# cell pays lambda_3 = qchisq(0.99, 1) + log 2 pi + log sigma_33
Sfb3 <- matrix(1, 3, 3)
wfb3 <- collect_warnings(scfb3 <- VIM:::.gloc_cat_score(matrix(c(0.3, -0.2, 2.5), 1),
                                                        matrix(FALSE, 1, 3), matrix(c(1, 1, 0), 1),
                                                        matrix(0, 1, 3), Sfb3, matrix(1, 1, 1), 1L))
expect_equal(length(wfb3), 1L)
expect_true(startsWith(wfb3[1], "cellGLoc: "))
expect_equal(scfb3, -0.5 * (floor_part(Sfb3[1:2, 1:2], c(0.3, -0.2)) + 2 * log(2 * pi)) -
               0.5 * (qchisq(0.99, 1) + log(2 * pi) + log(1)), tolerance = 1e-10)

# --- the R40 stall rule (fix round, I1): an iteration is progress when max |dW|
# or dR improves by more than 1% on its own best; progress resets the counter,
# anything else increments it
expect_identical(VIM:::.gloc_stall_update(dW = 0.10, dR = 0.05, dW_best = 0.10, dR_best = 0.10,
                                          stall = 7L),
                 list(dW_best = 0.10, dR_best = 0.05, stall = 0L))     # dW flat, dR improving
expect_identical(VIM:::.gloc_stall_update(dW = 0.05, dR = 0.10, dW_best = 0.10, dR_best = 0.10,
                                          stall = 7L),
                 list(dW_best = 0.05, dR_best = 0.10, stall = 0L))     # dW improving, dR flat
expect_identical(VIM:::.gloc_stall_update(dW = 0.0995, dR = 0.0995, dW_best = 0.10,
                                          dR_best = 0.10, stall = 7L),
                 list(dW_best = 0.10, dR_best = 0.10, stall = 8L))     # both flat (within 1%)
# without the EM dR is 0 at every iteration, and the counter follows the rule
# before 7.5.0, which read dW alone (bbe7379)
dW_seq <- c(1, 0.5, 0.498, 0.499, 0.3, 0.2985, 0.2984, 0.1)
old_best <- Inf; old_stall <- 0L; traj_old <- integer(0)
now <- list(dW_best = Inf, dR_best = Inf, stall = 0L); traj_now <- integer(0)
for (x in dW_seq) {
  if (x < 0.99 * old_best) { old_best <- x; old_stall <- 0L } else old_stall <- old_stall + 1L
  now <- VIM:::.gloc_stall_update(x, 0, now$dW_best, now$dR_best, now$stall)
  traj_old <- c(traj_old, old_stall); traj_now <- c(traj_now, now$stall)
}
expect_identical(traj_now, traj_old)
expect_identical(traj_old, c(0L, 0L, 1L, 2L, 0L, 1L, 2L, 0L))

if (requireNamespace("cellWise", quietly = TRUE)) {
  # s5 and fit5 (spec test 4 above): gen_cat(400, 5, miss_f = 0.2), the default fit
  X20 <- as.matrix(s5$d[, c("x1", "x2", "x3")])
  M20 <- is.na(X20)
  cp20 <- VIM:::.gloc_cat_prepare(s5$d, c("f", "g"))
  cand20 <- VIM:::.gloc_cat_candidates(cp20, s5$d, ~ .)
  inc20 <- which(rowSums(cp20$Mc)[cand20$pr_row] > 0L)   # candidates of incomplete rows
  rows20 <- cand20$pr_row[inc20]

  # spec test 13 -- reduction: every candidate given its row's weight row, the
  # score differs from 7.5.0's density term by one constant within each row, and
  # the E-step's posteriors are 7.5.0's
  sc13 <- VIM:::.gloc_cat_score(X20, M20, fit5$W[rows20, , drop = FALSE], fit5$B, fit5$Sigma,
                                cand20$Up[inc20, , drop = FALSE], rows20)
  ll13 <- VIM:::.gloc_cat_loglik(X20, M20, fit5$W, fit5$B, fit5$Sigma,
                                 cand20$Up[inc20, , drop = FALSE], rows20)
  spread13 <- tapply(sc13 - ll13, rows20, function(z) max(z) - min(z))
  expect_true(max(spread13) < 1e-10)
  es13a <- VIM:::.gloc_cat_estep(X20, M20, fit5$W, fit5$B, fit5$Sigma, cp20, cand20,
                                 fit5$cat_priors)
  es13b <- VIM:::.gloc_cat_estep(X20, M20, fit5$W, fit5$B, fit5$Sigma, cp20, cand20,
                                 fit5$cat_priors, Wc = fit5$W[cand20$pr_row, , drop = FALSE])
  expect_true(max(abs(es13b$post$f - es13a$post$f)) < 1e-10)

  # spec test 15 -- the returned W is the posterior mixture of the candidates' weight
  # rows, and the candidates' posterior weights sum to 1 per row
  cw15 <- fit5$cat_weights
  expect_identical(names(cw15), c("row", "levels", "prob", "W"))
  expect_identical(cw15$row, rows20)                         # candidate-table order
  expect_identical(dim(cw15$W), c(length(rows20), 3L))
  expect_identical(colnames(cw15$W), c("x1", "x2", "x3"))
  expect_true(max(abs(tapply(cw15$prob, cw15$row, sum) - 1)) < 1e-12)
  mix15 <- t(vapply(split(seq_along(cw15$row), cw15$row),
                    function(k) colSums(cw15$prob[k] * cw15$W[k, , drop = FALSE]), numeric(3)))
  expect_true(max(abs(fit5$W[as.integer(rownames(mix15)), ] - mix15)) < 1e-12)
  # levels: each candidate's level of f, and the row's observed g
  expect_true(is.data.frame(cw15$levels) && nrow(cw15$levels) == length(rows20))
  expect_identical(as.character(cw15$levels$f), as.character(cand20$Fp$f[inc20]))
  expect_identical(as.character(cw15$levels$g), as.character(s5$d$g[rows20]))
  expect_true(all(tapply(as.character(cw15$levels$f), cw15$row,
                         function(z) identical(sort(z), c("a", "b", "c")))))
  # NULL, but present, in the binary corner, under "level" and without a missing
  # categorical cell
  none15 <- list(binary   = suppressWarnings(VIM::imputeCellGLoc(s5$d, weights = "binary")),
                 level    = suppressWarnings(VIM::imputeCellGLoc(s5$d, categorical = "level")),
                 complete = suppressWarnings(VIM::imputeCellGLoc(s5$truth)))
  for (nm in names(none15)) {
    expect_true("cat_weights" %in% names(none15[[nm]]), info = nm)
    expect_null(none15[[nm]]$cat_weights, info = nm)
  }

  # spec tests 13 and 15 on rows missing both f and g (fix round, M3): s7 and fit7
  # (spec test 7 above), gen_cat(300, 7, miss_f = 0.2, miss_g = 0.2), the data of
  # the test-14 fixture, fitted in the soft corner. Such a row has one candidate per
  # level combination, and the mean-field sweeps use the score per combination.
  X7s <- as.matrix(s7$d[, c("x1", "x2", "x3")])
  M7s <- is.na(X7s)
  cp7s <- VIM:::.gloc_cat_prepare(s7$d, c("f", "g"))
  cand7s <- VIM:::.gloc_cat_candidates(cp7s, s7$d, ~ .)
  both7s <- which(rowSums(cp7s$Mc) == 2L)
  k7s <- which(cand7s$pr_row %in% both7s)             # their candidates in the table
  expect_true(length(both7s) > 0L && length(k7s) == 6L * length(both7s))
  sc7s <- VIM:::.gloc_cat_score(X7s, M7s, fit7$W[cand7s$pr_row[k7s], , drop = FALSE], fit7$B,
                                fit7$Sigma, cand7s$Up[k7s, , drop = FALSE], cand7s$pr_row[k7s])
  ll7s <- VIM:::.gloc_cat_loglik(X7s, M7s, fit7$W, fit7$B, fit7$Sigma,
                                 cand7s$Up[k7s, , drop = FALSE], cand7s$pr_row[k7s])
  expect_true(max(tapply(sc7s - ll7s, cand7s$pr_row[k7s], function(z) max(z) - min(z))) < 1e-10)
  es7a <- VIM:::.gloc_cat_estep(X7s, M7s, fit7$W, fit7$B, fit7$Sigma, cp7s, cand7s,
                                fit7$cat_priors)
  es7b <- VIM:::.gloc_cat_estep(X7s, M7s, fit7$W, fit7$B, fit7$Sigma, cp7s, cand7s,
                                fit7$cat_priors, Wc = fit7$W[cand7s$pr_row, , drop = FALSE])
  for (v in c("f", "g"))
    expect_true(max(abs(es7b$post[[v]][as.character(both7s), ] -
                          es7a$post[[v]][as.character(both7s), ])) < 1e-10, info = v)
  cw7s <- fit7$cat_weights
  kb7 <- which(cw7s$row %in% both7s)
  expect_identical(length(kb7), 6L * length(both7s))
  expect_true(max(abs(tapply(cw7s$prob[kb7], cw7s$row[kb7], sum) - 1)) < 1e-12)
  mix7s <- t(vapply(split(kb7, cw7s$row[kb7]),
                    function(k) colSums(cw7s$prob[k] * cw7s$W[k, , drop = FALSE]), numeric(3)))
  expect_true(max(abs(fit7$W[as.integer(rownames(mix7s)), ] - mix7s)) < 1e-12)
  # prob is the product of the marginal posteriors
  rk7 <- as.character(cw7s$row[kb7])
  expect_true(max(abs(cw7s$prob[kb7] -
                        fit7$cat_posterior$f[cbind(rk7, as.character(cw7s$levels$f[kb7]))] *
                        fit7$cat_posterior$g[cbind(rk7, as.character(cw7s$levels$g[kb7]))])) < 1e-12)

  # the non-convergence warning names the posterior-weighted weight change under
  # per-level detection (fix round, M4), and keeps the words the simulation scripts
  # classify by; without per-level detection its wording is unchanged
  expect_true(any(grepl("did not converge", w7, fixed = TRUE)))
  expect_true(any(grepl("max |dW| over the candidates weighted by their posteriors", w7,
                        fixed = TRUE)))
  w7l <- collect_warnings(VIM::imputeCellGLoc(s7$d, maxit = 1, categorical = "level"))
  expect_true(any(grepl("did not converge", w7l, fixed = TRUE)))
  expect_false(any(grepl("over the candidates", w7l, fixed = TRUE)))

  # per-level detection with rows above the combination cap (fix round, I2): three
  # 7-level factors give 343 > 256 combinations, so a row missing all three keeps
  # one weight row, at its mode design row, and enters no pseudo-row
  set.seed(2020)
  n_cap <- 300
  Fcap <- data.frame(h1 = factor(sample(letters[1:7], n_cap, TRUE)),
                     h2 = factor(sample(LETTERS[1:7], n_cap, TRUE)),
                     h3 = factor(sample(paste0("t", 1:7), n_cap, TRUE)))
  Xcap <- matrix(rnorm(n_cap * 3), n_cap) %*% chol(0.5 * diag(3) + 0.5)
  Xcap[, 1] <- Xcap[, 1] + 0.8 * as.integer(Fcap$h1)
  Xcap[, 2] <- Xcap[, 2] + 0.6 * as.integer(Fcap$h2)
  Xcap[, 3] <- Xcap[, 3] - 0.5 * as.integer(Fcap$h3)
  colnames(Xcap) <- paste0("x", 1:3)
  dcap <- data.frame(Xcap, Fcap)
  capped <- 1:4                                     # rows missing all three factors
  dcap[capped, c("h1", "h2", "h3")] <- NA
  dcap$x2[capped[1:2]] <- NA                         # and continuous cells in two of them
  dcap$h1[11:40] <- NA; dcap$h2[41:60] <- NA; dcap$h3[61:75] <- NA   # one factor missing
  wcap <- collect_warnings(fcap <- VIM::imputeCellGLoc(dcap))
  expect_equal(sum(grepl("more than 256 combinations", wcap, fixed = TRUE)), 1L)
  cwcap <- fcap$cat_weights
  expect_false(any(capped %in% cwcap$row))
  expect_identical(sort(unique(cwcap$row)), 11:75)
  Wcap <- fcap$W[capped, , drop = FALSE]
  expect_true(all(Wcap >= 0 & Wcap <= 1))
  expect_true(all(Wcap[is.na(as.matrix(dcap[capped, c("x1", "x2", "x3")]))] == 0))
  expect_true(max(abs(tapply(cwcap$prob, cwcap$row, sum) - 1)) < 1e-12)
  mixcap <- t(vapply(split(seq_along(cwcap$row), cwcap$row),
                     function(k) colSums(cwcap$prob[k] * cwcap$W[k, , drop = FALSE]), numeric(3)))
  expect_true(max(abs(fcap$W[as.integer(rownames(mixcap)), ] - mixcap)) < 1e-12)
  # maxit = 0 returns the start's W: the robust start on the prior-expected design
  # rows (the capped rows at their mode rows), fitted on the complete-category rows
  fcap0 <- suppressWarnings(VIM::imputeCellGLoc(dcap, maxit = 0))
  Xc0 <- as.matrix(dcap[, c("x1", "x2", "x3")]); Mc0 <- is.na(Xc0)
  cpc0 <- VIM:::.gloc_cat_prepare(dcap, c("h1", "h2", "h3"))
  candc0 <- suppressWarnings(VIM:::.gloc_cat_candidates(cpc0, dcap, ~ .))
  ccc0 <- rowSums(cpc0$Mc) == 0L
  pric0 <- VIM:::.gloc_cat_fit_priors(cpc0$F[ccc0, , drop = FALSE], rep(1, sum(ccc0)),
                                      cpc0$levels)
  esc0 <- VIM:::.gloc_cat_estep(Xc0, Mc0, NULL, NULL, NULL, cpc0, candc0, pric0)
  stc0 <- VIM:::.gloc_start_robust(Xc0, esc0$Ubar, Mc0, warn_design = FALSE,
                                   patterns = candc0$pats_rows, fit_rows = ccc0)
  expect_identical(fcap0$W, stc0$W)
  expect_identical(fcap0$B, stc0$B)
}

# spec test 14 -- the binary corner under "em", with missing categorical cells,
# returns 7.5.0's fit (Ruling R72). A regression guard: it passed before 7.5.1.
# The fixture was built with the durable VIM 7.5.0 library (commit 15b08a3)
# before any 7.5.1 code existed:
#   .libPaths(c("<VIM 7.5.0 library>", .libPaths())); library(VIM)
#   stopifnot(packageVersion("VIM") == "7.5.0")
#   d <- gen_cat(300, 7, miss_f = 0.2, miss_g = 0.2)$d        # gen_cat of this file
#   fit <- imputeCellGLoc(d, weights = "binary")
#   saveRDS(list(data = d,
#                fit = fit[c("B", "Sigma", "W", "U", "imputed", "cat_posterior",
#                            "criterion", "converged", "iterations")],
#                vim = "7.5.0", commit = "15b08a3"),
#           "inst/tinytest/gloc_em_binary_ref_750.rds")
if (at_home() && requireNamespace("cellWise", quietly = TRUE)) {
  ref750 <- readRDS("gloc_em_binary_ref_750.rds")
  bitref750 <- identical(Sys.getenv("VIM_BITREF"), "true")
  fit14 <- suppressWarnings(VIM::imputeCellGLoc(ref750$data, weights = "binary"))
  expect_identical(fit14$converged, ref750$fit$converged)
  expect_identical(fit14$iterations, ref750$fit$iterations)
  for (k in c("B", "Sigma", "W", "U", "imputed", "cat_posterior", "criterion")) {
    if (bitref750) expect_identical(fit14[[k]], ref750$fit[[k]], info = k)
    else expect_equal(fit14[[k]], ref750$fit[[k]], tolerance = 1e-10, info = k)
  }
}

# spec test 10 -- the flag lock (regression). A three-level factor shifts x1 by
# +3, 0, -3 and x2 by +2, 0, 0; g shifts x3. In 7.5.0 detection in a row with f
# missing ran at the expected design row, flagged the decisive cell x1 there, and
# the E-step then dropped it, so the posterior could not move. On the rows with f
# missing, (a) the posterior mode hits at least as often as the classifier that
# uses the fit's own B, Sigma and prior on every observed continuous cell, less
# 0.05, and (b) x1 has W < 0.5 in at most 8% of them. Both failed on 7.5.0
# (hit 0.917 against 1.000, x1 flagged in 12.5%).
if (at_home() && requireNamespace("cellWise", quietly = TRUE)) {
  gen_lock <- function(n, seed) {
    set.seed(seed)
    f <- factor(sample(c("A", "B", "C"), n, TRUE, prob = c(0.5, 0.3, 0.2)),
                levels = c("A", "B", "C"))
    g <- factor(sample(c("u", "v"), n, TRUE))                  # independent of f
    X <- matrix(rnorm(n * 4), n) %*% chol(0.5 * diag(4) + 0.5)
    X[, 1] <- X[, 1] + c(3, 0, -3)[as.integer(f)]
    X[, 2] <- X[, 2] + c(2, 0, 0)[as.integer(f)]
    X[, 3] <- X[, 3] + (g == "v")
    colnames(X) <- paste0("x", 1:4)
    truth <- data.frame(X, f = f, g = g)
    d <- truth
    d$f[sample(n, round(0.12 * n))] <- NA
    for (v in c("x2", "x3", "x4")) d[[v]][sample(n, round(0.10 * n))] <- NA   # x1 observed
    list(d = d, truth = truth)
  }
  s10 <- gen_lock(400, 1)
  f10 <- suppressWarnings(VIM::imputeCellGLoc(s10$d))
  rows10 <- which(is.na(s10$d$f))
  cp10 <- VIM:::.gloc_cat_prepare(s10$d, c("f", "g"))
  lev10 <- cp10$levels$f
  P10 <- VIM:::.gloc_cat_prior(f10$cat_priors$f, cp10$F[rows10, , drop = FALSE])
  X10 <- as.matrix(s10$d[, paste0("x", 1:4)])
  noflag10 <- vapply(seq_along(rows10), function(k) {
    i <- rows10[k]; o <- which(!is.na(X10[i, ]))
    Fi <- cp10$F[rep(i, length(lev10)), , drop = FALSE]
    Fi$f <- factor(lev10, levels = lev10)
    Ui <- VIM:::.gloc_design_rows(Fi, ~ ., cp10$levels)
    mu <- Ui %*% f10$B[colnames(Ui), , drop = FALSE]      # by name, not by position
    So <- f10$Sigma[o, o, drop = FALSE]
    s <- vapply(seq_along(lev10), function(l) {
      r <- X10[i, o] - mu[l, o]
      log(P10[k, l]) - 0.5 * (drop(r %*% solve(So, r)) +
                                as.numeric(determinant(So)$modulus) + length(o) * log(2 * pi))
    }, 0)
    lev10[which.max(s)]
  }, "")
  truth10 <- as.character(s10$truth$f[rows10])
  hit10 <- mean(as.character(f10$imputed$f[rows10]) == truth10)
  expect_true(hit10 >= mean(noflag10 == truth10) - 0.05)
  expect_true(mean(f10$W[rows10, "x1"] < 0.5) <= 0.08)
}

# ==========================================================================
# Task 21: mixture imputation, the MI draws and R65 (spec §12.9, tests 16, 17)
# ==========================================================================
# The fixture needs what gen_cat() alone does not produce: missing CONTINUOUS
# cells in rows that also miss a categorical cell. Those are the only rows the
# mixture imputation touches. f shifts x1 and x2 by 0, 4, -3 and x3 by half of
# that, so a row's candidates sit far apart and their flags differ, which is
# what makes the rule visible at all.
if (requireNamespace("cellWise", quietly = TRUE)) {
  s21 <- gen_cat(300, 21, miss_f = 0.2, miss_g = 0.2)
  d21 <- s21$d
  mis21 <- which(rowSums(is.na(d21[, c("f", "g")])) > 0L)
  obs21 <- setdiff(seq_len(300), mis21)
  d21$x1[mis21[seq(1L, length(mis21), by = 3L)]] <- NA
  d21$x3[mis21[seq(2L, length(mis21), by = 3L)]] <- NA
  d21$x2[obs21[1:10]] <- NA                       # and some outside those rows
  fit21 <- suppressWarnings(VIM::imputeCellGLoc(d21))
  X21 <- as.matrix(d21[, c("x1", "x2", "x3")])
  M21 <- is.na(X21)
  cp21 <- VIM:::.gloc_cat_prepare(d21, c("f", "g"))
  cand21 <- VIM:::.gloc_cat_candidates(cp21, d21, ~ .)
  inc21 <- which(rowSums(cp21$Mc)[cand21$pr_row] > 0L)
  cw21 <- fit21$cat_weights
  expect_identical(cw21$row, cand21$pr_row[inc21])
  expect_true(sum(M21[unique(cw21$row), ]) >= 20L)   # the rows the mixture touches

  # spec test 16, first bullet -- the continuous cells of such a row are the
  # posterior mixture of .gloc_impute() over the row's candidates, each at its
  # own design row with its own weight row. The reference takes the design rows
  # from the candidate table rather than from cat_weights$levels, so it pins
  # that the two routes agree as well.
  k21 <- which(rowSums(M21[cw21$row, , drop = FALSE]) > 0L)
  rw21 <- cw21$row[k21]
  Xk21 <- VIM:::.gloc_impute(X21[rw21, , drop = FALSE],
                             cand21$Up[inc21[k21], , drop = FALSE],
                             fit21$B, fit21$Sigma, M21[rw21, , drop = FALSE],
                             W = cw21$W[k21, , drop = FALSE])
  mix21 <- rowsum(Xk21 * cw21$prob[k21], rw21, reorder = TRUE)
  at21 <- as.integer(rownames(mix21))
  mm21 <- M21[at21, , drop = FALSE]
  got21 <- as.matrix(fit21$imputed[at21, c("x1", "x2", "x3")])
  expect_true(max(abs(got21[mm21] - mix21[mm21])) < 1e-10)
  # the rule is not the 7.5.0 one (impute at the expected design row with the
  # mixture W), so the test above is not vacuous
  old21 <- VIM:::.gloc_impute(X21, fit21$U, fit21$B, fit21$Sigma, M21,
                              W = fit21$W)[at21, , drop = FALSE]
  expect_true(max(abs(old21[mm21] - mix21[mm21])) > 1e-6)
  # rows without a missing categorical cell keep the 7.5.0 imputation
  oth21 <- setdiff(which(rowSums(M21) > 0L), at21)
  expect_true(length(oth21) > 0L)
  expect_identical(unname(as.matrix(fit21$imputed[oth21, c("x1", "x2", "x3")])),
                   unname(VIM:::.gloc_impute(X21, fit21$U, fit21$B, fit21$Sigma, M21,
                                             W = fit21$W)[oth21, , drop = FALSE]))

  # the same under design = ~ 1, which has no combination table, so the mixture's
  # candidate design rows come from the other branch of .gloc_cat_candidates()
  f21i <- suppressWarnings(VIM::imputeCellGLoc(d21, design = ~ 1))
  cwi <- f21i$cat_weights
  ki <- which(rowSums(M21[cwi$row, , drop = FALSE]) > 0L)
  Xki <- VIM:::.gloc_impute(X21[cwi$row[ki], , drop = FALSE],
                            matrix(1, length(ki), 1, dimnames = list(NULL, "(Intercept)")),
                            f21i$B, f21i$Sigma, M21[cwi$row[ki], , drop = FALSE],
                            W = cwi$W[ki, , drop = FALSE])
  mixi <- rowsum(Xki * cwi$prob[ki], cwi$row[ki], reorder = TRUE)
  ati <- as.integer(rownames(mixi)); mmi <- M21[ati, , drop = FALSE]
  expect_true(max(abs(as.matrix(f21i$imputed[ati, c("x1", "x2", "x3")])[mmi] -
                        mixi[mmi])) < 1e-10)

  # spec test 16, second bullet
  expect_identical(VIM:::.gloc_draw_mi(fit21, d21, noise = FALSE), fit21$imputed)
  expect_identical(VIM:::.gloc_draw_mi(f21i, d21, design = ~ 1, noise = FALSE),
                   f21i$imputed)

  # spec test 16, third bullet -- a draw with the levels fixed imputes each row
  # that misses a categorical cell with the weight row of the candidate drawn,
  # not with the row's own (mixture) weights. The posteriors are made degenerate
  # at the LEAST likely level, so the drawn levels are determined and differ from
  # the modes the fit reports; the reference then repeats the draw's own
  # random-number order -- one runif() block per categorical variable, then one
  # rnorm() per row with a missing cell in the order .gloc_impute() names them.
  fix21 <- fit21
  Fd21 <- cp21$F
  for (v in names(fix21$cat_posterior)) {
    P <- fix21$cat_posterior[[v]]
    sel <- max.col(-P, ties.method = "first")
    P[] <- 0; P[cbind(seq_len(nrow(P)), sel)] <- 1
    fix21$cat_posterior[[v]] <- P
    Fd21[[v]][as.integer(rownames(P))] <- colnames(P)[sel]
  }
  Ud21 <- VIM:::.gloc_design_rows(Fd21, ~ ., cp21$levels)
  keyc21 <- do.call(paste, c(lapply(cw21$levels, as.character), list(sep = "\r")))
  keyd21 <- do.call(paste, c(lapply(Fd21, as.character), list(sep = "\r")))
  rws21 <- sort(unique(cw21$row))
  kk21 <- match(paste(rws21, keyd21[rws21]), paste(cw21$row, keyc21))
  expect_false(anyNA(kk21))
  Wd21 <- fit21$W
  Wd21[rws21, ] <- cw21$W[kk21, , drop = FALSE]
  expect_true(max(abs(Wd21 - fit21$W)) > 0.1)        # the two rules really differ
  ref_draw21 <- function(Wuse) {
    set.seed(2126)
    for (v in names(fix21$cat_posterior)) stats::runif(nrow(fix21$cat_posterior[[v]]))
    im <- VIM:::.gloc_impute(X21, Ud21, fit21$B, fit21$Sigma, M21, W = Wuse, cov = TRUE)
    Xi <- im$X
    for (nm in names(im$cond_cov)) {
      i <- as.integer(nm); ms <- which(M21[i, ])
      Xi[i, ms] <- Xi[i, ms] +
        drop(stats::rnorm(length(ms)) %*% VIM:::.gloc_chol_psd(im$cond_cov[[nm]]))
    }
    Xi
  }
  set.seed(2126)
  drw21 <- VIM:::.gloc_draw_mi(fix21, d21)
  drwX21 <- as.matrix(drw21[, c("x1", "x2", "x3")])
  expect_identical(as.character(drw21$f[rws21]), as.character(Fd21$f[rws21]))
  expect_true(max(abs(drwX21 - ref_draw21(Wd21))) < 1e-12)
  expect_true(max(abs(drwX21 - ref_draw21(fit21$W))) > 1e-6)

  # Task 20's review left this owed here: in the soft corner the posteriors are a
  # function of the CANDIDATES' weight rows, so .gloc_cat_posterior_for()
  # reproduces them only once it is given cat_weights (spec §12.9, "Multiple
  # imputation"). The binary-corner case is above, under Task 7b.
  fo21 <- VIM:::.gloc_cat_posterior_for(fit21, d21, W = fit21$W,
                                        cat_weights = fit21$cat_weights)
  expect_equal(fo21$cat_posterior, fit21$cat_posterior, tolerance = 1e-10)
  expect_equal(unname(fo21$U), unname(fit21$U), tolerance = 1e-10)
  expect_identical(attr(fo21, "dropped"), 0L)
  expect_identical(fo21$cat_weights$row, cw21$row)
  expect_identical(fo21$cat_weights$W, cw21$W)
  expect_equal(fo21$cat_weights$prob, cw21$prob, tolerance = 1e-10)
  # without them, the 7.5.0 E-step, which is a different answer here
  fo21b <- VIM:::.gloc_cat_posterior_for(fit21, d21, W = fit21$W)
  expect_true(max(abs(fo21b$cat_posterior$f - fit21$cat_posterior$f)) > 1e-6)
  expect_null(fo21b$cat_weights)
  # cat_weights that do not belong to this data's candidate table are refused
  bad21 <- cw21; bad21$row <- rev(bad21$row)
  expect_error(VIM:::.gloc_cat_posterior_for(fit21, d21, W = fit21$W, cat_weights = bad21),
               "candidate table")
  bad21b <- cw21; bad21b$levels$f <- rev(bad21b$levels$f)
  expect_error(VIM:::.gloc_cat_posterior_for(fit21, d21, W = fit21$W, cat_weights = bad21b),
               "candidate table")

  # the mixture's own two refusals (fix round, M1). A design other than the fit's
  # gives the candidate rows the wrong columns: in .gloc_impute_mix() directly, and
  # through the point draw, which is the route a caller reaches it by.
  Xi21 <- VIM:::.gloc_impute(X21, fit21$U, fit21$B, fit21$Sigma, M21, W = fit21$W)
  expect_error(VIM:::.gloc_impute_mix(Xi21, X21, M21, fit21$B, fit21$Sigma, cw21,
                                      ~ f, cp21$levels),
               "do not match B")
  expect_error(VIM:::.gloc_draw_mi(fit21, d21, design = ~ f, noise = FALSE),
               "do not match B")
  # a drawn level combination that cat_weights does not hold. One row's candidates
  # are all relabelled to a level that row was not drawn, so the row is still in
  # cat_weights$row -- a row that is absent from it is a capped row and
  # legitimately keeps fit$W -- but its drawn combination matches none of them.
  r0 <- rws21[which(as.character(Fd21$f[rws21]) != "a")[1]]
  bad21c <- fix21
  bad21c$cat_weights$levels$f[bad21c$cat_weights$row == r0] <- "a"
  expect_error(VIM:::.gloc_draw_mi(bad21c, d21), "not in")
}

# spec test 17 -- R65 (Ruling R79): the design drops a factor's own contrasts
# attribute whenever the factor has an unused level (both modes) or, under
# "level", whenever a missing value of it becomes a level of its own through
# addNA(). Both losses were silent to the user before 7.5.1: the first arrived
# as base R's "contrasts dropped from factor ... due to missing levels", which
# the EM's extra design sites had to muffle, the second not at all. The fit now
# says so once per factor, with the "cellGLoc: " prefix, and no base-R copy
# reaches the user. The data are the settling script's of the Task 13 fix wave.
#
# "The numbers do not change" is pinned twice. Within the fit, by taking the
# attribute away, which must leave everything the fit returns alone. Across the
# change, by the fixture gloc_r65_ref_pre.rds, built with the commit BEFORE R65
# (baccf09, the mixture-imputation commit of this task) so that a later edit to
# .gloc_contrast_lost() or to one of the muffled design sites cannot move a
# fitted number unnoticed:
#   .libPaths(c("<library with VIM at baccf09>", .libPaths())); library(VIM)
#   stopifnot(packageVersion("VIM") == "7.5.1")
#   <the set.seed(1) block and cases17 below, as `cases`>
#   fits <- list()
#   for (nm in names(cases)) for (md in c("em", "level")) {
#     f <- suppressWarnings(imputeCellGLoc(cases[[nm]], categorical = md))
#     fits[[paste(nm, md)]] <- f[c("B", "Sigma", "W", "imputed")]
#   }
#   saveRDS(list(cases = cases, fits = fits, vim = "7.5.1", commit = "baccf09"),
#           "inst/tinytest/gloc_r65_ref_pre.rds")
set.seed(1)
n17 <- 300
d17 <- data.frame(x1 = rnorm(n17), x2 = rnorm(n17), x3 = rnorm(n17),
                  f = factor(sample(c("a", "b", "c"), n17, TRUE),
                             levels = c("a", "b", "c", "z")),
                  g = factor(sample(c("u", "v"), n17, TRUE)))
contrasts(d17$f) <- contr.sum(4)
cases17 <- list(A = local({ z <- d17; z$f[1:30] <- NA; z }),   # the factor's own cells
                B = local({ z <- d17; z$g[1:30] <- NA; z }),   # another factor's
                C = d17)                                       # neither
# the arms: both modes in the soft corner, and "level" in the binary corner --
# the warning is keyed on the design, so it must not depend on the corner
arms17 <- list("em" = list(categorical = "em"),
               "level" = list(categorical = "level"),
               "level/binary" = list(categorical = "level", weights = "binary"))
if (requireNamespace("cellWise", quietly = TRUE)) {
  strip17 <- function(dd) lapply(dd, function(z) if (is.factor(z)) as.character(z) else z)
  for (nm17 in names(cases17)) for (md17 in names(arms17)) {
    inf17 <- paste(nm17, md17)
    w17 <- collect_warnings(
      f17 <- do.call(VIM::imputeCellGLoc, c(list(cases17[[nm17]]), arms17[[md17]])))
    hit17 <- grepl("^cellGLoc: .*contrasts", w17)
    expect_identical(sum(hit17), 1L, info = inf17)
    expect_true(all(grepl("'f'", w17[hit17], fixed = TRUE)), info = inf17)
    expect_false(any(grepl("contrasts dropped", w17, fixed = TRUE)), info = inf17)
    # The warning is true: if the design ignores the attribute, taking it away
    # must change no number. A second fit per case, so it is at_home only.
    if (at_home()) {
      dz17 <- cases17[[nm17]]; attr(dz17$f, "contrasts") <- NULL
      f17z <- suppressWarnings(
        do.call(VIM::imputeCellGLoc, c(list(dz17), arms17[[md17]])))
      for (k17 in c("B", "Sigma", "W", "U", "criterion"))
        expect_identical(f17[[k17]], f17z[[k17]], info = paste(inf17, k17))
      expect_identical(strip17(f17$imputed), strip17(f17z$imputed), info = inf17)
    }
  }
  # a factor whose contrasts the design DOES use stays silent
  d17ok <- d17; d17ok$f <- droplevels(d17ok$f); contrasts(d17ok$f) <- contr.sum(3)
  w17ok <- collect_warnings(f17ok <- VIM::imputeCellGLoc(d17ok))
  expect_false(any(grepl("contrasts", w17ok, fixed = TRUE)))
  expect_identical(rownames(f17ok$B), c("(Intercept)", "f1", "f2", "gv"))
}

# the pre-R65 fixture (fix round, I1): every fit of the six cases is what the
# commit before the warning returned
if (at_home() && requireNamespace("cellWise", quietly = TRUE)) {
  ref65 <- readRDS("gloc_r65_ref_pre.rds")
  bitref65 <- identical(Sys.getenv("VIM_BITREF"), "true")
  expect_identical(ref65$commit, "baccf09")
  expect_identical(ref65$cases, cases17)          # the fixture's data are these data
  for (nm65 in names(ref65$fits)) {
    md65 <- sub("^. ", "", nm65)
    f65 <- suppressWarnings(
      VIM::imputeCellGLoc(ref65$cases[[substr(nm65, 1L, 1L)]], categorical = md65))
    for (k65 in c("B", "Sigma", "W", "imputed")) {
      if (bitref65) expect_identical(f65[[k65]], ref65$fits[[nm65]][[k65]],
                                     info = paste(nm65, k65))
      else expect_equal(f65[[k65]], ref65$fits[[nm65]][[k65]], tolerance = 1e-10,
                        info = paste(nm65, k65))
    }
  }
}
