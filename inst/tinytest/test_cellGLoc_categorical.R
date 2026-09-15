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
# a factor with its own contrasts attribute stops with the hint (R26)
d5s <- d5; contrasts(d5s$f) <- contr.sum(3)
expect_error(VIM:::.gloc_cat_candidates(VIM:::.gloc_cat_prepare(d5s, c("f", "g")), d5s, ~ .),
             "categorical = \"level\"")

# a row with more combinations than the cap stays out of the table, once warned
wc <- collect_warnings(cand5c <- VIM:::.gloc_cat_candidates(cp5, d5, ~ ., max_combos = 2L))
expect_equal(length(wc), 1L)
expect_true(grepl("^cellGLoc: ", wc))
expect_false(1L %in% cand5c$pr_row)
es5c <- VIM:::.gloc_cat_estep(X5, M5, matrix(1, 7, 2), B5, diag(2), cp5, cand5c, pri5)
expect_equal(unname(es5c$post$f["1", ]), c(2, 2, 2) / 6, tolerance = 1e-12)   # complete-row frequencies
expect_identical(VIM:::.gloc_cat_change(es5$post, NULL), Inf)
expect_identical(VIM:::.gloc_cat_change(es5$post, es5$post), 0)
