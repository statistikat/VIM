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
