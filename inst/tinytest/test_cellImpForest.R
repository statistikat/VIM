gen <- function(n = 300, p = 5, rho = 0.6, seed = 1) {
  set.seed(seed)
  Sig <- matrix(rho, p, p); diag(Sig) <- 1
  as.data.frame(matrix(stats::rnorm(n * p), n) %*% chol(Sig))
}
d <- gen()

# clean data: few false flags, converges, complete output of the right shape
set.seed(2); r0 <- cellImpForest(d, num.trees = 200)
expect_true(mean(r0$flags) <= 0.04)                  # spike: 0.016
expect_true(r0$converged)
expect_equal(dim(r0$imputed), dim(d))
expect_false(anyNA(r0$imputed))
expect_true(all(r0$W >= 0 & r0$W <= 1))
expect_true(inherits(r0, "cellImpForest"))
expect_equal(length(r0$scales), ncol(d))

# one gross cell: flagged and repaired towards the clean value
d1 <- d; d1[7, 2] <- d1[7, 2] + 10
set.seed(2); r1 <- cellImpForest(d1, num.trees = 200)
expect_true(r1$flags[7, 2])
expect_true(abs(r1$imputed[7, 2] - d[7, 2]) < 2)
expect_true(all(abs(r1$Z[r1$flags & !r1$missing]) > 2.53))    # release pass: survivors are real

# missing cells plus the gross cell
d2 <- d1; set.seed(9); d2[cbind(sample(300, 30), sample(5, 30, TRUE))] <- NA
set.seed(2); r2 <- cellImpForest(d2, num.trees = 200)
expect_false(anyNA(r2$imputed))
expect_true(r2$flags[7, 2])
expect_equal(r2$missing, is.na(d2))
expect_equal(r2$imputed[!is.na(d2) & !r2$flags], d2[!is.na(d2) & !r2$flags])  # observed cells kept

# deterministic under set.seed
set.seed(3); a <- cellImpForest(d2, num.trees = 100)
set.seed(3); b <- cellImpForest(d2, num.trees = 100)
expect_equal(a$imputed, b$imputed)

# one detection pass still finds the gross cell; maxit_detect = 0 never flags
set.seed(2); r3 <- cellImpForest(d1, maxit_detect = 1, num.trees = 200)
expect_true(r3$flags[7, 2])
set.seed(2); r4 <- cellImpForest(d2, maxit_detect = 0, num.trees = 100)
expect_equal(sum(r4$flags), 0L)
expect_false(anyNA(r4$imputed))
expect_true(r4$converged)

# xgboost engine on the same gross cell
set.seed(2); r5 <- cellImpForest(d1, engine = "xgboost", nrounds = 100)
expect_true(r5$flags[7, 2])

# review focus 1: constant column
dc <- d; dc$V6 <- 1
set.seed(2); rc <- suppressWarnings(cellImpForest(dc, num.trees = 50, maxit = 2))
expect_false(anyNA(rc$imputed))
expect_equal(sum(rc$flags[, 6]), 0L)

# review focus 2: too few usable rows -> column not modelled, warned, output complete
dt <- d[1:12, 1:3]; dt[1:3, 1] <- NA
expect_warning(rt <- cellImpForest(dt, num.trees = 30), "not modelled")
expect_false(anyNA(rt$imputed))
expect_message(suppressWarnings(cellImpForest(dt, num.trees = 30, trace = TRUE)), "skipped")

# review focus 3: a row with nothing observed
da <- d; da[1, ] <- NA
set.seed(2); ra <- suppressWarnings(cellImpForest(da, num.trees = 50, maxit = 2))
expect_false(anyNA(ra$imputed))

# errors
dn <- d; dn$V3 <- NA
expect_error(cellImpForest(dn), "entirely missing")
expect_error(cellImpForest(d[, 1, drop = FALSE]), "two columns")

# --- categorical cells ---
set.seed(5); n <- 400
z <- stats::rnorm(n)
x1 <- z + 0.5 * stats::rnorm(n); x2 <- z + 0.5 * stats::rnorm(n); x3 <- z + 0.5 * stats::rnorm(n)
g <- factor(ifelse(z > 0.3, "high", ifelse(z < -0.3, "low", "mid")))
dcat <- data.frame(x1, x2, x3, g)
bad <- sample(which(g == "high"), 10)
dcat$g[bad] <- "low"                                   # miscoded: contradicts x1..x3
lab <- seq_len(n) %in% bad
auc6 <- function(score, lab) {                       # Mann-Whitney AUC
  r <- rank(score)
  (sum(r[lab]) - sum(lab) * (sum(lab) + 1) / 2) / (sum(lab) * sum(!lab))
}
set.seed(6); rg <- cellImpForest(dcat, num.trees = 300)
expect_true(auc6(-rg$P[, 4], lab) > 0.9)                # miscoded cells are the least plausible
expect_true(sum(rg$flags[bad, 4]) >= 7)
expect_true(mean(rg$flags[!lab, 4]) <= 0.05)
expect_true(all(rg$imputed$g[bad][rg$flags[bad, 4]] != "low"))   # flagged cells repaired
expect_true(all(rg$W[, 4] %in% c(0, 1)))               # categorical cells: no soft weight
dm <- dcat; dm$g[1:40] <- NA
set.seed(6); rm_ <- cellImpForest(dm, num.trees = 100)
expect_false(anyNA(rm_$imputed$g)); expect_equal(levels(rm_$imputed$g), levels(dcat$g))
# review focus 4: rare level, single-level column, logical and character columns keep their type
dr <- dcat; dr$g <- as.character(dr$g); dr$g[1] <- "rare"
set.seed(6); rr <- suppressWarnings(cellImpForest(dr, num.trees = 100, maxit = 2))
expect_true(is.character(rr$imputed$g)); expect_false(anyNA(rr$imputed$g))
ds <- dcat; ds$one <- factor("a"); ds$flag <- x2 > 0; ds$flag[1:5] <- NA
expect_message(rs <- suppressWarnings(cellImpForest(ds, num.trees = 50, maxit = 2, trace = TRUE)), "skipped")
expect_true(is.logical(rs$imputed$flag)); expect_false(anyNA(rs$imputed))
# xgboost engine on the categorical column (its probabilities are sharper: no false-flag bound here)
set.seed(6); rx <- suppressWarnings(cellImpForest(dcat, engine = "xgboost", nrounds = 80, maxit = 3))
expect_true(auc6(-rx$P[, 4], lab) > 0.9)
expect_true(sum(rx$flags[bad, 4]) >= 7)

# fix round 1: a declared-but-unused level, and an ordered factor, survive unchanged
du <- dcat; levels(du$g) <- c(levels(dcat$g), "unused")
set.seed(6); ru <- cellImpForest(du, num.trees = 300)
expect_identical(levels(ru$imputed$g), levels(du$g))
dord <- dcat; dord$g <- factor(dcat$g, levels = c("low", "mid", "high"), ordered = TRUE)
set.seed(6); rord <- cellImpForest(dord, num.trees = 300)
expect_identical(levels(rord$imputed$g), levels(dord$g))
expect_true(is.ordered(rord$imputed$g))
