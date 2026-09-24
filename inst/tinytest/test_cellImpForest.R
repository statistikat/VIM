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
# final review I2: `released` marks cells flagged in the loop and restored by the release pass
expect_true(is.matrix(r1$released) && is.logical(r1$released))
expect_equal(dim(r1$released), dim(d1))
expect_false(any(r1$released & (r1$flags | r1$missing)))
expect_equal(r1$imputed[r1$released], d1[r1$released])       # restored cells hold their values

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

# final review I3: maxit_detect is capped at maxit - 1, so the release pass judges every flagged
# cell with a fit that never saw it; maxit = 1 turns detection off
d15 <- d; set.seed(21); c15 <- cbind(sample(300, 15), sample(5, 15, TRUE))
d15[c15] <- d15[c15] + 4                                     # 15 cells shifted by 4 SD
set.seed(2); r15 <- suppressWarnings(cellImpForest(d15, maxit = 2, num.trees = 200))
expect_true(all(r15$flags[c15]))
set.seed(2); r15a <- suppressWarnings(cellImpForest(d15, maxit = 1, num.trees = 200))
expect_equal(sum(r15a$flags), 0L)
expect_message(suppressWarnings(cellImpForest(d15, maxit = 2, maxit_detect = 3, num.trees = 50)),
               "maxit_detect")
msg15 <- character(0)                                         # the default is capped silently
withCallingHandlers(suppressWarnings(cellImpForest(d15, maxit = 2, num.trees = 50)),
                    message = function(m) {
                      msg15 <<- c(msg15, conditionMessage(m)); invokeRestart("muffleMessage")
                    })
expect_false(any(grepl("maxit_detect", msg15)))

# xgboost engine on the same gross cell
set.seed(2); r5 <- cellImpForest(d1, engine = "xgboost", nrounds = 100)
expect_true(r5$flags[7, 2])

# final review I4: the change measure uses the MAD -> SD -> 1 scale of each column, so a column
# whose MAD is 0 (60 % zeros) does not blow it up
dz <- d; set.seed(31)
dz$V6 <- exp(dz$V1 + stats::rnorm(300, sd = 0.3)); dz$V6[sample(300, 180)] <- 0
dz$V6[sample(300, 30)] <- NA
msgz <- character(0)
set.seed(2)
withCallingHandlers(suppressWarnings(cellImpForest(dz, num.trees = 100, trace = TRUE)),
                    message = function(m) {
                      msgz <<- c(msgz, conditionMessage(m)); invokeRestart("muffleMessage")
                    })
chg <- as.numeric(sub(".*change ", "", grep("change", msgz, value = TRUE)))
expect_true(length(chg) > 0)
expect_true(all(chg < 100))

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

# fix round 2: a column that fits early, then starves once its own cells are flagged
d10 <- gen(n = 10, p = 3, seed = 6); d10[1, 1] <- d10[1, 1] + 15
set.seed(2); expect_warning(r10 <- cellImpForest(d10, num.trees = 80), "after iteration")
expect_false(anyNA(r10$imputed))

# final review I6: integer columns are returned as double, also when none of their cells is imputed
di <- d[1:100, 1:3]; di$k <- 1L + (seq_len(100) %% 3L)
set.seed(2); ri <- suppressWarnings(cellImpForest(di, num.trees = 50, maxit_detect = 0))
expect_true(is.double(ri$imputed$k))

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
expect_true(sum(rg$flags[bad, 4]) >= 6)                 # base-rate score: 7 of 10 on this seed
expect_true(mean(rg$flags[!lab, 4]) <= 0.02)            # 3 of 390 on this seed
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
# xgboost engine on the categorical column (sharper probabilities: more hits, more false flags)
set.seed(6); rx <- suppressWarnings(cellImpForest(dcat, engine = "xgboost", nrounds = 80, maxit = 3))
expect_true(auc6(-rx$P[, 4], lab) > 0.9)
expect_true(sum(rx$flags[bad, 4]) >= 9)                 # 10 of 10 on this seed
expect_true(mean(rx$flags[!lab, 4]) <= 0.05)            # 14 of 390 on this seed

# fix round 1: a declared-but-unused level, and an ordered factor, survive unchanged
du <- dcat; levels(du$g) <- c(levels(dcat$g), "unused")
set.seed(6); ru <- cellImpForest(du, num.trees = 300)
expect_identical(levels(ru$imputed$g), levels(du$g))
dord <- dcat; dord$g <- factor(dcat$g, levels = c("low", "mid", "high"), ordered = TRUE)
set.seed(6); rord <- cellImpForest(dord, num.trees = 300)
expect_identical(levels(rord$imputed$g), levels(dord$g))
expect_true(is.ordered(rord$imputed$g))

# final review C1: with no signal a rare level is not flagged for being rare (base-rate score).
# The old score (relative to the most probable level) flagged 40 of 40 and 15 of 25 cells here
# and starved column b; the base-rate score flags 7 and 4 -- out-of-bag class probabilities of
# a rare level are noisy under no signal, so the bound is not tighter
set.seed(11); n <- 500
dns <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n), x3 = stats::rnorm(n),
                  b = factor(sample(rep(c("common", "rare"), c(460, 40)))),       # 92/8
                  f3 = factor(sample(rep(c("a", "b", "c"), c(250, 225, 25)))))    # a 5 % level
set.seed(12); rns <- cellImpForest(dns, num.trees = 200)
expect_true(mean(rns$flags[dns$b == "rare", "b"]) <= 0.25)
expect_true(mean(rns$flags[dns$f3 == "c", "f3"]) <= 0.25)
expect_equal(sum(rns$flags[dns$b == "common", "b"]), 0L)

# --- uncertainty, m, methods, dispatcher ---
d <- gen(); set.seed(9); d[cbind(sample(300, 30), sample(5, 30, TRUE))] <- NA
miss <- is.na(d)
set.seed(7); rp <- suppressWarnings(cellImpForest(d, uncert = "pmm", num.trees = 100, maxit = 2))
for (j in 1:5) expect_true(all(rp$imputed[miss[, j], j] %in% d[!miss[, j], j]))  # PMM: observed values
set.seed(7); rq <- suppressWarnings(cellImpForest(d, uncert = "quantile", num.trees = 100, maxit = 2))
expect_false(anyNA(rq$imputed))
set.seed(7); rm2 <- suppressWarnings(cellImpForest(d, uncert = "pmm", m = 2L, num.trees = 60, maxit = 2))
expect_equal(length(rm2$imputed), 2L)
expect_false(identical(rm2$imputed[[1]], rm2$imputed[[2]]))
expect_error(cellImpForest(d, m = 2L), "uncert")
# final review I7: argument checks, and the quantile -> pmm coercion is announced
expect_error(cellImpForest(d, m = 0), "'m'")
expect_error(cellImpForest(d, uncert = "pmm", m = 1.5), "'m'")
expect_error(cellImpForest(d, maxit = 0), "'maxit'")
expect_error(cellImpForest(d, maxit_detect = -1), "'maxit_detect'")
expect_error(cellImpForest(d, rho_min = 0), "'rho_min'")
expect_error(cellImpForest(d, rho_min = 1.5), "'rho_min'")
expect_error(cellImpForest(d, psi_c = 0), "'psi_c'")
expect_message(suppressWarnings(cellImpForest(d, engine = "xgboost", uncert = "quantile",
                                              nrounds = 20, maxit = 2)), "pmm")

set.seed(7); r <- suppressWarnings(cellImpForest(d, num.trees = 60, maxit = 2))
expect_stdout(print(r), "cellImpForest")
s <- summary(r)
expect_true(is.data.frame(s))
expect_equal(nrow(s), 5L)
expect_equal(names(s), c("column", "missing", "flagged", "released", "scale"))
expect_equal(s$released, unname(colSums(r$released)))
pdf(NULL); expect_silent(plot(r)); dev.off()
set.seed(7); rd <- suppressWarnings(imputeCellwise(d, method = "cellImpForest", num.trees = 60, maxit = 2))
expect_true(inherits(rd, "cellImpForest"))
