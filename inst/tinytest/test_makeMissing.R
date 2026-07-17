library(VIM)

## Wave 3 (audit P1.22): makeMissing() -- amputation generator for simulation
## studies (mice::ampute-equivalent, variable-wise). Generates MCAR/MAR/MNAR
## missingness in complete data and returns the amputed data with an
## attr(., "where") indicator matrix that plugs directly into
## evaluation()/nrmse()/pfc():
##   amp <- makeMissing(dat, prop = 0.2, mechanism = "MAR")
##   imp <- vimpute(amp)
##   evaluation(dat, imp, m = attr(amp, "where"))

set.seed(99)
n <- 1000
dat <- data.frame(
  x = rnorm(n),
  y = rnorm(n, mean = 5),
  g = factor(sample(c("a", "b", "c"), n, replace = TRUE))
)

## --- MCAR: exact counts, indicator, type stability -------------------------------
amp <- makeMissing(dat, prop = 0.2, mechanism = "MCAR", seed = 1)
expect_true(is.data.frame(amp) && !data.table::is.data.table(amp),
            info = "data.frame in, data.frame out")
expect_equal(sum(is.na(amp$x)), round(0.2 * n))
expect_equal(sum(is.na(amp$y)), round(0.2 * n))
expect_equal(sum(is.na(amp$g)), round(0.2 * n))

w <- attr(amp, "where")
expect_true(is.matrix(w) && is.logical(w))
expect_equal(dim(w), dim(as.matrix(dat)))
expect_equal(unname(colSums(w)), unname(colSums(is.na(amp))))
expect_true(all(is.na(amp[w])), info = "where must mark exactly the amputed cells")
expect_equal(attr(amp, "mechanism"), "MCAR")

## data.table in -> data.table out, and the input is not modified by reference
dt <- data.table::as.data.table(dat)
amp_dt <- makeMissing(dt, prop = 0.1, mechanism = "MCAR", seed = 2)
expect_true(data.table::is.data.table(amp_dt))
expect_equal(sum(is.na(dt)), 0L, info = "input data.table must stay complete")

## seed reproducibility
amp_a <- makeMissing(dat, prop = 0.2, mechanism = "MCAR", seed = 7)
amp_b <- makeMissing(dat, prop = 0.2, mechanism = "MCAR", seed = 7)
expect_identical(amp_a, amp_b)
amp_c <- makeMissing(dat, prop = 0.2, mechanism = "MCAR", seed = 8)
expect_false(identical(amp_a, amp_c))

## --- vars restricts the amputation ------------------------------------------------
amp_v <- makeMissing(dat, prop = 0.3, mechanism = "MCAR", vars = "y", seed = 3)
expect_equal(sum(is.na(amp_v$y)), round(0.3 * n))
expect_equal(sum(is.na(amp_v$x)), 0L)
expect_equal(sum(is.na(amp_v$g)), 0L)

## --- MAR: missingness in y is driven by the other (observed) variables -----------
amp_mar <- makeMissing(dat, prop = 0.3, mechanism = "MAR", vars = "y", seed = 4)
miss <- is.na(amp_mar$y)
shift <- mean(dat$x[miss]) - mean(dat$x[!miss])
expect_true(shift > 0.2,
            info = "MAR: rows with high driver values must be more often missing")

## weights control the drivers and the direction
amp_neg <- makeMissing(dat, prop = 0.3, mechanism = "MAR", vars = "y",
                       weights = c(x = -1), seed = 4)
miss_neg <- is.na(amp_neg$y)
expect_true(mean(dat$x[miss_neg]) - mean(dat$x[!miss_neg]) < -0.2,
            info = "negative weight must flip the MAR direction")

## --- MNAR: missingness in y is driven by y's own (unobserved) values -------------
amp_mnar <- makeMissing(dat, prop = 0.3, mechanism = "MNAR", vars = "y", seed = 5)
miss_mnar <- is.na(amp_mnar$y)
expect_true(mean(dat$y[miss_mnar]) - mean(dat$y[!miss_mnar]) > 0.2,
            info = "MNAR: high true values must be more often missing")

## --- validation --------------------------------------------------------------------
dat_na <- dat; dat_na$x[1] <- NA
expect_error(makeMissing(dat_na, prop = 0.2), pattern = "complete")
expect_error(makeMissing(dat, prop = 0), pattern = "prop")
expect_error(makeMissing(dat, prop = 1.5), pattern = "prop")
expect_error(makeMissing(dat, vars = "zzz"), pattern = "zzz")
expect_error(makeMissing(dat, mechanism = "MNAR", vars = "g"),
             pattern = "numeric")
expect_error(makeMissing(dat[, "g", drop = FALSE], mechanism = "MAR"),
             pattern = "driver")

## --- round-trip: plugs into evaluation() -------------------------------------------
small <- dat[1:120, ]
amp_s <- makeMissing(small, prop = 0.15, mechanism = "MAR", vars = c("x", "y"),
                     seed = 6)
imp_s <- kNN(amp_s, imp_var = FALSE)
ev <- evaluation(small, imp_s, m = attr(amp_s, "where"))
expect_true(is.finite(ev$error) && ev$error >= 0)
