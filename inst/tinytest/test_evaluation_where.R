# evaluation(): 'where' is the documented alias for the mask argument 'm'
# (matching makeMissing()'s attr(., "where") name); 'm' stays for backward
# compatibility; supplying both is an error.

suppressMessages(library(VIM))

set.seed(3)
truth <- data.frame(a = rnorm(40), b = rnorm(40),
                    g = factor(sample(c("x", "y"), 40, replace = TRUE)))
imp <- truth
mask <- matrix(FALSE, nrow(truth), ncol(truth),
               dimnames = list(NULL, names(truth)))
mask[sample(40, 8), "a"] <- TRUE
mask[sample(40, 6), "g"] <- TRUE
imp$a[mask[, "a"]] <- truth$a[mask[, "a"]] + 0.5
imp$g[mask[, "g"]] <- factor("x", levels = levels(truth$g))

# --- where= gives the same result as m= ---------------------------------
expect_equal(evaluation(truth, imp, where = mask),
             evaluation(truth, imp, m = mask),
             info = "where= is an exact alias for m=")

# --- positional third argument keeps working ------------------------------
expect_equal(evaluation(truth, imp, mask),
             evaluation(truth, imp, where = mask),
             info = "positional mask (old style) unchanged")

# --- both supplied is an error --------------------------------------------
expect_error(evaluation(truth, imp, m = mask, where = mask),
             pattern = "either",
             info = "m and where together error")
