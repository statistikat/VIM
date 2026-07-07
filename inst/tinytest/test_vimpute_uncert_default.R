library(VIM)

## Wave 2: safe default uncert = "pmm" (audit P2 "safe defaults"; Matthias's
## call 2026-07-07: deliberate, NEWS-documented change at 7.3.0).
## The old default (uncert = "none", pmm = FALSE) imputed numeric targets with
## deterministic conditional means -- imputed values cluster at the regression
## surface, understating variability and losing any density-overlay comparison
## against mice. The default is now a PMM draw among the 5 nearest donors
## (random draw), so default imputations are observed donor values.
## uncert = "none" restores the old behaviour explicitly.

data(sleep, package = "VIM")
d <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]
miss_sleep <- which(is.na(d$Sleep))
obs_sleep <- d$Sleep[!is.na(d$Sleep)]

## --- the default is pmm -------------------------------------------------------
expect_identical(formals(vimpute)$uncert, "pmm")

## --- PMM property: default numeric imputations are observed donor values ------
out <- vimpute(d, method = "ranger", sequential = FALSE, seed = 1,
               imp_var = FALSE, verbose = FALSE)
expect_true(all(out$Sleep[miss_sleep] %in% obs_sleep),
            info = "default imputations are not donor values (PMM not applied)")
## reproducible under seed
out2 <- vimpute(d, method = "ranger", sequential = FALSE, seed = 1,
                imp_var = FALSE, verbose = FALSE)
expect_identical(out, out2)

## --- uncert = "none" restores deterministic conditional-mean behaviour --------
out_none <- vimpute(d, method = "ranger", sequential = FALSE, seed = 1,
                    imp_var = FALSE, verbose = FALSE, uncert = "none")
expect_false(all(out_none$Sleep[miss_sleep] %in% obs_sleep))

## --- pmm = TRUE with the DEFAULT uncert must not warn about a conflict --------
w1 <- character()
invisible(withCallingHandlers(
  vimpute(d, method = "ranger", sequential = FALSE, pmm = TRUE, seed = 1,
          imp_var = FALSE, verbose = FALSE),
  warning = function(cond) {
    w1 <<- c(w1, conditionMessage(cond)); invokeRestart("muffleWarning")
  }
))
expect_false(any(grepl("takes precedence", w1)),
             info = "spurious pmm-vs-uncert warning with the default uncert")

## --- explicitly setting both still warns --------------------------------------
expect_warning(
  vimpute(d, method = "ranger", sequential = FALSE, pmm = TRUE,
          uncert = "normalerror", seed = 1, imp_var = FALSE, verbose = FALSE),
  "takes precedence"
)

## --- m > 1 defaults are now proper-ish: stochastic, no improper-MI warning ----
w2 <- character()
res <- withCallingHandlers(
  vimpute(d, method = "ranger", sequential = FALSE, m = 2, seed = 2,
          imp_var = FALSE, verbose = FALSE),
  warning = function(cond) {
    w2 <<- c(w2, conditionMessage(cond)); invokeRestart("muffleWarning")
  }
)
expect_false(any(grepl("m > 1 without", w2)),
             info = "default m > 1 still triggers the improper-MI warning")
expect_inherits(res, "vimmi")
expect_false(identical(res$imp[["Sleep"]][[1]], res$imp[["Sleep"]][[2]]),
             info = "default m > 1 imputations are identical (not stochastic)")

## --- wrappers keep their deterministic contract (uncert pinned to "none") -----
set.seed(1)
rf <- rangerImpute(Sleep ~ Dream + Span + BodyWgt, sleep, imp_var = FALSE)
expect_false(all(rf$Sleep[miss_sleep] %in% obs_sleep),
             info = "rangerImpute silently switched to PMM draws")
