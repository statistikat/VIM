# Run from package root with the current VIM loaded (pkgload::load_all()).
# Reuse the exact population, missingness and contamination seeds of the vignette.
benchmark_code <- tempfile(fileext = ".R")
knitr::purl("vignettes/vimpute-benchmark.Rmd.orig", output = benchmark_code, quiet = TRUE)
exprs <- parse(benchmark_code)
unlink(benchmark_code)
e <- new.env()
skip <- c("res", "summary_tab", "mar", "mixed_res", "mixed_summary",
          "contamination_res", "clean", "idx", "contamination_summary", "degradation")
for (ex in exprs) {
  if (is.call(ex) && identical(ex[[1]], as.name("set.seed"))) eval(ex, e)
  if (is.call(ex) && identical(ex[[1]], as.name("<-"))) {
    lhs <- as.character(ex[[2]])[1]
    if (!lhs %in% skip) eval(ex, e)
  }
}
rows <- list()
for (mechanism in c("MCAR", "MAR")) for (r in 1:3) {
  run_seed <- 7301L + r + if (mechanism == "MAR") 10000L else 0L
  set.seed(run_seed)
  truth <- e$mixed_full[sample(nrow(e$mixed_full), e$N), ]
  amp <- VIM::makeMissing(truth, prop = .2, mechanism = mechanism,
    vars = e$mixed_targets, seed = 3000L + r,
    weights = if (mechanism == "MAR") c(x = 1, z = -.5) else NULL)
  w <- attr(amp, "where")
  set.seed(run_seed + 200000L)
  plan <- lapply(c("x", "z", "continuous"), function(v) {
    obs <- which(!w[, v]); list(rows = sample(obs, length(obs)),
      signs = sample(c(-1, 1), length(obs), TRUE), scale = sd(truth[[v]]))
  }); names(plan) <- c("x", "z", "continuous")
  for (source in c("clean", "predictors", "response", "both")) {
    d <- amp
    cols <- switch(source, clean = character(), predictors = c("x", "z"),
                   response = "continuous", both = names(plan))
    for (v in cols) {
      p <- plan[[v]]; k <- floor(.1 * length(p$rows)); ids <- p$rows[seq_len(k)]
      d[[v]][ids] <- d[[v]][ids] + 8 * p$scale * p$signs[seq_len(k)]
    }
    bad_donors <- if ("continuous" %in% cols)
      d$continuous[plan$continuous$rows[seq_len(floor(.1 * length(plan$continuous$rows)))]] else numeric()
    for (uncert in c("none", "pmm")) for (safeguard in c("off", "predictors", "donors", "both")) {
      clean_x <- safeguard %in% c("predictors", "both")
      donor_cut <- if (safeguard %in% c("donors", "both")) 4.5 else Inf
      out <- VIM::vimpute(d, method = "cellwise",
        learner_params = list(continuous = list(engine = "cellwise-mm",
          clean_predictors = clean_x, donor_cutoff = donor_cut),
          binary = list(clean_predictors = clean_x),
          category = list(clean_predictors = clean_x)),
        sequential = FALSE, uncert = uncert, imp_var = FALSE, seed = run_seed + 100000L)
      yimp <- out$continuous[w[, "continuous"]]
      hit <- if (length(bad_donors)) mean(vapply(yimp, function(y)
        any(abs(y - bad_donors) < 1e-8), logical(1))) else 0
      rows[[length(rows) + 1L]] <- data.frame(mechanism, rep = r, source, uncert, safeguard,
        nrmse = VIM::nrmse(truth$continuous, out$continuous, m = w[, "continuous"]),
        contaminated_donor_fraction = hit)
    }
  }
}
result <- do.call(rbind, rows)
print(aggregate(cbind(nrmse, contaminated_donor_fraction) ~ source + uncert + safeguard,
                result, mean), row.names = FALSE)
