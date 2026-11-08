# Refresh selected methods from saved clean truth/masks without refitting TabICL.
# Source tabicl2.R first. Output must differ from source; original cache is read-only.
uci_refresh_methods <- function(source, output, config = uci_config(),
    refresh = c("kNN", "kNN_rf", "cellwise", "cellwise_seq10")) {
  if (normalizePath(source) == normalizePath(output, mustWork = FALSE))
    stop("Use a separate output directory to preserve the original run")
  old <- readRDS(file.path(source, "config.rds"))
  if (!identical(old[setdiff(names(old), "methods")],
                 config[setdiff(names(config), "methods")]))
    stop("The saved sampling/model configuration differs")
  if (length(setdiff(setdiff(config$methods, refresh), old$methods)))
    stop("Every new method must be refreshed")
  methods <- uci_methods(config)[refresh]
  stopifnot(all(refresh %in% config$methods))
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  cfg_path <- file.path(output, "config.rds")
  if (file.exists(cfg_path) && !identical(readRDS(cfg_path), config))
    stop("Refresh output has a different configuration")
  saveRDS(config, cfg_path)
  old_options <- options(VIM.ncores = 1L, ranger.num.threads = 1L)
  on.exit(options(old_options))
  for (n in config$n) {
    src <- file.path(source, paste0("n", n))
    dst <- file.path(output, paste0("n", n))
    dir.create(dst, recursive = TRUE, showWarnings = FALSE)
    single <- config; single$n <- n
    expected_old <- old; expected_old$n <- n
    stopifnot(identical(readRDS(file.path(src, "complete.rds")), expected_old))
    saveRDS(single, file.path(dst, "config.rds"))
    cp <- file.path(dst, "refresh-checkpoint.rds")
    if (file.exists(cp)) {
      state <- readRDS(cp)
    } else {
      runs <- read.csv(file.path(src, "runs.csv"), stringsAsFactors = FALSE)
      metrics <- read.csv(file.path(src, "metrics.csv"), stringsAsFactors = FALSE)
      stopifnot(all(runs$status == "ok"))
      keep <- setdiff(config$methods, refresh)
      state <- list(runs = runs[runs$method %in% keep, ],
        metrics = metrics[metrics$method %in% keep, ],
        artifacts = readRDS(file.path(src, "artifacts.rds")), done = character())
    }
    original_runs <- read.csv(file.path(src, "runs.csv"), stringsAsFactors = FALSE)
    for (key in names(state$artifacts)) {
      a <- state$artifacts[[key]]
      # Match the cached scenario by its key; reuse its exact model seed.
      keys <- with(original_runs, paste(dataset, repetition, mechanism, rate, sep = "-"))
      meta <- original_runs[match(key, keys), , drop = FALSE]
      stopifnot(nrow(meta) == 1L, !is.na(meta$seed))
      targets <- names(which(colSums(a$mask) > 0))
      incomplete <- a$truth
      for (v in names(incomplete)) incomplete[[v]][a$mask[, v]] <- NA
      incomplete <- droplevels(incomplete)
      set.seed(meta$seed)
      for (method in sample(refresh)) {
        id <- paste(key, method, sep = "/")
        if (id %in% state$done) next
        message("n=", n, " ", id)
        warnings <- character(); started <- proc.time()[["elapsed"]]
        value <- tryCatch(withCallingHandlers({
          completed <- methods[[method]](incomplete, targets, meta$model_seed)
          list(completed = completed,
               scores = uci_score(a$truth, completed, a$mask, targets))
        }, warning = function(w) {
          warnings <<- unique(c(warnings, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }), error = identity)
        row <- meta
        row$method <- method
        row$elapsed <- proc.time()[["elapsed"]] - started
        row$status <- if (inherits(value, "error")) "failed" else "ok"
        row$error <- if (inherits(value, "error")) conditionMessage(value) else ""
        row$warnings <- paste(warnings, collapse = " | ")
        same <- function(d) with(d, dataset == row$dataset & repetition == row$repetition &
                                  mechanism == row$mechanism & rate == row$rate & method == row$method)
        state$runs <- rbind(state$runs[!same(state$runs), ], row)
        state$metrics <- state$metrics[!same(state$metrics), ]
        if (!inherits(value, "error")) {
          state$metrics <- rbind(state$metrics, cbind(row[rep(1L, nrow(value$scores)), , drop = FALSE], value$scores))
          state$artifacts[[key]]$completed[[method]] <- value$completed
          state$done <- c(state$done, id)
        }
        saveRDS(state, paste0(cp, ".tmp"))
        if (!file.rename(paste0(cp, ".tmp"), cp)) stop("Checkpoint write failed")
        if (inherits(value, "error")) stop("Failed ", id, ": ", conditionMessage(value))
      }
    }
    stopifnot(all(state$runs$status == "ok"),
      nrow(state$runs) == length(state$artifacts) * length(config$methods),
      length(state$done) == length(state$artifacts) * length(refresh))
    write.csv(state$runs, file.path(dst, "runs.csv"), row.names = FALSE)
    write.csv(state$metrics, file.path(dst, "metrics.csv"), row.names = FALSE)
    saveRDS(state$artifacts, file.path(dst, "artifacts.rds"))
    for (f in c("preprocessing.csv", "sources.csv", "session.txt"))
      stopifnot(file.copy(file.path(src, f), file.path(dst, f), overwrite = TRUE))
    writeLines(c(paste("Selective refresh:", paste(refresh, collapse = ", ")),
      paste("Refreshed:", Sys.time()), paste("Source:", normalizePath(source)),
      paste("Source config MD5:", tools::md5sum(file.path(source, "config.rds"))),
      "Other methods retained unchanged; see original session above.",
      capture.output(utils::sessionInfo())), file.path(dst, "session-extension.txt"))
    uci_distribution_summary(dst,
      mechanism = if ("MAR" %in% config$mechanisms) "MAR" else config$mechanisms[1L],
      rate = if (.2 %in% config$rates) .2 else config$rates[1L])
    saveRDS(single, file.path(dst, "complete.rds"))
  }
  uci_collect_sizes(output)
  invisible(output)
}
