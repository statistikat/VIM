# Adapted from transformerImpute-rebased:inst/benchmarks/uci-transformer.R.
# Source then call uci_run(); nothing downloads or trains on source().
uci_config <- function(full = FALSE) {
  list(protocol = "makeMissing-evaluation-tabicl2-v1",
       n = c(250L, 500L, 1000L, 2000L), rates = c(.1, .2, .4),
       repetitions = if (full) 10L else 3L, mechanisms = c("MCAR", "MAR"),
       seed = 20261010L,
       methods = c("kNN", "ranger", "ranger_seq10", "xgboost", "xgboost_seq10", "vimpute_tabicl2",
                   "vimpute_tabicl2_finetune", "vimpute_tabicl2_seq10",
                   "vimpute_tabicl2_finetune_seq10"),
       tabicl2 = list(finetune = FALSE, n_estimators = 8L, device = "cpu", n_jobs = 1L),
       tabicl2_finetune = list(epochs = 5L, patience = 3L, learning_rate = 1e-5,
         finetune_params = list(n_estimators_finetune = 2L,
                               n_estimators_validation = 2L, amp = FALSE)),
       trees = 100L, boosting_rounds = 30L)
}

uci_prepare <- function(cache, download = FALSE) {
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  urls <- c(adult = "https://archive.ics.uci.edu/static/public/2/adult.zip",
            bank = "https://archive.ics.uci.edu/static/public/222/bank+marketing.zip")
  archives <- file.path(cache, c("adult.zip", "bank-marketing.zip"))
  for (i in seq_along(urls)) {
    if (!file.exists(archives[i])) {
      if (!download) stop("Missing archive: ", archives[i], "; opt in with download=TRUE")
      utils::download.file(urls[i], archives[i], mode = "wb", quiet = TRUE)
    }
  }
  ad <- file.path(cache, "adult"); ba <- file.path(cache, "bank")
  dir.create(ad, showWarnings = FALSE); dir.create(ba, showWarnings = FALSE)
  utils::unzip(archives[1], exdir = ad)
  utils::unzip(archives[2], exdir = ba)
  utils::unzip(file.path(ba, "bank.zip"), exdir = ba)
  nm <- c("age", "workclass", "fnlwgt", "education", "education_num",
          "marital_status", "occupation", "relationship", "race", "sex",
          "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")
  read_adult <- function(path, skip = 0L) utils::read.table(path, sep = ",", header = FALSE,
    col.names = nm, strip.white = TRUE, na.strings = "?", skip = skip,
    stringsAsFactors = FALSE, comment.char = "", quote = "")
  adult <- rbind(read_adult(file.path(ad, "adult.data")),
                 read_adult(file.path(ad, "adult.test"), 1L))
  adult$income <- sub("\\.$", "", adult$income)
  bank <- utils::read.csv(file.path(ba, "bank-full.csv"), sep = ";",
                          stringsAsFactors = FALSE)
  stopifnot(nrow(adult) == 48842L, nrow(bank) == 45211L)
  raw <- list(Adult = adult, Bank = bank)
  audit <- do.call(rbind, lapply(names(raw), function(nm) {
    x <- raw[[nm]]
    data.frame(dataset = nm, variable = names(x), rows = nrow(x),
      existing_na = vapply(x, function(z) sum(is.na(z)), integer(1)),
      unknown = vapply(x, function(z) sum(z == "unknown", na.rm = TRUE), integer(1)),
      complete_rows = sum(stats::complete.cases(x)))
  }))
  data <- lapply(raw, function(x) {
    x <- x[stats::complete.cases(x), , drop = FALSE]
    # Keep numeric values unchanged, including structural sentinel pdays=-1.
    x[] <- lapply(x, function(z) if (is.character(z)) factor(z) else as.numeric(z))
    x
  })
  list(data = data, audit = audit,
       sources = data.frame(url = unname(urls), md5 = unname(tools::md5sum(archives))))
}

uci_methods <- function(config) {
  allowed <- c("kNN", "ranger", "ranger_seq10", "xgboost", "xgboost_seq10", "vimpute_tabicl2",
                   "vimpute_tabicl2_finetune", "vimpute_tabicl2_seq10",
                   "vimpute_tabicl2_finetune_seq10")
  if (any(!config$methods %in% allowed)) stop("Unknown benchmark method")
  setNames(lapply(config$methods, function(method) {
    function(d, targets, seed) {
      set.seed(seed)
      if (startsWith(method, "vimpute_tabicl2")) {
        fine <- grepl("finetune", method, fixed = TRUE)
        seq10 <- endsWith(method, "seq10")
        pars <- utils::modifyList(config$tabicl2,
                                 if (fine) config$tabicl2_finetune else list())
        pars$finetune <- fine
        return(uci_prediction_table(as.data.frame(VIM::vimpute(d, method = "tabicl2",
          learner_params = list(tabicl2 = pars), sequential = seq10,
          nseq = if (seq10) 10L else 1L, pmm = FALSE, uncert = "none",
          seed = seed, imp_var = FALSE)), d, targets))
      }
      if (method %in% c("ranger_seq10", "xgboost_seq10")) {
        learner <- sub("_seq10$", "", method)
        pars <- if (learner == "ranger") list(num.trees = config$trees, num.threads = 1L)
                else list(nrounds = config$boosting_rounds, nthread = 1L)
        return(uci_prediction_table(as.data.frame(VIM::vimpute(d, method = learner,
          learner_params = setNames(list(pars), learner), sequential = TRUE,
          nseq = 10L, pmm = FALSE, uncert = "none", seed = seed, imp_var = FALSE)), d, targets))
      }
      out <- d
      for (v in targets) {
        predictors <- setdiff(names(d), v)
        form <- stats::reformulate(predictors, response = v)
        ans <- switch(method,
          kNN = VIM::kNN(d, variable = v, dist_var = predictors, k = 5L,
                        imp_var = FALSE, useImputedDist = FALSE),
          ranger = VIM::rangerImpute(form, d, imp_var = FALSE,
                                    num.trees = config$trees, num.threads = 1L),
          xgboost = VIM::xgboostImpute(form, d, imp_var = FALSE,
                                      nrounds = config$boosting_rounds, nthread = 1L))
        out[[v]] <- ans[[v]]
      }
      out
    }
  }), config$methods)
}

# Hurdle-model probabilities are diagnostics, not completed survey variables.
# They are populated only at imputed rows and may therefore legitimately be NA.
# Reject all other schema changes rather than silently hiding missing columns.
uci_prediction_table <- function(completed, input, targets) {
  extra <- setdiff(names(completed), names(input))
  if (nrow(completed) != nrow(input) ||
      length(setdiff(names(input), names(completed))) ||
      any(!extra %in% paste0(targets, "_zero_prob")))
    stop("Completion changed the table schema")
  completed[, names(input), drop = FALSE]
}

uci_score <- function(truth, completed, mask, targets) {
  if (!identical(dim(truth), dim(completed)) || !identical(names(truth), names(completed)))
    stop("Completion changed the table schema")
  for (v in names(truth)) {
    keep <- !mask[, v]
    if (!identical(as.character(truth[[v]][keep]), as.character(completed[[v]][keep])))
      stop("Changed observed values: ", v)
  }
  do.call(rbind, lapply(targets, function(v) {
    take <- mask[, v]; a <- truth[[v]]; b <- completed[[v]]
    if (anyNA(b) || (is.numeric(b) && any(!is.finite(b)))) stop("Invalid completion: ", v)
    numeric <- is.numeric(a)
    if (numeric && !is.numeric(b)) stop("Changed numeric type: ", v)
    if (!numeric && any(!as.character(b) %in% levels(a))) stop("Invalid category: ", v)
    delta <- if (numeric) b[take] - a[take] else NULL
    sd_ref <- if (numeric) stats::sd(a) else NA_real_
    # evaluation() sums errors over its input table. Restrict that table to
    # masked cells explicitly, rather than relying on observed values matching.
    pointwise <- if (any(take)) VIM::evaluation(
      data.frame(value = if (numeric) a[take] else as.character(a[take])),
      data.frame(value = if (numeric) b[take] else as.character(b[take])),
      where = matrix(TRUE, sum(take), 1L),
      vartypes = if (numeric) "numeric" else "factor") else NULL
    rmse <- if (numeric && any(take)) sqrt(pointwise$err_num) else NA_real_
    sd_masked <- if (numeric && sum(take) > 1L) stats::sd(a[take]) else NA_real_
    # Numeric ECDF sup distance and categorical total variation, whole table.
    dist <- if (numeric) {
      points <- sort(unique(c(a, b)))
      max(abs(stats::ecdf(a)(points) - stats::ecdf(b)(points)))
    } else {
      pa <- prop.table(table(factor(a, levels = levels(a))))
      pb <- prop.table(table(factor(b, levels = levels(a))))
      sum(abs(pa - pb)) / 2
    }
    # For binary factors the second declared level is coded 1.
    num <- function(x) if (is.numeric(x)) x else as.numeric(x == levels(a)[2L])
    relationship <- function(x) {
      x <- num(x)
      if (stats::sd(x) == 0) NA_real_ else stats::cor(x, truth$age)
    }
    data.frame(variable = v, type = if (numeric) "numeric" else "categorical",
      n_masked = sum(take), realized_rate = mean(take),
      rmse = rmse,
      mae = if (numeric && any(take)) mean(abs(delta)) else NA_real_,
      nrmse = if (numeric && is.finite(sd_masked) && sd_masked > 0)
        VIM::nrmse(x = a, y = b, m = take) else NA_real_,
      nrmse_reference = if (numeric && sd_ref > 0) rmse/sd_ref else NA_real_,
      accuracy = if (!numeric && any(take)) 1 - pointwise$err_cat else NA_real_,
      misclassification = if (!numeric && any(take)) pointwise$err_cat else NA_real_,
      mean_difference = if (numeric) mean(b) - mean(a) else NA_real_,
      sd_difference = if (numeric) stats::sd(b) - sd_ref else NA_real_,
      marginal_distance = dist, correlation_reference = relationship(a),
      correlation_completed = relationship(b), correlation_difference = relationship(b) - relationship(a))
  }))
}

# Sample sizes are separate experiments with identical budgets and seed schedules.
# Each size has its own artifacts/checkpoint; top-level CSVs always include n.
uci_validate_sizes <- function(n) {
  if (!is.numeric(n) || !length(n) || anyNA(n) ||
      any(!is.finite(n) | n < 50 | n != floor(n)) || anyDuplicated(n))
    stop("n must contain distinct integer sample sizes of at least 50")
  invisible(n)
}

uci_collect_sizes <- function(output) {
  config <- readRDS(file.path(output, "config.rds"))
  for (n in config$n) {
    single <- config; single$n <- n
    marker <- file.path(output, paste0("n", n), "complete.rds")
    if (!file.exists(marker) || !identical(single, readRDS(marker)))
      stop("Incomplete size or incompatible configuration: ", n)
  }
  for (file in c("runs.csv", "metrics.csv", "distributions.csv")) {
    tables <- lapply(config$n, function(n) {
      path <- file.path(output, paste0("n", n), file)
      if (!file.exists(path)) stop("Missing size results: ", path)
      d <- utils::read.csv(path, stringsAsFactors = FALSE)
      d$n <- n
      d[, c("n", setdiff(names(d), "n")), drop = FALSE]
    })
    utils::write.csv(do.call(rbind, tables), file.path(output, file), row.names = FALSE)
  }
  first <- file.path(output, paste0("n", config$n[1L]))
  for (file in c("preprocessing.csv", "sources.csv"))
    if (!file.copy(file.path(first, file), output, overwrite = TRUE))
      stop("Could not copy ", file)
  provenance <- unlist(lapply(config$n, function(n) {
    directory <- file.path(output, paste0("n", n))
    files <- file.path(directory, c("session.txt", "session-extension.txt"))
    c(paste("Number of rows:", n),
      unlist(lapply(files[file.exists(files)], readLines)))
  }))
  writeLines(provenance, file.path(output, "session.txt"))
  invisible(output)
}

uci_run <- function(config = uci_config(), cache = "uci-cache", output = "tabicl2-results",
                    download = FALSE, resume = FALSE) {
  uci_validate_sizes(config$n)
  path <- file.path(output, "config.rds")
  if (file.exists(path)) {
    if (!resume) stop("Output already contains a run; use resume=TRUE")
    if (!identical(config, readRDS(path))) stop("Cannot resume with a different configuration")
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  saveRDS(config, path)
  for (n in config$n) {
    single <- config; single$n <- n
    directory <- file.path(output, paste0("n", n))
    marker <- file.path(directory, "complete.rds")
    if (resume && file.exists(marker)) {
      if (!identical(single, readRDS(marker))) stop("Completed size configuration differs")
      next
    }
    message("Number of rows: ", n)
    uci_run_single(single, cache, directory, download, resume)
    uci_distribution_summary(directory)
    saveRDS(single, marker)
  }
  uci_collect_sizes(output)
}

uci_run_single <- function(config, cache = "uci-cache", output = "tabicl2-results",
                    download = FALSE, resume = FALSE) {
  stopifnot(length(config$n) == 1L, config$n >= 50L, config$repetitions >= 1L)
  if (file.exists(file.path(output, "config.rds"))) {
    if (!resume) stop("Output already contains a run; use resume=TRUE")
    if (!identical(config, readRDS(file.path(output, "config.rds"))))
      stop("Cannot resume with a different configuration")
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  prepared <- uci_prepare(cache, download)
  methods <- uci_methods(config)
  results <- runs <- artifacts <- list()
  checkpoint <- file.path(output, "checkpoint.rds")
  if (resume && file.exists(checkpoint)) {
    saved <- readRDS(checkpoint)
    results <- saved$results; runs <- saved$runs; artifacts <- saved$artifacts
  }
  targets_by_dataset <- list(Adult = c("hours_per_week", "income"), Bank = c("balance", "y"))
  saveRDS(config, file.path(output, "config.rds"))
  utils::write.csv(prepared$audit, file.path(output, "preprocessing.csv"), row.names = FALSE)
  utils::write.csv(prepared$sources, file.path(output, "sources.csv"), row.names = FALSE)
  for (di in seq_along(prepared$data)) for (rep in seq_len(config$repetitions)) {
    dataset <- names(prepared$data)[di]; targets <- targets_by_dataset[[dataset]]
    sample_seed <- config$seed + di * 10000L + rep * 100L
    set.seed(sample_seed)
    source <- prepared$data[[dataset]]
    if (config$n > nrow(source)) stop("Requested sample exceeds complete reference rows")
    row_ids <- sample.int(nrow(source), config$n)
    truth <- droplevels(source[row_ids, , drop = FALSE])
    for (mi in seq_along(config$mechanisms)) for (ri in seq_along(config$rates)) {
      mechanism <- config$mechanisms[mi]; rate <- config$rates[ri]
      key <- paste(dataset, rep, mechanism, rate, sep = "-")
      if (!is.null(artifacts[[key]])) next
      seed <- sample_seed + mi * 10L + ri
      # Independent stream: otherwise makeMissing's MCAR rows can coincide
      # exactly with a model's random validation split at the same seed.
      model_seed <- seed + 1000000L
      incomplete <- VIM::makeMissing(truth, prop = rate, mechanism = mechanism,
        vars = targets, weights = if (mechanism == "MAR") c(age = 1) else NULL, seed = seed)
      mask <- attr(incomplete, "where")
      # Do not supply category levels learned only from the withheld cells.
      incomplete <- droplevels(incomplete)
      artifacts[[key]] <- list(truth = truth, row_ids = row_ids, mask = mask,
                               completed = list())
      # Randomize method order to reduce systematic warm-cache/order effects.
      set.seed(seed)
      for (method in sample(names(methods))) {
        message(key, " / ", method)
        warnings <- character(); started <- proc.time()[["elapsed"]]
        value <- tryCatch(withCallingHandlers({
          completed <- methods[[method]](incomplete, targets, model_seed)
          scores <- uci_score(truth, completed, mask, targets)
          list(completed = completed, scores = scores)
        }, warning = function(w) {
          warnings <<- unique(c(warnings, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }), error = identity)
        meta <- data.frame(dataset, repetition = rep, mechanism, rate, method,
          sample_seed, seed, model_seed, elapsed = proc.time()[["elapsed"]] - started,
          status = if (inherits(value, "error")) "failed" else "ok",
          error = if (inherits(value, "error")) conditionMessage(value) else "",
          warnings = paste(warnings, collapse = " | "))
        runs[[length(runs) + 1L]] <- meta
        if (!inherits(value, "error")) {
          results[[length(results) + 1L]] <- cbind(meta, value$scores)
          artifacts[[key]]$completed[[method]] <- value$completed
        }
        # Incremental outputs survive an interrupted run; failures never disappear.
        utils::write.csv(do.call(rbind, runs), file.path(output, "runs.csv"), row.names = FALSE)
        if (length(results)) utils::write.csv(do.call(rbind, results),
                                              file.path(output, "metrics.csv"), row.names = FALSE)
      }
      # Commit only complete scenarios; resume reruns an interrupted scenario.
      saveRDS(list(results = results, runs = runs, artifacts = artifacts),
              paste0(checkpoint, ".tmp"))
      if (!file.rename(paste0(checkpoint, ".tmp"), checkpoint)) stop("Checkpoint write failed")
    }
  }
  saveRDS(artifacts, file.path(output, "artifacts.rds"))
  packages <- c("VIM", "reticulate", "ranger", "xgboost", "mlr3",
                "mlr3learners", "mlr3pipelines")
  versions <- vapply(packages, function(p) if (requireNamespace(p, quietly = TRUE))
    as.character(utils::packageVersion(p)) else "unavailable", character(1))
  python <- if (any(startsWith(config$methods, "vimpute_tabicl2")))
    reticulate::import("importlib.metadata")$version("tabicl") else NULL
  provenance <- capture.output(list(config = config, packages = versions, python = python,
    system = Sys.info()[c("sysname", "release", "machine")], threads = Sys.getenv(c("OMP_NUM_THREADS", "RETICULATE_PYTHON")), session = utils::sessionInfo()))
  writeLines(trimws(paste(sub("[ \t]+$", "", provenance), collapse = "\n"),
                    which = "right"), file.path(output, "session.txt"))
  invisible(list(runs = do.call(rbind, runs), metrics = do.call(rbind, results)))
}

# Optional compact marginal summaries for a predeclared illustrative scenario.
# No individual rows are copied into the package documentation.
uci_distribution_summary <- function(output, repetition = 1L, mechanism = "MAR", rate = .2) {
  config <- readRDS(file.path(output, "config.rds"))
  if (dir.exists(file.path(output, paste0("n", config$n[1L])))) {
    for (n in config$n) uci_distribution_summary(file.path(output, paste0("n", n)),
                                                repetition, mechanism, rate)
    return(invisible(uci_collect_sizes(output)))
  }
  artifacts <- readRDS(file.path(output, "artifacts.rds"))
  rows <- list()
  for (dataset in c("Adult", "Bank")) {
    key <- paste(dataset, repetition, mechanism, rate, sep = "-")
    a <- artifacts[[key]]
    if (is.null(a)) next
    targets <- names(which(colSums(a$mask) > 0))
    tables <- c(list(Reference = a$truth), a$completed)
    for (method in names(tables)) for (v in targets) {
      x <- tables[[method]][[v]]
      numeric <- is.numeric(x)
      summary <- if (numeric) {
        p <- seq(0, 1, .1)
        data.frame(position = p, category = NA_character_,
                   value = as.numeric(stats::quantile(x, p)))
      } else {
        p <- prop.table(table(factor(x, levels = levels(a$truth[[v]]))))
        data.frame(position = NA_real_, category = names(p), value = as.numeric(p))
      }
      rows[[length(rows) + 1L]] <- cbind(data.frame(dataset, repetition, mechanism, rate,
        method, variable = v, type = if (numeric) "numeric" else "categorical"), summary)
    }
  }
  result <- do.call(rbind, rows)
  if (!is.null(result)) utils::write.csv(result, file.path(output, "distributions.csv"), row.names = FALSE)
  invisible(result)
}

