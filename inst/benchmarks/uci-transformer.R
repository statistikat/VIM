# Source this file, then call uci_run(). Nothing downloads or trains on source().
# See vignettes/transformerImpute-benchmark.Rmd for the protocol.
uci_config <- function(full = FALSE, foundation = FALSE) {
  list(protocol = "makeMissing-evaluation-v1", n = c(240L, 500L, 1000L, 2000L, 5000L), rates = c(.1, .2, .4),
       repetitions = if (full) 10L else 3L, mechanisms = c("MCAR", "MAR"),
       seed = 20261010L,
       methods = c("transformerImpute", "tabular", "shared", "kNN", "ranger", "ranger_seq10", "xgboost", "xgboost_seq10",
                   if (foundation) c("tabimpute", "tabiclImpute", "tabpfnImpute")),
       transformer = list(epochs = if (full) 30L else 5L, batch_size = 64L,
                          patience = 3L, embedding_dim = 16L, num_heads = 2L),
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

# Select the separately installed PyTorch environment before Python initializes.
uci_foundation_setup <- function(local = ".local", device = "cpu") {
  local <- normalizePath(local, mustWork = TRUE)
  python <- file.path(local, "foundation-env", "bin", "python")
  manifest <- utils::read.csv(file.path(local, "foundation-models.csv"),
                              stringsAsFactors = FALSE)
  stopifnot(file.exists(python), all(file.exists(manifest$path)),
            device %in% c("cpu", "mps", "cuda"))
  if (reticulate::py_available(initialize = FALSE) &&
      normalizePath(dirname(reticulate::py_config()$python)) != normalizePath(dirname(python)))
    stop("Start a fresh R session before selecting the foundation environment")
  checkpoint <- function(name) {
    path <- manifest$path[manifest$name == name]
    if (length(path) != 1L) stop("Missing or ambiguous model manifest entry: ", name)
    path
  }
  Sys.setenv(RETICULATE_PYTHON = python, HF_HOME = file.path(local, "huggingface"),
    HF_HUB_OFFLINE = "1", OMP_NUM_THREADS = "1",
    TABPFN_ALLOW_CPU_LARGE_DATASET = "1", TABPFN_DISABLE_TELEMETRY = "1",
    VIM_TABIMPUTE_DEVICE = device,
    VIM_TABIMPUTE_CHECKPOINT = checkpoint("tabimpute"),
    VIM_TABPFN_CLASSIFIER = checkpoint("tabpfn_classifier"),
    VIM_TABPFN_REGRESSOR = checkpoint("tabpfn_regressor"))
  invisible(manifest)
}

uci_methods <- function(config) {
  exports <- getNamespaceExports("VIM")
  need <- c(transformerImpute = "transformerImpute", tabular = "fit_tabular_transformer",
            shared = "fit_shared_transformer", kNN = "kNN", ranger = "rangerImpute",
            ranger_seq10 = "vimpute", xgboost = "xgboostImpute", xgboost_seq10 = "vimpute",
            tabimpute = "tabimpute", tabiclImpute = "tabiclImpute", tabpfnImpute = "tabpfnImpute")
  if (any(!config$methods %in% names(need))) stop("Unknown benchmark method")
  setNames(lapply(config$methods, function(method) {
    function(d, targets, seed) {
      if (!need[[method]] %in% exports) stop("Unavailable in this VIM version: ", need[[method]])
      set.seed(seed)
      if (method %in% c("tabimpute", "tabiclImpute", "tabpfnImpute")) {
        torch <- reticulate::import("torch")
        torch$set_num_threads(1L)
        invisible(gc())
        reticulate::import("gc")$collect()
        if (isTRUE(torch$backends$mps$is_available())) torch$mps$empty_cache()
        if (method == "tabimpute") {
          checkpoint <- Sys.getenv("VIM_TABIMPUTE_CHECKPOINT")
          if (!nzchar(checkpoint) || !file.exists(checkpoint))
            stop("Set VIM_TABIMPUTE_CHECKPOINT to a local checkpoint")
          backend <- VIM::tabimpute_backend(backend = "categorical",
            device = Sys.getenv("VIM_TABIMPUTE_DEVICE", "cpu"),
            checkpoint_path = checkpoint, allow_download = FALSE)
          # Bound cell-token attention on wide one-hot tables; process every row.
          backend$model$imputer$max_num_rows <- 32L
          backend$model$imputer$max_num_chunks <- 1L
          runtime_path <- system.file("benchmarks", package = "VIM")
          if (file.exists("inst/benchmarks/uci_foundation_runtime.py"))
            runtime_path <- normalizePath("inst/benchmarks")
          runtime <- reticulate::import_from_path("uci_foundation_runtime", runtime_path)
          backend$model <- runtime$VectorizedTabImpute(backend$model)
          return(as.data.frame(VIM::tabimpute(d, variables = targets, seed = seed,
            tabimpute_module = backend, imp_var = FALSE)))
        }
        if (method == "tabiclImpute")
          return(as.data.frame(VIM::tabiclImpute(d, variables = targets, seed = seed,
            n_estimators = 1L, n_iterations = 1L, device = "cpu",
            allow_download = FALSE, imp_var = FALSE)))
        classifier <- Sys.getenv("VIM_TABPFN_CLASSIFIER")
        regressor <- Sys.getenv("VIM_TABPFN_REGRESSOR")
        if (!all(nzchar(c(classifier, regressor))) ||
            !all(file.exists(c(classifier, regressor))))
          stop("Set VIM_TABPFN_CLASSIFIER and VIM_TABPFN_REGRESSOR to local checkpoints")
        return(as.data.frame(VIM::tabpfnImpute(d, variables = targets, seed = seed,
          n_estimators = 1L, n_permutations = 1L, device = "cpu",
          classifier_path = classifier, regressor_path = regressor,
          allow_download = FALSE, imp_var = FALSE)))
      }
      neural <- method %in% c("transformerImpute", "tabular", "shared")
      if (neural) {
        keras::k_clear_session()
        # Seeds Python random, numpy and TF/keras initialization, in addition to R.
        reticulate::import("tf_keras")$utils$set_random_seed(as.integer(seed))
      }
      if (method %in% c("ranger_seq10", "xgboost_seq10")) {
        learner <- sub("_seq10$", "", method)
        learner_params <- if (learner == "ranger")
          list(ranger = list(num.trees = config$trees, num.threads = 1L)) else
          list(xgboost = list(nrounds = config$boosting_rounds, nthread = 1L))
        return(as.data.frame(VIM::vimpute(d, method = learner,
          sequential = TRUE, nseq = 10L, pmm = FALSE, uncert = "none",
          learner_params = learner_params,
          seed = seed, tune = FALSE, imp_var = FALSE, verbose = FALSE)))
      }
      pars <- config$transformer
      if (method == "shared") {
        model <- do.call(VIM::fit_shared_transformer,
                         c(list(data = d, variables = targets, seed = seed, verbose = 0L), pars))
        return(as.data.frame(predict(model, d)))
      }
      out <- d
      for (v in targets) {
        # Always start from the SAME incomplete table, not previous completions.
        predictors <- setdiff(names(d), v)
        form <- stats::reformulate(predictors, response = v)
        ans <- switch(method,
          transformerImpute = do.call(VIM::transformerImpute,
            c(list(data = d, target = v, cat_vars = names(d)[vapply(d, is.factor, logical(1))],
                   head_dim = "compact", pooling = "masked", feed_forward_dim = 32L,
                   dense_dim = 16L, decimal_points = 0L, decimal_points_target = 0L,
                   learning_rate = .001, verbose = 0L), pars)),
          tabular = {
            model <- do.call(VIM::fit_tabular_transformer,
                            c(list(data = d, target = v, seed = seed, verbose = 0L), pars))
            predict(model, d)
          },
          kNN = VIM::kNN(d, variable = v, dist_var = predictors, k = 5L,
                         imp_var = FALSE, useImputedDist = FALSE),
          ranger = VIM::rangerImpute(form, d, imp_var = FALSE,
                                     num.trees = config$trees, num.threads = 1L),
          xgboost = VIM::xgboostImpute(form, d, imp_var = FALSE,
                                       nrounds = config$boosting_rounds, nthread = 1L))
        out[[v]] <- as.data.frame(ans)[[v]]
      }
      out
    }
  }), config$methods)
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

uci_run <- function(config = uci_config(), cache = "uci-cache", output = "uci-results",
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

uci_run_single <- function(config, cache = "uci-cache", output = "uci-results",
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
  packages <- c("VIM", "keras", "tensorflow", "reticulate", "ranger", "xgboost", "mlr3",
                "mlr3learners", "mlr3pipelines")
  versions <- vapply(packages, function(p) if (requireNamespace(p, quietly = TRUE))
    as.character(utils::packageVersion(p)) else "unavailable", character(1))
  python <- if (any(config$methods %in% c("transformerImpute", "tabular", "shared")))
    reticulate::py_run_string("import importlib.metadata as md\nversions = {p: md.version(p) for p in ['tensorflow', 'tf-keras', 'numpy']}")$versions else NULL
  provenance <- capture.output(list(config = config, packages = versions, python = python,
    system = Sys.info()[c("sysname", "release", "machine")], threads = Sys.getenv(c("OMP_NUM_THREADS", "TF_NUM_INTRAOP_THREADS",
    "TF_NUM_INTEROP_THREADS", "TF_DETERMINISTIC_OPS")), session = utils::sessionInfo()))
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

# Add a method to a completed local experiment without retraining competitors.
# Requires the full local artifacts.rds, intentionally not bundled with VIM.
uci_extend_single <- function(output, methods = "ranger_seq10", scope = NULL) {
  config <- readRDS(file.path(output, "config.rds"))
  if (!identical(config$protocol, uci_config()$protocol))
    stop("Different benchmark protocol; regenerate with uci_run() instead of mixing results")
  if (any(methods %in% config$methods)) stop("Method already present in this run")
  artifacts <- readRDS(file.path(output, "artifacts.rds"))
  runs <- utils::read.csv(file.path(output, "runs.csv"), stringsAsFactors = FALSE)
  metrics <- utils::read.csv(file.path(output, "metrics.csv"), stringsAsFactors = FALSE)
  # Preserve the baseline until all files of the extension are committed.
  # A restart during final CSV/artifact writes must not append duplicate rows.
  base_file <- file.path(output, "extension-base.rds")
  baseline <- if (file.exists(base_file)) readRDS(base_file) else NULL
  if (is.null(baseline) || !identical(baseline$config, config)) {
    session_file <- file.path(output, "session-extension.txt")
    baseline <- list(config = config, artifacts = artifacts, runs = runs, metrics = metrics,
      session = if (file.exists(session_file)) readLines(session_file) else character())
    saveRDS(baseline, paste0(base_file, ".tmp"))
    if (!file.rename(paste0(base_file, ".tmp"), base_file)) stop("Baseline checkpoint failed")
  }
  artifacts <- baseline$artifacts; runs <- baseline$runs; metrics <- baseline$metrics
  extra <- config; extra$methods <- methods
  functions <- uci_methods(extra)
  for (key in names(artifacts)) {
    a <- artifacts[[key]]
    fields <- strsplit(key, "-", fixed = TRUE)[[1L]]
    reference <- runs[runs$dataset == fields[1L] & runs$repetition == as.integer(fields[2L]) &
      runs$mechanism == fields[3L] & runs$rate == as.numeric(fields[4L]), , drop = FALSE][1L, ]
    if (!is.null(scope) && (!config$n %in% scope$n || !reference$rate %in% scope$rates))
      next
    targets <- names(which(colSums(a$mask) > 0L))
    incomplete <- a$truth
    for (v in targets) incomplete[[v]][a$mask[, v]] <- NA
    incomplete <- droplevels(incomplete)
    for (method in methods) {
      checkpoint <- file.path(output, "extension-checkpoints", method, paste0(key, ".rds"))
      if (file.exists(checkpoint)) {
        cached <- readRDS(checkpoint)
        if (!identical(cached$config, config))
          stop("Extension checkpoint configuration differs")
        runs <- rbind(runs, cached$row)
        if (!is.null(cached$scores)) {
          metrics <- rbind(metrics, cbind(cached$row, cached$scores))
          artifacts[[key]]$completed[[method]] <- cached$completed
        }
        next
      }
      message(key, " / ", method)
      warnings <- character(); started <- proc.time()[["elapsed"]]
      value <- tryCatch(withCallingHandlers({
        completed <- functions[[method]](incomplete, targets, reference$model_seed)
        scores <- uci_score(a$truth, completed, a$mask, targets)
        list(completed = completed, scores = scores)
      }, warning = function(w) {
        warnings <<- unique(c(warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }), error = identity)
      row <- reference
      rownames(row) <- NULL
      row$method <- method; row$elapsed <- proc.time()[["elapsed"]] - started
      row$status <- if (inherits(value, "error")) "failed" else "ok"
      row$error <- if (inherits(value, "error")) conditionMessage(value) else ""
      row$warnings <- paste(warnings, collapse = " | ")
      runs <- rbind(runs, row)
      if (!inherits(value, "error")) {
        metrics <- rbind(metrics, cbind(row, value$scores))
        artifacts[[key]]$completed[[method]] <- value$completed
      }
      dir.create(dirname(checkpoint), recursive = TRUE, showWarnings = FALSE)
      saveRDS(list(config = config, scope = scope, row = row,
        scores = if (!inherits(value, "error")) value$scores else NULL,
        completed = if (!inherits(value, "error")) value$completed else NULL),
        paste0(checkpoint, ".tmp"))
      if (!file.rename(paste0(checkpoint, ".tmp"), checkpoint))
        stop("Could not save extension checkpoint")
    }
  }
  for (v in c("error", "warnings")) {
    runs[[v]][is.na(runs[[v]])] <- ""
    metrics[[v]][is.na(metrics[[v]])] <- ""
  }
  config$methods <- c(config$methods, methods)
  if (!is.null(scope)) config$extensions <- c(config$extensions,
                                             list(c(list(methods = methods), scope)))
  # Earlier scenario checkpoints predate the added methods.
  unlink(file.path(output, "checkpoint.rds"))
  saveRDS(artifacts, file.path(output, "artifacts.rds"))
  utils::write.csv(runs, file.path(output, "runs.csv"), row.names = FALSE, na = "")
  utils::write.csv(metrics, file.path(output, "metrics.csv"), row.names = FALSE)
  python <- if (any(methods %in% c("tabimpute", "tabiclImpute", "tabpfnImpute")))
    list(executable = reticulate::py_config()$python,
         versions = setNames(lapply(c("tabimpute", "tabicl", "tabpfn", "tabpfn-extensions",
           "torch", "numpy", "scikit-learn"), function(p)
             reticulate::import("importlib.metadata")$version(p)),
           c("tabimpute", "tabicl", "tabpfn", "tabpfn-extensions",
             "torch", "numpy", "scikit-learn")),
         torch_threads = reticulate::import("torch")$get_num_threads(),
         tabimpute_device = Sys.getenv("VIM_TABIMPUTE_DEVICE", "cpu"),
         other_foundation_devices = "cpu",
         tabimpute_row_chunk = 32L, tabimpute_input_builder = "vectorized-equivalent",
         mps_cache_cleanup = "between chunks and method calls",
         ensemble_members = 1L,
         imputation_passes = 1L) else NULL
  provenance <- capture.output(list(added_methods = methods, scope = scope, config = config, python = python,
    system = Sys.info()[c("sysname", "release", "machine")], session = utils::sessionInfo()))
  session_file <- file.path(output, "session-extension.txt")
  writeLines(c(baseline$session, trimws(paste(sub("[ \t]+$", "", provenance),
                    collapse = "\n"), which = "right")), session_file)
  uci_distribution_summary(output)
  # Config is the commit marker: only advertise the added methods after all
  # their outputs and provenance are complete.
  saveRDS(config, file.path(output, "config.rds"))
  unlink(base_file)
  invisible(list(runs = runs, metrics = metrics))
}

# Extend all sizes using their saved reference tables and masks.
uci_extend <- function(output, methods = "ranger_seq10", sample_sizes = NULL, rates = NULL) {
  config <- readRDS(file.path(output, "config.rds"))
  scope <- if (!is.null(sample_sizes) || !is.null(rates))
    list(n = if (is.null(sample_sizes)) config$n else sample_sizes,
         rates = if (is.null(rates)) config$rates else rates) else NULL
  if (!is.null(scope) && (!length(scope$n) || !length(scope$rates) ||
      any(!scope$n %in% config$n) || any(!scope$rates %in% config$rates)))
    stop("Extension scope must use existing sample sizes and missingness rates")
  if (!dir.exists(file.path(output, paste0("n", config$n[1L]))))
    return(uci_extend_single(output, methods, scope))
  if (any(methods %in% config$methods)) stop("Method already present in this run")
  for (n in config$n) {
    directory <- file.path(output, paste0("n", n))
    single <- readRDS(file.path(directory, "config.rds"))
    missing <- setdiff(methods, single$methods)
    if (length(missing)) uci_extend_single(directory, missing, scope) else
      uci_distribution_summary(directory)
    saveRDS(readRDS(file.path(directory, "config.rds")),
            file.path(directory, "complete.rds"))
  }
  config$methods <- c(config$methods, methods)
  if (!is.null(scope)) config$extensions <- c(config$extensions,
                                             list(c(list(methods = methods), scope)))
  saveRDS(config, file.path(output, "config.rds"))
  uci_collect_sizes(output)
}
