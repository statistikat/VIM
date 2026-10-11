# Source this file, then call uci_run(). Nothing downloads or trains on source().
# See vignettes/transformerImpute-benchmark.Rmd for the protocol.
uci_config <- function(full = FALSE) {
  list(protocol = "makeMissing-evaluation-v1", n = if (full) 2000L else 240L, rates = c(.1, .2, .4),
       repetitions = if (full) 10L else 3L, mechanisms = c("MCAR", "MAR"),
       seed = 20261010L,
       methods = c("transformerImpute", "tabular", "shared", "kNN", "ranger", "ranger_seq10", "xgboost", "xgboost_seq10"),
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

uci_methods <- function(config) {
  exports <- getNamespaceExports("VIM")
  need <- c(transformerImpute = "transformerImpute", tabular = "fit_tabular_transformer",
            shared = "fit_shared_transformer", kNN = "kNN", ranger = "rangerImpute",
            ranger_seq10 = "vimpute", xgboost = "xgboostImpute", xgboost_seq10 = "vimpute")
  if (any(!config$methods %in% names(need))) stop("Unknown benchmark method")
  setNames(lapply(config$methods, function(method) {
    function(d, targets, seed) {
      if (!need[[method]] %in% exports) stop("Unavailable in this VIM version: ", need[[method]])
      set.seed(seed)
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

uci_run <- function(config = uci_config(), cache = "uci-cache", output = "uci-results",
                    download = FALSE) {
  stopifnot(config$n >= 50L, config$repetitions >= 1L)
  if (file.exists(file.path(output, "config.rds")))
    stop("Output already contains a run; choose a new directory")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  prepared <- uci_prepare(cache, download)
  methods <- uci_methods(config)
  results <- runs <- artifacts <- list()
  targets_by_dataset <- list(Adult = c("hours_per_week", "income"), Bank = c("balance", "y"))
  saveRDS(config, file.path(output, "config.rds"))
  utils::write.csv(prepared$audit, file.path(output, "preprocessing.csv"), row.names = FALSE)
  utils::write.csv(prepared$sources, file.path(output, "sources.csv"), row.names = FALSE)
  for (di in seq_along(prepared$data)) for (rep in seq_len(config$repetitions)) {
    dataset <- names(prepared$data)[di]; targets <- targets_by_dataset[[dataset]]
    sample_seed <- config$seed + di * 10000L + rep * 100L
    set.seed(sample_seed)
    source <- prepared$data[[dataset]]
    row_ids <- sample.int(nrow(source), min(config$n, nrow(source)))
    truth <- droplevels(source[row_ids, , drop = FALSE])
    for (mi in seq_along(config$mechanisms)) for (ri in seq_along(config$rates)) {
      mechanism <- config$mechanisms[mi]; rate <- config$rates[ri]
      seed <- sample_seed + mi * 10L + ri
      # Independent stream: otherwise makeMissing's MCAR rows can coincide
      # exactly with a model's random validation split at the same seed.
      model_seed <- seed + 1000000L
      incomplete <- VIM::makeMissing(truth, prop = rate, mechanism = mechanism,
        vars = targets, weights = if (mechanism == "MAR") c(age = 1) else NULL, seed = seed)
      mask <- attr(incomplete, "where")
      # Do not supply category levels learned only from the withheld cells.
      incomplete <- droplevels(incomplete)
      key <- paste(dataset, rep, mechanism, rate, sep = "-")
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
      saveRDS(artifacts, file.path(output, "artifacts.rds"), compress = "xz")
    }
  }
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
uci_extend <- function(output, methods = "ranger_seq10") {
  config <- readRDS(file.path(output, "config.rds"))
  if (!identical(config$protocol, uci_config()$protocol))
    stop("Different benchmark protocol; regenerate with uci_run() instead of mixing results")
  if (any(methods %in% config$methods)) stop("Method already present in this run")
  artifacts <- readRDS(file.path(output, "artifacts.rds"))
  runs <- utils::read.csv(file.path(output, "runs.csv"), stringsAsFactors = FALSE)
  metrics <- utils::read.csv(file.path(output, "metrics.csv"), stringsAsFactors = FALSE)
  extra <- config; extra$methods <- methods
  functions <- uci_methods(extra)
  for (key in names(artifacts)) {
    a <- artifacts[[key]]
    fields <- strsplit(key, "-", fixed = TRUE)[[1L]]
    reference <- runs[runs$dataset == fields[1L] & runs$repetition == as.integer(fields[2L]) &
      runs$mechanism == fields[3L] & runs$rate == as.numeric(fields[4L]), , drop = FALSE][1L, ]
    targets <- names(which(colSums(a$mask) > 0L))
    incomplete <- a$truth
    for (v in targets) incomplete[[v]][a$mask[, v]] <- NA
    incomplete <- droplevels(incomplete)
    for (method in methods) {
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
    }
  }
  for (v in c("error", "warnings")) {
    runs[[v]][is.na(runs[[v]])] <- ""
    metrics[[v]][is.na(metrics[[v]])] <- ""
  }
  config$methods <- c(config$methods, methods)
  saveRDS(config, file.path(output, "config.rds"))
  saveRDS(artifacts, file.path(output, "artifacts.rds"), compress = "xz")
  utils::write.csv(runs, file.path(output, "runs.csv"), row.names = FALSE, na = "")
  utils::write.csv(metrics, file.path(output, "metrics.csv"), row.names = FALSE)
  provenance <- capture.output(list(added_methods = methods, config = config,
    system = Sys.info()[c("sysname", "release", "machine")], session = utils::sessionInfo()))
  writeLines(trimws(paste(sub("[ \t]+$", "", provenance), collapse = "\n"), which = "right"),
             file.path(output, "session-extension.txt"))
  uci_distribution_summary(output)
  invisible(list(runs = runs, metrics = metrics))
}
