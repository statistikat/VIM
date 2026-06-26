#' Check whether the optional TabImpute backend is available
#'
#' `tabimpute_available()` checks for the optional R and Python dependencies
#' needed by [tabimpute()]. It never errors and does not run at package load
#' time.
#'
#' @return A logical scalar.
#' @export
tabimpute_available <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }

  ok <- tryCatch(
    .tabimpute_has_supported_api(.tabimpute_import()),
    error = function(e) FALSE
  )

  isTRUE(ok)
}

#' Impute missing values using TabImpute
#'
#' `tabimpute()` provides an experimental interface to a Python TabImpute
#' backend for zero-shot missing-data imputation in tabular data. The Python
#' dependency is optional and imported lazily via `reticulate` only when the
#' function is called on data with selected missing values.
#'
#' TabImpute support is experimental. It provides single imputed values and is
#' not a replacement for multiple imputation. Benchmark results carefully before
#' using it in survey or statistical-production workflows. Observed values are
#' preserved, and only originally missing values in selected variables are
#' replaced.
#'
#' The adapter targets the Python package `tabimpute` and its
#' `tabimpute.interface.ImputePFN`, `tabimpute.interface.TabImputeCategorical`,
#' and `tabimpute.tabimpute_v2.TabImputeV2` classes. The Python package
#' downloads model checkpoints from Hugging Face when no `checkpoint_path` is
#' supplied, so pass `checkpoint_path` for offline or production use.
#'
#' @param data A `data.frame`, tibble, or `data.table`.
#' @param variables Character vector of variables to impute. If `NULL`, all
#'   variables with missing values are considered, excluding `id_vars`.
#' @param id_vars Character vector of variables that must not be imputed or sent
#'   to the backend.
#' @param mixed Logical. Whether to allow mixed numeric/categorical data to be
#'   passed to the backend. If `FALSE`, non-numeric backend columns are rejected.
#' @param max_rows Optional maximum number of rows allowed.
#' @param max_cols Optional maximum number of columns allowed.
#' @param python Optional path to the Python executable used by `reticulate`.
#' @param tabimpute_module Optional Python module/object, or an R function for
#'   testing, used instead of importing the default Python module.
#' @param backend Character string selecting the Python backend. `"auto"` uses
#'   `TabImputeCategorical` when categorical columns are present and `ImputePFN`
#'   otherwise. `"v2"` selects `TabImputeV2` and currently supports numeric data
#'   only.
#' @param device Device string passed to the Python backend, for example
#'   `"cpu"` or `"cuda"`.
#' @param checkpoint_path Optional path to a local TabImpute checkpoint. If
#'   `NULL`, the Python package may try to download the checkpoint.
#' @param preprocessors Optional Python preprocessors passed to the backend
#'   constructor.
#' @param nhead Number of attention heads passed to `ImputePFN` backends.
#' @param backend_max_rows Optional row chunk size passed to `ImputePFN`.
#' @param backend_max_chunks Optional maximum number of chunks passed to
#'   `ImputePFN`.
#' @param seed Optional seed passed to the backend where supported.
#' @param verbose Logical. If `TRUE`, print informative messages.
#' @param imp_var Logical. If `TRUE`, add logical imputation indicator columns.
#' @param imp_suffix Suffix for imputation indicator columns.
#' @param ... Additional arguments passed to the backend adapter.
#' @return An imputed data set. `data.table` inputs return a `data.table`;
#'   other inputs return a `data.frame`.
#' @family imputation methods
#' @export
#' @examples
#' \dontrun{
#' data(sleep, package = "VIM")
#' if (tabimpute_available()) {
#'   tabimpute(sleep)
#' }
#' }
tabimpute <- function(
    data,
    variables = NULL,
    id_vars = NULL,
    mixed = TRUE,
    max_rows = NULL,
    max_cols = NULL,
    python = NULL,
    tabimpute_module = NULL,
    backend = c("auto", "imputepfn", "categorical", "v2"),
    device = "cpu",
    checkpoint_path = NULL,
    preprocessors = NULL,
    nhead = 2L,
    backend_max_rows = NULL,
    backend_max_chunks = NULL,
    seed = NULL,
    verbose = FALSE,
    imp_var = TRUE,
    imp_suffix = "imp",
    ...
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame, tibble, or data.table.", call. = FALSE)
  }

  .tabimpute_validate_scalar_flag(mixed, "mixed")
  .tabimpute_validate_scalar_flag(verbose, "verbose")
  .tabimpute_validate_scalar_flag(imp_var, "imp_var")
  .tabimpute_validate_name_part(imp_suffix, "imp_suffix")
  .tabimpute_validate_name_part(device, "device")
  .tabimpute_validate_size_limit(max_rows, "max_rows")
  .tabimpute_validate_size_limit(max_cols, "max_cols")
  .tabimpute_validate_size_limit(backend_max_rows, "backend_max_rows")
  .tabimpute_validate_size_limit(backend_max_chunks, "backend_max_chunks")
  .tabimpute_validate_size_limit(nhead, "nhead")
  backend <- match.arg(backend)

  original <- data
  data_df <- as.data.frame(data, stringsAsFactors = FALSE)
  names_data <- names(data_df)

  if (is.null(names_data) || any(!nzchar(names_data))) {
    stop("`data` must have non-empty column names.", call. = FALSE)
  }

  id_vars <- .tabimpute_normalize_names(id_vars, "id_vars", allow_null = TRUE)
  unknown_id <- setdiff(id_vars, names_data)
  if (length(unknown_id) > 0L) {
    stop("Unknown `id_vars`: ", paste(unknown_id, collapse = ", "), call. = FALSE)
  }

  explicit_variables <- !is.null(variables)
  if (is.null(variables)) {
    variables <- names_data[vapply(data_df, anyNA, logical(1))]
  } else {
    variables <- .tabimpute_normalize_names(variables, "variables", allow_null = FALSE)
    unknown <- setdiff(variables, names_data)
    if (length(unknown) > 0L) {
      stop("Unknown `variables`: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
  }

  if (length(id_vars) > 0L) {
    variables_after_id <- setdiff(variables, id_vars)
    if (explicit_variables && length(variables) > 0L && length(variables_after_id) == 0L) {
      stop("All selected `variables` are listed in `id_vars`; nothing can be imputed.",
           call. = FALSE)
    }
    variables <- variables_after_id
  }

  if (!is.null(max_rows) && nrow(data_df) > max_rows) {
    stop("`data` has more rows than `max_rows`.", call. = FALSE)
  }
  if (!is.null(max_cols) && ncol(data_df) > max_cols) {
    stop("`data` has more columns than `max_cols`.", call. = FALSE)
  }

  variables <- variables[vapply(data_df[variables], anyNA, logical(1))]

  if (length(variables) == 0L) {
    if (isTRUE(verbose)) {
      message("No missing values found in selected variables; returning data unchanged.")
    }
    return(original)
  }

  prep <- .tabimpute_prepare_data(
    data = data_df,
    variables = variables,
    id_vars = id_vars,
    mixed = mixed
  )

  module <- .tabimpute_import(tabimpute_module = tabimpute_module, python = python)

  completed <- .tabimpute_python_impute(
    x = prep$x,
    mask = prep$mask,
    meta = prep$meta,
    module = module,
    backend = backend,
    device = device,
    checkpoint_path = checkpoint_path,
    preprocessors = preprocessors,
    nhead = nhead,
    backend_max_rows = backend_max_rows,
    backend_max_chunks = backend_max_chunks,
    seed = seed,
    verbose = verbose,
    ...
  )

  .tabimpute_restore_data(
    original = original,
    completed = completed,
    prep = prep,
    variables = variables,
    imp_var = imp_var,
    imp_suffix = imp_suffix
  )
}

.tabimpute_dependency_error <- function() {
  paste(
    "TabImpute support requires optional Python dependencies.",
    "Install the R package 'reticulate' and install the Python TabImpute package",
    "in the Python environment used by reticulate.",
    "The VIM package itself does not install Python dependencies automatically.",
    sep = "\n"
  )
}

.tabimpute_import <- function(tabimpute_module = NULL, python = NULL) {
  if (!is.null(tabimpute_module)) {
    return(tabimpute_module)
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(.tabimpute_dependency_error(), call. = FALSE)
  }

  if (!is.null(python)) {
    tryCatch(
      reticulate::use_python(python, required = TRUE),
      error = function(e) {
        stop(
          "Could not configure the requested Python executable for TabImpute:\n",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  if (!reticulate::py_module_available("tabimpute")) {
    stop(.tabimpute_dependency_error(), call. = FALSE)
  }

  interface <- tryCatch(
    reticulate::import("tabimpute.interface", delay_load = FALSE, convert = FALSE),
    error = function(e) {
      stop(.tabimpute_dependency_error(), "\n\nImport error:\n", conditionMessage(e),
           call. = FALSE)
    }
  )
  v2 <- tryCatch(
    reticulate::import("tabimpute.tabimpute_v2", delay_load = FALSE, convert = FALSE),
    error = function(e) NULL
  )

  api <- list(interface = interface, v2 = v2)
  class(api) <- "vim_tabimpute_python_api"
  if (!.tabimpute_has_supported_api(api)) {
    stop(.tabimpute_unsupported_api_error(), call. = FALSE)
  }

  api
}

.tabimpute_has_supported_api <- function(module) {
  if (is.function(module)) {
    return(TRUE)
  }
  if (inherits(module, "vim_tabimpute_python_api")) {
    has_imputepfn <- .tabimpute_py_has_attr(module$interface, "ImputePFN")
    has_categorical <- .tabimpute_py_has_attr(module$interface, "TabImputeCategorical")
    has_v2 <- !is.null(module$v2) && .tabimpute_py_has_attr(module$v2, "TabImputeV2")
    return(has_imputepfn || has_categorical || has_v2)
  }
  if ((is.list(module) || is.environment(module)) && !.tabimpute_is_python_object(module)) {
    return(is.function(module$impute))
  }
  if (requireNamespace("reticulate", quietly = TRUE)) {
    return(
      .tabimpute_py_has_attr(module, "impute") ||
        .tabimpute_py_has_attr(module, "ImputePFN") ||
        .tabimpute_py_has_attr(module, "TabImputeCategorical") ||
        .tabimpute_py_has_attr(module, "TabImputeV2")
    )
  }
  FALSE
}

.tabimpute_unsupported_api_error <- function() {
  paste(
    "Unsupported TabImpute Python API.",
    "The current VIM adapter expects the Python package 'tabimpute' to expose",
    "'tabimpute.interface.ImputePFN' or 'TabImputeCategorical', or",
    "'tabimpute.tabimpute_v2.TabImputeV2'.",
    "Please pass a compatible object with `tabimpute_module` or report the installed",
    "TabImpute version/API so VIM can add support.",
    sep = "\n"
  )
}

.tabimpute_prepare_data <- function(data, variables, id_vars = NULL, mixed = TRUE) {
  backend_names <- setdiff(names(data), id_vars)
  if (length(backend_names) == 0L) {
    stop("No non-identifier columns are available for TabImpute.", call. = FALSE)
  }

  meta <- lapply(data[backend_names], .tabimpute_column_meta)
  names(meta) <- backend_names

  unsupported <- names(meta)[vapply(meta, function(x) x$kind %in% c("list", "matrix", "unsupported"),
                                    logical(1))]
  if (length(unsupported) > 0L) {
    stop(
      "TabImpute does not support list, matrix, or unsupported columns: ",
      paste(unsupported, collapse = ", "),
      call. = FALSE
    )
  }

  if (!isTRUE(mixed)) {
    non_numeric <- names(meta)[!vapply(meta, function(x) {
      x$kind %in% c("numeric", "integer", "logical", "Date", "POSIXct")
    }, logical(1))]
    if (length(non_numeric) > 0L) {
      stop(
        "The current TabImpute integration received non-numeric/non-logical columns while `mixed = FALSE`: ",
        paste(non_numeric, collapse = ", "),
        call. = FALSE
      )
    }
  }

  x <- as.data.frame(data[backend_names], stringsAsFactors = FALSE)
  for (nm in backend_names) {
    x[[nm]] <- .tabimpute_prepare_column(x[[nm]], meta[[nm]])
  }

  mask <- matrix(FALSE, nrow = nrow(data), ncol = length(backend_names),
                 dimnames = list(NULL, backend_names))
  for (nm in variables) {
    mask[, nm] <- is.na(data[[nm]])
  }

  list(
    x = x,
    mask = mask,
    meta = meta,
    variables = variables,
    id_vars = id_vars,
    backend_names = backend_names
  )
}

.tabimpute_column_meta <- function(x) {
  if (is.matrix(x)) {
    return(list(kind = "matrix", class = class(x)))
  }
  if (inherits(x, "POSIXlt")) {
    return(list(kind = "unsupported", class = class(x)))
  }
  if (inherits(x, "Date")) {
    return(list(kind = "Date", class = class(x)))
  }
  if (inherits(x, "POSIXct")) {
    return(list(kind = "POSIXct", class = class(x), tzone = .tabimpute_tzone(x)))
  }
  if (is.ordered(x)) {
    return(list(kind = "ordered", class = class(x), levels = levels(x)))
  }
  if (is.factor(x)) {
    return(list(kind = "factor", class = class(x), levels = levels(x)))
  }
  if (is.logical(x)) {
    return(list(kind = "logical", class = class(x)))
  }
  if (is.integer(x)) {
    return(list(kind = "integer", class = class(x)))
  }
  if (is.numeric(x)) {
    return(list(kind = "numeric", class = class(x)))
  }
  if (is.character(x)) {
    return(list(kind = "character", class = class(x)))
  }
  if (is.list(x)) {
    return(list(kind = "list", class = class(x)))
  }
  list(kind = "unsupported", class = class(x))
}

.tabimpute_tzone <- function(x) {
  tz <- attr(x, "tzone")
  if (is.null(tz) || !nzchar(tz[1L])) {
    return("")
  }
  tz[1L]
}

.tabimpute_prepare_column <- function(x, meta) {
  switch(
    meta$kind,
    factor = as.character(x),
    ordered = as.character(x),
    Date = as.numeric(x),
    POSIXct = as.numeric(x),
    x
  )
}

.tabimpute_python_impute <- function(x, mask, meta, module,
                                     backend = c("auto", "imputepfn", "categorical", "v2"),
                                     device = "cpu",
                                     checkpoint_path = NULL,
                                     preprocessors = NULL,
                                     nhead = 2L,
                                     backend_max_rows = NULL,
                                     backend_max_chunks = NULL,
                                     seed = NULL,
                                     verbose = FALSE,
                                     ...) {
  if (is.function(module)) {
    return(.tabimpute_call_r_backend(module, x, mask, seed, verbose, ...))
  }
  if ((is.list(module) || is.environment(module)) &&
      !.tabimpute_is_python_object(module) &&
      is.function(module$impute)) {
    return(.tabimpute_call_r_backend(module$impute, x, mask, seed, verbose, ...))
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(.tabimpute_dependency_error(), call. = FALSE)
  }

  backend <- match.arg(backend)
  resolved_backend <- .tabimpute_resolve_backend(backend, meta)
  .tabimpute_validate_backend_types(resolved_backend, meta)
  .tabimpute_set_python_seed(seed)

  imputer <- .tabimpute_python_imputer(
    module = module,
    backend = resolved_backend,
    device = device,
    checkpoint_path = checkpoint_path,
    preprocessors = preprocessors,
    nhead = nhead,
    backend_max_rows = backend_max_rows,
    backend_max_chunks = backend_max_chunks,
    verbose = verbose,
    ...
  )

  x_py <- .tabimpute_to_numpy(x, meta, resolved_backend)
  args <- .tabimpute_python_impute_args(
    x_py = x_py,
    meta = meta,
    backend = resolved_backend
  )

  result <- tryCatch(
    do.call(imputer$impute, args),
    error = function(e) {
      stop(
        "TabImpute backend call failed:\n",
        conditionMessage(e),
        "\n\nIf the Python package tried to download a model checkpoint, ",
        "pass `checkpoint_path` to a local checkpoint for offline use.",
        call. = FALSE
      )
    }
  )

  reticulate::py_to_r(result)
}

.tabimpute_resolve_backend <- function(backend, meta) {
  if (backend != "auto") {
    return(backend)
  }
  if (.tabimpute_has_categorical_columns(meta)) {
    "categorical"
  } else {
    "imputepfn"
  }
}

.tabimpute_has_categorical_columns <- function(meta) {
  any(vapply(meta, function(x) {
    x$kind %in% c("factor", "ordered", "character", "logical")
  }, logical(1)))
}

.tabimpute_validate_backend_types <- function(backend, meta) {
  if (backend == "categorical") {
    return(invisible(TRUE))
  }

  non_numeric <- names(meta)[!vapply(meta, function(x) {
    x$kind %in% c("numeric", "integer", "logical", "Date", "POSIXct")
  }, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      "TabImpute backend '", backend, "' supports numeric/logical columns only. ",
      "Non-numeric columns found: ", paste(non_numeric, collapse = ", "),
      ". Use `backend = \"categorical\"` or `backend = \"auto\"` with `mixed = TRUE`.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.tabimpute_python_imputer <- function(module, backend, device, checkpoint_path,
                                      preprocessors, nhead, backend_max_rows,
                                      backend_max_chunks, verbose, ...) {
  if (.tabimpute_py_has_attr(module, "impute")) {
    return(module)
  }

  if (!inherits(module, "vim_tabimpute_python_api")) {
    module <- .tabimpute_python_api_from_module(module)
  }

  args <- .tabimpute_constructor_args(
    backend = backend,
    device = device,
    checkpoint_path = checkpoint_path,
    preprocessors = preprocessors,
    nhead = nhead,
    backend_max_rows = backend_max_rows,
    backend_max_chunks = backend_max_chunks,
    verbose = verbose,
    ...
  )

  cls <- switch(
    backend,
    imputepfn = module$interface$ImputePFN,
    categorical = module$interface$TabImputeCategorical,
    v2 = {
      if (is.null(module$v2) || !.tabimpute_py_has_attr(module$v2, "TabImputeV2")) {
        stop("The installed TabImpute package does not expose TabImputeV2.",
             call. = FALSE)
      }
      module$v2$TabImputeV2
    }
  )

  tryCatch(
    do.call(cls, args),
    error = function(e) {
      stop(
        "Could not initialize the TabImpute Python backend:\n",
        conditionMessage(e),
        "\n\nIf no checkpoint is cached locally, pass `checkpoint_path` ",
        "or configure the Python environment so it can download the checkpoint.",
        call. = FALSE
      )
    }
  )
}

.tabimpute_python_api_from_module <- function(module) {
  api <- list(interface = module, v2 = module)
  class(api) <- "vim_tabimpute_python_api"
  if (!.tabimpute_has_supported_api(api)) {
    stop(.tabimpute_unsupported_api_error(), call. = FALSE)
  }
  api
}

.tabimpute_constructor_args <- function(backend, device, checkpoint_path,
                                        preprocessors, nhead, backend_max_rows,
                                        backend_max_chunks, verbose, ...) {
  extra <- list(...)
  args <- list(device = device)
  if (!is.null(checkpoint_path)) {
    args$checkpoint_path <- checkpoint_path
  }
  if (!is.null(preprocessors)) {
    args$preprocessors <- preprocessors
  }

  if (backend %in% c("imputepfn", "categorical")) {
    args$nhead <- as.integer(nhead)
  }
  if (backend %in% c("imputepfn", "v2")) {
    args$verbose <- isTRUE(verbose)
  }
  if (backend == "imputepfn") {
    if (!is.null(backend_max_rows)) {
      args$max_num_rows <- as.integer(backend_max_rows)
    }
    if (!is.null(backend_max_chunks)) {
      args$max_num_chunks <- as.integer(backend_max_chunks)
    }
  } else if (!is.null(backend_max_rows) || !is.null(backend_max_chunks)) {
    stop("`backend_max_rows` and `backend_max_chunks` are only supported by backend = \"imputepfn\".",
         call. = FALSE)
  }

  c(args, extra)
}

.tabimpute_python_impute_args <- function(x_py, meta, backend) {
  if (backend == "categorical") {
    categorical <- .tabimpute_categorical_indices(meta, ordered = FALSE)
    ordered <- .tabimpute_categorical_indices(meta, ordered = TRUE)
    return(list(
      x_py,
      categorical_columns = as.list(as.integer(categorical)),
      ordered_categorical_columns = as.list(as.integer(ordered))
    ))
  }

  list(x_py, return_full = FALSE, num_repeats = 1L)
}

.tabimpute_categorical_indices <- function(meta, ordered = FALSE) {
  kinds <- vapply(meta, function(x) x$kind, character(1))
  if (ordered) {
    idx <- which(kinds == "ordered")
  } else {
    idx <- which(kinds %in% c("factor", "ordered", "character", "logical"))
  }
  idx - 1L
}

.tabimpute_to_numpy <- function(x, meta, backend) {
  np <- reticulate::import("numpy", delay_load = FALSE, convert = FALSE)
  if (backend == "categorical") {
    rows <- lapply(seq_len(nrow(x)), function(i) {
      lapply(seq_along(x), function(j) .tabimpute_python_scalar(x[[j]][i], meta[[j]]))
    })
    return(np$array(reticulate::r_to_py(rows), dtype = "object"))
  }

  x_num <- as.data.frame(lapply(x, as.numeric), stringsAsFactors = FALSE)
  mat <- as.matrix(x_num)
  storage.mode(mat) <- "double"
  np$array(mat, dtype = "float64")
}

.tabimpute_python_scalar <- function(value, meta) {
  if (length(value) == 0L || is.na(value)) {
    return(NULL)
  }
  switch(
    meta$kind,
    integer = as.integer(value),
    logical = as.logical(value),
    numeric = as.numeric(value),
    Date = as.numeric(value),
    POSIXct = as.numeric(value),
    as.character(value)
  )
}

.tabimpute_set_python_seed <- function(seed) {
  if (is.null(seed)) {
    return(invisible(FALSE))
  }
  seed <- as.integer(seed)
  np <- reticulate::import("numpy", delay_load = FALSE, convert = FALSE)
  np$random$seed(seed)
  torch <- tryCatch(
    reticulate::import("torch", delay_load = FALSE, convert = FALSE),
    error = function(e) NULL
  )
  if (!is.null(torch)) {
    torch$manual_seed(seed)
    cuda_available <- tryCatch(reticulate::py_to_r(torch$cuda$is_available()),
                               error = function(e) FALSE)
    if (isTRUE(cuda_available)) {
      torch$cuda$manual_seed_all(seed)
    }
  }
  invisible(TRUE)
}

.tabimpute_call_r_backend <- function(fun, x, mask, seed = NULL, verbose = FALSE, ...) {
  args <- c(
    list(x = x, mask = mask),
    if (!is.null(seed)) list(seed = seed) else list(),
    list(verbose = isTRUE(verbose)),
    list(...)
  )
  do.call(fun, args)
}

.tabimpute_is_python_object <- function(x) {
  inherits(x, "python.builtin.object") ||
    inherits(x, "python.builtin.module") ||
    inherits(x, "python.builtin.type")
}

.tabimpute_py_has_attr <- function(x, attr) {
  if (!requireNamespace("reticulate", quietly = TRUE) || is.null(x)) {
    return(FALSE)
  }
  isTRUE(tryCatch(reticulate::py_has_attr(x, attr), error = function(e) FALSE))
}

.tabimpute_restore_data <- function(original, completed, prep, variables,
                                    imp_var = TRUE, imp_suffix = "imp") {
  completed <- .tabimpute_completed_to_data_frame(completed, prep)
  out <- as.data.frame(original, stringsAsFactors = FALSE)

  for (var in variables) {
    rows <- prep$mask[, var]
    if (!any(rows)) {
      next
    }

    restored <- .tabimpute_restore_column(
      original = out[[var]],
      completed = completed[[var]],
      meta = prep$meta[[var]],
      name = var
    )

    new_col <- out[[var]]
    new_col[rows] <- restored[rows]
    out[[var]] <- .tabimpute_restore_column(
      original = out[[var]],
      completed = new_col,
      meta = prep$meta[[var]],
      name = var
    )
  }

  if (isTRUE(imp_var)) {
    for (var in variables) {
      rows <- prep$mask[, var]
      if (!any(rows)) {
        next
      }
      imp_col <- paste0(var, "_", imp_suffix)
      if (imp_col %in% names(out)) {
        warning("The following TRUE/FALSE imputation status variables will be updated: ",
                imp_col, call. = FALSE)
      }
      out[[imp_col]] <- as.logical(rows)
    }
  }

  if (data.table::is.data.table(original)) {
    return(data.table::as.data.table(out))
  }

  out
}

.tabimpute_completed_to_data_frame <- function(completed, prep) {
  if (requireNamespace("reticulate", quietly = TRUE)) {
    completed <- tryCatch(reticulate::py_to_r(completed), error = function(e) completed)
  }
  completed <- as.data.frame(completed, stringsAsFactors = FALSE, optional = TRUE)

  if (nrow(completed) != nrow(prep$x)) {
    stop("TabImpute backend returned a result with a different number of rows.",
         call. = FALSE)
  }
  if (ncol(completed) != length(prep$backend_names)) {
    stop("TabImpute backend returned a result with a different number of columns.",
         call. = FALSE)
  }

  default_names <- paste0("V", seq_len(ncol(completed)))
  if (is.null(names(completed)) || identical(names(completed), default_names)) {
    names(completed) <- prep$backend_names
  }
  if (!all(prep$backend_names %in% names(completed))) {
    stop("TabImpute backend result must preserve column names.", call. = FALSE)
  }

  completed[, prep$backend_names, drop = FALSE]
}

.tabimpute_restore_column <- function(original, completed, meta, name) {
  switch(
    meta$kind,
    numeric = as.numeric(completed),
    integer = .tabimpute_restore_integer(completed),
    logical = .tabimpute_restore_logical(completed, name),
    character = as.character(completed),
    factor = .tabimpute_restore_factor(completed, meta, name, ordered = FALSE),
    ordered = .tabimpute_restore_factor(completed, meta, name, ordered = TRUE),
    Date = .tabimpute_restore_date(completed),
    POSIXct = .tabimpute_restore_posixct(completed, meta),
    completed
  )
}

.tabimpute_restore_integer <- function(x) {
  suppressWarnings(as.integer(round(as.numeric(x))))
}

.tabimpute_restore_logical <- function(x, name) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    out <- rep(NA, length(x))
    out[!is.na(x)] <- x[!is.na(x)] >= 0.5
    return(out)
  }

  chr <- tolower(as.character(x))
  out <- rep(NA, length(chr))
  out[chr %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[chr %in% c("false", "f", "0", "no", "n")] <- FALSE
  bad <- !is.na(chr) & is.na(out)
  if (any(bad)) {
    warning("Could not restore some predicted values for logical variable '", name,
            "'; setting them to NA.", call. = FALSE)
  }
  out
}

.tabimpute_restore_factor <- function(x, meta, name, ordered = FALSE) {
  chr <- as.character(x)
  unknown <- !is.na(chr) & !(chr %in% meta$levels)
  if (any(unknown)) {
    warning(
      "Predicted values outside existing levels for factor variable '", name,
      "' were set to NA: ", paste(unique(chr[unknown]), collapse = ", "),
      call. = FALSE
    )
    chr[unknown] <- NA_character_
  }
  factor(chr, levels = meta$levels, ordered = ordered)
}

.tabimpute_restore_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (is.numeric(x) || is.integer(x)) {
    return(as.Date(round(as.numeric(x)), origin = "1970-01-01"))
  }
  as.Date(x)
}

.tabimpute_restore_posixct <- function(x, meta) {
  tz <- if (is.null(meta$tzone)) "" else meta$tzone
  if (inherits(x, "POSIXct")) {
    return(as.POSIXct(x, tz = tz))
  }
  if (is.numeric(x) || is.integer(x)) {
    return(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = tz))
  }
  as.POSIXct(x, tz = tz)
}

.tabimpute_normalize_names <- function(x, arg, allow_null) {
  if (is.null(x)) {
    if (allow_null) {
      return(character(0))
    }
    stop("`", arg, "` must be a character vector.", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("`", arg, "` must be a character vector.", call. = FALSE)
  }
  if (anyNA(x) || any(!nzchar(x))) {
    stop("`", arg, "` must not contain missing or empty names.", call. = FALSE)
  }
  unique(x)
}

.tabimpute_validate_scalar_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

.tabimpute_validate_name_part <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-empty character string.", call. = FALSE)
  }
  invisible(TRUE)
}

.tabimpute_validate_size_limit <- function(x, arg) {
  if (is.null(x)) {
    return(invisible(TRUE))
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1L) {
    stop("`", arg, "` must be a positive numeric scalar.", call. = FALSE)
  }
  invisible(TRUE)
}
