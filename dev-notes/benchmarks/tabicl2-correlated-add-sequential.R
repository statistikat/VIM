# Add only the missing pretrained sequential variant to saved paired repetitions.
# The original cache remains unchanged; completed additions resume without fitting.
tabicl_correlated_add_sequential <- function(input, output, backend = NULL) {
  candidates <- file.path(c('.', '..', '../..'), 'dev-notes', 'benchmarks', 'tabicl2.R')
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) stop('Run from the VIM source tree')
  root <- dirname(dirname(dirname(normalizePath(candidates[1L]))))
  spec <- readRDS(file.path(input, 'config.rds'))
  method <- 'vimpute_tabicl2_seq10'
  stopifnot(!method %in% spec$config$methods,
            identical(spec$backend, if (is.null(backend)) 'python' else 'supplied'),
            normalizePath(input) != normalizePath(output, mustWork = FALSE))
  files <- c('R/tabiclImpute.R', 'R/helper_vimpute.R', 'dev-notes/benchmarks/tabicl2.R')
  hashes <- setNames(unname(tools::md5sum(file.path(root,files))), files)
  if (!identical(hashes, spec$code_hashes[files])) stop('Learner or benchmark source changed')
  paths <- file.path(input, sprintf('rep%03d', seq_len(spec$repetitions)), 'experiment.rds')
  if (any(!file.exists(paths))) stop('Original experiment is incomplete')
  expanded <- spec
  expanded$protocol <- 'tabicl-correlated-y-only-mcar-repetitions-v1-plus-pretrained-sequential'
  expanded$config$methods <- c('vimpute_tabicl2', method,
    'vimpute_tabicl2_finetune', 'vimpute_tabicl2_finetune_seq10')
  expanded$parent_config_md5 <- unname(tools::md5sum(file.path(input, 'config.rds')))
  expanded$parent_repetition_md5 <- unname(tools::md5sum(paths))
  expanded$addition_code_md5 <- unname(tools::md5sum(
    file.path(root,'dev-notes/benchmarks/tabicl2-correlated-add-sequential.R')))
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  config_path <- file.path(output, 'config.rds')
  if (file.exists(config_path)) {
    if (!identical(readRDS(config_path), expanded)) stop('Different expansion specification')
  } else saveRDS(expanded, config_path)
  bench <- new.env(parent = environment())
  sys.source(file.path(root,'dev-notes/benchmarks/tabicl2.R'), bench)
  for (id in seq_along(paths)) {
    folder <- file.path(output, sprintf('rep%03d', id))
    path <- file.path(folder, 'experiment.rds')
    if (file.exists(path)) next
    old <- readRDS(paths[id])
    stopifnot(identical(old$config, spec$config), identical(names(old$truth), c('X','Y','A','B')),
      identical(colSums(old$mask), c(X=0, Y=as.double(sum(old$mask)), A=0, B=0)))
    if (is.null(backend)) backend <- VIM:::.tabicl_import()
    calls <- list()
    wrap <- function(kind) {
      force(kind)
      function(...) {
        args <- list(...)
        calls[[length(calls)+1L]] <<- list(kind=kind,args=args)
        do.call(backend[[kind]],args)
      }
    }
    constructors <- c('TabICLRegressor','TabICLClassifier',
      'FinetunedTabICLRegressor','FinetunedTabICLClassifier')
    config <- spec$config
    config$methods <- method
    config$tabicl2$tabicl_module <- setNames(lapply(constructors,wrap),constructors)
    run <- bench$uci_methods(config)[[method]]
    message('Pretrained sequential repetition ', id, '/', spec$repetitions)
    started <- proc.time()[['elapsed']]
    ans <- run(old$incomplete, 'Y', old$design$seed+2L)
    elapsed <- proc.time()[['elapsed']]-started
    stopifnot(identical(dim(ans),dim(old$truth)), identical(names(ans),names(old$truth)), !anyNA(ans),
      all(is.finite(ans$Y)), all(vapply(calls,function(x) x$kind=='TabICLRegressor',logical(1))))
    for (v in names(ans)) stopifnot(identical(ans[[v]][!old$mask[,v]],old$truth[[v]][!old$mask[,v]]))
    take <- old$mask[,'Y']; error <- ans$Y[take]-old$truth$Y[take]
    metrics <- data.frame(method,variable='Y',n_masked=sum(take),
      rmse=sqrt(mean(error^2)),mae=mean(abs(error)),
      nrmse=sqrt(mean(error^2))/stats::sd(old$truth$Y[take]),accuracy=NA_real_,
      elapsed,model_fits=length(calls),sweeps=length(calls))
    result <- old
    result$completed[[method]] <- ans
    result$fit_calls[[method]] <- calls
    result$metrics <- rbind(old$metrics, metrics)
    result$config <- expanded$config
    # Three-way prediction comparisons belong to the parent run. Paired RMSE
    # comparisons for all four methods are calculated by the summary helper.
    result$parent_differences <- result$differences
    result$differences <- NULL
    dir.create(folder,recursive=TRUE,showWarnings=FALSE)
    saveRDS(result,paste0(path,'.tmp'))
    if (!file.rename(paste0(path,'.tmp'),path)) stop('Checkpoint write failed')
  }
  invisible(output)
}
