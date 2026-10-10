#' Experimental bootstrap and predictive-mean-matching transformer imputations
#'
#' Fits a fresh model to a bootstrap sample for each completed dataset and
#' draws numeric values from observed donors matched on model predictions.
#' This candidate procedure is not a claim of proper multiple imputation;
#' evaluate bias and interval coverage for the intended analysis. Only a single
#' numeric target is supported. Survey weights, clustering, and categorical
#' target draws are not implemented.
#' @param data Data frame.
#' @param target Single numeric target column with missing values.
#' @param m Number of imputations, at least two.
#' @param donors Number of nearest observed donors available for sampling.
#' @param seed Optional seed.
#' @param bootstrap Resample observed training rows for each fit.
#' @param fitter Optional function(training, target, seed) returning a function
#'   that scores every row of newdata, including observed target rows. If NULL,
#'   uses [fit_tabular_transformer()].
#' @param ... Training arguments for the default tabular transformer fitter.
#' @return A [vimmi] object with experimental status recorded as an attribute.
#' @export
transformer_mi <- function(data,target,m=5L,donors=5L,seed=NULL,bootstrap=TRUE,fitter=NULL,...) {
  call <- match.call()
  if(!is.data.frame(data) || length(target)!=1L || !target %in% names(data) ||
     !is.numeric(data[[target]]) || inherits(data[[target]],c("Date","POSIXt")))
    stop("A single numeric target is required")
  for(x in list(m,donors))
    if(length(x)!=1L || !is.finite(x) || x<1 || x!=floor(x)) stop("m and donors must be positive integers")
  if(m<2L) stop("m must be at least two")
  .tabimpute_validate_scalar_flag(bootstrap,"bootstrap")
  original <- as.data.frame(data)
  observed <- which(!is.na(original[[target]])); missing <- which(is.na(original[[target]]))
  if(!length(missing) || length(observed)<5L || any(!is.finite(original[[target]][observed])))
    stop("Need missing cells and at least five finite observed target values")
  if(donors>length(observed)) stop("donors exceeds observed donor count")
  training_args <- list(...)
  if(!is.null(fitter) && length(training_args)) stop("Pass custom fitter settings in the closure, not ...")
  if(is.null(fitter)) fitter <- function(training,target,seed) {
    model <- do.call(fit_tabular_transformer,c(list(data=training,target=target,seed=seed),training_args))
    function(newdata) {
      score_data <- newdata; score_data[[target]][] <- NA
      predict(model,score_data)[[target]]
    }
  }
  if(!is.function(fitter)) stop("fitter must be a function")
  draws <- .transformer_seed(seed, {
    lapply(seq_len(m),function(i) {
      fit_seed <- sample.int(.Machine$integer.max,1L)
      rows <- if(bootstrap) observed[sample.int(length(observed),length(observed),replace=TRUE)] else observed
      score <- fitter(original[rows,,drop=FALSE],target,fit_seed)
      if(!is.function(score)) stop("fitter must return a scoring function")
      prediction <- score(original)
      if(!is.numeric(prediction) || length(prediction)!=nrow(original) || any(!is.finite(prediction)))
        stop("fitter returned invalid prediction scores")
      vapply(missing,function(row) {
        distance <- abs(prediction[observed]-prediction[row])
        # Random tie breaking prevents an arbitrary preference for early rows.
        nearest <- order(distance,runif(length(distance)))[seq_len(donors)]
        original[[target]][observed[nearest[sample.int(length(nearest),1L)]]]
      }, numeric(1))
    })
  })
  imp <- as.data.frame(do.call(cbind,draws)); names(imp) <- as.character(seq_len(m))
  if(is.integer(original[[target]])) imp[] <- lapply(imp,as.integer)
  where <- matrix(FALSE,nrow(original),ncol(original),dimnames=list(NULL,names(original)))
  where[missing,target] <- TRUE
  result <- new_vimmi(data=data,imp=setNames(list(imp),target),where=where,m=m,
    nmis=colSums(where),method=setNames(list("transformer_pmm_experimental"),target),
    boot=bootstrap,uncert="pmm",call=call,seed=seed)
  attr(result,"experimental") <- "Bootstrap + PMM candidate; inferential validity requires coverage evaluation"
  result
}

# Internal simulation harness, intentionally opt-in (not an inferential guarantee).
.transformer_mi_simulation <- function(methods,replications=100L,n=200L,
    mechanisms=c("MCAR","MAR","MNAR"),seed=1L,output=NULL) {
  rows <- list(); index <- 0L
  .transformer_seed(seed, {
    for(rep in seq_len(replications)) {
      x <- rnorm(n); truth <- data.frame(x=x,y=1+2*x+rnorm(n))
      for(mechanism in mechanisms) {
        mask <- .transformer_benchmark_mask(truth,.3,mechanism,seed+rep)
        data <- truth; data$y[mask[,"y"]] <- NA
        for(name in names(methods)) {
          index <- index+1L; start <- proc.time()[["elapsed"]]
          outcome <- tryCatch({
            mi <- methods[[name]](data,seed+rep)
            datasets <- if(inherits(mi,"vimmi")) vim_complete(mi,"all") else mi
            if(!is.list(datasets) || length(datasets)<2L) stop("Method must supply multiple completed datasets")
            estimates <- vapply(datasets,function(d) {
              if(anyNA(d$y) || !identical(d$y[!is.na(data$y)],data$y[!is.na(data$y)])) stop("Invalid completed data")
              model <- stats::lm(y~x,data=d)
              c(estimate=stats::coef(model)[[2]],within=stats::vcov(model)[2,2])
            },numeric(2))
            m <- ncol(estimates); q <- mean(estimates[1,]); u <- mean(estimates[2,]); b <- stats::var(estimates[1,])
            total <- u+(1+1/m)*b
            df <- if(b>0) (m-1)*(1+u/((1+1/m)*b))^2 else Inf
            half <- stats::qt(.975,df)*sqrt(total)
            c(estimate=q,within=u,between=b,total=total,covered=abs(q-2)<=half,width=2*half)
          },error=identity)
          failed <- inherits(outcome,"error")
          values <- if(failed) setNames(rep(NA_real_,6),c("estimate","within","between","total","covered","width")) else outcome
          rows[[index]] <- cbind(data.frame(method=name,replication=rep,mechanism=mechanism,
            elapsed=proc.time()[["elapsed"]]-start,error=if(failed) conditionMessage(outcome) else ""),as.data.frame(as.list(values)))
        }
      }
    }
  })
  runs <- do.call(rbind,rows)
  summary <- do.call(rbind,lapply(split(runs,list(runs$method,runs$mechanism),drop=TRUE),function(d) {
    ok <- d[!nzchar(d$error),,drop=FALSE]; count <- nrow(ok)
    coverage <- if(count) mean(ok$covered) else NA_real_
    data.frame(method=d$method[1],mechanism=d$mechanism[1],runs=nrow(d),failures=sum(nzchar(d$error)),
      bias=mean(ok$estimate)-2,empirical_variance=stats::var(ok$estimate),
      within=mean(ok$within),between=mean(ok$between),coverage=coverage,
      coverage_mcse=if(count) sqrt(coverage*(1-coverage)/count) else NA_real_,
      interval_width=mean(ok$width),mean_seconds=mean(d$elapsed))
  }))
  report <- list(runs=runs,summary=summary,seed=seed,
    note="Normal linear slope estimand=2; small runs are smoke tests; MNAR is sensitivity only")
  if(!is.null(output)) saveRDS(report,output)
  report
}
