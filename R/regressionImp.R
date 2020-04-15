# ---------------------------------------
# Author: Alexander Kowarik
# ---------------------------------------



#' Regression Imputation
#'
#' Impute missing values based on a regression model.
#'
#'
#' [lm()] is used for family "normal" and [glm()] for all other families.
#' (robust=TRUE: [lmrob()], [glmrob()])
#'
#' @param formula model formula to impute one variable
#' @param data A data.frame or survey object containing the data
#' @param family family argument for [glm()]. `"AUTO"` (the default) tries to choose
#' automatically and is the only really tested option!!!
#' @param robust `TRUE`/`FALSE` if robust regression should be used. See details.
#' @param mod_cat `TRUE`/`FALSE` if `TRUE` for categorical variables the level with
#' the highest prediction probability is selected, otherwise it is sampled
#' according to the probabilities.
#' @param imp_var `TRUE`/`FALSE` if a `TRUE`/`FALSE` variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix used for TF imputation variables
#' @return the imputed data set.
#' @author Alexander Kowarik
#' @references A. Kowarik, M. Templ (2016) Imputation with
#' R package VIM.  *Journal of
#' Statistical Software*, 74(7), 1-16.
#' @keywords manip
#' @family imputation methods
#' @examples
#'
#' data(sleep)
#' sleepImp1 <- regressionImp(Dream+NonD~BodyWgt+BrainWgt,data=sleep)
#' sleepImp2 <- regressionImp(Sleep+Gest+Span+Dream+NonD~BodyWgt+BrainWgt,data=sleep)
#'
#' data(testdata)
#' imp_testdata1 <- regressionImp(b1+b2~x1+x2,data=testdata$wna)
#' imp_testdata3 <- regressionImp(x1~x2,data=testdata$wna,robust=TRUE)
#'
#' @export regressionImp
regressionImp <- function(formula, data, family = "AUTO", robust = FALSE, imp_var = TRUE,
    imp_suffix = "imp", mod_cat = FALSE) {
  UseMethod("regressionImp", data)
}

#' @rdname regressionImp
#' @export

regressionImp.data.frame <- function(formula, data, family = "AUTO", robust = FALSE, imp_var = TRUE,
    imp_suffix = "imp", mod_cat = FALSE) {
  regressionImp_work(formula = formula, data = data, family = family, robust = robust,
      imp_var = imp_var, imp_suffix = imp_suffix, mod_cat = mod_cat)
}

#' @rdname regressionImp
#' @export

regressionImp.survey.design <- function(formula, data, family, robust, imp_var = TRUE,
    imp_suffix = "imp", mod_cat = FALSE) {
  data$variables <- regressionImp_work(formula = formula, data = data$variables, family = family,
      robust = robust, imp_var = imp_var, imp_suffix = imp_suffix, mod_cat = mod_cat)
  data$call <- sys.call(-1)
  data
}

#' @rdname regressionImp
#' @export

regressionImp.default <- function(formula, data, family = "AUTO", robust = FALSE, imp_var = TRUE,
    imp_suffix = "imp", mod_cat = FALSE) {
  regressionImp_work(formula = formula, data = as.data.frame(data), family = family,
      robust = robust, imp_var = imp_var, imp_suffix = imp_suffix, mod_cat = mod_cat)
}

regressionImp_work <- function(formula, family, robust, data, imp_var, imp_suffix, mod_cat) {

  formchar <- as.character(formula)
  lhs <- gsub(" ", "", strsplit(formchar[2], "\\+")[[1]])
  rhs <- formchar[3]
  rhs2 <- gsub(" ", "", strsplit(rhs, "\\+")[[1]])
  #Missings in RHS variables
  TFna2 <- apply(data[, c(rhs2), drop = FALSE], 1, function(x) !any(is.na(x)))
  for(lhsV in lhs) {
    form <- as.formula(paste(lhsV, "~", rhs))
    lhs_vector <- data[[lhsV]]
    #Check if there are missings in this LHS variable
    if (!any(is.na(lhs_vector))) {
      cat(paste0("No missings in ", lhsV, ".\n"))
    } else {
      if (!inherits(family, "function")) {
        if (family == "AUTO") {
          TFna <- TFna2 & !is.na(data[, lhsV])
          if (inherits(lhs_vector, "numeric")) {
            nLev <- 0
            if (robust) {
              fn <- lmrob
            } else {
              fn <- lm
            }
            mod <- fn(form, data = data[TFna, ])
          } else if (inherits(lhs_vector, "factor") || inherits(lhs_vector, "character")) {
            if (inherits(lhs_vector, "character")) {
              dataset[[lhsV]] <- as.factor(dataset[[lhsV]])
              lhs_vector <- data[[lhsV]]
            }
            nLev <- length(levels(lhs_vector))
            if (nLev == 2) {
              fam <- binomial
              if (robust) {
                fn <- glmrob
              } else {
                fn <- glm
              }
              mod <- fn(form, data = data[TFna, ], family = fam)
            } else {
              ## TODO: what to do if this clause is executed and !all(!TFna3) ?
              co <- capture.output(mod <- multinom(form, data[TFna, ]))
            }
          }
          if (imp_var) {
            if (imp_var %in% colnames(data)) {
              data[, paste(lhsV, "_", imp_suffix, sep = "")] <- as.logical(data[, paste(lhsV, "_", imp_suffix, sep = "")])
              warning(paste("The following TRUE/FALSE imputation status variables will be updated:",
                      paste(lhsV, "_", imp_suffix, sep = "")))
            } else {
              data$NEWIMPTFVARIABLE <- is.na(lhs_vector)
              colnames(data)[ncol(data)] <- paste(lhsV, "_", imp_suffix, sep = "")
            }
          }
          TFna1 <- is.na(lhs_vector)
          TFna3 <- TFna1 & TFna2
          if (all(!TFna3)) {
            #Check if there are missings in this LHS variable where theR RHS variables are "not missing"
            cat(paste0("No missings in ", lhsV, " with valid values in the predictor variables.\n"))
          } else {
            tmp <- data[TFna3, ]
            tmp[, lhsV] <- 1
            if (nLev > 2) {
              if (mod_cat) {
                pre <- predict(mod, newdata = tmp)
              }else{
                pre <- predict(mod, newdata = tmp, type = "probs")
                pre <- levels(data[, lhsV])[apply(pre, 1, function(x) sample(1:length(x), 1, prob = x))]
              }
            } else if (nLev == 2) {
              if (mod_cat) {
                pre <- predict(mod, newdata = tmp, type = "response")
                pre <- levels(lhs_vector)[as.numeric(pre>.5)+1]
              } else {
                pre <- predict(mod, newdata = tmp, type = "response")
                pre <- levels(lhs_vector)[sapply(pre, function(x) sample(1:2, 1, prob = c(1 - x, x)))]
              }
            } else {
              pre <- predict(mod, newdata = tmp)
            }
            if (sum(TFna1) > sum(TFna3))
              cat(paste("There still missing values in variable", lhsV, ". Probably due to missing values in the regressors.\n"))
            data[TFna3, lhsV] <- pre
          }
        }
      }else{
        TFna1 <- is.na(data[, lhsV])
        TFna3 <- TFna1 & TFna2
        tmp <- data[TFna3, ]
        tmp[, lhsV] <- 1
        if(robust)
          mod <- glmrob(form, data = data, family = family)
        else
          mod <- glm(form, data = data, family = family)
        pre <- predict(mod, newdata = tmp, type = "response")
        data[TFna3, lhsV] <- pre
      }
    }
  }
  invisible(data)
}
