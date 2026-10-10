# VIM <a href="https://statistikat.github.io/VIM/"><img src="man/figures/VIM-logo.png" align="right" style="height:138px;background:white" /></a>


[![R-CMD-check](https://github.com/statistikat/VIM/workflows/R-CMD-check/badge.svg)](https://github.com/statistikat/VIM/actions)
[![Codecov test coverage](https://codecov.io/gh/statistikat/VIM/branch/master/graph/badge.svg)](https://app.codecov.io/gh/statistikat/VIM?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/VIM)](https://CRAN.R-project.org/package=VIM)
[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)

This package introduces new tools for the visualization of missing and/or imputed values, which can be used for exploring the data and the structure of the missing and/or imputed values. Depending on this structure of the missing values, the corresponding methods may help to identify the mechanism generating the missings and allow exploration of the data including missing values. In addition, the quality of imputation can be visually explored using various univariate, bivariate, multiple and multivariate plot methods.

### Installation

```r
## Install release version from CRAN
install.packages("VIM")

## Install development version from GitHub
remotes::install_github("statistikat/VIM")
```

### Usage

The core functionality of VIM can be categorized into two groups

1. Imputation functions: `vimpute()` provides unified single and multiple
   imputation over exchangeable machine-learning and robust methods (random
   forest, XGBoost, regularized, robust regression, GAM/robust GAM — and any
   registered mlr3 learner via `register_vimpute_method()`), with per-variable
   specs, hyperparameter tuning, mice-compatible pooling (`vimmi`,
   `vim_as_mids()`), and diagnostics (`plot()` chains/density, `overimpute()`,
   per-variable model quality). Classic workhorses such as `kNN()`,
   `hotdeck()` and `irmi()` as well as the cellwise-robust family
   (`imputeCellwise()` and friends) complement it, and `makeMissing()`
   generates MCAR/MAR/MNAR missingness for simulation studies.
2. Visualization functions such as `histMiss()` or `barMiss()` provide extensions
   of common base graphics which use a special way of highlighting missing
   and imputed values. For more details, see the [visualization vignette](https://statistikat.github.io/VIM/articles/VisualImp.html).

Additionally, datasets are included to showcase the functions mentioned above.
Other functions provide tabular aggregations of missings and visualization
of spatial data.

### Further reading

* A hands-on guide for VIM can be found in the [get started vignette](https://statistikat.github.io/VIM/articles/VIM.html). Additional
  vignettes can be found in the [articles section](https://statistikat.github.io/VIM/articles/).
* The [reference documentation](https://statistikat.github.io/VIM/reference)
  provides detailed documentation about all exposed functions from the package.

### Publications

* Journal of Statistical Software, 2016: [Imputation with the R Package VIM](https://doi.org/10.18637/jss.v074.i07)
* Advances in Data Analysis and Classification, 2012: [Exploring incomplete data using visualization techniques](https://doi.org/10.1007/s11634-011-0102-y)
* Computational Statistics & Data Analysis, 2011: [Iterative stepwise regression imputation using standard
and robust methods](https://doi.org/10.1016/j.csda.2011.04.012)
