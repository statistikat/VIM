# VIM

[![R-CMD-check](https://github.com/statistikat/VIM/workflows/R-CMD-check/badge.svg)](https://github.com/statistikat/VIM/actions)
[![Codecov test coverage](https://codecov.io/gh/statistikat/VIM/branch/master/graph/badge.svg)](https://app.codecov.io/gh/statistikat/VIM?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/VIM)](https://CRAN.R-project.org/package=VIM)
[![Downloads](http://cranlogs.r-pkg.org/badges/VIM)](https://CRAN.R-project.org/package=VIM)
[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)

This package introduces new tools for the visualization of missing and/or imputed values, which can be used for exploring the data and the structure of the missing and/or imputed values. Depending on this structure of the missing values, the corresponding methods may help to identify the mechanism generating the missings and allows to explore the data including missing values. In addition, the quality of imputation can be visually explored using various univariate, bivariate, multiple and multivariate plot methods.

### Installation

```r
## Install release version from CRAN
install.packages("VIM")

## Install development version from GitHub
remotes::install_github("statistikat/VIM")
```

### Usage

The core functionality of VIM can be categorized into two groups

1. Imputation functions such as `kNN()` and `hotdeck()` implement techniques
   to replace missing values with imputed values.
2. Visualization functions such as `histMiss()` or `barMiss()` provide extensions
   of common base graphics which use a special way of highlighting missing
   and imputed values. For more details, see the [visualization vignette](http://statistikat.github.io/VIM/articles/VisualImp.html).

Additionaly, datasets are included to showcase the functions mentioned above.
Other functions provide tabular aggregations of missings and visualization
of spatial data.

### Further reading

* A hands-on guide for VIM can be found in the [get started vignette](http://statistikat.github.io/VIM/articles/VIM.html). Additional
  vignettes can be found in the [articles section](http://statistikat.github.io/VIM/articles/).
* The [reference documentation](http://statistikat.github.io/VIM/reference)
  provides detailed documentation about all exposed functons from the package.
  
### Publications

* Journal of Statistical Software, 2016: [Imputation with the R Package VIM](https://www.researchgate.net/publication/309336197_Imputation_with_the_R_package_VIM)
* Advances in Data Analysis and Classification, 2012: [Exploring incomplete data using visualization techniques](https://www.researchgate.net/publication/226283718_Exploring_incomplete_data_using_visualization_techniques)
* Computational Statistics & Data Analysis, 2011: [Iterative stepwise regression imputation using standard
and robust methods](http://file.statistik.tuwien.ac.at/filz/papers/CSDA11TKF.pdf)
