# VIM

[![Build Status](https://travis-ci.org/statistikat/VIM.svg?branch=master)](https://travis-ci.org/github/statistikat/VIM)
[![Coverage Status](https://coveralls.io/repos/github/statistikat/VIM/badge.svg?branch=master)](https://coveralls.io/github/statistikat/VIM?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/VIM)](https://CRAN.R-project.org/package=VIM)
[![Downloads](http://cranlogs.r-pkg.org/badges/VIM)](https://CRAN.R-project.org/package=VIM)
[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)

This package introduces new tools for the visualization of missing and/or imputed values, which can be used for exploring the data and the structure of the missing and/or imputed values. Depending on this structure of the missing values, the corresponding methods may help to identify the mechanism generating the missings and allows to explore the data including missing values. In addition, the quality of imputation can be visually explored using various univariate, bivariate, multiple and multivariate plot methods.

### Installation

VIM can be installed from CRAN or from GitHub

```r
## install release version from CRAN
install.packages("VIM")

## install development version from GitHub
remotes::install_github("statistikat/VIM")
```

### Further reading

* A hands-on guide for VIM can be found in the [get started vignette](http://statistikat.github.io/VIM/articles/VIM.html).
* The [reference documentation](http://statistikat.github.io/VIM/reference)
  provides detaled documentation about all exposed functons from the package.
  
### Related Publications

* [Exploring incomplete data using visualization techniques](https://www.researchgate.net/publication/226283718_Exploring_incomplete_data_using_visualization_techniques)
* [Iterative stepwise regression imputation using standard
and robust methods](http://file.statistik.tuwien.ac.at/filz/papers/CSDA11TKF.pdf)
* [Imputation with the R Package VIM](https://www.researchgate.net/publication/309336197_Imputation_with_the_R_package_VIM)
