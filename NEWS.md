# VIM 7.0.0
 - new function vimpute that uses mlr3 backend for a flexible imputation method.
 
# VIM 6.x.x
 - fix infinite loop in matchImpute in case all observations of a variable are missing
 - remove parameter metric from kNN because it was not used
 - add function xgboostImpute for using a simple xgboostModel to impute

# VIM 6.2.3
- default robust regression method for irmi for numeric variables changes from rlm to lmrob.

# VIM 6.1.1
- ordFun as parameter of kNN to control the function applied to ordinal variables
- methodStand option in gowerD and kNN to switch between range and interquartile range for the standardization of numerical variables
- donorcond in kNN and hotdeck extended so it also accepts NULL as list element and multiple conditions as character vector

# VIM 6.0.2
- error message in `hotdeck()` when ord_var and variable overlap
- family argument of class 'family' now work in regressionImp

# VIM 6.0.1

- add new vignettes explaining all remaining imputation methods (`irmi()`,
  `kNN()`, `hotdeck()` and `regressionImp()`). Thanks @wolfgangrannetbauer
  (#44, #45)
- Allow missing and imputed values in several visualization functions.
    - The new functionalities are showcased in the new [visualization vignette](http://statistikat.github.io/VIM/articles/VisualImp.html) (#46). Thanks
  @wolfgangrannetbauer!
- Add `tableMiss()`: A table that highlights missing and imputed values via
  colors (#47).
- Bug fix for kNN (#48) Thanks @torockel

# VIM 6.0.0

- extend documentation with new vignettes and pkgdown
- add rangerImpute() to impute values with `ranger::ranger()` (#35)
- remove support for survey objects (#36)
- remove exports for VIMGUI (#40)
- change data.table dependency from depends to imports (#41)
- bugfixes for `irmi()` with logical and integer columns (#42)

# VIM 5.1.1

* updates for `gowerD()`
* separate help pages for `maxCat()` and `sampleCat()`
* remove links to certain packages

# VIM 5.0.0

## New datasets

* bcancer
* brittleness
* colic
* diabetes
* food
* pulplignin
* toydataMiss
* wine

# VIM 4.9.1

* data set `collision` added

# VIM 4.8.0

* fixed a bug in the distance computation when different variable types are used

# VIM 4.7.18

* added `imp_var` and `imp_suffix` to `irmi()`, so it is more consistent with the other functions (#27)

# VIM 4.7.17

* added parameter addRF, only RF to `kNN()`, to use random forest in combination with `kNN()`

# VIM 4.7.12

* testthat package used for all tests
* added travis automatically building/checking
* covr for code coverage (vis functions are currently not covered by any tests)

# VIM 4.7.1
* new imputation function `matchImpute()` for imputing randomly within groups

# VIM 4.7.0

* remove handling of `impNA` in `hotdeck()`
* add regression tests
* bugfix for `irmi()` with factors (#13). Thanks @Deleetdk
* bugfix with colorspace package (#4)

# VIM 4.6.1

* use ordered logistic regression for ordinal variables in `irmi()` (#23)
* add support for ordered factors (#7)
* bugfixes (#8, #9)

# VIM 4.6.0

* bugfixes for `kNN()` and `data.table`
* bugfix for labelled vars in `hotdeck()`
* add JSS citation
* bugfix, if a `data.frame` is passed to `irmi()` (#6)
  
# VIM 4.5.0

* new option for `kNN()`: `weightDist` to use the distances for the k nearest neighbours as weights
* The R function `which.minN()` is not used anymore, instead there is a C++ function, `kNN()` is now about 1.6 times faster on a replication (100x) of the sleep dataset
* Bytecompile is enabled

# VIM 4.4.0

* bugfix  wrong observations marked as imputed in `hotdeck()`
* random sorting is now used in `hotdeck()` if no `ord_var` is defined

# VIM 4.3.0

* bugfix for `hotdeck()` with `makeNA`

# VIM 4.2.3

* bugfix for the computation of distances for ordered variables

# VIM 4.2.1

* new option for `kNN()` `useImputedDist` if the imputed values of a variable should be used in subsequent imputation of another variable.

# VIM 4.2.0

* bug fixed in `irmi()` with newer version of nnet (multinom) and if residual scale can not be computed (noise)
* Improvement Gower dist with only missing values in data.x or data.y

# VIM 4.1.0

* new parameter `modelFormula` in `irmi()`
* bug fixes in `irmi()`
* updated `hotdeck()` based on data.table -> faster and quite stable
* bug fix if range of a variable is 0 in gower.dist
* small fixed `kNN()`

# VIM 4.0.1

* small bugfix for using `makeNA` in `kNN()`
* "Nothing to impute"-Error is now a warning
* `imp_var` now updates existing TF `imp_vars` (with warning)
 
# VIM 4.0.0

* new pacakge VIMGUI contains all GUI functions
* vignettes moved to VIMGUI
* new imputation function `regressionImp()`
* roxygen style comments -> help files
