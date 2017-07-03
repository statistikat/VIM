# irmi should work on data_frame
# 
# Author: Alexander Kowarik (Issue 11 Deleetdk)
###############################################################################
library(VIM)
test_df = data.frame(ord = ordered(sample(c(letters[1:2], NA), size = 1000, replace = T)), v1 = rnorm(1000), v2 = rnorm(1000))
irmi(test_df)