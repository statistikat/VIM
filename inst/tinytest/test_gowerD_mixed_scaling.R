library(VIM)

## Semi-continuous (mixed) distance variables must be range-scaled like numeric
## ones (Wave 1 tail, audit P1.40). gowerD() scaled only `numerical` columns, so
## a mixed variable whose scale exceeds ~1 dominated the Gower distance -- two
## non-spike values 50 apart contributed 50 while every other variable type
## contributes at most 1 -- inverting kNN's neighbour ordering. The C++ kernel
## is unchanged; the scaling happens in the R wrapper (mixed.constant scaled
## along, spike excluded from the range estimate).

## --- gowerD: a single mixed variable contributes at most its nominal max ----
d <- gowerD(data.frame(v = c(0, 100, 50)), numerical = vector(), mixed = "v")
expect_true(max(d) <= 1 + 1e-8,
            info = "unscaled mixed variable dominates the Gower distance")

## --- kNN: the nearer non-spike donor must win, not the spike donor ----------
## inc has a point mass at 0; recipient inc = 100 should draw y from inc = 90
## (nearest non-spike donor), not from the inc = 0 spike donor.
df <- data.frame(inc = c(0, 50, 90, 100), y = c(10, 20, 30, NA))
out <- kNN(df, variable = "y", dist_var = "inc", mixed = "inc", k = 1, imp_var = FALSE)
expect_equal(out$y[4], 30)
## treating the same column as numeric already gives the nearest donor; the
## mixed path must now agree
out_num <- kNN(df, variable = "y", dist_var = "inc", k = 1, imp_var = FALSE)
expect_equal(out$y[4], out_num$y[4])
