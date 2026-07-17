library(VIM)

## Wave 2: type-stable returns (audit P2.30; Matthias's call 2026-07-07).
## vimpute() previously always returned a data.table (even for data.frame
## input), and tune = TRUE / pred_history = TRUE silently switched the return
## type to a bare list(data=, tuning_log=/pred_history=) -- breaking downstream
## code that expects data. New contract: the return is ALWAYS the imputed data,
## classed like the input (data.frame in -> data.frame out, data.table in ->
## data.table out), with diagnostics attached as attributes "tuning_log" and
## "pred_history".

data(sleep, package = "VIM")
d_df <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]   # data.frame input
d_dt <- data.table::as.data.table(d_df)                    # data.table input

## --- input class is preserved -------------------------------------------------
out_df <- vimpute(d_df, method = "ranger", sequential = FALSE, seed = 1,
                  imp_var = FALSE, verbose = FALSE)
expect_false(data.table::is.data.table(out_df),
             info = "data.frame input must not come back as a data.table")
expect_true(is.data.frame(out_df))
expect_equal(sum(is.na(out_df)), 0L)

out_dt <- vimpute(d_dt, method = "ranger", sequential = FALSE, seed = 1,
                  imp_var = FALSE, verbose = FALSE)
expect_true(data.table::is.data.table(out_dt))

## same values regardless of input class (same seed)
expect_equal(as.data.frame(out_dt), out_df)

## --- tune = TRUE returns data with the report as an attribute -----------------
res_tuned <- vimpute(d_df, method = "ranger", tune = TRUE, sequential = FALSE,
                     seed = 1, imp_var = FALSE, verbose = FALSE)
expect_true(is.data.frame(res_tuned),
            info = "tune = TRUE must not switch the return type to a bare list")
expect_false(data.table::is.data.table(res_tuned))
expect_equal(sum(is.na(res_tuned$Sleep)), 0L)
tl <- attr(res_tuned, "tuning_log")
expect_true(is.list(tl) && length(tl) > 0,
            info = "tuning_log attribute missing from tuned result")
expect_true(any(vapply(tl, function(e) isTRUE(e$tuned), logical(1))))

## --- pred_history = TRUE returns data with the history as an attribute --------
res_ph <- vimpute(d_df, method = "ranger", pred_history = TRUE,
                  sequential = FALSE, seed = 1, imp_var = FALSE, verbose = FALSE)
expect_true(is.data.frame(res_ph) && !data.table::is.data.table(res_ph))
ph <- attr(res_ph, "pred_history")
expect_true(!is.null(ph) && is.data.frame(ph),
            info = "pred_history attribute missing")
expect_true(all(c("variable", "index", "predicted_values") %in% names(ph)))

## --- m > 1 still returns a vimmi (unchanged) ----------------------------------
res_mi <- vimpute(d_df, method = "ranger", sequential = FALSE, m = 2,
                  boot = TRUE, uncert = "normalerror", seed = 2,
                  imp_var = FALSE, verbose = FALSE)
expect_inherits(res_mi, "vimmi")
