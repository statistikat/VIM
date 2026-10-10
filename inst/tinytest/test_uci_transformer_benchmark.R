# Pure-R protocol checks: no network, Python, training or optional learners.
e <- new.env(parent = baseenv())
# The script intentionally uses stats/utils explicitly except standard base calls.
sys.source(system.file('benchmarks', 'uci-transformer.R', package = 'VIM'), envir = e)
set.seed(81)
d <- data.frame(age = seq_len(100), value = rnorm(100),
                group = factor(rep(c('no', 'yes'), 50)))
a <- e$uci_mask(d, c('value', 'group'), .2, 'MAR', 12)
b <- e$uci_mask(d, c('value', 'group'), .2, 'MAR', 12)
expect_identical(a, b)
expect_false(any(a$mask[, 'age']))
expect_equal(mean(a$probability), .2, tolerance = 1e-5)
expect_true(all(diff(a$probability) > 0))
d2 <- d; d2$value <- rev(d2$value); d2$group <- rev(d2$group)
expect_identical(a, e$uci_mask(d2, c('value', 'group'), .2, 'MAR', 12))
expect_equal(e$uci_mask(d, c('value', 'group'), .4, 'MCAR', 12)$probability,
             rep(.4, 100))
mask <- a$mask
perfect <- e$uci_score(d, d, mask, c('value', 'group'))
expect_equal(perfect$rmse[1], 0)
expect_equal(perfect$accuracy[2], 1)
expect_equal(perfect$marginal_distance, c(0, 0))
completed <- d
completed$value[mask[, 'value']] <- completed$value[mask[, 'value']] + 2
completed$group[mask[, 'group']] <- ifelse(d$group[mask[, 'group']] == 'yes', 'no', 'yes')
s <- e$uci_score(d, completed, mask, c('value', 'group'))
expect_equal(s$rmse[1], 2)
expect_equal(s$mae[1], 2)
expect_equal(s$nrmse[1], 2 / sd(d$value))
expect_equal(s$accuracy[2], 0)
expect_equal(s$misclassification[2], 1)
expect_equal(s$mean_difference[1], 2 * mean(mask[, 'value']))
completed$age[1] <- -1
expect_error(e$uci_score(d, completed, mask, c('value', 'group')), 'Changed observed')
completed <- d; completed$value[which(mask[, 'value'])[1]] <- NA
expect_error(e$uci_score(d, completed, mask, c('value', 'group')), 'Invalid completion')
expect_error(e$uci_prepare(tempfile(), download = FALSE), 'Missing archive')
