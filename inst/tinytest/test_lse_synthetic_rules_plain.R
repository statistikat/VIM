## lse_synthetic_rules must be a plain, namespace-free object.
## R-devel's R CMD check warns when a data file carries namespace references to
## packages outside the recursive strong dependencies; `validate` is
## Suggests-only, so the rule sets are shipped as data.frames of rule text and
## turned into validator objects on demand with validate::validator(.data = ).
utils::data("lse_synthetic_rules", package = "VIM")

## no serialized reference to the validate namespace (what the CRAN check sees)
expect_equal(
  length(grepRaw("validate", serialize(lse_synthetic_rules, NULL), fixed = TRUE)),
  0L
)

expect_true(is.list(lse_synthetic_rules))
expect_equal(names(lse_synthetic_rules), c("accounting", "extra", "edit"))
expect_true(all(vapply(lse_synthetic_rules, is.data.frame, logical(1))))
for (rules in lse_synthetic_rules) {
  expect_true(is.data.frame(rules) && all(c("name", "rule") %in% names(rules)))
  expect_true(is.data.frame(rules) && is.character(rules$name) && is.character(rules$rule))
}
expect_equal(
  vapply(lse_synthetic_rules, function(r) if (is.data.frame(r)) nrow(r) else NA_integer_, integer(1)),
  c(accounting = 36L, extra = 56L, edit = 92L)
)

## the plain form rebuilds into validate::validator objects of the same size
if (requireNamespace("validate", quietly = TRUE) && is.data.frame(lse_synthetic_rules$edit)) {
  v <- validate::validator(.data = lse_synthetic_rules$edit)
  expect_equal(length(v), 92L)
  expect_equal(names(v), lse_synthetic_rules$edit$name)
}
