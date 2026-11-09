reference <- new.env(parent = asNamespace("VIM"))
sys.source(file.path("fixtures", "hotdeck-reference.R"), envir = reference)

run_hotdeck <- function(fun, input, args, seed) {
  input <- data.table::copy(input)
  set.seed(seed)
  warnings <- character()
  error <- NULL
  result <- tryCatch(
    withCallingHandlers(
      do.call(fun, c(list(data = input), args)),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error <<- list(class = class(e), message = conditionMessage(e))
      NULL
    }
  )
  list(result = result, rng = .Random.seed, warnings = warnings,
       error = error, input = input)
}

compare_hotdeck <- function(input, args, label, seed = 104L) {
  expect_identical(run_hotdeck(hotdeck, input, args, seed),
                   run_hotdeck(reference$hotdeck, input, args, seed),
                   info = label)
}

mixed <- data.frame(
  group = rep(c("A", "B", "C"), each = 5L),
  order = rep(c(1L, 1L, 2L, 3L, 4L), 3L),
  number = rep(c(NA, NA, 8, 11, 99), 3L),
  count = rep(c(NA_integer_, 2L, 99L, 4L, 8L), 3L),
  text = rep(c(NA, NA, "x", "y", "sentinel"), 3L),
  category = ordered(rep(c(NA, "low", NA, "high", "low"), 3L)),
  date = as.Date("2020-01-01") + rep(c(NA, NA, 0, 1, 2), 3L)
)
mixed$number[mixed$group == "C"] <- NA
mixed <- mixed[c(seq(1, 15, by = 2), seq(2, 14, by = 2)), ]
mixed_args <- list(
  variable = c("number", "count", "text", "category", "date"),
  domain_var = "group", makeNA = list(99, 99L, "sentinel", NULL, NULL),
  donorcond = list(c("> 5", "< 20"), NULL, "!= 'sentinel'", NULL, NULL)
)
compare_hotdeck(mixed, c(mixed_args, list(ord_var = "order", imp_suffix = "filled")),
                "tied ordering, domains, mixed types and donor restrictions")
compare_hotdeck(mixed, c(mixed_args, list(imp_var = FALSE)),
                "random ordering and no indicator columns", seed = 202L)

keyed <- data.table::as.data.table(mixed)
data.table::setkeyv(keyed, c("group", "order"))
keep_na <- list(variable = c("number", "count"), domain_var = "group",
                ord_var = "order", makeNA = list(99, 99L), impNA = FALSE)
compare_hotdeck(keyed, keep_na, "keyed input and preservation of existing NAs")
compare_hotdeck(mixed, list(variable = "count", ord_var = "order",
                           donorcond = list("> 100")), "no eligible donors")

for (gap in 49:51) {
  values <- c(1L, rep(NA_integer_, gap), 2L, 3L, NA_integer_, 4L)
  input <- data.frame(order = seq_along(values), value = values)
  # At 51 the legacy implementation warns and draws even if it found a donor.
  if (gap < 51L)
    compare_hotdeck(input, list(variable = "value", ord_var = "order"),
                    paste("explicit ordering with a missing run of", gap))
  compare_hotdeck(input, list(variable = "value"),
                  paste("random ordering with a missing run of", gap))
}
