## `diabetes` is a synthetic replacement (synvey, M. Templ) for the Pima Indians
## Diabetes data. It keeps VIM's historical schema so that user code runs
## unchanged, but follows mlbench::PimaIndiansDiabetes2's NA coding: only
## physically impossible values are NA, so 0 pregnancies is a value, not NA.
utils::data("diabetes", package = "VIM")

expect_equal(dim(diabetes), c(768L, 9L))
expect_equal(
  names(diabetes),
  c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin",
    "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")
)
expect_true(is.factor(diabetes$Outcome))
expect_equal(levels(diabetes$Outcome), c("no", "yes"))
expect_true(all(vapply(diabetes[c("Pregnancies", "Glucose", "BloodPressure",
                                  "SkinThickness", "Insulin", "Age")],
                       is.integer, logical(1))))
expect_true(all(vapply(diabetes[c("BMI", "DiabetesPedigreeFunction")],
                       is.double, logical(1))))

## 0 pregnancies is a valid value (the pre-7.3.0 copy had recoded it to NA)
expect_equal(sum(is.na(diabetes$Pregnancies)), 0L)
expect_true(any(diabetes$Pregnancies == 0L))
## no missing values in the fully observed variables
expect_equal(sum(is.na(diabetes$Age)), 0L)
expect_equal(sum(is.na(diabetes$Outcome)), 0L)
## the structured missingness that makes the data useful for imputation demos
expect_true(sum(is.na(diabetes$Insulin)) > sum(is.na(diabetes$SkinThickness)))
expect_true(sum(is.na(diabetes$SkinThickness)) > sum(is.na(diabetes$BloodPressure)))
