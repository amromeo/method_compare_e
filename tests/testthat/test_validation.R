library(testthat)

validation_path <- system.file("app/modules/domain/validation.R", package = "methodCompare")
error_path <- system.file("app/modules/infra/error_handling.R", package = "methodCompare")
source(validation_path)
source(error_path)

test_that("load_test_limits falls back when file missing", {
  tmp <- tempfile()
  limits <- load_test_limits(tmp)
  expect_true(is.numeric(limits))
  expect_gt(length(limits), 0)
})

test_that("validate_medical_data catches non-numeric", {
  df <- data.frame(X = c("a", "b"), Y = c(1, 2))
  result <- validate_medical_data(df, "demo")
  expect_false(result$valid)
  expect_true(any(grepl("numeric", result$errors)))
})
