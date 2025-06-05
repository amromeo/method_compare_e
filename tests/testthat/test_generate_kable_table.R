library(testthat)
library(dplyr)
library(kableExtra)
library(knitr)

source("../../www/funs.R")

test_that("generateKableTable handles empty data frame", {
  out <- generateKableTable(data.frame(), format = "html")
  output_str <- paste(out, collapse = "")
  expect_true(grepl("No data available", output_str))
})

test_that("generateKableTable formats Pass/Fail correctly", {
  df <- data.frame(Sample = c("A", "B"),
                   X = c(1.0, 1.0),
                   Y = c(1.0, 2.0),
                   PassFail = c("PASS", "FAIL"))
  out <- generateKableTable(df, format = "html")
  output_str <- paste(out, collapse = "")
  expect_true(grepl("rgba\\(40, 167, 69", output_str))
  expect_true(grepl("rgba\\(220, 53, 69", output_str))
})
