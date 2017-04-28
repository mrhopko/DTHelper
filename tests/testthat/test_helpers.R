library(sparkline)
library(DT)
library(testthat)
library(data.table)

source("../../R/helpers.R")

context("Test Helpers")

x <- data.frame(col1 = c(1,2,3), col2 = c("a","b","c"), col3 = c(2,3,4))

x2 <- data.table(year = c(2000:2005), col1 = c(0:5), agroup = c(rep("a",3),rep("b",3)))

test_that("Helpers work", {

  targets_int <- c(2,3)
  expect_equal(targets_helper(x, targets_int), c(1,2))

  targets_chr <- c("col1","col2")
  expect_equal(targets_helper(x, targets_chr), c(0, 1))

  targets_chr <- c("col2")
  expect_equal(targets_helper(x, targets_chr), c(1))

  targets_error <- c("errorcol")
  expect_error(targets_helper(x, targets_error))

  expect_equal("[1,2,3]", vector_to_js_array(c(1,2,3)))

  l <- list(n1 = "v1", n2 = "v2")
  expect_equal(list_to_name_value_chr(l), "n1:v1,n2:v2")

  x2[, test_lags := list(dt_list_of_lags(col1, c(0:1))), by = agroup]

  expect_equal(x2[6, test_lags], list(c(5,4)))

  x2[, test_trend := least_squares_over_lags(col1, lags_x = c(0,2)), by = agroup]

  expect_equal(wrap_chr("a", "'"), "'a'")

  expect_equal(logical_to_js(TRUE), "true")
  expect_equal(logical_to_js(FALSE), "false")
  expect_equal(logical_to_js("TRUE"), "true")
  expect_equal(logical_to_js("FALSE"), "false")
  expect_equal(logical_to_js("test"), "test")


  expect_equal(to_js(TRUE), "true")
  expect_equal(to_js("test"), "'test'")
  expect_equal(to_js(NULL), NULL)
})

