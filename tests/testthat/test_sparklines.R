library(sparkline)
library(DT)
library(testthat)

source("../../R/sparkline_helpers.R")

context("Sparklines")

x <- data.frame(col1 = c(1,2,3), col2 = c("a","b","c"), col3 = c(2,3,4))

test_that("colDef works", {

  test_col_def <- sparkline_colDef(x = x, targets = "col2", class_suffix = "test", width = "10%")

  expect_equal(test_col_def,
               list(
                 targets = 1,
                 visible = TRUE,
                 width = "10%",
                 render = htmlwidgets::JS("function(data, type, full){ ",
                                          paste0("return '<span class=", "sparktest", ">' + data + '</span>' }"))
               ))

})

test_that("spark options and callback", {

  test_options_bar <- spark_options(type = "bar", lineColor = "blue", spark_options_chart_type = spark_options_bar(barColor = "green"))
  test_options_line <- spark_options(type = "line", lineColor = "blue", spark_options_chart_type = spark_options_line(defaultPixelsPerValue = 1))

  expect_equal(test_options, "type:bar,lineColor:blue,barColor:green")

  test_callback <- sparkline_fnDrawCallback(c("sparkbar", "sparkline"), c(test_options_bar, test_options_line))

  test_arrow <- arrow_row_callback(x, targets = c("col1"), gradient_cols = c("col3"), arrow_html = "&#8680;", title_column = "col2")
  test_arrow2 <- arrow_row_callback(x, targets = c("col1","col1"), gradient_cols = c("col3","col3"), arrow_html = "&#8680;")

  test_row <- row_callback(x, targets = c("col1"), value_cols = c("col3"), title_column = "col2")
  test_callback <- dt_rowCallback(c(test_row,test_arrow2))

})
