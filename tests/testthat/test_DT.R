library(data.table)
library(DT)
library(DTHelper)
library(sparkline)

x <- data.table(week_no = c(rep(c(1:10),3)), col1 = c(1:30), grp = c(rep("a",10), rep("b",10), rep("c",10)))

x[, lag_col := list(DTHelper::dt_list_of_lags(col1, c(0:4))), by = grp]
x[, lag_col2 := list(DTHelper::dt_list_of_lags(col1, c(0:3))), by = grp]
x[, trend_col := seq(from = -5, to = 5, length.out = 30)]

colDefs <- list(
  DTHelper::spark_colDef(x, "lag_col", "line"),
  DTHelper::spark_colDef(x, "lag_col2", "bar")
)

bar_options <- DTHelper::spark_options(type = "bar",
                                       chartRangeMin = 0,
                                       spark_options_chart_type = spark_options_bar(barColor = '#7CEBB2',
                                                                                    negBarColor = '#FF9C9C',
                                                                                    barWidth = 200,
                                                                                    width = 50))

line_options <- spark_options(type = "line",
                                        chartRangeMin = 0,
                                        lineColor = '#7CEBB2')

fnDrawCallback <- DTHelper::spark_fnDrawCallback(c("sparkbar", "sparkline"), c(bar_options, line_options))

rowCallback <- DTHelper::dt_rowCallback(arrow_row_callback(x, "trend_col", "trend_col"))



d1 <- DT::datatable(x,
                    rownames = FALSE,
                    options = list(dom = "t",
                                   paging = FALSE,
                                   columnDefs = colDefs,
                                   rowCallback = rowCallback,
                                   fnDrawCallback = fnDrawCallback))

d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency("sparkline"))
d1

'<span><p style=\"transform:  rotate(' + Math.atan(data[4]) * (-1) + 'rad)\">&#8680;</p></span>'
'<span>' + '<p style=\"transform: rotate(' + Math.atan(data[0]) * (-1) + 'rad)\">&#8680;</p>' + '</span>'
