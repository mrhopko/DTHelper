## ------------------------------------------------------------------------
library(data.table)
library(DT)
library(DTHelper)
library(sparkline)

# create a data set using data.table
x <- data.table(week_no = c(rep(c(1:10),3)), 
                col1 = c(1:30), 
                col2 = c(21:50),
                col3 = round(runif(30, min = 0, max = 30),0),
                grp = c(rep("a",10), rep("b",10), rep("c",10)))

head(x)

## ------------------------------------------------------------------------
# Order the data by week
x <- x[order(week_no, decreasing = FALSE)]

# Use the DTHelper lag function
# Note the additional list() when using a by reference call
x[, col1_lag := list(dt_list_of_lags(col1, lags_x = c(0:5))), by = grp]
x[, col1_lagb := list(dt_list_of_lags(col1, lags_x = c(0:3))), by = grp]
x[, col_stack_lag := list(dt_list_of_lags_2(col1, col2, lags_x = c(0:4))), by = grp]

head(x)

## ------------------------------------------------------------------------

# Create the options
bar_options <- 
  spark_options(type = "bar",
               chartRangeMin = 0,
               spark_options_chart_type = 
                spark_options_bar(barColor = '#7CEBB2',
                                  negBarColor = '#FF9C9C',
                                  barWidth = 200,
                                  width = 50))

stack_bar_options <- 
  spark_options(type = "bar",
               chartRangeMin = 0,
               spark_options_chart_type = 
                spark_options_bar(barColor = '#7CEBB2',
                                  negBarColor = '#FF9C9C',
                                  stackedBarColor = c("#FFC9C9", "#7CEBB2"),
                                  barWidth = 200,
                                  width = 50))


line_options <- 
  spark_options(type = "line",
                disableHiddenCheck = "false",
                chartRangeMin = 0)

d1 <- datatable(x, 
                rownames = FALSE,
                options = list(dom = "t", paging = FALSE)) %>% 
  formatSparkline(columns = "col1_lag", sparklineOpts = line_options) %>% 
  formatSparkline(columns = "col1_lagb", sparklineOpts = bar_options) %>% 
  formatSparkline(columns = "col_stack_lag", sparklineOpts = stack_bar_options)

d1

## ------------------------------------------------------------------------

# create a data set using data.table
x <- data.table(week_no = c(rep(c(1:10),3)), 
                col1 = c(1:30), 
                col2 = c(21:50),
                col3 = round(runif(30, min = 0, max = 30),0),
                grp = c(rep("a",10), rep("b",10), rep("c",10)))

# create a trend column based on lagged values
x <- x[order(week_no, decreasing = FALSE)]
x[, col3_trend := least_squares_over_lags(col3, c(0:4)), by = grp]
x[, col3_trend_arrow := "duntmatter"]
x[, col3_tooltip := ifelse(col3_trend < 0, "bad", "good")]

d2 <- DT::datatable(x,
                    rownames = FALSE,
                    options = list(dom = "t",
                                   paging = FALSE)) %>% 
  formatArrow(columns = "col3_trend_arrow", valueColumns = "col3_trend", titleColumn = "col3_tooltip") %>% 
  formatTooltip(columns = "col1", titleColumn = "col3_tooltip")

d2


## ------------------------------------------------------------------------
# create a data set using data.table
x <- data.table(week_no = c(rep(c(1:10),2)), 
                col1 = c(1:20), 
                col2 = c(21:40),
                col3 = round(runif(20, min = 0, max = 20),0),
                grp = c(rep("a",10), rep("b",10)))

# Create a datatable with some conditional formatting
d3 <- 
  DT::datatable(x,
                rownames = FALSE,
                options = list(
                  paging = FALSE)
                ) %>% 
  DT::formatStyle(c("week_no"), 
                  valueColumns = "col1",
                  background = style_color_bar_stack(x$col1, "#FFC9C9", "#7CEBB2", min_range = min(x$col1), max_range = max(x$col1)),
                  backgroundSize = '98% 78%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  
d3


