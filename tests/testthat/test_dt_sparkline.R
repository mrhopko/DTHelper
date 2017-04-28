library(testthat)
library(DTHelper)
library(data.table)
library(sparkline)
library(DT)

x <- data.table(col1 = c(1,2,3), col2 = c(2,3,4))

js1 <- appendfnDrawCallback(js = NULL, tplSparkline("test1","test1"))
js2 <- appendfnDrawCallback(js1, tplSparkline("test2","test2"))
js2

col <- colSparkline("col2", colnames(x), rownames(x))
col

arrow_template <- tplArrow(0, 1, arrowHTML = "&#8680;")

appendf <- appendFormatter(NULL, columns = "col1", names = names(x), rownames = FALSE, template = tplArrow, 0)
