#' Add trend arrows to a DT datatable
#'
#' @param table the DT datatable
#' @param columns column names or indicies to be replaced with arrows
#' @param valueColumns values to use to rotate the arrows (defaults to columns)
#' @param arrowHTML arrow symbol to use (defaults to &#8680;)
#' @param titleColumn column to use as a tooltip (defaults to NULL)
#' @return table updated with arrow formatting
#' @export
formatArrow <- function(table, columns, valueColumns = columns, arrowHTML = "&#8680;", titleColumn = NULL) {

  if (inherits(columns, 'formula')) columns <- all.vars(columns)
  x <- table$x
  colnames <- base::attr(x, 'colnames', exact = TRUE)
  rownames <- base::attr(x, 'rownames', exact = TRUE)

  valueColumns <- name2int(valueColumns, colnames, rownames)

  if (!is.null(titleColumn)) titleColumn <- name2int(titleColumn, colnames, rownames)

  x$options$rowCallback = appendFormatter(
    x$options$rowCallback, columns, colnames, rownames, tplArrow, valueColumns, arrowHTML, titleColumn
  )

  table$x <- x
  table
}

#' Add trend arrows to a DT datatable
#'
#' @param table the DT datatable
#' @param columns column names or indicies to be replaced with arrows
#' @param valueColumns values to use to rotate the arrows (defaults to columns)
#' @param titleColumn column to use as a tooltip (defaults to NULL)
#' @return table updated with arrow formatting
#' @export
formatTooltip <- function(table, columns, valueColumns = columns, titleColumn) {

  if (inherits(columns, 'formula')) columns <- all.vars(columns)
  x <- table$x
  colnames <- base::attr(x, 'colnames', exact = TRUE)
  rownames <- base::attr(x, 'rownames', exact = TRUE)

  valueColumns <- name2int(valueColumns, colnames, rownames)

  titleColumn <- name2int(titleColumn, colnames, rownames)

  x$options$rowCallback = appendFormatter(
    x$options$rowCallback, columns, colnames, rownames, tplToolTip, valueColumns, titleColumn
  )

  table$x <- x
  table


}

#' Special bar stack function to be used within DT style functions
#'
#' @param data data
#' @param color_1 first colour of bar
#' @param color_2 second colour of bar
#' @param angle rotate?
#' @param min_range min of chart
#' @param max_range of chart
#' @export
styleColorBarStack <- function(data, color_1, color_2, angle = 90, min_range, max_range) {
  r = max_range - min_range
  JS(sprintf("isNaN(parseFloat(value)) || value <= %s ? 'linear-gradient(%sdeg, %s ' + (%s - value)/%s * 100 + '%%, %s ' + (%s - value)/%s * 100 + '%%)' : 'linear-gradient(%sdeg, %s ' + (%s - value)/%s * 100 + '%%, %s ' + (%s - value)/%s * 100 + '%%)'",
             min_range, angle, color_2, max_range, r, color_2, max_range, r, angle, color_2, max_range, r, color_1, max_range, r))
}

# Arrow template
tplArrow <- function(columns, valueColumns, arrowHTML, titleColumn = NULL, ...) {

  rowjs_notitle <- function(dt_ta, dt_va) {
    paste0("$('td:eq(", dt_ta, ")', row).html(",
           "'<span>' + ",
           "(data[", dt_va, "] == null ? '' : '<p style=\"float:left;margin:0px;transform: rotate(' + Math.atan(data[", dt_va, "]) * (-1) + 'rad)\">",  arrowHTML, "</p>') + ",
           "'</span>'",
           ");")
  }

  rowjs_title <- function(dt_ta, dt_va, dt_ti) {
    paste0("$('td:eq(", dt_ta, ")', row).html(",
           "'<span title=\"' + data[", dt_ti, "] + '\">' + ",
           "(data[", dt_va, "] == null ? '' : '<p style=\"float:left;margin:0px;transform: rotate(' + Math.atan(data[", dt_va, "]) * (-1) + 'rad)\">",  arrowHTML, "</p>') + ",
           "'</span>'",
           ");")
  }

  if (is.null(titleColumn)) {
    unlist(Map(rowjs_notitle, dt_ta = columns, dt_va = valueColumns))
  } else {
    unlist(Map(rowjs_title, dt_ta = columns, dt_va = valueColumns, dt_ti = titleColumn))
  }

}

# Arrow template
tplToolTip <- function(columns, valueColumns, titleColumn = NULL, ...) {

    rowjs_title <- function(dt_ta, dt_va, dt_ti) {
      paste0("$('td:eq(", dt_ta, ")', row).html(",
             "'<span title=\"' + data[", dt_ti, "] + '\">' + ",
             "(data[", dt_va, "] == null ? '' : data[", dt_va, "]) + ",
             "'</span>'",
             ");")
    }

    unlist(Map(rowjs_title, dt_ta = columns, dt_va = valueColumns, dt_ti = titleColumn))

}

# Generic formatter append from DT
appendFormatter = function(js, name, names, rownames = TRUE, template, ...) {
  js = if (length(js) == 0) c('function(row, data) {', '}') else {
    unlist(strsplit(as.character(js), '\n'))
  }
  i = name2int(name, names, rownames)
  htmlwidgets::JS(append(
    js, after = 1,
    template(i, ..., names, rownames)
  ))
}




#' Create a rowCallBack js function to pass to DT
# dt_rowCallback <- function(callback_vector) {
#
#   htmlwidgets::JS("function(row, data) {",
#                   callback_vector,
#                   "}")
#
# }

#' Create an arrow style row callback
# arrow_row_callback <- function(x, targets, gradient_cols, arrow_html = "&#8680;", title_column = NULL) {
#
#   dt_target <- targets_helper(x, targets)
#
#   dt_value <- targets_helper(x, gradient_cols)
#
#   dt_title <- if (!is.null(title_column)) targets_helper(x, title_column)
#
#   if (length(targets) != length(gradient_cols)) stop("targets and gradient cols must be the same length")
#
#   rowjs_notitle <- function(dt_ta, dt_va) {
#     paste0("$('td:eq(", dt_ta, ")', row).html(",
#            "'<span>' + ",
#            "(data[", dt_va, "] == null ? '' : '<p style=\"float:left;margin:0px;transform: rotate(' + Math.atan(data[", dt_va, "]) * (-1) + 'rad)\">",  arrow_html, "</p>') + ",
#            "'</span>'",
#            ");")
#   }
#
#   rowjs_title <- function(dt_ta, dt_va, dt_ti) {
#     paste0("$('td:eq(", dt_ta, ")', row).html(",
#            "'<span title=\"' + data[", dt_ti, "] + '\">' + ",
#            "(data[", dt_va, "] == null ? '' : '<p style=\"float:left;margin:0px;transform: rotate(' + Math.atan(data[", dt_va, "]) * (-1) + 'rad)\">",  arrow_html, "</p>') + ",
#            "'</span>'",
#            ");")
#   }
#
#   if (is.null(title_column)) {
#     unlist(Map(rowjs_notitle, dt_ta = dt_target, dt_va = dt_value))
#   } else {
#     unlist(Map(rowjs_title, dt_ta = dt_target, dt_va = dt_value, dt_ti = dt_title))
#   }
#
# }

#' Create an plain row with a tooltip
# row_callback <- function(x, targets, title_column = NULL, value_cols = NULL) {
#
#   dt_target <- targets_helper(x, targets)
#
#   dt_value <-
#     if (length(value_cols) != length(targets)) {
#       dt_target
#     } else {
#       targets_helper(x, value_cols)
#     }
#
#   dt_title <- if (!is.null(title_column)) targets_helper(x, title_column)
#
#   rowjs_notitle <- function(dt_ta, dt_va) {
#     paste0("$('td:eq(", dt_ta, ")', row).html(",
#            "'<span>' + ",
#            "(data[", dt_va, "] == null ? '' : data[", dt_va, "]) + ",
#            "'</span>'",
#            ");")
#   }
#
#   rowjs_title <- function(dt_ta, dt_va, dt_ti) {
#     paste0("$('td:eq(", dt_ta, ")', row).html(",
#            "'<span title=\"' + data[", dt_ti, "] + '\">' + ",
#            "(data[", dt_va, "] == null ? '' : data[", dt_va, "]) + ",
#            "'</span>'",
#            ");")
#   }
#
#   if (is.null(title_column)) {
#     unlist(Map(rowjs_notitle, dt_ta = dt_target, dt_va = dt_value))
#   } else {
#     unlist(Map(rowjs_title, dt_ta = dt_target, dt_va = dt_value, dt_ti = dt_title))
#   }
#
# }


