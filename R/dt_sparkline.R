#' Add a sparkline column to a DT datatable
#'
#' @param table The DT
#' @param columns column names or indicies of the data to be used as sparkline columns
#' @param sparklineOpts options passed to sparkline - use spark_options to create
#' @param class_suffix optional suffix (prefixed with spark) - randomly generated if not provided
#' @param width column width
#' @param ... Other options passed to the column definition
#' @return table DT with updated options and dependencies
#' @export
formatSparkline <- function(table, columns, sparklineOpts, class_suffix = '', width = "5%", ...) {

  if (class_suffix == '') class_suffix = paste0(sample(letters, 12), collapse = "")

  #Get target columns
  if (inherits(columns, 'formula')) columns = all.vars(columns)
  x = table$x
  colnames = base::attr(x, 'colnames', exact = TRUE)
  rownames = base::attr(x, 'rownames', exact = TRUE)

  #append new column definition
  options <- x$options
  options <- appendColumnDefs(options, colSparkline(columns, colnames, rownames, class_suffix, ...))

  #append new callback
  js <- options$fnDrawCallback
  options$fnDrawCallback <- appendfnDrawCallback(js, tplSparkline(class_suffix, sparklineOpts))

  table$x$options <- options

  #add widget dependency
  table <- sparkline::spk_add_deps(table)

  table
}


#' create a list to be used as a column definition in a DT datatable
#'
#' @param columns column names or indicies
#' @param colnames column names in data set
#' @param rownames row names in data set
#' @param class_suffix class name will be spark + suffix
#' @param width Width of column
#' @param render_js alternative js to render
#' @param ... additional options passed to the column definition
colSparkline <- function(columns, colnames, rownames, class_suffix = '', width = "5%", render_js = NULL, ...) {

  i <- name2int(columns, colnames, rownames)

  class_name <- paste0("spark",class_suffix)

  if (is.null(render_js)) {
    render_js <- htmlwidgets::JS("function(data, type, full){ ",
                                 paste0("return '<span class=", class_name, ">' + data + '</span>' }"))
  }

  list(
    targets = i,
    width = width,
    render = render_js,
    ...
  )
}


#' Create a sparkline template to be used in a drawback in the DT datatable
#'
#' @param class_name will be prefixed with spark
#' @param spark_opts Spark options as javascript (generate with spark_options_)
#' @return single row of javascript to be appended into the fnDrawCallback of the DT
tplSparkline <- function(class_name, spark_opts) {

  cl <- paste0("spark", class_name)

  sparkline_drawbacks <- paste0(
    "$('.", cl, ":not(:has(canvas))').sparkline('html', { ", spark_opts," });"
  )

  sparkline_drawbacks

}


#' Append a column definition to the column definitions in options
#'
#' @param options list of DT options
#' @param def list representing column definition
#' @return options appended with new definition
appendColumnDefs <- function(options, def) {
  defs <- options[['columnDefs']]
  if (is.null(defs)) defs <- list()
  defs[[length(defs) + 1]] <- def
  options$columnDefs <- defs
  options
}

#' Append a fnDrawCallback row into the existing js
#'
#' @param js The existing js
#' @param template the js to be inserted into the fdDrawCallback
#' @return js containing the fnDrawCallback with the template inserted
appendfnDrawCallback <- function(js, template) {

  js <- if (length(js) == 0) c('function (oSettings, json) {', '}') else {
    unlist(strsplit(as.character(js), '\n'))
  }

  htmlwidgets::JS(append(
    js, after = 1,
    template
  ))

}

# turn character/logical indices to numeric indices
name2int <- function(name, names, rownames) {
  if (is.numeric(name)) {
    i = if (all(name >= 0)) name else seq_along(names)[name]
    if (!rownames) i = i - 1
    return(i)
  }
  i = unname(setNames(seq_along(names), names)[name]) - 1
  if (any(is.na(i))) stop(
    'You specified the columns: ', paste(name, collapse = ', '), ', ',
    'but the column names of the data are ', paste(names, collapse = ', ')
  )
  i
}




