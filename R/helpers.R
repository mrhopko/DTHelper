#' identifies columns from a numeric or character vector - returns js position
#'
#' @param x data
#' @param targets character or numeric vector of columns to be found in x
#' @return numeric vector of js positions of columns (i.e expected position - 1)
targets_helper <- function(x, targets) {

  cnames = base::attr(x, 'colnames', exact = TRUE)

  if (is.null(cnames)) {
    cnames <- colnames(x)
  }

  if (class(targets) %in% c("numeric","integer","double")) {
    targets - 1
  } else {
    if (all(targets %in% cnames)) {
      match(targets, cnames) - 1
    } else {
      stop(paste0("The following columns are not in x: ", paste0(setdiff(targets, cnames), collapse = ",")))
    }
  }

}

#' converts array to character indicating a js [,] type array
#'
#' @param v vector
#' @return numeric vector of js positions of columns (i.e expected position - 1)
vector_to_js_array <- function(v) {
  if (is.null(v)) return(NULL)
  paste0("[", paste0(v, collapse = ","),"]")
}

#' convert a list to a string of name:values
#'
#' @param l a list
#' @return string of name:values from list
#' @export
list_to_name_value_chr <- function(l) {
  paste0(
    unlist(
      Map(function(n,v) paste0(n, ":", v), n = names(l), v = unlist(l))
    ),
    collapse = ","
  )
}

#' to be used within data.table data to create list columns (use := list(dt_list_of_lags()))
#'
#' @param x_vector column to be lagged
#' @param lags_x vector of lag values (eg c(0:2))
#' @return listcolumn of lagged vectors
#' @export
dt_list_of_lags <- function(x_vector, lags_x) {
  b <- data.table::transpose(data.table::shift(x_vector, lags_x))
  b <- lapply(b, function(y) y[!is.na(y)])
  b
}

#' to be used within data.table data to create list columns (use := list(dt_list_of_lags()))
#'
#' @param x_vector_1 column to be lagged
#' @param x_vector_2 column to be lagged
#' @param lags_x vector of lag values (eg c(0:2))
#' @return listcolumn of lagged vectors with : separator
#' @export
dt_list_of_lags_2 <- function(x_vector_1, x_vector_2, lags_x) {
  b1 <- data.table::transpose(data.table::shift(x_vector_1, lags_x))
  b2 <- data.table::transpose(data.table::shift(x_vector_2, lags_x))
  b <- Map(function(x1, x2) paste(x1[!is.na(x1)], x2[!is.na(x1)], sep = ":"), x1 = b1, x2 = b2)
  b
}

#' Calcaulte least squares gradiant
#'
#' @param x column to be lagged
#' @param lags_x vector of lag values (eg c(0:2))
#' @param re_scale_x x if required
#' @return vector of trend values
#' @export
least_squares_over_lags <- function(x, lags_x, re_scale_x = 1) {
  periods_x = rev(lags_x)
  l_vals <- data.table::transpose(data.table::shift(x, lags_x))
  unlist(lapply(l_vals, function(ylist) {
    p_x <- periods_x * re_scale_x
    y <- unlist(ylist)
    ((length(y[!is.na(y)]) * sum(y * p_x, na.rm = TRUE)) -
        (sum(y, na.rm = TRUE) * sum(p_x[!is.na(y)]))) /
      (length(y[!is.na(y)]) * sum(p_x[!is.na(y)] ^ 2) - sum(p_x[!is.na(y)]) ^ 2)
  }))
}

#' wrap characters within a vector with wrap_with
#'
#' @param x vector to wrap
#' @param wrap_with character to wrap stuff
#' @return character vector
wrap_chr <- function(x, wrap_with = "'") {
  unlist(
    lapply(x, function(y) {
      if (is.null(y)) {
        NULL
      } else {
        paste0(wrap_with, y, wrap_with)
      }
    }

    )
  )
}


#' convert logical values to javascript true/false
#'
#' @param x vector to convert
#' @return character vector
logical_to_js <- function(x) {

  f <- function(y) {
    if (is.null(y)) {
      return(NULL)
    } else if (class(y) == "logical") {
      if (y == TRUE) return("true")
      if (y == FALSE) return("false")
    } else if (y == "true") {
      return("true")
    } else if (y == "false") {
      return("false")
    } else if (y == "FALSE") {
      return("false")
    } else if (y == "TRUE") {
      return("true")
    } else {
      return(y)
    }
  }

  unlist(lapply(x, f))
}


#' check for logical and wrap in '
#'
#' @param x vector to convert
#' @return character vector
to_js <- function(x) {
  if (is.null(x)) return(NULL)
  y <- logical_to_js(x)
  ifelse(y %in% c("true","false"), y, wrap_chr(y, "'"))
}

