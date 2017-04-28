#' Create sparkline options js to include in column call back
#'
#' @param type One of 'line' (default), 'bar', 'tristate', 'discrete', 'bullet', 'pie' or 'box'
#' @param width Width of the chart - Defaults to 'auto' - May be any valid css width - 1.5em, 20px, etc (using a number without a unit specifier won't do what you want) - This option does nothing for bar and tristate chars (see barWidth)
#' @param height Height of the chart - Defaults to 'auto' (line height of the containing tag)
#' @param lineColor Used by line and discrete charts to specify the colour of the line drawn as a CSS values string
#' @param fillColor Specify the colour used to fill the area under the graph as a CSS value. Set to false to disable fill
#' @param chartRangeMin Specify the minimum value to use for the range of Y values of the chart - Defaults to the minimum value supplied
#' @param chartRangeMax Specify the maximum value to use for the range of Y values of the chart - Defaults to the maximum value supplied
#' @param composite If true then don't erase any existing chart attached to the tag, but draw another chart over the top - Note that width and height are ignored if an existing chart is detected.
#' @param enableTagOptions If true then options can be specified as attributes on each tag to be transformed into a sparkline, as well as passed to the sparkline() function. See also tagOptionPrefix
#' @param tagOptionPrefix String that each option passed as an attribute on a tag must begin with. Defaults to 'spark'
#' @param tagValuesAtribute The name of the tag attribute to fetch values from, if present - Defaults to 'values'
#' @param disableHiddenCheck Set to true to disable checking for hidden sparklines. This can be beneficial if you know you'll never attempt to draw a sparkline into a hidden parent element as it avoids a browser reflow for the test, increasing rendering performance.
#' @param spark_options_chart_type specific spark_options function for the charttype
#' @param ... create additional options if required
#' @return javascript call back
#' @export
spark_options <- function(type = NULL,
                          width = NULL,
                          height = NULL,
                          lineColor = NULL,
                          fillColor = NULL,
                          chartRangeMin = NULL,
                          chartRangeMax = NULL,
                          composite = NULL,
                          enableTagOptions = NULL,
                          tagOptionPrefix = NULL,
                          tagValuesAtribute = NULL,
                          disableHiddenCheck = NULL,
                          spark_options_chart_type = NULL,
                          ...) {

  arg_list <-
    rlist::list.clean(
      list(
        type = wrap_chr(type),
        width = width,
        height = height,
        lineColor = to_js(lineColor),
        fillColor = to_js(fillColor),
        chartRangeMin = chartRangeMin,
        chartRangeMax = chartRangeMax,
        composite = logical_to_js(composite),
        enableTagOptions = logical_to_js(enableTagOptions),
        tagOptionPrefix = wrap_chr(tagOptionPrefix),
        tagValuesAtribute = wrap_chr(tagValuesAtribute),
        disableHiddenCheck = logical_to_js(disableHiddenCheck),
        ...
      ))

  if (length(spark_options_chart_type) == 0) {

    list_to_name_value_chr(arg_list)

  } else {

    paste(list_to_name_value_chr(arg_list), spark_options_chart_type, sep = ",")

  }

}

#' Create sparkline options js to include in column call back
#'
#' @param barColor CSS colour used for postive values
#' @param negBarColor CSS colour used for negative values
#' @param zeroColor CSS colour used for values equal to zero
#' @param nullColor CSS colour used for values equal to null - By default null values are omitted entirely, but setting this adds a thin marker for the entry - This can be useful if your chart is pretty sparse; perhaps try setting it to a light grey or something equally unobtrusive
#' @param barWidth Width of each bar, in pixels (integer)
#' @param barSpacing Space between each bar, in pixels (integer)
#' @param zeroAxis Centers the y-axis at zero if true (default)
#' @param colorMap A range map to map specific values to selected colours. For example if you want all values of -2 to appear yellow, use colorMap: { '-2': '#ff0' }.
#' @param stackedBarColor An array of colours to use for stacked bar charts. The first series will use the first value in the array, the second series will use the second, etc
#' @param ... pass additional options to list
#' @return javascript call back
#' @export
spark_options_bar <- function(barColor = NULL,
                              negBarColor = NULL,
                              zeroColor = NULL,
                              nullColor = NULL,
                              barWidth = NULL,
                              barSpacing = NULL,
                              zeroAxis = NULL,
                              colorMap = NULL,
                              stackedBarColor = NULL,
                              ...) {
  arg_list <-
    rlist::list.clean(
      list(
        barColor = to_js(barColor),
        negBarColor = to_js(negBarColor),
        zeroColor = to_js(zeroColor),
        nullColor = to_js(nullColor),
        barWidth = barWidth,
        barSpacing = barSpacing,
        zeroAxis = zeroAxis,
        colorMap = vector_to_js_array(to_js(colorMap)),
        stackedBarColor = vector_to_js_array(to_js(stackedBarColor)),
        ...
      )
    )

  list_to_name_value_chr(arg_list)

}

#' Create sparkline options js to include in column call back
#'
#' @param defaultPixelsPerValue 	Defaults to 3 pixels of width for each value in the chart
#' @param spotColor The CSS colour of the final value marker. Set to false or an empty string to hide it
#' @param minSpotColor The CSS colour of the marker displayed for the mimum value. Set to false or an empty string to hide it
#' @param maxSpotColor The CSS colour of the marker displayed for the maximum value. Set to false or an empty string to hide it
#' @param spotRadius Radius of all spot markers, In pixels (default: 1.5) - Integer
#' @param valueSpots Specifies which points to draw spots on, and with which colour. Accepts a range. For example, to render green spots on all values less than 50 and red on values higher use {':49': 'green, '50:': 'red'}
#' @param highlightSpotColor Specifies a colour for the spot that appears on a value when moused over. Set to null to disable
#' @param highlightLineColor Specifies a colour for the vertical line that appears through a value when moused over. Set to null to disable.
#' @param lineWidth In pixels (default: 1) - Integer
#' @param normalRangeMin Specify threshold values between which to draw a bar to denote the "normal" or
#' @param normalRangeMax expected range of values. For example the green bar here  might denote a normal operating temperature range
#' @param drawNormalOnTop By default the normal range is drawn behind the fill area of the chart. Setting this option to true causes it to be drawn over the top of the fill area
#' @param xvalues See below
#' @param chartRangeClip If true then the y values supplied to plot will be clipped to fall between chartRangeMin and chartRangeMax - By default chartRangeMin/Max just ensure that the chart spans at least that range of values, but does not constrain it
#' @param chartRangeMinX Specifies the minimum value to use for the X value of the chart
#' @param chartRangeMaxX Specifies the maximum value to use for the X value of the chart
#' @param ... pass additional options to list
#' @return javascript call back
#' @export
spark_options_line <- function(defaultPixelsPerValue = NULL,
                              spotColor = NULL,
                              minSpotColor = NULL,
                              maxSpotColor = NULL,
                              spotRadius = NULL,
                              valueSpots = NULL,
                              highlightSpotColor = NULL,
                              highlightLineColor = NULL,
                              lineWidth = NULL,
                              normalRangeMin = NULL,
                              normalRangeMax = NULL,
                              drawNormalOnTop = NULL,
                              xvalues = NULL,
                              chartRangeClip = NULL,
                              chartRangeMinX = NULL,
                              chartRangeMaxX = NULL,
                              ...) {
  arg_list <-
    rlist::list.clean(
      list(
        defaultPixelsPerValue = defaultPixelsPerValue,
        spotColor = to_js(spotColor),
        minSpotColor = to_js(minSpotColor),
        maxSpotColor = to_js(maxSpotColor),
        spotRadius = spotRadius,
        valueSpots = valueSpots,
        highlightSpotColor = to_js(highlightSpotColor),
        highlightLineColor = to_js(highlightLineColor),
        lineWidth = lineWidth,
        normalRangeMin = normalRangeMin,
        normalRangeMax = normalRangeMax,
        drawNormalOnTop = to_js(drawNormalOnTop),
        xvalues = vector_to_js_array(xvalues),
        chartRangeClip = to_js(chartRangeClip),
        chartRangeMinX = chartRangeMinX,
        chartRangeMaxX = chartRangeMaxX,
        ...
      )
    )

  list_to_name_value_chr(arg_list)

}

#' Create sparkline options js to include in column call back
#'
#' @param targetColor The CSS colour of the vertical target marker
#' @param targetWidth The width of the target marker in pixels (integer)
#' @param performanceColor The CSS color of the performance measure horizontal bar
#' @param rangeColors Colors to use for each qualitative range background color - This must be a javascript array. eg ['red','green', '#22f']
#' @param ... pass additional options to list
#' @return javascript call back
#' @export
spark_options_bullet <- function(targetColor = NULL,
                                 targetWidth = NULL,
                                 performanceColor = NULL,
                                 rangeColors = NULL,
                                 ...) {

  arg_list <-
    rlist::list.clean(
      list(
        targetColor = to_js(targetColor),
        targetWidth = targetWidth,
        performanceColor = to_js(performanceColor),
        rangeColors = vector_to_js_array(rangeColors),
        ...
      )
    )

  list_to_name_value_chr(arg_list)

}


#' Create sparkline options js to include in column call back
#'
#' @param sliceColors An array of CSS colors to use for pie slices
#' @param offset Angle in degrees to offset the first slice - Try -90 or +90
#' @param borderWidth Width of the border to draw around the whole pie chart, in pixels. Defaults to 0 (no border)
#' @param borderColor CSS color to use to draw the pie border. Defaults to #000
#' @param ... pass additional options to list
#' @return javascript call back
#' @export
spark_options_pie <- function(sliceColors = NULL,
                                 offset = NULL,
                                 borderWidth = NULL,
                                 borderColor = NULL,
                                 ...) {

  arg_list <-
    rlist::list.clean(
      list(
        sliceColors = vector_to_js_array(to_js(sliceColors)),
        offset = offset,
        borderWidth = borderWidth,
        borderColor = wrap_chr(borderColor),
        ...
      )
    )

  list_to_name_value_chr(arg_list)

}


#' Create sparkline options js to include in column call back
#'
#' @param raw If set to false (default) then the values supplied are used to caculate the box data points for you. If true then you must pre-calculate the points (see below)
#' @param showOutliers If true (default) then outliers (values > 1.5x the IQR) are marked with circles and the whiskers are placed at Q1 and Q3 instead of the least and greatest value
#' @param outlierIQR Set the inter-quartile range multipler used to calculate values that qualify as an outlier - Defaults to 1.5
#' @param boxLineColor CSS line colour used to outline the box
#' @param boxFillColor CSS fill colour used for the box
#' @param whiskerColor CSS colour used to draw the whiskers
#' @param outlierLineColor CSS colour used to draw the outlier circles
#' @param outlierFillColor CSS colour used to fill the outlier circles
#' @param spotRadius Radius in pixels to draw the outlier circles
#' @param medianColor CSS colour used to draw the median line
#' @param target If set to a value, then a small crosshair is drawn at that point to represent a target value
#' @param targetColor CSS colour used to draw the target crosshair, if set
#' @param minValue If minvalue and maxvalue are set then the scale of the plot is fixed. By default minValue and maxValue are deduced from the values supplied
#' @param maxValue See minValue
#' @param ... pass additional options to list
#' @return javascript call back
#' @export
spark_options_box <- function(raw = NULL,
                              showOutliers = NULL,
                              outlierIQR = NULL,
                              boxLineColor = NULL,
                              boxFillColor = NULL,
                              whiskerColor = NULL,
                              outlierLineColor = NULL,
                              outlierFillColor = NULL,
                              spotRadius = NULL,
                              medianColor = NULL,
                              target = NULL,
                              targetColor = NULL,
                              minValue = NULL,
                              maxValue = NULL,
                              ...) {

  arg_list <-
    rlist::list.clean(
      list(
        raw = to_js(raw),
        showOutliers = to_js(showOutliers),
        outlierIQR = outlierIQR,
        boxLineColor = wrap_chr(boxLineColor),
        boxFillColor = wrap_chr(boxFillColor),
        whiskerColor = wrap_chr(whiskerColor),
        outlierLineColor = wrap_chr(outlierLineColor),
        outlierFillColor = wrap_chr(outlierFillColor),
        spotRadius = spotRadius,
        medianColor = wrap_chr(medianColor),
        target = target,
        targetColor = wrap_chr(targetColor),
        minValue = minValue,
        maxvalue = maxValue,
        ...
      )
    )

  list_to_name_value_chr(arg_list)

}

spark_options_interactive <- function() {

}

spark_options_tooltip <- function() {

}




# colDef <- function(x, targets, visible = "true", width = "5%", ...) {
#
#   targets_int <- targets_helper(x, targets)
#
#   list(
#     targets = targets_int,
#     #visible = visible,
#     width = width,
#     ...
#   )
# }

# spark_colDef <- function(x, targets, class_suffix = '', width = "5%", render_js = NULL, ...) {
#
#   class_name <- paste0("spark",class_suffix)
#
#   if (is.null(render_js)) {
#     render_js <- htmlwidgets::JS("function(data, type, full){ ",
#                                  paste0("return '<span class=", class_name, ">' + data + '</span>' }"))
#   }
#
#   colDef(x, targets, width = width, render = render_js, ...)
#
# }

# spark_fnDrawCallback <- function(class_name, spark_opts) {
#
#   cl <- paste0("spark", class_name)
#
#   sparkline_drawbacks <- paste0(
#      "$('.", cl, ":not(:has(canvas))').sparkline('html', { ", spark_opts," });"
#     )
#
#   htmlwidgets::JS("function (oSettings, json) {",
#      paste0(sparkline_drawbacks, collapse = "\n"),
#      "}"
#      )
# }







