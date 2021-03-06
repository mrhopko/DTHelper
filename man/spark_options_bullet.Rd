% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparkline_helper.R
\name{spark_options_bullet}
\alias{spark_options_bullet}
\title{Create sparkline options js to include in column call back}
\usage{
spark_options_bullet(targetColor = NULL, targetWidth = NULL,
  performanceColor = NULL, rangeColors = NULL, ...)
}
\arguments{
\item{targetColor}{The CSS colour of the vertical target marker}

\item{targetWidth}{The width of the target marker in pixels (integer)}

\item{performanceColor}{The CSS color of the performance measure horizontal bar}

\item{rangeColors}{Colors to use for each qualitative range background color - This must be a javascript array. eg ['red','green', '#22f']}

\item{...}{pass additional options to list}
}
\value{
javascript call back
}
\description{
Create sparkline options js to include in column call back
}
