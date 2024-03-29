% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{ez.savexlist}
\alias{ez.savexlist}
\alias{ez.writexlist}
\title{Save multiple data frames to multiple sheets individually}
\usage{
ez.savexlist(
  xlist,
  file = "RData.xlsx",
  withFilter = TRUE,
  rowNames = TRUE,
  colNames = TRUE,
  ...
)

ez.writexlist(
  xlist,
  file = "RData.xlsx",
  withFilter = TRUE,
  rowNames = TRUE,
  colNames = TRUE,
  ...
)
}
\arguments{
\item{xlist}{a list of data frames. eg, list(sheetA=df1,sheetB=df2) where sheetA/B become sheet names; list(df1,df2) where it auto names Sheet1/2
\cr Other parameters in \code{\link[openxlsx]{writeData}}}
}
\value{
returns file path
}
\description{
Save multiple data frames to multiple sheets individually
}
