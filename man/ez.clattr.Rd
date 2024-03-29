% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frame.R
\name{ez.clattr}
\alias{ez.clattr}
\title{remove specified attributes}
\usage{
ez.clattr(
  x,
  col = NULL,
  attrs = c("variable.labels", "label", "value.labels", "labels"),
  ...
)
}
\arguments{
\item{x}{a data frame or a vector}

\item{col}{evaluated by \code{\link{ez.selcol}}(x,col). Or, NULL=all cols.}

\item{attrs}{variable label: c('variable.labels', 'label'); value labels: c('value.labels', 'labels'). run names(attributes(x)) to see all attributes}
}
\value{
returns a new data frame or vector
}
\description{
remove specified attributes
}
\note{
this function uses a different mechanism from sjmisc_set_labels(x,"") which works only for value labels: haven style ("labels") or foreign style ("value.labels")
}
