% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{ez.fishers}
\alias{ez.fishers}
\title{fisher.test(y,x), for many y and/or many x}
\usage{
ez.fishers(
  df,
  y,
  x,
  report = T,
  view = F,
  plot = F,
  pmethods = c("bonferroni", "fdr"),
  compare = "col",
  cols = 3,
  lab.size = 18,
  text.size = 16,
  width = 300,
  error = T,
  pe = F,
  ...
)
}
\arguments{
\item{df}{a data frame, internal go through dropna --> ez.2factor}

\item{y}{compatible with \code{\link{ez.selcol}}}

\item{x}{compatible with \code{\link{ez.selcol}}}

\item{report}{print results (in APA format)}

\item{view}{call View(result)}

\item{plot}{T/F, the black dash line is bonferroni p = 0.05 (again for tests only with a non-NA p values), the grey black dash is uncorrected p = 0.05}

\item{pmethods}{c('bonferroni','fdr'), type p.adjust.methods for all methods. This correction applies for all possible tests that have been/could be done.}

\item{compare}{For posthoc, see more \code{\link{fisher.posthoc}}. If "row", treats the rows as the grouping variable. If "column", treats the columns as the grouping variable.}

\item{cols}{number of columns for multiplot. NULL=auto calculate}

\item{width}{width for toString(counts,width=width)}

\item{error}{whether show error message when error occurs}
}
\value{
an invisible data frame or list of data frame (if many y and many x)
}
\description{
fisher.test(y,x), for many y and/or many x
}
\note{
odds ratio only exist for 2x2 table, otherwise NA (arbitrary assigned by jerry)
\cr also computes chisq.test, however whenever any of expected count in cells was less than five (not observed frequency), fisher.test preferred because of cells that have expected count less than 5.
\cr fisher.test() does not produce a test statistic, but SPSS does (termed as D(x), or FI(x), see p 151 of IBM SPSS Exact Tests)
}
