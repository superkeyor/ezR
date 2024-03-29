% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/os.R
\name{ez.retry}
\alias{ez.retry}
\title{retry, wrapper of \code{\link{try}}}
\usage{
ez.retry(
  expr,
  times = 5,
  pause = 3,
  verbose = FALSE,
  isError = function(x) "try-error" \%in\% class(x)
)
}
\arguments{
\item{expr}{no need to quote the expr}

\item{times}{max number of retry attempts}

\item{pause}{time in seconds between attempts}
}
\description{
retry, wrapper of \code{\link{try}}
}
\note{
see also \code{\link[purrr]{insistently}}
retry(func_that_might_fail(param1, param2), times=10, pause=2) to retry calling that function 
with those parameters, give up after 10 errors, and pause 2 seconds between attempts.
}
