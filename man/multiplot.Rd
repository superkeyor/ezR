% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{multiplot}
\alias{multiplot}
\title{Multiple plot function, accepts a list of lattice plot (not ggplot) objects}
\usage{
multiplot(
  ...,
  plotlist = NULL,
  cols = NULL,
  layout = NULL,
  title = "",
  title.size = 20
)
}
\arguments{
\item{plotlist}{objects can be passed in ..., or to plotlist (as a list of plot objects)
(p1,p2,p3), (plotlist=list(p1,p2,p3)), or (p1,plotlist=list(p2,p3))}

\item{title}{string, NULL or '' both fine}

\item{cols:}{Number of columns in layout. If present, 'cols' is ignored. If both cols and layout NULL, auto calculate}

\item{layout:}{A matrix specifying the layout.
\cr If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
\cr then plot 1 will go in the upper left, 2 will go in the upper right, and
\cr 3 will go all the way across the bottom.}
}
\value{
returns nothing (NULL)
}
\description{
Multiple plot function, accepts a list of lattice plot (not ggplot) objects
}
\examples{
# examples for ggmultiplot, as reference here
plots <- list()  # new empty list
for (i in 1:6) {
    p1 = qplot(1:10, rnorm(10), main = i)
    plots[[i]] <- p1  # add each plot into plot list
}
ggmultiplot(plotlist = plots, cols = 3)

ggmultiplot(p1,p2,p3, cols = 3)

plots <- list()
for (i in 1:5) {
    p1 = qplot(1:10, rnorm(10), main = i)
    plots[[i]] <- p1
}
layout <- matrix(c(1, 1, 2, 3, 4, 5), nrow = 2, byrow = TRUE)
ggmultiplot(plotlist = plots, layout = layout)
layout <- matrix(c(1, NA, 2,
                   3, 4, 5), nrow = 2, byrow = TRUE)  # NA for placeholder
ggmultiplot(plotlist = plots, layout = layout)

}
\references{
inspired by \href{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}{Cookbook R}
}
