% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balance.plot.R
\name{balance.plot}
\alias{balance.plot}
\title{Calculate and Visualize Balances}
\usage{
balance.plot(
  x,
  y,
  d.group,
  n.group,
  boxplot.split = TRUE,
  weigh.var = FALSE,
  size.text = 20,
  size.pt = 4
)
}
\arguments{
\item{x}{A matrix with rows as samples (N) and columns as components (D).}

\item{y}{A serial binary partition matrix with rows as components (D) and
columns as balances (D-1).}

\item{d.group}{A vector of group labels for components. Optional.
If provided, used to color component points.}

\item{n.group}{A vector of group labels for samples. Optional.
If provided, used to color sample points.}

\item{boxplot.split}{A boolean. Toggles whether to split the boxplot
by \code{n.group}. TRUE better resembles balance dendrogram style.}

\item{weigh.var}{A boolean. Toggles whether to weigh line width
by the proportion of explained variance. Only do this if balances
come from an SBP that decomposes variance.}

\item{size.text}{An integer. Sets legend text size.}

\item{size.pt}{An integer. Sets point size.}
}
\value{
A list of the "partition" \code{ggplot} object, the "distribution"
 \code{ggplot} object, and the per-sample balances.
}
\description{
This function calculates balances based on the compositional data
 set and serial binary partition (SBP) matrix provided, then generates a
 figure from the results.
}
\examples{
library(balance)
data(iris)
x <- iris[,1:4]
sbp <- sbp.fromPBA(x)
balance(x, sbp)

}
\author{
Thom Quinn
}
