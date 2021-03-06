% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sbp.R
\name{sbp.fromHclust}
\alias{sbp.fromHclust}
\title{Build SBP Matrix from hclust Object}
\usage{
sbp.fromHclust(hclust)
}
\arguments{
\item{hclust}{An \code{hclust} object.}
}
\value{
An SBP matrix.
}
\description{
This function builds an SBP matrix from an \code{hclust} object
 as produced by the \code{hclust} function.
}
\examples{
library(balance)
data(cars)
h <- hclust(dist(cars))
sbp.fromHclust(h)

}
\author{
Thom Quinn
}
