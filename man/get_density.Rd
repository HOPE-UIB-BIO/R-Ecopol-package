% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_density.R
\name{get_density}
\alias{get_density}
\title{Get density from a vector of numbers}
\usage{
get_density(data_source, values_range, reflected = TRUE, ...)
}
\arguments{
\item{data_source}{Vector with numeric values}

\item{values_range}{A numeric vector of length 2 specifying the range to
rescale the data to. If missing, the range of the data is used.}

\item{reflected}{Logical. Whether or not to reflect the data around 0?
(from Density Estimation for Statistics and Data Analysis by B.W.Silverman)}

\item{...}{Additional parameters used in \code{stats::density}}
}
\value{
Data.frame with the \code{data_source} values and estimated density+
}
\description{
This function calculates the density of a numeric vector, potentially
rescaling it to a specific range and reflecting it. it is a wrapper for
\code{stats::density()}.
}
\seealso{
\code{\link[stats:density]{stats::density()}}
}
