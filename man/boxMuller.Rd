% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxMuller.R
\name{boxMuller}
\alias{boxMuller}
\title{Standard normal distribution using Box Muller Transformation}
\usage{
boxMuller(n)
}
\arguments{
\item{n}{An integer or number}
}
\value{
a vector of \code{n} samples
}
\description{
Creates samples from the standard normal distribution using Box Muller Transformation
}
\examples{
set.seed(1223)
test = boxMuller(500)
}
