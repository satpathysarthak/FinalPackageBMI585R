% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppv.R
\name{ppv}
\alias{ppv}
\title{Positive Predictive Value or Precision}
\usage{
ppv(pred, truth)
}
\arguments{
\item{pred}{a logical vector of predicted values.}

\item{truth}{a logical vector of true values.}
}
\value{
a numerical value for accuracy
}
\description{
Positive Predictive Value, or PPV, is defined as # True Positives / (# True Positives + # False Positives). PPV is also referred to as precision.
}
\examples{
truth = c(T,T,T,T,T,F,F,F,F,F)
pred = c(T,F,T,T,F,T,T,T,F,F)
ppv(pred,truth)
}
