% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linreg.R
\docType{class}
\name{ridgereg-class}
\alias{ridgereg-class}
\alias{ridgereg}
\title{Performs Ridge Regression model.}
\arguments{
\item{formula}{an object of class "formula" (or one that can be coerced to that class):
a symbolic description of the model to be fitted.
The details of model specification are given under ‘Details’.}

\item{data}{data frame, list or environment
(or object coercible by as.data.frame to a data frame) containing the
variables in the model. If not found in data, the variables are taken from
environment(formula),}
}
\description{
Performs Ridge Regression model.
}

\examples{
data(iris)
ridge <- ridgereg(Petal.Length~Species, data = iris)

}
