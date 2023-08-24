% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BootGOFTestLM.R
\name{BootGOFTestLM}
\alias{BootGOFTestLM}
\alias{print.BootGOFTestLM}
\title{Bootstrap goodness-of-fit procedure for linear models}
\usage{
BootGOFTestLM(model, data, boot_iter = 1000, level = 0.95, return_dist = FALSE)

\method{print}{BootGOFTestLM}(x, ...)
}
\arguments{
\item{model}{A fitted `lm` object.}

\item{data}{A dataframe used to fit `model`.}

\item{boot_iter}{An integer indicating number of bootstrap iterations to perform.}

\item{level}{Confidence level of the bootstrap interval used in the test.}

\item{return_dist}{A logical specifying whether to optionally return the bootstrap distribution. Defaults to FALSE.}
}
\value{
A list containing the specification and results of the test.
}
\description{
Performs a bootstrap goodness-of-fit procedure to assess the fit of a normal linear regression model
}