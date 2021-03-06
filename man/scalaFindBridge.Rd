% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scalaFindBridge.R
\name{scalaFindBridge}
\alias{scalaFindBridge}
\title{Find a Scala Bridge}
\usage{
scalaFindBridge(reference = NULL)
}
\arguments{
\item{reference}{Either: i. An rscala reference, or ii. \code{NULL} (in which
case the environment path is searched).}
}
\value{
A Scala bridge.
}
\description{
This function attempts to find an instance of a Scala bridge based on an
rscala reference or by searching the environment path.
}
