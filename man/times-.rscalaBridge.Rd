% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ops.R
\name{*.rscalaBridge}
\alias{*.rscalaBridge}
\title{Evaluation Operator}
\usage{
\method{*}{rscalaBridge}(bridge, snippet)
}
\arguments{
\item{bridge}{A Scala bridge.}

\item{snippet}{String providing a Scala code snippet.}
}
\value{
Returns a vector or matrix of \R's basic types (if possible) or an
  rscala reference (otherwise).
}
\description{
This operator compiles and executes a snippet of Scala code.  All definitions
are \emph{local} to the supplied Scala snippet.  Subsequent uses of the same
code snippet skips the time-consuming compilation step.  The return value is
a vector or matrix of \R's basic types (if possible) or an rscala reference
(otherwise).
}
\examples{
\donttest{
s <- scala()
s * 'scala.util.Random.nextDouble() <= 0.75'
s(mean=10, sd=2.5) * 'mean + sd * scala.util.Random.nextGaussian()'
close(s)
}

}
\seealso{
\code{\link{^.rscalaBridge}}, \code{\link{+.rscalaBridge}},
  \code{\link{scala}}
}
