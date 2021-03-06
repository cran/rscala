% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ops.R
\name{+.rscalaBridge}
\alias{+.rscalaBridge}
\title{Declaration Operator}
\usage{
\method{+}{rscalaBridge}(bridge, snippet)
}
\arguments{
\item{bridge}{A Scala bridge.}

\item{snippet}{String providing a Scala code snippet.}
}
\value{
Returns \code{NULL}, invisibly.
}
\description{
This operator compiles and executes a snippet of Scala code \emph{in Scala's
global environment}, where subsequent uses of the same code snippet do
\emph{not} skip the time-consuming compilation step and the return value is
\code{NULL}.  As such, this operator is used to define \emph{global} imports,
objects, classes, methods, etc.
}
\examples{
\donttest{
s <- scala()
s + '
  import scala.util.Random.nextInt
  import scala.math.{Pi, log, exp, sqrt}
  val const = -log(sqrt(2*Pi))
  def dnorm(x: Double, mean: Double, sd: Double, logScale: Boolean) = {
    val z = ( x - mean ) / sd
    val result = const - log(sd) - z * z / 2
    if ( logScale ) result else exp(result)
  }
'
s $ const()
s $ nextInt(100L)
s $ dnorm(8, 10, 2, FALSE)
close(s)
}

}
\seealso{
\code{\link{*.rscalaBridge}}, \code{\link{^.rscalaBridge}},
  \code{\link{scala}}
}
