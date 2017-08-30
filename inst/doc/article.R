## ----setup, include=FALSE---------------------------------------------------------------
library("knitr")
opts_chunk$set(fig.path='figure/latex-', cache.path='cache/latex-', cache=TRUE, size="small", out.columns=84, fig.align='center', fig.show='hold')
options(formatR.arrow=TRUE, width=90)
hook_source <- knit_hooks$get('source')
knit_hooks$set(source = function(x, options) {
  txt <- hook_source(x, options)
  # extend the default source hook
  gsub('~', '\\\\mytilde', txt)
})
#hook_output = knit_hooks$get('output')
#knit_hooks$set(output = function(x, options) {
#  if (!is.null(n <- options$out.lines)) {
#    x = unlist(strsplit(x, '\n'))
#    if (length(x) > n) {
#      # truncate the output
#      x = c(head(x, n), '....\n')
#    }
#    x = paste(x, collapse = '\n') # paste first n lines together
#  }
#  if (!is.null(n <- options$out.columns)) {
#    x = unlist(strsplit(x, '\n'))
#    probs <- nchar(x)>options$out.columns
#    x[probs] <- paste(substr(x[probs],1,options$out.columns-3), '...', sep="")
#    x = paste(x, collapse = '\n') # paste first n lines together
#  }
#  hook_output(x, options)
#})

## ----installPackage,eval=FALSE----------------------------------------------------------
#  install.packages('rscala')

## ----library----------------------------------------------------------------------------
library('rscala')

## ----instantiateInterpreter,eval=FALSE--------------------------------------------------
#  s <- scala()

## ----instantiateInterpreterHidden,eval=TRUE,include=FALSE-------------------------------
set.seed(2234)
s <- scala(serialize.output=TRUE)
s %@% 'scala.util.Random.setSeed(3242)'

## ----scalaInfo,eval=FALSE---------------------------------------------------------------
#  scalaInfo(s)

## ----scalaInfo2,eval=FALSE--------------------------------------------------------------
#  scalaInfo(verbose=TRUE)

## ----evaluateScalaCodeSimple------------------------------------------------------------
s %@% '
  val c_10_3 = (1 to 3).map( i => {
    (10-i+1) / i.toDouble
  }).product.toInt
'

## ----evaluateScalaCodeSimple3-----------------------------------------------------------
s %@% 'print("10 choose 3 is " + c_10_3 + ".")'

## ----evaluateScalaCodeSimple2-----------------------------------------------------------
tenChooseThree <- s %~% '(1 to 3).map( i => (10-i+1) / i.toDouble ).product'
tenChooseThree == choose(10,3)

## ----evaluateScalaCodeStringInterpolation-----------------------------------------------
n <- 10
k <- 3
label <- "number of threesomes among ten people"
s %@% '
  val count = (1 to @{k}).foldLeft(1) { (prod,i) => prod * (@{n}-i+1)/i }
  println("The @{label} is " + count + ".")
'

## ----evaluateScalaCodeInterpolationError------------------------------------------------
s %~% 'math.pow(count, 20) == @{tenChooseThree^20}'

## ----evaluateScalaCodeVector------------------------------------------------------------
s %~% 'Array.fill(2)(Array.fill(5)(scala.util.Random.nextDouble))'

## ----evaluateScalaCodeReference---------------------------------------------------------
rng <- s %~% 'new scala.util.Random()'
rng

oneToTenReference <- s %.~% 'Array.range(1,11)'
oneToTenReference

## ----gettingUsingDollarSign1------------------------------------------------------------
s %@% 'val fibSeq = Array[Double](0, 1, 1, 2, 3, 5, 8, 13, 21)'
fibSeqAsDouble  <- s %~%  'fibSeq'
fibSeqReference <- s %.~% 'fibSeq'

## ----gettingUsingDollarSign2------------------------------------------------------------
fibSeqAsDouble <- s$fibSeq
fibSeqAsDouble <- s$val('fibSeq')

## ----gettingUsingDollarSign3------------------------------------------------------------
fibSeqReference <- s$.val('fibSeq')

## ----gettingUsingDollarSign4------------------------------------------------------------
s$fibSeq <- c(0, 1, 1, 2, 3, 5, 8, 13, 21)
s$copyOfFibSeqReference <- fibSeqReference

## ----evaluateScalaCodeInstantiate-------------------------------------------------------
seed <- 123L
rng <- s %.~% 'new scala.util.Random(@{seed})'
rng <- s$.scala.util.Random$new(seed)
rng <- s$do('scala.util.Random')$new(seed)

## ----evaluateScalaCodeReference2--------------------------------------------------------
rng$setSeed(24234L)
rng$nextInt(10L)
oneToTenReference$sum()

## ----evaluateScalaCodeReference3--------------------------------------------------------
intReference <- rng$nextInt(10L, .AS.REFERENCE=TRUE)

## ----evaluateScalaCodeReference1--------------------------------------------------------
rng$self()

## ----evaluateScalaCodeCompanion---------------------------------------------------------
oneToTenReference <- s %.~% 'Array.range(1, 11)'
oneToTenReference <- s$.Array$range(1L, 11L, .AS.REFERENCE=TRUE)
oneToTenReference <- s$do('Array')$range(1L, 11L, .AS.REFERENCE=TRUE)

## ----nullAsArgument---------------------------------------------------------------------
s$.java.lang.System$setProperties(scalaNull('java.util.Properties'))

## ----lengthOne--------------------------------------------------------------------------
setter <- s %.~% '
  object setter {
    def apply[T](x: Array[T], value: T) = x.indices.foreach { x(_) = value }
  }
  setter
'

## ----lengthOne2-------------------------------------------------------------------------
arr <- s %.~% 'Array(math.Pi, math.E)'
arr$mkString("<", ", ", ">")
setter$apply(I(arr), 3)
arr$mkString("<", ", ", ">")

## ----evaluateScalaCodeApply-------------------------------------------------------------
fibSeqAsInt <- s %~% 'Array(0, 1, 1, 2, 3, 5, 8, 13, 21)'
fibSeqAsInt <- s %~% 'Array.apply(0, 1, 1, 2, 3, 5, 8, 13, 21)'
fibSeqAsInt <- s$.Array$apply(0L, 1L, 1L, 2L, 3L, 5L, 8L, 13L, 21L)
fibSeqAsInt <- s$do('Array')$apply(0L, 1L, 1L, 2L, 3L, 5L, 8L, 13L, 21L)

## ----evaluateScalaCodeUpdate------------------------------------------------------------
s %@% 'val fibSeq = Array[Double](0, 1, 1, 2, 3, 5, 8, 13, 21)'
fibSeqReference <- s %.~% 'fibSeq'
s %@% 'fibSeq(1) = math.Pi'
s$.fibSeq$update(1L, pi)
s$do('fibSeq')$update(1L, pi)
fibSeqReference$update(1L, pi)

## ----evaluateScalaCodeApplyWithQuote----------------------------------------------------
fibSeqAsDouble <- s %~% 'Array[Double](0, 1, 1, 2, 3, 5, 8, 13, 21)'
fibSeqAsDouble <- s %~% 'Array.apply[Double](0, 1, 1, 2, 3, 5, 8, 13, 21)'
fibSeqAsDouble <- s$.Array$'apply[Double]'(0L, 1L, 1L, 2L, 3L, 5L, 8L, 13L, 21L)
fibSeqAsDouble <- s$do('Array')$'apply[Double]'(0L, 1L, 1L, 2L, 3L, 5L, 8L, 13L, 21L)

## ----evaluateScalaCodeApplyWithQuoteSimple----------------------------------------------
fibSeqAsDouble <- s$.Array$apply(0, 1, 1, 2, 3, 5, 8, 13, 21)

## ----quoteWhenNecessary-----------------------------------------------------------------
list <- s$.List$apply(1L, 2L, 3L)
augmentedList <- list$':+'(100L)
paste0(augmentedList$toString(), " now contains 100.")

## ----bellUsingR-------------------------------------------------------------------------
bell.version1 <- function(n=1, format=c("character","integer","double","log")[3]) {
  if ( n <= 0 ) stop("'n' must be at least 1.")
  if ( n == 1 ) return(1)
  r1 <- r2 <- numeric(n)
  r1[1] <- 1
  for ( k in 2:n ) {
    r2[1] <- r1[k-1]
    for ( i in 2:k ) r2[i] <- r1[i-1] + r2[i-1]
    r1 <- r2
  }
  value <- r2[n]
  if ( format == "character" ) sprintf("%0.0f", value)
  else if ( format == "integer" ) as.integer(value)
  else if ( format == "double" ) value
  else if ( format == "log" ) log(value)
}

## ----bellUsingScala---------------------------------------------------------------------
bell.version2 <- function(n=1L, format=c("character","integer","double","log")[3]) {
  if ( n <= 0 ) stop("'n' must be at least 1.")
  s %!% '
    var r1 = new Array[BigInt](n)
    var r2 = new Array[BigInt](n)
    r1(0) = BigInt(1)
    for ( k <- 1 until n ) {
      r2(0) = r1(k-1)
      for ( i <- 1 to k ) r2(i) = r1(i-1) + r2(i-1)
      val tmp = r1; r1 = r2; r2 = tmp
    }
    val value = r1(n-1)
    format match {
      case "character" => value.toString
      case "integer" => value.toInt
      case "double" => value.toDouble
      case "log" =>
        val blex = value.bitLength - 1022
        if ( blex > 0 ) math.log( (value >> blex).toDouble ) + blex * math.log(2)
        else math.log(value.toDouble)
    }
  '
}

## ----callBellUsingScala-----------------------------------------------------------------
cpuFirstEval <- system.time(
   bigNumber <- bell.version2(500, format="character")
)
cat(paste(strsplit(bigNumber, "(?<=.{80})", perl = TRUE)[[1]], collapse="\n"), "\n")

## ----setInScala-------------------------------------------------------------------------
s %@% '
  R.set("zone", java.util.TimeZone.getDefault.getDisplayName)
  R.set("atLeast8", scala.util.Properties.isJavaAtLeast("1.8"))
'
zone
atLeast8

## ----getInScala-------------------------------------------------------------------------
s %@% '
  val result = R.get("T")
  println("The result is " + result)
  if ( result._1.asInstanceOf[Boolean] ) {
    println("Good, nobody messed with the value of T.")
  }
'

## ----getInScala2------------------------------------------------------------------------
s %@% 'if ( R.getL0("T") ) { println("Good, nobody messed with the value of T.") } '

## ----evalInScala------------------------------------------------------------------------
set.seed(324)
s %~% 'R.evalD1("rnorm(100, sd=3)").map(math.pow(_, 2)).sum'

## ----invokeInScala----------------------------------------------------------------------
set.seed(324)
s %~% '
  val mean = 100
  R.invokeD1("rnorm", mean, "sd" -> 3).map(math.pow(_, 2)).sum
'

## ----enc--------------------------------------------------------------------------------
f <- function(n,alpha) sapply(alpha, function(a) sum(a / (1:n + a - 1)))
f(100, 1.0)

## ----rootFinding------------------------------------------------------------------------
bisection <- function(func=NULL, lower=1.0, upper=1.0, epsilon=0.000000001) s %!% '
  def g(x: Double) = R.invokeD0(func, x)
  val (fLower, fUpper) = (g(lower), g(upper))
  if ( fLower * fUpper > 0 ) sys.error("lower and upper do not straddle the root.")
  @scala.annotation.tailrec
  def engine(l: Double, u: Double, fLower: Double, fUpper: Double): Double = {
    if ( math.abs( l - u ) <= epsilon ) ( l + u ) / 2
    else {
      val c = ( l + u ) / 2
      val fCenter = g(c)
      if ( fLower * fCenter < 0 ) engine(l, c, fLower, fCenter)
      else engine(c, u, fCenter, fUpper)
    }
  }
  engine(lower, upper, fLower, fUpper)
'

bisection(function(a) f(100, a) - 10, 0.1, 20)

## ----recursion--------------------------------------------------------------------------
recursive.sum <- function(n=0L) s %!% '
  if ( n <= 0 ) 0 else n + R.invokeI0("recursive.sum", n - 1)
'
recursive.sum(10)

## ----showCPUFirstEval-------------------------------------------------------------------
cpuFirstEval

## ----callBellUsingScala2----------------------------------------------------------------
cpuSecondEval <- system.time(
   bigNumber <- bell.version2(500, format="character")
)
cpuSecondEval
cpuFirstEval['elapsed'] / cpuSecondEval['elapsed']

## ----methodsAreCached-------------------------------------------------------------------
rng <- s$.scala.util.Random$new()
first  <- system.time( rng$nextGaussian() )['elapsed']
second <- system.time( rng$nextGaussian() )['elapsed']
c(first=first, second=second, ratio=first/second)

## ----makeFasterNextGaussian-------------------------------------------------------------
fasterNextGaussian <- rng$nextGaussian(.EVALUATE=FALSE)

## ----benchmarkAgainstRJava--------------------------------------------------------------
library('rJava', verbose=FALSE, quietly=TRUE)
invisible(
  rJava::.jinit(
    list.files(file.path(scalaInfo(s)$home, "lib"), full.names=TRUE)
  )
)
rngRJava <- rJava::.jnew("scala.util.Random")
fasterNextGaussianRJava <- function() rJava::.jcall(rngRJava, "D", "nextGaussian")

if ( suppressWarnings(require('microbenchmark', quietly=TRUE)) ) {
  timings <- summary(microbenchmark(fasterNextGaussianRJava(), fasterNextGaussian(),
    rngRJava$nextGaussian(), rng$nextGaussian(), times=1000))
} else load('timings.RData')

## ----benchmarkAgainstRJavaTable, echo=FALSE, results='asis'-----------------------------
library(xtable)
units <- attr(timings,"unit")
reps <- timings[1,"neval"]
timings <- cbind(timings[,c("expr")],c("rJava","rscala"),timings[,c("lq","mean","median","uq")])
colnames(timings) <- c("Expression","Package","Q1","Mean","Median","Q3")
xtab <- xtable(timings, label="overhead")
caption(xtab) <- paste0("Comparison of execution time of various ways to call the \\code{nextGaussian} method of an instance of the \\code{scala.util.Random} class.  Since the method itself is relatively fast, the timings here are an indication of the overhead involved with the various techniques.  Each expression was evaluated ",reps," times and the results are in ",units,".")
print(xtab,include.rownames=FALSE,booktabs=TRUE)

## ----onLoad, eval=FALSE-----------------------------------------------------------------
#  .onLoad <- function(libname, pkgname) {
#    .rscalaPackage(pkgname)
#  }

## ----onLoad2, eval=FALSE----------------------------------------------------------------
#  .onLoad <- function(libname, pkgname) {
#    snippet <- '
#      import org.ddahl.shallot._
#      import org.apache.commons.math3.random.{ RandomDataGenerator => RDG }
#  
#      def rdg() = {
#        val ints = R.evalI1("runif(2,-.Machine$integer.max,.Machine$integer.max)")
#        val seed = ((ints(0).asInstanceOf[Long]) << 32) | (ints(1) & 0xffffffffL)
#        val r = new RDG()
#        r.reSeed(seed)
#        r
#      }
#    '
#    ## Users may want to use 'options(rscala.heap.maximum="2G")'.
#    .rscalaPackage(pkgname,classpath.packages="commonsMath",snippet=snippet)
#    ## This circumvents a bug in the class loader of Scala 2.11.x.
#    sInfo <- scalaInfo()
#    if ( ( ! is.null(sInfo) ) && ( sInfo$major.release == "2.11" ) ) {
#      s$.org.apache.commons.math3.random.EmpiricalDistribution$new()
#    }
#  }

## ----onUnload, eval=FALSE---------------------------------------------------------------
#  .onUnload <- function(libpath) {
#    .rscalaPackageUnload()
#  }

## ----mailR.rJava, eval=FALSE------------------------------------------------------------
#  base_dir <- .jnew("java.io.File", normalizePath(getwd()))

## ----mailR.rscala, eval=FALSE-----------------------------------------------------------
#  base_dir <- s$.java.io.File$new(normalizePath(getwd()))

## ----closeScala,eval=TRUE,include=FALSE-------------------------------------------------
close(s)

