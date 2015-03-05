## Scala scripting over TCP/IP

scalaInterpreter <- function(classpath=character(0),scala.home=NULL,java.home=NULL,java.heap.maximum="256M",java.opts=NULL,debug.filename=NULL) {
  debug <- !is.null(debug.filename)
  userJars <- unlist(strsplit(classpath,.Platform$path.sep))
  if ( is.null("java.opts") ) {
    java.opts <- c(paste("-Xmx",java.heap.maximum,sep=""),"-Xms32M")
  }
  sInfo <- scalaInfo(scala.home)
  rsJar <- rscalaJar(sInfo$version)
  args <- c(
    java.opts,
    paste("-Xbootclasspath/a:",shQuote(paste(c(rsJar,sInfo$jars),collapse=.Platform$path.sep)),sep=""),
    "-classpath",
    '""',
    paste("-Dscala.home=",shQuote(sInfo$home),sep=""),
    "-Dscala.usejavacp=true",
    "-Denv.emacs=",
    "scala.tools.nsc.MainGenericRunner",
    "-howtorun:script","-Xnojline",
    "-classpath",shQuote(paste(c(rsJar,userJars),collapse=.Platform$path.sep)))
  pkgPath <- system.file(package="rscala")
  if ( debug ) {
    cat("R DEBUG:\n")
    cat("Command line:\n")
    cat(paste("<",args,">",sep="",collapse="\n"),"\n",sep="")
  }
  portsFilename <- tempfile("rscala-")
  Sys.setenv(RSCALA_TUNNELING="TRUE")
  bootstrap.filename <- tempfile("rscala-")
  bootstrap.file <- file(bootstrap.filename, "w")
  writeLines(c(sprintf('org.ddahl.rscala.ScalaServer($intp,raw"%s").run()',portsFilename),'sys.exit(0)'),bootstrap.file)
  close(bootstrap.file)
  stdout <- ifelse(!is.null(debug.filename),sprintf("%s-stdout",debug.filename),FALSE)
  stderr <- ifelse(!is.null(debug.filename),sprintf("%s-stderr",debug.filename),FALSE)
  system2(javaCmd(java.home),args,wait=FALSE,stdin=bootstrap.filename,stdout=stdout,stderr=stderr)
  sockets <- newSockets(portsFilename,debug)
  assign("debug",!debug,envir=sockets[['env']])
  assign("markedForGC",integer(0),envir=sockets[['env']])
  intpSettings(sockets,debug=debug,interpolate=TRUE,length.one.as.vector=FALSE,quiet=FALSE)
  sockets
}

newSockets <- function(portsFilename,debug) {
  getPortNumbers <- function() {
    delay <- 0.05
    while ( TRUE ) {
      if ( delay > 20 ) stop("Cannot start JVM.")
      Sys.sleep(delay)
      delay <- 1.2*delay
      if ( file.exists(portsFilename) ) {
        line <- scan(portsFilename,n=2,what=character(0),quiet=TRUE)
        if ( length(line) > 0 ) return(as.numeric(line))
      }
    }
  }
  ports <- getPortNumbers()
  file.remove(portsFilename)
  if ( debug ) cat("R DEBUG: Trying to connect to port:",paste(ports,collapse=","),"\n")
  socketConnectionIn  <- socketConnection(port=ports[1],blocking=TRUE,open="ab",timeout=2678400)
  socketConnectionOut <- socketConnection(port=ports[2],blocking=TRUE,open="rb",timeout=2678400)
  env <- new.env()
  assign("open",TRUE,envir=env)
  assign("debug",debug,envir=env)
  assign("length.one.as.vector",FALSE,envir=env)
  result <- list(socketIn=socketConnectionIn,socketOut=socketConnectionOut,env=env)
  class(result) <- "ScalaInterpreter"
  result
}

intpEval.ScalaInterpreter <- function(interpreter,snippet,interpolate="",quiet="") {
  cc(interpreter)
  snippet <- paste(snippet,collapse="\n")
  if ( ( ( interpolate == "" ) && ( get("interpolate",envir=interpreter[['env']]) ) ) || ( interpolate == TRUE ) ) {
    snippet <- strintrplt(snippet,parent.frame())
  }
  wb(interpreter,EVAL)
  wc(interpreter,snippet)
  rServe(interpreter)
  status <- rb(interpreter,integer(0))
  echoResponseScala(interpreter,quiet)
  if ( status == ERROR ) {
    stop("Evaluation error.")
  }
  invisible(NULL)
}

'%~%.ScalaInterpreter' <- function(interpreter,snippet) {
  cc(interpreter)
  snippet <- paste(snippet,collapse="\n")
  if ( get("interpolate",envir=interpreter[['env']]) ) {
    snippet <- strintrplt(snippet,parent.frame())
  }
  result <- evalAndGet(interpreter,snippet,FALSE)
  if ( is.null(result) ) invisible(result)
  else result
}

'%.~%.ScalaInterpreter' <- function(interpreter,snippet) {
  cc(interpreter)
  snippet <- paste(snippet,collapse="\n")
  if ( get("interpolate",envir=interpreter[['env']]) ) {
    snippet <- strintrplt(snippet,parent.frame())
  }
  result <- evalAndGet(interpreter,snippet,TRUE)
  if ( is.null(result) ) invisible(result)
  else result
}

print.ScalaInterpreter <- function(x,...) {
  cat("ScalaInterpreter\n")
  invisible(x)
}

toString.ScalaInterpreter <- function(x,...) {
  "ScalaInterpreter"
}

print.ScalaInterpreterReference <- function(x,...) {
  typeInfo <- if ( substring(x$typeInfo,1,2) == "()" ) substring(x$typeInfo,3)
  else x$typeInfo
  cat("ScalaInterpreterReference... ")
  if ( is.null(x$env) ) {
    cat(x$identifier,": ",typeInfo,"\n",sep="")
  } else {
    cat("*: ",typeInfo,"\n",sep="")
  }
  invisible(x)
}

toString.ScalaInterpreterReference <- function(x,...) {
  if ( is.null(x$env) ) x$identifier
  else ""
}

intpGet.ScalaInterpreter <- function(interpreter,identifier,as.reference=FALSE) {
  cc(interpreter)
  if ( as.reference ) {
    if ( inherits(identifier,"ScalaInterpreterReference") ) return(identifier)
    wb(interpreter,GET_REFERENCE)
    wc(interpreter,as.character(identifier[1]))
    response <- rb(interpreter,integer(0))
    if ( response == OK ) {
      id <- rc(interpreter)
      typeInfo <- rc(interpreter)
      env <- if ( substr(id,1,1) == "." ) {
        env <- new.env()
        reg.finalizer(env,function(e) {
          assign("markedForGC",c(as.integer(substring(id,2)),get("markedForGC",interpreter[['env']])),interpreter[['env']])
        })
        env
      } else NULL
      result <- list(interpreter=interpreter,identifier=id,typeInfo=typeInfo,env=env)
      class(result) <- "ScalaInterpreterReference"
      return(result)
    } else if ( response == ERROR ) {
      echoResponseScala(interpreter,FALSE)
      stop(paste("Exception thrown.",sep=""))
    } else if ( response == UNDEFINED_IDENTIFIER ) {
      stop(paste("Undefined identifier: ",identifier[1],sep=""))
    } else stop("Protocol error.")
  }
  i <- if ( inherits(identifier,"ScalaInterpreterReference") ) identifier$identifier else as.character(identifier[1])
  wb(interpreter,GET)
  wc(interpreter,i)
  dataStructure <- rb(interpreter,integer(0))
  if ( dataStructure == NULLTYPE ) {
    NULL
  } else if ( dataStructure == ATOMIC ) {
    dataType <- rb(interpreter,integer(0))
    if ( dataType == STRING ) {
      rc(interpreter)
    } else {
      v <- rb(interpreter,typeMap[[dataType]])
      if ( dataType == BOOLEAN ) as.logical(v)
      else v
    }
  } else if ( dataStructure == VECTOR ) {
    length <- rb(interpreter,integer(0))
    dataType <- rb(interpreter,integer(0))
    if ( dataType == STRING ) {
      if ( length > 0 ) sapply(1:length,function(x) rc(interpreter))
      else character(0)
    } else {
      v <- rb(interpreter,typeMap[[dataType]],length)
      if ( dataType == BOOLEAN ) as.logical(v)
      else v
    }
  } else if ( dataStructure == MATRIX ) {
    dim <- rb(interpreter,integer(0),2L)
    dataType <- rb(interpreter,integer(0))
    if ( dataType == STRING ) {
      v <- matrix("",nrow=dim[1],ncol=dim[2])
      if ( dim[1] > 0 ) for ( i in 1:dim[1] ) {
        if ( dim[2] > 0 ) for ( j in 1:dim[2] ) {
          v[i,j] <- rc(interpreter)
        }
      }
      v
    } else {
      v <- matrix(rb(interpreter,typeMap[[dataType]],dim[1]*dim[2]),nrow=dim[1],byrow=TRUE)
      if ( dataType == BOOLEAN ) mode(v) <- "logical"
      v
    }
  } else if ( dataStructure == ERROR ) {
    echoResponseScala(interpreter,FALSE)
    stop(paste("Exception thrown.",sep=""))
  } else if ( dataStructure == UNDEFINED_IDENTIFIER ) {
    stop(paste("Undefined identifier: ",i,sep=""))
  } else if ( dataStructure == UNSUPPORTED_STRUCTURE ) {
    stop("Unsupported data structure.")
  } else stop("Protocol error.")
}

'$.ScalaInterpreter' <- function(interpreter,identifier) {
  cc(interpreter)
  if ( identifier == "def" ) function(args,body) {
    body <- paste(body,collapse="\n")
    if ( get("interpolate",envir=interpreter[['env']]) ) {
      args <- strintrplt(args,parent.frame())
      body <- strintrplt(body,parent.frame())
    }
    intpDef(interpreter,args,body,interpolate=FALSE)
  } else if ( identifier == "val" ) function(x) {
    intpGet(interpreter,x)
  } else if ( identifier == ".val" ) function(x) {
    intpGet(interpreter,x,as.reference=TRUE)
  } else if ( substring(identifier,1,1) == "." ) {
    identifier = substring(identifier,2)
    if ( identifier == "" ) intpGet(interpreter,".")
    else if ( identifier == "." ) intpGet(interpreter,".",as.reference=TRUE)
    else intpGet(interpreter,identifier,as.reference=TRUE)
  } else {
    intpGet(interpreter,identifier)
  }
}

intpSet.ScalaInterpreter <- function(interpreter,identifier,value,length.one.as.vector="",quiet="") {
  cc(interpreter)
  if ( inherits(value,"ScalaInterpreterReference") ) {
    wb(interpreter,SET)
    wc(interpreter,identifier)
    wb(interpreter,REFERENCE)
    wc(interpreter,value$identifier)
    response <- rb(interpreter,integer(0))
    echoResponseScala(interpreter,quiet)
    if ( response == ERROR ) {
      stop(paste("Exception thrown.",sep=""))
    }
  } else {
    if ( ! is.atomic(value) ) stop("Data structure is not supported.")
    if ( is.vector(value) ) {
      type <- checkType(value)
      wb(interpreter,SET)
      wc(interpreter,identifier)
      if ( ( length(value) == 1 ) && ( ( ( length.one.as.vector == "" ) && ( ! get("length.one.as.vector",envir=interpreter[['env']]) ) ) || length.one.as.vector == FALSE ) ) {
        wb(interpreter,ATOMIC)
      } else {
        wb(interpreter,VECTOR)
        wb(interpreter,length(value))
      }
      wb(interpreter,type)
      if ( type == STRING ) {
        if ( length(value) > 0 ) for ( i in 1:length(value) ) wc(interpreter,value[i])
      } else {
        if ( type == BOOLEAN ) wb(interpreter,as.integer(value))
        else wb(interpreter,value)
      }
    } else if ( is.matrix(value) ) {
      type <- checkType(value)
      wb(interpreter,SET)
      wc(interpreter,identifier)
      wb(interpreter,MATRIX)
      wb(interpreter,dim(value))
      wb(interpreter,type)
      if ( nrow(value) > 0 ) for ( i in 1:nrow(value) ) {
        if ( type == STRING ) {
          if ( ncol(value) > 0 ) for ( j in 1:ncol(value) ) wc(interpreter,value[i,j])
        }
        else if ( type == BOOLEAN ) wb(interpreter,as.integer(value[i,]))
        else wb(interpreter,value[i,])
      }
    } else if ( is.null(value) ) {
      wb(interpreter,SET)
      wc(interpreter,identifier)
      wb(interpreter,NULLTYPE)
    } else stop("Data structure is not supported.")
    echoResponseScala(interpreter,quiet)
  }
  invisible()
}

'$<-.ScalaInterpreter' <- function(interpreter,identifier,value) {
  cc(interpreter)
  intpSet(interpreter,identifier,value)
  interpreter
}

intpDef.ScalaInterpreter <- function(interpreter,args,body,interpolate="",quiet="") {
  cc(interpreter)
  tmpFunc <- NULL
  body <- paste(body,collapse="\n")
  if ( ( ( interpolate == "" ) && ( get("interpolate",envir=interpreter[['env']]) ) ) || ( interpolate == TRUE ) ) {
    args <- strintrplt(args,parent.frame())
    body <- strintrplt(body,parent.frame())
  }
  wb(interpreter,DEF)
  wc(interpreter,args)
  wc(interpreter,body)
  status <- rb(interpreter,integer(0))
  if ( status == OK ) {
    status <- rb(interpreter,integer(0))
    if ( status == OK ) {
      status <- rb(interpreter,integer(0))
      if ( status == OK ) {
        status <- rb(interpreter,integer(0))
        if ( status == OK ) {
          functionName <- rc(interpreter)
          length <- rb(interpreter,integer(0))
          functionParamNames <- if ( length > 0 ) sapply(1:length,function(x) rc(interpreter))
          else character(0)
          functionParamTypes <- if ( length > 0 ) sapply(1:length,function(x) rc(interpreter))
          else character(0)
          convertedCode <- list()
          if ( length > 0 ) for ( i in 1:length ) {
            convertedCode[[i]] <- convert(functionParamNames[i],functionParamTypes[i])
          }
          convertedCodeStr <- paste("  ",unlist(convertedCode),sep="",collapse="\n")
          argsStr <- paste(functionParamNames,collapse=",")
          if ( nchar(argsStr) > 0 ) argsStr <- paste(argsStr,",",sep="")
          functionSnippet <- strintrplt('
  tmpFunc <- function(${argsStr}as.reference=FALSE,quiet="",gc=FALSE) {
    rscala:::wb(interpreter,rscala:::CLEAR)
  ${convertedCodeStr}
    if ( gc ) intpGC(interpreter)
    rscala:::wb(interpreter,rscala:::INVOKE)
    rscala:::wc(interpreter,"${functionName}")
    rscala:::rServe(interpreter)
    status <- rscala:::rb(interpreter,integer(0))
    rscala:::echoResponseScala(interpreter,quiet)
    if ( status == rscala:::ERROR ) {
      stop("Invocation error.")
    } else {
      result <- intpGet(interpreter,"?",as.reference=as.reference)
      if ( is.null(result) ) invisible(result)
      else result
    }
  }')
          source(textConnection(functionSnippet),local=TRUE)
        } else {
          echoResponseScala(interpreter,quiet)
          stop("Evaluation error.")
        }
      } else {
        echoResponseScala(interpreter,quiet)
        stop("Evaluation error.")
      }
    } else {
      echoResponseScala(interpreter,quiet)
      stop("Unsupported data type.")
    }
  } else {
    echoResponseScala(interpreter,quiet)
    stop("Error in parsing function arguments.")
  }
  echoResponseScala(interpreter,quiet)
  if ( is.null(tmpFunc) ) invisible()
  else tmpFunc
}

intpGC.ScalaInterpreter <- function(interpreter) {
  cc(interpreter)
  gc()
  markedForGC <- get("markedForGC",interpreter[["env"]])
  if ( length(markedForGC) > 0 ) {
    wb(interpreter,GC)
    wb(interpreter,length(markedForGC))
    wb(interpreter,markedForGC)
    assign("markedForGC",integer(0),interpreter[["env"]])
  }
  invisible()
}

intpReset.ScalaInterpreter <- function(interpreter) {
  cc(interpreter)
  wb(interpreter,RESET)
}

close.ScalaInterpreter <- function(con,...) {
  cc(con)
  assign("open",FALSE,envir=con[['env']])
  wb(con,EXIT)
}

rscalaJar <- function(version="") {
  if ( version == "" ) major.version <- ".*"
  else major.version = substr(version,1,4)
  list.files(system.file("java",package="rscala"),pattern=paste("rscala_",major.version,'-.*[0-9]\\.jar',sep=""),full.names=TRUE)
}

javaCmd <- function(java.home=NULL) {
  if ( is.null(java.home) ) java.home <- ""
  candidate <- normalizePath(file.path(java.home,"bin","java"),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  candidate <- normalizePath(Sys.getenv("JAVACMD"),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  candidate <- normalizePath(file.path(Sys.getenv("JAVA_HOME"),"bin","java"),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  candidate <- normalizePath(Sys.which("java"),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  stop("Cannot find Java executable.  Please set 'java.home' argument, set 'JAVACMD' or 'JAVA_HOME' environment variables, or add 'java' to your shell's search path.")
}

scalaCmd <- function(scala.home=NULL) {
  if ( is.null(scala.home) ) scala.home <- ""
  candidate <- normalizePath(file.path(scala.home,"bin","scala"),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  candidate <- normalizePath(file.path(Sys.getenv("SCALA_HOME"),"bin","scala"),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  candidate <- normalizePath(Sys.which("scala"),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  installDir <- normalizePath(file.path("~",".rscala",sprintf("scala-%s",CURRENT_SUPPORTED_SCALA_VERSION)),mustWork=FALSE)
  candidate <- file.path(installDir,"bin","scala")
  if ( file.exists(candidate) ) return(candidate)
  if ( ! interactive() ) installDir <- normalizePath(file.path(tempdir(),sprintf("scala-%s",CURRENT_SUPPORTED_SCALA_VERSION)),mustWork=FALSE)
  installScala(installDir)
  candidate <- file.path(installDir,"bin","scala")
  if ( file.exists(candidate) ) return(candidate)
  stop("Cannot find Scala executable.  Please set 'scala.home' argument, set the 'SCALA_HOME' environment variable, or add 'scala' to your shell's search path.")
}

scalaInfo <- function(scala.home=NULL) {
  cmd <- scalaCmd(scala.home)
  home <- dirname(dirname(cmd))
  libraryJar <- file.path(home,"lib","scala-library.jar")
  jars <- list.files(file.path(home,"lib"),"*.jar",full.names=TRUE)
  if ( ! ( libraryJar %in% jars ) ) stop(sprintf("Found Scala executable (%s) but not its library JAR (%s).",cmd,libraryJar))
  version <- suppressWarnings(
    tryCatch(
      system2(cmd,c("-e",shQuote("print(util.Properties.versionNumberString)")),stdout=TRUE,stderr=FALSE)
    ,error=function(e) {})
  )
  if ( ( is.null(version) ) || ( length(version) != 1 ) ) stop(sprintf("Unexpected version from Scala executable (%s): %s.",cmd,version))
  major.version = substr(version,1,4)
  if ( ! ( major.version %in% c("2.10","2.11") ) ) stop(sprintf("Unsuported major version from Scala executable (%s): %s.",cmd,major.version))
  list(cmd=cmd,home=home,version=version,major.version=major.version,jars=jars)
}

# Private

evalAndGet <- function(interpreter,snippet,as.reference) {
  tryCatch({
    intpEval(interpreter,snippet,interpolate=FALSE)
    intpGet(interpreter,".",as.reference=as.reference)
  },error=function(e) return(invisible()))
}

checkType <- function(x) {
  if ( is.integer(x) ) INTEGER
  else if ( is.double(x) ) DOUBLE
  else if ( is.logical(x) ) BOOLEAN
  else if ( is.character(x) ) STRING
  else stop("Unsupported data type.")
}

convert <- function(x,t,include.set=TRUE) {
  if ( t == "Int" ) {
    tt <- "atomic"
    tm <- "integer"
    loav <- FALSE
  } else if ( t == "Double" ) {
    tt <- "atomic"
    tm <- "double"
    loav <- FALSE
  } else if ( t == "Boolean" ) {
    tt <- "atomic"
    tm <- "logical"
    loav <- FALSE
  } else if ( t == "String" ) {
    tt <- "atomic"
    tm <- "character"
    loav <- FALSE
  } else if ( t == "Array[Int]" ) {
    tt <- "vector"
    tm <- "integer"
    loav <- TRUE
  } else if ( t == "Array[Double]" ) {
    tt <- "vector"
    tm <- "double"
    loav <- TRUE
  } else if ( t == "Array[Boolean]" ) {
    tt <- "vector"
    tm <- "logical"
    loav <- TRUE
  } else if ( t == "Array[String]" ) {
    tt <- "vector"
    tm <- "character"
    loav <- TRUE
  } else if ( t == "Array[Array[Int]]" ) {
    tt <- "matrix"
    tm <- "integer"
    loav <- TRUE
  } else if ( t == "Array[Array[Double]]" ) {
    tt <- "matrix"
    tm <- "double"
    loav <- TRUE
  } else if ( t == "Array[Array[Boolean]]" ) {
    tt <- "matrix"
    tm <- "logical"
    loav <- TRUE
  } else if ( t == "Array[Array[String]]" ) {
    tt <- "matrix"
    tm <- "character"
    loav <- TRUE
  } else {
    tt <- "reference"
    tm <- "reference"
    loav <- FALSE
  }
  v <- character(0)
  if ( tt == "atomic" ) v <- c(v,sprintf("%s <- as.vector(%s)[1]",x,x))
  else if ( tt == "vector" ) v <- c(v,sprintf("%s <- as.vector(%s)",x,x))
  else if ( tt == "matrix" ) v <- c(v,sprintf("%s <- as.matrix(%s)",x,x))
  if ( tm != "reference" ) v <- c(v,sprintf("storage.mode(%s) <- '%s'",x,tm))
  if ( length(v) != 0 ) {
    v <- c(sprintf("if ( ! inherits(%s,'ScalaInterpreterReference') ) {",x),paste("  ",v,sep=""),"}")
  }
  if ( include.set ) c(v,sprintf("intpSet(interpreter,'.',%s,length.one.as.vector=%s,quiet=TRUE)",x,loav))
  else {
    if ( loav ) c(v,sprintf("%s <- rJava::.jarray(%s,dispatch=TRUE)",x,x),sprintf("%s <- rJava::.jcast(%s,.jclass(%s))",x,x,x))
    else v
  }
}

echoResponseScala <- function(interpreter,quiet) {
  response <- rc(interpreter)
  if ( ( ( quiet == "" ) && ( ! get("quiet",envir=interpreter[['env']]) ) ) || ( quiet == FALSE ) ) {
    cat(response)
  }
}

cc <- function(c) {
  if ( ! get("open",envir=c[['env']]) ) stop("The connection has already been closed.")
}

wb <- function(c,v) writeBin(v,c[['socketIn']],endian="big")

wc <- function(c,v) {
  length <- nchar(v,type="bytes")
  wb(c,length)
  if ( length > 0 ) writeChar(v,c[['socketIn']],length,eos=NULL,useBytes=TRUE)
}

rb <- function(c,v,n=1L) readBin(c[['socketOut']],v,n,endian="big")

rc <- function(c) {
  length <- rb(c,integer(0))
  readChar(c[['socketOut']],length,useBytes=TRUE)
}

installScala <- function(installPath) {
  cat("\nThe Scala executable is not found.\n")
  if ( interactive() ) {
    prompt <- sprintf("Would you like to install it now at %s? [Y/n] ",installPath)
    ans <- "NONSENSE"
    while (!(ans %in% c("","Y","y","N","n"))) ans <- readline(prompt=prompt)
  } else {
    cat("Installing in R's temporary directory since this session is not interactive.\n")
    ans <- "y"
  }
  if ( ans %in% c("","Y","y") ) {
    url <- sprintf("http://downloads.typesafe.com/scala/%s/scala-%s.tgz",CURRENT_SUPPORTED_SCALA_VERSION,CURRENT_SUPPORTED_SCALA_VERSION)
    installPath <- dirname(installPath)
    dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
    destfile <- file.path(installPath,basename(url))
    result <- download.file(url,destfile)
    if ( result != 0 ) return()
    result <- untar(destfile,exdir=installPath,tar="internal")    # Use internal to avoid problems on a Mac.
    unlink(destfile)
    if ( result != 0 ) return()
  }
}

