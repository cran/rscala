## Scala scripting over TCP/IP

scalaInterpreter <- function(classpath=character(0),scala.home=NULL,java.home=NULL,java.heap.maximum="256M",java.opts=NULL,debug.filename=NULL) {
  debug <- !is.null(debug.filename)
  userJars <- unlist(strsplit(classpath,.Platform$path.sep))
  if ( is.null(java.opts) ) {
    java.opts <- c(paste("-Xmx",java.heap.maximum,sep=""),"-Xms32M")
  }
  sInfo <- scalaInfo(scala.home)
  rsJar <- rscalaJar(sInfo$version)
  rsClasspath <- shQuote(paste(c(rsJar,userJars),collapse=.Platform$path.sep))
  args <- c(
    java.opts,
    paste("-Xbootclasspath/a:",shQuote(paste(c(rsJar,sInfo$jars),collapse=.Platform$path.sep)),sep=""),
    "-classpath",
    '""',
    paste("-Dscala.home=",shQuote(sInfo$home),sep=""),
    "-Dscala.usejavacp=true",
    "-Denv.emacs=",
    paste("-Drscala.classpath=",rsClasspath,sep=""),
    "scala.tools.nsc.MainGenericRunner",
    "-howtorun:script","-Xnojline",
    "-classpath",rsClasspath)
  if ( debug ) {
    cat("R DEBUG:\n")
    cat("Command line:\n")
    cat(paste("<",args,">",sep="",collapse="\n"),"\n",sep="")
  }
  portsFilename <- tempfile("rscala-")
  Sys.setenv(RSCALA_TUNNELING="TRUE")
  bootstrap.filename <- tempfile("rscala-")
  bootstrap.file <- file(bootstrap.filename, "w")
  writeLines(c(sprintf('org.ddahl.rscala.ScalaServer(org.ddahl.rscala.ScalaInterpreterAdapter($intp),raw"%s").run()',portsFilename),'sys.exit(0)'),bootstrap.file)
  close(bootstrap.file)
  stdout <- ifelse(!is.null(debug.filename),sprintf("%s-stdout",debug.filename),FALSE)
  stderr <- ifelse(!is.null(debug.filename),sprintf("%s-stderr",debug.filename),FALSE)
  system2(javaCmd(java.home),args,wait=FALSE,stdin=bootstrap.filename,stdout=stdout,stderr=stderr)
  sockets <- newSockets(portsFilename,debug)
  sockets[['scalaInfo']] <- sInfo
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
  functionCache <- new.env()
  env <- new.env()
  assign("open",TRUE,envir=env)
  assign("debug",debug,envir=env)
  assign("length.one.as.vector",FALSE,envir=env)
  result <- list(socketIn=socketConnectionIn,socketOut=socketConnectionOut,env=env,functionCache=functionCache)
  class(result) <- "ScalaInterpreter"
  status <- rb(result,integer(0))
  if ( ( length(status) == 0 ) || ( status != OK ) ) stop("Error instantiating interpreter.")
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
  result <- evalAndGet(interpreter,snippet,NA)
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
  type <- if ( substring(x[['type']],1,2) == "()" ) substring(x[['type']],3)
  else x[['type']]
  cat("ScalaInterpreterReference... ")
  if ( is.null(x[['env']]) ) {
    cat(x[['identifier']],": ",type,"\n",sep="")
  } else {
    cat("*: ",type,"\n",sep="")
  }
  invisible(x)
}

toString.ScalaInterpreterReference <- function(x,...) {
  if ( is.null(x[['env']]) ) x[['identifier']]
  else ""
}

print.ScalaInterpreterItem <- function(x,...) {
  cat("ScalaInterpreterItem\n")
  invisible(x)
}

toString.ScalaInterpreterItem <- function(x,...) {
  "ScalaInterpreterItem"
}

"$.ScalaInterpreterReference" <- function(reference,methodName) {
  type <- reference[['type']]
  functionCache <- reference[['interpreter']][['functionCache']]
  env <- reference[['interpreter']][['env']]
  function(...,evaluate=TRUE,length.one.as.vector="") {
    loav <- ! ( ( ( length.one.as.vector == "" ) && ( ! get("length.one.as.vector",envir=env) ) ) || length.one.as.vector == FALSE )
    args <- list(...)
    nArgs <- length(args)
    names <- paste0("x",1:nArgs)
    types <- sapply(args,deduceType,length.one.as.vector=loav)
    argsStr <- paste(names,types,sep=": ",collapse=", ")
    namesStr <- paste(names,collapse=", ")
    key <- paste0(type,"$",methodName,"#",paste(types,collapse=","))
    if ( ! exists(key,envir=functionCache) ) {
      f <- if ( nArgs == 0 ) {
        intpDef(reference[['interpreter']],'',paste0('rscalaReference.',methodName),reference=reference)
      } else {
        intpDef(reference[['interpreter']],argsStr,paste0('rscalaReference.',methodName,"(",namesStr,")"),reference=reference)
      }
      assign(key,f,envir=functionCache)
    } else {
      f <- get(key,envir=functionCache)
      assign("rscalaReference",reference,envir=environment(f),inherits=TRUE)
    }
    if ( evaluate ) f(...)
    else f
  }
}

"$.ScalaInterpreterItem" <- function(item,methodName) {
  interpreter <- item[['interpreter']]
  type <- item[['item.name']]
  functionCache <- interpreter[['functionCache']]
  env <- interpreter[['env']]
  function(...,evaluate=TRUE,length.one.as.vector="") {
    loav <- ! ( ( ( length.one.as.vector == "" ) && ( ! get("length.one.as.vector",envir=env) ) ) || length.one.as.vector == FALSE )
    args <- list(...)
    nArgs <- length(args)
    names <- paste0("x",1:nArgs)
    types <- sapply(args,deduceType,length.one.as.vector=loav)
    argsStr <- paste(names,types,sep=": ",collapse=", ")
    namesStr <- paste(names,collapse=", ")
    key <- paste0(type,"@",methodName,"#",paste(types,collapse=","))
    if ( ! exists(key,envir=functionCache) ) {
      f <- if ( nArgs == 0 ) {
        if ( methodName == "new" ) {
          intpDef(interpreter,'',paste0('new ',type))
        } else {
          intpDef(interpreter,'',paste0(type,'.',methodName))
        }
      } else {
        if ( methodName == "new" ) {
          intpDef(interpreter,argsStr,paste0('new ',type,"(",namesStr,")"))
        } else {
          intpDef(interpreter,argsStr,paste0(type,'.',methodName,"(",namesStr,")"))
        }
      }
      assign(key,f,envir=functionCache)
    } else {
      f <- get(key,envir=functionCache)
    }
    if ( evaluate ) f(...)
    else f
  }
}

intpGet.ScalaInterpreter <- function(interpreter,identifier,as.reference=NA) {
  cc(interpreter)
  if ( ( ! is.na(as.reference) ) && ( as.reference ) ) {
    if ( inherits(identifier,"ScalaInterpreterReference") ) return(identifier)
    wb(interpreter,GET_REFERENCE)
    wc(interpreter,as.character(identifier[1]))
    response <- rb(interpreter,integer(0))
    if ( response == OK ) {
      id <- rc(interpreter)
      type <- rc(interpreter)
      env <- if ( substr(id,1,1) == "." ) {
        env <- new.env()
        reg.finalizer(env,function(e) {
          assign("markedForGC",c(as.integer(substring(id,2)),get("markedForGC",interpreter[['env']])),interpreter[['env']])
        })
        env
      } else NULL
      result <- list(interpreter=interpreter,identifier=id,type=type,env=env)
      class(result) <- "ScalaInterpreterReference"
      return(result)
    } else if ( response == ERROR ) {
      echoResponseScala(interpreter,FALSE)
      stop(paste("Exception thrown.",sep=""))
    } else if ( response == UNDEFINED_IDENTIFIER ) {
      stop(paste("Undefined identifier: ",identifier[1],sep=""))
    } else stop("Protocol error.")
  }
  i <- if ( inherits(identifier,"ScalaInterpreterReference") ) identifier[['identifier']] else as.character(identifier[1])
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
    echoResponseScala(interpreter,TRUE)
    stop(paste("Undefined identifier: ",i,sep=""))
  } else if ( dataStructure == UNSUPPORTED_STRUCTURE ) {
    if ( is.na(as.reference) ) {
      intpGet(interpreter,identifier,as.reference=TRUE)
    } else {
      stop("Unsupported data structure.")
    }
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
  } else if ( identifier == "do" ) function(item.name) {
    result <- list(interpreter=interpreter,item.name=item.name)
    class(result) <- "ScalaInterpreterItem"
    result
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

deduceType <- function(value,length.one.as.vector) {
  if ( inherits(value,"ScalaInterpreterReference") ) value[['type']]
  else {
    if ( ! is.atomic(value) ) stop("Data structure is not supported.")
    if ( is.vector(value) ) {
      type <- checkType2(value)
      if ( ( length(value) == 1 ) && ( ! length.one.as.vector ) ) type
      else paste0("Array[",type,"]")
    } else if ( is.matrix(value) ) paste0("Array[Array[",checkType2(value),"]]")
    else if ( is.null(value) ) "Any"
    else stop("Data structure is not supported.")
  }
}

intpSet.ScalaInterpreter <- function(interpreter,identifier,value,length.one.as.vector="",quiet="") {
  cc(interpreter)
  if ( inherits(value,"ScalaInterpreterReference") ) {
    wb(interpreter,SET)
    wc(interpreter,identifier)
    wb(interpreter,REFERENCE)
    wc(interpreter,value[['identifier']])
    status <- rb(interpreter,integer(0))
    echoResponseScala(interpreter,quiet)
    if ( status == ERROR ) {
      stop("Setting error.")
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

intpDef.ScalaInterpreter <- function(interpreter,args,body,interpolate="",quiet="",reference=NULL) {
  cc(interpreter)
  tmpFunc <- NULL
  args <- gsub("^\\s+$","",args)
  body <- paste(body,collapse="\n")
  if ( ( ( interpolate == "" ) && ( get("interpolate",envir=interpreter[['env']]) ) ) || ( interpolate == TRUE ) ) {
    args <- strintrplt(args,parent.frame())
    body <- strintrplt(body,parent.frame())
  }
  wb(interpreter,DEF)
  rscalaReference <- reference
  prependedArgs <- if ( ! is.null(reference) ) paste0("rscalaReference: ",reference[['type']],ifelse(args=="","",","),args)
  else args
  wc(interpreter,prependedArgs)
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
          functionReturnType <- rc(interpreter)
          convertedCode <- list()
          if ( length > 0 ) for ( i in 1:length ) {
            convertedCode[[i]] <- convert(functionParamNames[i],functionParamTypes[i])
          }
          convertedCodeStr <- paste("    ",unlist(convertedCode),sep="",collapse="\n")
          argsStr <- if ( ! is.null(reference) ) paste(functionParamNames[-1],collapse=",")
          else paste(functionParamNames,collapse=",")
          if ( nchar(argsStr) > 0 ) argsStr <- paste(argsStr,",",sep="")
          functionSnippet <- strintrplt('
  tmpFunc <- function(${argsStr}as.reference=NA,quiet="",gc=FALSE) {
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
  if ( is.null(tmpFunc) ) return(invisible())
  attr(tmpFunc,"args") <- args
  attr(tmpFunc,"body") <- body
  attr(tmpFunc,"type") <- functionReturnType
  tmpFunc
}

scalap <- function(interpreter,item.name) {
  if ( ! inherits(interpreter,"ScalaInterpreter") ) stop("The first argument must be an interpreter.")
  cc(interpreter)
  wb(interpreter,SCALAP)
  wc(interpreter,item.name)
  cat(rc(interpreter),"\n")
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

rscalaPackage <- function(pkgname) {
  environmentOfDependingPackage <- parent.env(parent.frame())
  E <- new.env(parent=environmentOfDependingPackage)
  E$initialized <- FALSE
  E$pkgname <- pkgname
  E$jars <- list.files(system.file("java",package=pkgname),pattern=".*.jar",full.names=TRUE)
  assign("E",E,envir=environmentOfDependingPackage)
  invisible()
}

rscalaLoad <- function(classpath=NULL,...) {
  E <- get("E",envir=parent.env(parent.frame()))
  if ( E$initialized ) return(invisible(FALSE))
  if ( is.null(classpath) ) classpath <- E$jars
  else E$jars <- classpath
  E$s <- scalaInterpreter(classpath=classpath,...)
  E$initialized <- TRUE
  invisible(TRUE)
}

rscalaJar <- function(version="") {
  if ( version == "" ) major.version <- ".*"
  else major.version = substr(version,1,4)
  list.files(system.file("java",package="rscala"),pattern=paste("rscala_",major.version,'-.*[0-9]\\.jar',sep=""),full.names=TRUE)
}

javaCmd <- function(java.home=NULL) {
  if ( is.null(java.home) ) java.home <- ""
  candidate <- normalizePath(file.path(java.home,"bin",sprintf("java%s",ifelse(.Platform$OS=="windows",".exe",""))),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  candidate <- normalizePath(Sys.getenv("JAVACMD"),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  candidate <- normalizePath(file.path(Sys.getenv("JAVA_HOME"),"bin",sprintf("java%s",ifelse(.Platform$OS=="windows",".exe",""))),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  candidate <- normalizePath(Sys.which("java"),mustWork=FALSE)
  if ( file.exists(candidate) ) return(candidate)
  if ( .Platform$OS == "windows" ) {
    readRegistryKey <- function(key.name,value.name) {
      v <- suppressWarnings(system2("reg",c("query",shQuote(key.name),"/v",value.name),stdout=TRUE))
      a <- attr(v,"status")
      if ( (!is.null(a)) && ( a != 0 ) ) return(NULL)
      vv <- v[grep(value.name,v)[1]]
      gsub("(^[[:space:]]+|[[:space:]]+$)", "",strsplit(vv,"REG_SZ")[[1]][2])
    }
    java.key <- "HKEY_LOCAL_MACHINE\\Software\\JavaSoft\\Java Runtime Environment"
    java.version <- readRegistryKey(java.key,"CurrentVersion")
    java.home <- readRegistryKey(sprintf("%s\\%s",java.key,java.version),"JavaHome")
    if ( ! is.null(java.home) ) {
      candidate <- normalizePath(file.path(java.home,"bin",sprintf("java%s",ifelse(.Platform$OS=="windows",".exe",""))),mustWork=FALSE)
      if ( file.exists(candidate) ) return(candidate)
    }
  }
  msg <- sprintf("Cannot find Java executable.  Please set 'java.home' argument, set 'JAVACMD' or 'JAVA_HOME' environment variables, %sadd 'java' to your shell's search path%s.",
    ifelse(.Platform$OS=="windows","","or "),
    ifelse(.Platform$OS=="windows",", or set the appropriate Windows registry keys for your Java installation",""))
  stop(msg)
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

scalaInfo <- function(scala.home=NULL,java.home=NULL,...) {
  cmd <- scalaCmd(scala.home)
  home <- dirname(dirname(cmd))
  libraryJar <- file.path(home,"lib","scala-library.jar")
  jars <- list.files(file.path(home,"lib"),"*.jar",full.names=TRUE)
  if ( ! ( libraryJar %in% jars ) ) stop(sprintf("Found Scala executable (%s) but not its library JAR (%s).",cmd,libraryJar))
  version <- suppressWarnings(
    tryCatch(
      system2(javaCmd(java.home),c("-classpath",shQuote(paste(c(jars,rscalaJar()),collapse=.Platform$path.sep)),"org.ddahl.rscala.Helper"),stdout=TRUE,stderr=FALSE)
    ,error=function(e) {})
  )
  if ( ( is.null(version) ) || ( length(version) != 1 ) ) stop(sprintf("Unable to launch Scala executable (%s).  Perhaps your JAVACMD, JAVA_HOME, or PATH environment variables is not properly set.",cmd))
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

checkType2 <- function(x) {
  if ( is.integer(x) ) "Int"
  else if ( is.double(x) ) "Double"
  else if ( is.logical(x) ) "Boolean"
  else if ( is.character(x) ) "String"
  else stop("Unsupported data type.")
}

convert <- function(x,t) {
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
  c(v,sprintf("intpSet(interpreter,'.',%s,length.one.as.vector=%s,quiet=TRUE)",x,loav))
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

