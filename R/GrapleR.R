library(httr)
library(RCurl)
library(jsonlite)

validate_url <- function(url){
  valid_url <- FALSE

  valid_url = tryCatch({
    http_error(url)
  }, error = function(e) {
    valid_url <- FALSE
  })
  return(valid_url)
}

check_graple <- function(object) {
  errors <- character()

  input_dir <- character(0)
  results_dir <- character(0)

  if(length(object@GWSURL) > 0)
  {
    valid_url <- validate_url(object@GWSURL)

    if (!valid_url) {
      msg <- paste("Invalid url")
      errors <- c(errors, msg)
    }
  }
  else
  {
    msg <- paste("GWS URL not provided")
    errors <- c(errors, msg)
  }

  if(length(object@ExpRootDir) > 0)
  {
    input_dir <- object@ExpRootDir
    if(!dir.exists(input_dir))
    {
      msg <- paste("Input directory does not exist")
      errors <- c(errors, msg)
    }
  }
  else
  {
    msg <- paste("Path to Experiment Root Directory not provided")
    errors <- c(errors, msg)
  }

  if(length(object@ResultsDir) > 0)
  {
    results_dir <- object@ResultsDir
    if(!dir.exists(results_dir))
    {
      msg <- paste("Results directory does not exist")
      errors <- c(errors, msg)
    }
  }
  else
  {
    msg <- paste("Path to Results Directory not provided")
    errors <- c(errors, msg)
  }

  if((length(input_dir) > 0 && length(results_dir) > 0) && (length(grep(input_dir,results_dir)) || length(grep(results_dir, input_dir)))>0)
  {
    msg <- paste("Input/Result directory is a sub-directory of Result/Input")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

Graple <- setClass("Graple", slots = c(GWSURL = "character", ExpRootDir="character", ResultsDir="character", JobID="character",
                                       StatusCode="numeric", StatusMsg="character", ExpName="character", TempDir="character",
                                       Retention ="numeric"), prototype = list(GWSURL="http://graple.acis.ufl.edu",
                                       TempDir=tempdir()), validity = check_graple)

setGeneric(name="setTempDir",
           def=function(theObject,path)
           {
             standardGeneric("setTempDir")
           }
)


setGeneric(name="setGWSURL",
           def=function(theObject,url)
           {
             standardGeneric("setGWSURL")
           }
)

setGeneric(name="setExperimentDir",
           def=function(theObject,path)
           {
             standardGeneric("setExperimentDir")
           }
)

setGeneric(name="setResultsDir",
           def=function(theObject)
           {
             standardGeneric("setResultsDir")
           }
)

setGeneric(name="GrapleCheckService",
           def=function(theObject)
           {
             standardGeneric("GrapleCheckService")
           }
)

setGeneric(name="GrapleRunExperiment",
           def=function(theObject)
           {
             standardGeneric("GrapleRunExperiment")
           }
)

setGeneric(name="GrapleCheckExperimentCompletion",
           def=function(theObject)
           {
             standardGeneric("GrapleCheckExperimentCompletion")
           }
)

setGeneric(name="GrapleGetExperimentResults",
           def=function(theObject)
           {
             standardGeneric("GrapleGetExperimentResults")
           }
)

setGeneric(name="GrapleRunExperimentSweep",
           def=function(theObject, driverFileName, parameterName, startValue, endValue, numberOfIncrements)
           {
             standardGeneric("GrapleRunExperimentSweep")
           }
)

setGeneric(name="GrapleRunExperimentJob",
           def=function(theObject)
           {
             standardGeneric("GrapleRunExperimentJob")
           }
)

setGeneric(name="GrapleGetExperimentJobResults",
           def=function(theObject)
           {
             standardGeneric("GrapleGetExperimentJobResults")
           }
)

setGeneric(name="GrapleAbortExperiment",
           def=function(theObject)
           {
             standardGeneric("GrapleAbortExperiment")
           }
)

setMethod(f="setTempDir",
          signature="Graple",
          definition=function(theObject,path)
          {
            if(dir.exists(path)){
              theObject@TempDir <- path
              return(theObject)
            }
            else{
              theObject@StatusCode <- -1
              theObject@StatusMsg <- "Temporary directory provided does not exist"
            }
          }
)

setMethod(f="setGWSURL",
          signature="Graple",
          definition=function(theObject,url)
          {
            valid_url <- validate_url(object@GWSURL)

            if (!valid_url) {
              theObject@StatusCode <- -1
              theObject@StatusMsg <- "Invalid url"
            }
            else{
              theObject@GWSURL <- url
              theObject@StatusCode <- 1
              theObject@StatusMsg <- "GWSURL set successfully"
            }

            return(theObject)
          }
)

setMethod(f="setExperimentDir",
          signature="Graple",
          definition=function(theObject,path)
          {
            if(dir.exists(path)){
              theObject@ExpRootDir <- path
              return(theObject)
            }
            else{
              theObject@StatusCode <- -1
              theObject@StatusMsg <- "Experiment directory provided does not exist"
            }
          }
)

setMethod(f="GrapleCheckService",
          signature="Graple",
          definition=function(theObject)
          {
            qurl<-paste(theObject@GWSURL, "service_status", sep="/")
            status<- getURL(qurl)
            theObject@StatusCode <- 1
            theObject@StatusMsg <- paste(toString(fromJSON(status)[1]),toString(fromJSON(status)[2]))
            return(theObject)
          }
)

setMethod(f="GrapleRunExperiment",
          signature="Graple",
          definition=function(theObject, FilterName)
          {
            td<-getwd()
            setwd(theObject@TempDir)
            if(file.exists("sim.tar.gz")) file.remove("sim.tar.gz")
            tarfile = file.path(getwd(), "sim.tar.gz")
            setwd(theObject@ExpRootDir)
            tar(tarfile, ".", compression="gz", compression_level = 6, tar="internal")

            if(missing(FilterName)){
              qurl <- paste(theObject@GWSURL, "GrapleRun", sep="/")
            }
            else{
              qurl <- paste(theObject@GWSURL, "GrapleRun", FilterName, sep="/")
            }
            
            expid = postForm(qurl, files=fileUpload(tarfile))
            theObject@JobID <- toString(fromJSON(expid)[1])
            if(length(theObject@JobID) == 40){
              theObject@StatusCode <- 1
              theObject@StatusMsg <- "The simulation was submitted successfully"
            }

            if (file.exists(tarfile)) file.remove(tarfile)
            setwd(td)
            return (theObject)
          }
)

setMethod(f="GrapleCheckExperimentCompletion",
          signature="Graple",
          definition=function(theObject)
          {
            qurl <- paste(theObject@GWSURL, "GrapleRunStatus", theObject@JobID, sep="/")
            status <- getURL(qurl)
            theObject@StatusCode <- 1
            theObject@StatusMsg <- toString(fromJSON(status)[1])
            return (theObject)
          }
)

setMethod(f="GrapleGetExperimentResults",
          signature="Graple",
          definition=function(theObject)
          {
            dirname <- character(0)
            td<-setwd(theObject@ResultsDir)
            if(length(theObject@ExpName) > 0){
              dir.create(theObject@ExpName)
              dirname <- theObject@ExpName
            }
            else{
              dir.create(theObject@JobID)
              dirname <- theObject@JobID
            }
            qurl<-paste(theObject@GWSURL, "GrapleRunResults", theObject@JobID, sep="/")
            status<- getURL(qurl)
            if(toString(fromJSON(status)[2]) == "success"){
              qurl <- paste(theObject@GWSURL, theObject@JobID, "Results", "output.tar.gz", sep="/")
              resultfile <- file.path(theObject@TempDir,  "results.tar.gz")
              download.file(qurl, resultfile)
              setwd(theObject@TempDir)
              file.copy("results.tar.gz", theObject@ResultsDir)
              file.remove("results.tar.gz")
              setwd(theObject@ResultsDir)
              untar("results.tar.gz")
              file.remove("results.tar.gz")
              files <- list.files(".")
              lapply(files, function(x){untar(x); file.remove(x)})
              setwd(td)
              theObject@StatusCode <- 1
              theObject@StatusMsg <- 'The results have been downloaded to Results directory provided'
              return(theObject)
            }
          }
)

setMethod(f="GrapleRunExperimentSweep",
          signature="Graple",
          definition=function(theObject, driverFileName, parameterName, startValue, endValue, numberOfIncrements)
          {
            td<-getwd()
            setwd(theObject@TempDir)
            if(file.exists("sim.tar.gz")) file.remove("sim.tar.gz")
            tarfile = file.path(theObject@ExpRootDir, "sim.tar.gz")
            setwd(theObject@ExpRootDir)
            tar(tarfile, ".", compression="gz", compression_level = 6, tar="internal")

            qurl <- paste(theObject@GWSURL, "GrapleRunMetOffset", sep="/")

            status <- postForm(qurl, files=fileUpload(tarfile))
            print(fromJSON(status))
            theObject@JobID <- toString(fromJSON(status)[1])
            expid <- substr(status[1], start=13, stop=52)

            if (file.exists(tarfile)) file.remove(tarfile)

            if(missing(FilterName)){
              params <- paste(theObject@JobID, driverFileName, parameterName, startValue, endValue, numberOfIncrements, sep="*")
            }
            else
            {
              params <- paste(theObject@JobID, driverFileName, parameterName, startValue, endValue, numberOfIncrements, FilterName, sep="*")
            }
            qurl <- paste(theObject@GWSURL, "TriggerSimulation", params, sep="/")
            theObject@StatusMsg = toString(fromJSON(postForm(qurl, t="none")))
            #if(status <> "Success") print("Failed to start experiment")
            setwd(td)
            return (theObject)
          }
)

setMethod(f="GrapleRunExperimentJob",
          signature="Graple",
          definition=function(theObject)
          {
            td<-getwd()
            unlink("../tempGRAPLE", recursive = TRUE)
            dir.create("../tempGRAPLE")
            setwd("../tempGRAPLE")
            tarfile = file.path(getwd(), "sweepexp.tar.gz")
            setwd(simDir)
            tar(tarfile, ".", compression="gz", compression_level = 6, tar="internal")
            if(missing(FilterName)){
              qurl <- paste(theObject@GWSURL, "GrapleRunMetSample", sep="/")
            }
            else{
              qurl <- paste(theObject@GWSURL, "GrapleRunMetSample", FilterName, sep="/")
            }

            status <- postForm(qurl, files=fileUpload(tarfile))
            if (file.exists(tarfile)) file.remove(tarfile)
            unlink("../tempGRAPLE", recursive = TRUE)

            print(fromJSON(status))

            pid <- substr(status[1], start=57, stop=96)
            setwd(td)
            return (expid)
          }
)

setMethod(f="GrapleGetExperimentJobResults",
          signature="Graple",
          definition=function(theObject)
          {
            td<-getwd()
            qurl <- paste(theObject@GWSURL, "GrapleRunResultsMetSample", theObject@JobID, sep="/")
            status<- getURL(qurl)

            qurl <- paste(theObject@GWSURL, theObject@JobID, "Results", "output.tar.gz", sep="/")
            setwd(theObject@TempDir)
            resultfile <- file.path(theObject@TempDir,  "results.tar.gz")
            download.file(qurl, resultfile)
            file.copy("results.tar.gz", theObject@ResultsDir)
            file.remove("results.tar.gz")
            setwd(theObject@ResultsDir)
            untar("results.tar.gz")
            file.remove("results.tar.gz")
            files <- list.files(".", pattern = "\\.bz2\\.tar$")
            lapply(files, function(x){untar(x); file.remove(x)})
            setwd(td)
            theObject@StatusCode <- 1
            theObject@StatusMsg <- 'The results have been downloaded to Results directory provided'
            return(resultfile)
          }
)

setMethod(f="GrapleAbortExperiment",
          signature="Graple",
          definition=function(theObject)
          {
            qurl <- paste(theObject@GWSURL, "GrapleAbort", theObject@JobID, sep="/")
            status<- getURL(qurl)
            theObject@StatusMsg <- fromJSON(status)
            return (theObject)
          }
)

