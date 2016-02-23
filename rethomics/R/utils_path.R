#@include
dir.exists <- function(d) {
  de <- file.info(d)$isdir
  ifelse(is.na(de), FALSE, de)
}


checkDirExists <- function(dir){
  if(!dir.exists(dir))
    stop(sprintf("The directory %s does not exist",dir))  
}


NULL

getExtaDataDir <- function(){
  result_dir <- paste(path.package("rethomics"),"extdata/",sep='/')
  if(!dir.exists(result_dir))
    result_dir <- paste(path.package("rethomics"),"inst/extdata/",sep='/')
  if(!dir.exists(result_dir))
    stop("Could not f
         ind extra package data")
  return(result_dir)
  
}

#' Get the absolute path to a sample file.
#' 
#' This function is only for testing (and trying) purposes.
#' It provides a way to access raw data  (e.g. db files and dam text files) contained within this package.
#' @param path The relative path and name  of the samples to be loaded. When \code{path} is \code{NULL}, 
#' the function returns the list of all available samples.
#' @param get_sample_dir whether the function return the root directory of the sample data, nistead of sample files. 
#' @seealso \code{\link{loadEthoscopeData}} to read raw experimental data. 
#' @export
getSampleDataPath <- function(path=NULL, get_sample_dir=FALSE){
  result_dir <- getExtaDataDir()
  if(get_sample_dir)
    return(result_dir)
  
  if(is.null(path)){
    return(list.files(result_dir, recursive = T))
  }
  abs_path <- paste(result_dir, path, sep="/")
  if(!file.exists(abs_path))
    stop(sprintf("The file %s does not exist! Try listing files with `path=NULL`.", path))
  return(abs_path)
}
NULL
