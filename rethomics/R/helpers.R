#' Retrieves sample/example data contained within in this package.
#' 
#' This function is only for testing (and trying) purposes.
#' It provides a way to access raw data  (e.g. db files) contained within this package.
#' @param names The name of the samples to be loaded. When \code{names} is \code{NULL}, 
#' the function returns the list of all available samples.
#' @seealso \code{\link{loadEthoscopeData}} to obtain raw experimental data. 
#' @export
loadSampleData <- function(names=NULL){
  db_file <- system.file("data/db_files.tar.xz", package="rethomics")
  
  if(is.null(names)){
    content <- untar(db_file, list=T)
    db_files <- content
    out <- basename(content)[dirname(content) != '.']
    return(out)
  }
  
  d <- tempdir()
  file_name <- file.path("db_files",names)
  r <- untar(db_file, files=file_name,exdir=d)
  if(r == 2){
    unlink(d, recursive=T)
    stop("INVALID FILE NAME. List available files using `list=TRUE`")
  }
  out <-file.path(d,file_name)
  warning("Do not, forget to unlink file")
  return(out)
}
NULL

#' Checks if the expected columns are all in a given character vector. Through error if not
checkColumns <- function(expected_colnames, cols){
  col_not_found <- expected_colnames[!expected_colnames %in% cols]
  if(length(col_not_found)>0)
    stop(sprintf("The following columns are needed, but not found: %s",col_not_found))  
}

checkDirExists <- function(dir){
  if(!dir.exists(dir))
    stop(sprintf("The directory %s does not exist",dir))  
}