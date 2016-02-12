#@include
#' Trivially converts days to seconds
#' 
#' @param x number of seconds
#' @return the corresponding number of seconds
#' @seealso \code{\link{hours}} \code{\link{mins}}
#' @export
days <- function(x){
  x * 86400
}

NULL
#' Trivially converts hours to seconds
#' 
#' @param x number of seconds
#' @return the corresponding number of seconds
#' @seealso \code{\link{days}} \code{\link{mins}}
#' @export
hours <- function(x){
  x * 3600
}
NULL
#' Trivially converts minutes to seconds
#' 
#' @param x number of seconds
#' @return the corresponding number of seconds
#' @seealso \code{\link{days}} \code{\link{hours}}
#' @export
mins <- function(x){
  x * 60
}
NULL

dir.exists <- function(d) {
  de <- file.info(d)$isdir
  ifelse(is.na(de), FALSE, de)
}

dateStrToPosix <- function(date,tz="GMT"){
  date_char = ifelse(is.finite(date),as.character(date),date)
  ldt <- lapply(date_char,function(x,tz){as.data.table(parseDateStr(x,tz))},tz)
  rbindlist(ldt)[,date]
}

parseDateStr <- function(str, tz=''){
  if(is.infinite(str))
    return(list(date=str,has_time=TRUE))
  
  pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}(_[0-9]{2}-[0-9]{2}-[0-9]{2}){0,1}$"
  if(length(str) >1){
    stop("Dates must be checked one by one, you are providing several dates")
  }
  match = grep(pattern, str)
  if (length(match) != 1){
    stop(sprintf("Date '%s' is not formated correctly.
                 It must be either 'yyyy-mm-dd' or 'yyyy-mm-dd_hh-mm-ss'",str))
  }
  
  out <- list()
  
  if(nchar(str) == 10){
    date <- as.POSIXct(str, "%Y-%m-%d",tz=tz)
    out$date <- date
    out$has_time <- F
  }
  else{
    date <- as.POSIXct(str, "%Y-%m-%d_%H-%M-%S",tz=tz)
    out$date <- date
    out$has_time <- T
  }
  if(is.na(date)){
    stop(sprintf("Date '%s' seems to be formated correctly,
                 but we cannot read it as a date. 
                 Probably the numbers are wrong (e.g. 2015-30-02 does not exist)",str))
  }
  
  out
}
