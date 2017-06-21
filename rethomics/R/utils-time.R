#@include
#' Time conversion utilities
#' 
#' Trivial functions to convert time to seconds -- as `rethomics` uses second as a conventionnal unit of time. 
#' 
#' @details  Given an dummy function that takes time in second like: `myFunction(t)`, 
#' it is always preferqable to call `myFunction(days(1.5))` rather than `myFunction(60*60*24*1.5)`.
#' 
#' @param x Numerical vector to be converted in second
#' @return Number of seconds corresponding to `x` (1day = 86400s, 1h = 3600s and 1min = 60s)
#' @name time_conversion
NULL
#' @rdname time_conversion
#' @export
days <- function(x){
  x * 86400
}
#' @rdname time_conversion
#' @export
hours <- function(x){
  x * 3600
}
#' @rdname time_conversion
#' @export
mins <- function(x){
  x * 60
}

NULL


dateStrToPosix <- function(date,tz="GMT"){
  date_char = ifelse(is.finite(date),as.character(date),date)
  ldt <- lapply(date_char,function(x,tz){as.data.table(parseDateStr(x,tz))},tz)
  rbindlist(ldt)[,date]
}

parseDateStr <- function(str, tz=''){
  if(is.infinite(str))
    return(list(date=str,has_time=TRUE))
  
  
  if(length(str) >1){
    stop("Dates must be checked one by one, you are providing several dates")
  }
  if(! tz %in% c('', OlsonNames()))
    stop("This time zone does not exist. Use `OlsonNames()` to list time zones")
  
  pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}(_[0-9]{2}-[0-9]{2}-[0-9]{2}){0,1}$"
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
