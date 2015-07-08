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
