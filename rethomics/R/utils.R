#@include
#' Trivially converts days to seconds
#' 
#' @param x number of seconds
#' @seealso \code{\link{hours}} \code{\link{days}}
#' @export
days <- function(x){
  x * 86400
}

NULL
#' Trivially converts hours to seconds
#' 
#' @param x number of seconds
#' @seealso \code{\link{hours}} \code{\link{days}}
#' @export
hours <- function(x){
  x * 3600
}
NULL
#' Trivially converts minutes to seconds
#' 
#' @param x number of seconds
#' @seealso \code{\link{hours}} \code{\link{days}}
#' @export
mins <- function(x){
  x * 60
}
NULL