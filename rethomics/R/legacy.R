
replacedFunctionError <- function(new_name){
  help(new_name)
  stop(sprintf("This function does not exist any longer. please use `%s' instead",new_name))
}

#' @export
loadPsvData <- function(...){replacedFunctionError("loadEthoscopeData")}
#' @export
loadMetaData  <- function(...){replacedFunctionError("loadEthoscopeMetaData")}
#' @export
fetchDAMData <- function(...){replacedFunctionError("loadDailyDAM2Data")}
#' @export
queryDAMData  <- function(...){replacedFunctionError("loadDAM2Data")}
#' @export
queryDAMFiles  <- function(...){replacedFunctionError("loadDAM2Data")}
