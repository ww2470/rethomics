#' Checks if the expected column names are all  in a dataframe. 
#' Through error if not.
#' @param expected_colnames the colnmaes that should be in a dt
#' @param cols the actual column names
#' @noRd
checkColumns <- function(expected_colnames, data){
  cols <- colnames(data)
  col_not_found <- expected_colnames[!expected_colnames %in% cols]
  if(length(col_not_found)>0)
    stop(sprintf("The following columns are needed, but not found: %s",col_not_found))  
}

