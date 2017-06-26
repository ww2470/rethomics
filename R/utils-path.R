dir.exists <- function(d) {
  de <- file.info(d)$isdir
  ifelse(is.na(de), FALSE, de)
}

checkDirExists <- function(dir){
  if(!dir.exists(dir))
    stop(sprintf("The directory %s does not exist",dir))  
}

