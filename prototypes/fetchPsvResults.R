
library(data.table)

fetchPsvResultFiles <- function(result_dir,q=NULL){
  key <- c("time","machine_name")
  use_date <- F
  
  if(!is.null(q)){
    query <- copy(q)      
    if(!("time" %in% colnames(query)))
      stop("Query MUST have a `time` column")
    
    t <- query[,as.POSIXct(time, "%Y-%m-%d_%H-%M-%S", tz="GMT")]
    if(any(is.na(t)))
      warning("Some time stamps could not be converted to time. using date instead")    
      t <- query[,as.POSIXct(time, "%Y-%m-%d", tz="GMT")]
      use_date <- T
    
    query[,time :=t]
    setkeyv(query,key)
  }
  
  all_db_files <- list.files(result_dir,recursive=T, pattern="*.db")

  fields <- strsplit(all_db_files,"/")
  valid_files <- sapply(fields,length) == 4
  
  all_db_files <- all_db_files[valid_files]
  files_info <- do.call("rbind",fields[valid_files])
  files_info <- as.data.table(files_info)
  setnames(files_info, c("machine_id", "machine_name", "time","file"))
  if(use_date)
    files_info[,time:=as.POSIXct(time, "%Y-%m-%d", tz="GMT")]
  else
    files_info[,time:=as.POSIXct(time, "%Y-%m-%d_%H-%M-%S", tz="GMT")]
  
  files_info[,path := paste0(result_dir,all_db_files)]
  
  setkeyv(files_info,key)
    
  if(is.null(q))
    return(files_info)
  out <- files_info[query]
  setkeyv(out,colnames(query))
  
  dups_i <- duplicated(out,fromLast=T)
  dups <- out[dups_i]

  if(nrow(dups) > 0){
    str <- "Duplicated queries. Excluding the following files:"
    str_v <- sprintf("%s, %s",dups[,machine_name],dups[,time])
    str_e <- "The LATEST files were kept"
    warning(paste(c(str, str_v, str_e), sep="\n"))
  }
  
  cols <- unique(c("path",key(out),colnames(q)))
  
  out <- out[!dups_i,cols,with=F]
  
  nas <- is.na(out[,path]) 
  if(any(nas)){
    out_nas <- out[nas,]
    for(i in nrow(out_nas)){
      warning(sprintf("No result for machine_name == %s and time == %s. Omiting query",out_nas[i,machine_name],out_nas[i,time])) 
    }
  }
  na.omit(out)
}



# fetchPsvResultFiles(result_dir="/data/psv_results/",get_list=T)

# query <- data.table(time=c("2015-05-15_12-53-55"), machine_name=c("GGSM-005"),condition="blue",region_id=4:9)
# t<- fetchPsvResultFiles(result_dir="/data/psv_results/",query)

query <- data.table(time=c("2015-05-15"), machine_name=c("GGSM-005"),condition=c("blue","red"),region_id=4:9)
t<- fetchPsvResultFiles(result_dir="/data/psv_results/",query)
