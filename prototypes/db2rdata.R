rm(list=ls())



sqliteTableToDataTable <- function(name,connection){
  print(sprintf("Loading %s",name))
  dt <- dbGetQuery(connection, sprintf("SELECT * FROM %s",name))
  dt <- as.data.table(dt)
  dt
}

sqliteToRdb <- function(input_db, output_rdb){
  con <- dbConnect(SQLite(), input_db)
  
  list_of_table <- dbGetQuery(con,"SELECT name FROM sqlite_master WHERE type='table'")$name
  dt_list <- lapply(list_of_table, sqliteTableToDataTable,con)
  names(dt_list) <- list_of_table
  rdata_file <- sprintf("%s.RData",output_rdb)
  en <- list2env(dt_list)
  save(list=names(dt_list),envir = en, file=rdata_file)
  

  en <- local({load(rdata_file); environment()})  
  tools:::makeLazyLoadDB(en, output_rdb, compress=FALSE)
}


loadOneROIRDB <- function( FILE,  region_id, min_time=0, max_time=Inf,  reference_hour=NULL, columns = NULL){
  lazyLoad(FILE)  
  metadata <- METADATA
  var_map <- VAR_MAP
  setkey(var_map, var_name)
  roi_map <- ROI_MAP
  
  roi_row <- roi_map[roi_idx == region_id,]
  if(nrow(roi_row) == 0 ){
    warning(sprintf("ROI %i does not exist, skipping",region_id))
    return(NULL)
  }
  
  min_time <- min_time * 1000 
  roi_dt <- eval(as.symbol(sprintf("ROI_%i",region_id)))
  roi_dt[,region_id := region_id]
  if(!is.null(reference_hour)){
    p <- metadata$date_time
    hour_start <- as.numeric(format(p, "%H")) + as.numeric(format(p, "%M")) / 60 +  as.numeric(format(p, "%S")) / 3600
    ms_after_ref <- ((hour_start - reference_hour) %% 24) * 3600 * 1000
    roi_dt[, t:= (t + ms_after_ref) ]
  }
  
  roi_width <- max(c(roi_row[,w], roi_row[,h]))
  
  if(!is.null(columns)){
    columns_to_remove <- var_map$var_name[!var_map$var_name %in% columns]
    columns_to_remove <- c(columns_to_remove,"id")
    roi_dt[, (columns_to_remove) := NULL]
  }
  
  for(var_n in intersect(var_map$var_name,colnames(roi_dt))){
    if(var_map[var_n, functional_type] == "distance"){
      roi_dt[, (var_n) := get(var_n) / roi_width]
      }
    else if(var_map[var_n, sql_type] == "BOOLEAN"){
      roi_dt[, (var_n) := as.logical(get(var_n))]
      }
  }
  
  roi_dt[, t:= t/1e3]
  return(roi_dt)
}


library(rethomics)
#dt_list <- sqliteToRdb("/data/testrda/test_rdata.db","/data/testrda/test/my_file")

system.time({
for (i in 1:20){
  print(i)
  dt1 <- loadOneROIRDB("/data/testrda/test/my_file", region_id = i)#,columns=c("xy_dist_log10x1000","x", "has_interacted"))
  #dt <- loadOneROIRDB("/home/quentin/Desktop/test/my_file", region_id = i  )
  #dt <- loadOneROIRDB("/tmp/test/my_file", region_id =i, columns=c("xy_dist_log10x1000","x", "has_interacted"))
}
  })

system.time({
for(i in 1:20){
  print(i)
  dt2 <- rethomics:::loadOneROI("/data/testrda/test_rdata.db",i)#,columns=c("xy_dist_log10x1000","x", "has_interacted"))
}
}
)

