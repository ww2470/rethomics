
library(data.table)

# fetchPsvResultFiles <- function(result_dir,q=NULL){
#   key <- c("time","machine_name")
#   use_date <- F
#   
#   if(!is.null(q)){
#     query <- copy(q)      
#     if(!("time" %in% colnames(query)))
#       stop("Query MUST have a `time` column")
#     
#     t <- query[,as.POSIXct(time, "%Y-%m-%d_%H-%M-%S", tz="GMT")]
#     if(any(is.na(t)))
#       warning("Some time stamps could not be converted to time. using date instead")    
#       t <- query[,as.POSIXct(time, "%Y-%m-%d", tz="GMT")]
#       use_date <- T
#     
#     query[,time :=t]
#     setkeyv(query,key)
#   }
#   
#   all_db_files <- list.files(result_dir,recursive=T, pattern="*.db")
# 
#   fields <- strsplit(all_db_files,"/")
#   valid_files <- sapply(fields,length) == 4
#   
#   all_db_files <- all_db_files[valid_files]
#   files_info <- do.call("rbind",fields[valid_files])
#   files_info <- as.data.table(files_info)
#   setnames(files_info, c("machine_id", "machine_name", "time","file"))
#   if(use_date)
#     files_info[,time:=as.POSIXct(time, "%Y-%m-%d", tz="GMT")]
#   else
#     files_info[,time:=as.POSIXct(time, "%Y-%m-%d_%H-%M-%S", tz="GMT")]
#   
#   files_info[,path := paste0(result_dir,all_db_files)]
#   
#   setkeyv(files_info,key)
#     
#   if(is.null(q))
#     return(files_info)
#   out <- files_info[query]
#   setkeyv(out,colnames(query))
#   
#   dups_i <- duplicated(out,fromLast=T)
#   dups <- out[dups_i]
# 
#   if(nrow(dups) > 0){
#     str <- "Duplicated queries. Excluding the following files:"
#     str_v <- sprintf("%s, %s",dups[,machine_name],dups[,time])
#     str_e <- "The LATEST files were kept"
#     warning(paste(c(str, str_v, str_e), sep="\n"))
#   }
#   
#   cols <- unique(c("path",key(out),colnames(q)))
#   
#   out <- out[!dups_i,cols,with=F]
#   
#   nas <- is.na(out[,path]) 
#   if(any(nas)){
#     out_nas <- out[nas,]
#     for(i in nrow(out_nas)){
#       warning(sprintf("No result for machine_name == %s and time == %s. Omiting query",out_nas[i,machine_name],out_nas[i,time])) 
#     }
#   }
#   na.omit(out)
# }
# 

#the package live at `https://github.com/qgeissmann/rethomics`
# 1.install package named `devtools`
# 2. install package named `devtools`
# 3. library(devtools)
# 4. install_github("qgeissmann/rethomics", subdir="rethomics")
# 5. library(rethomics)

## in oder to check the package:
#path <- loadSampleData("validation.db")
#dt <- loadPsvData(path)
##voila!!


library(rethomics)
library(ggplot2)



map<- data.table(path=rep(c(
                      "/data/psv_results/00056dfce6e94dee9bb1a845281b086e/GGSM-005/2015-05-06_15-50-48/2015-05-06_15-50-48_00056dfce6e94dee9bb1a845281b086e.db",
                      "/data/psv_results/00066dfce6e94dee9bb1a845281b086e/GGSM-006/2015-05-06_15-51-39/2015-05-06_15-51-39_00066dfce6e94dee9bb1a845281b086e.db")
                      ,each=32),
                    region_id=rep(1:32,length.out=64))

map[,infected:=ifelse(region_id>16,F,T)]


dd <- loadPsvData(map,FUN=sleepAnnotation,reference_hour = 9)

overviewPlot(dd,"activity","infected")
overviewPlot(dd,"activity","infected",normalise_var_per_id = F)


# quick and lifespan analysis. TODO curate data first
ls_dt <- dd[,list(lifespan_day=max(t)/(24*3600)),
            by=c(key(dt),"infected")]
ggplot(ls_dt, aes(x=infected, y=lifespan_day,fill=infected)) + geom_boxplot()

ethogramPlot(dd,"activity","infected")

dd_cur <- subset(dd, region_id != 25 | experiment_id != "2015-05-06_15-51-39_00066dfce6e94dee9bb1a845281b086e.db")

ethogramPlot(dd_cur,"activity","infected")
ethogramPlot(dd_cur,"asleep","infected")


p <- ggplot(dd_cur[asleep==T,], aes(x_rel, colour=sex)) + geom_density()

dd_cur[,x_rel:=ifelse(region_id > 16, 1-x,x)]

ggplot(dd_cur[asleep==T & t < 4*24*3600,], aes(x_rel, colour=infected)) + geom_density()
ggplot(dd_cur[asleep==F,], aes(x_rel, colour=infected)) + geom_density()

ggplot(
  dd[region_id == 25 & experiment_id == "2015-05-06_15-51-39_00066dfce6e94dee9bb1a845281b086e.db",],
  aes(t,x)) + geom_line()
  
dd_i25 <- dd[region_id == 25 & experiment_id == "2015-05-06_15-51-39_00066dfce6e94dee9bb1a845281b086e.db",]
day <- function(nd){
  return(3600*24*nd)
}
ggplot(dd_i25[t> day(4)],
       aes(x, colour=infected)) + geom_density()
mean(dd_i25[t> day(4), w])

#sleepPlotPipeLine()