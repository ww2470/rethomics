#' TODO
#' 
#' TODO
#' TODO...... . .............. ... . .. . ...... 

#' @export
overviewPlot <- function(data,var,condition,summary_time_window=30*60,normalise_var_per_id=TRUE){
  dt = copy(data)
  setnames(dt,c(var,condition),c("var","condition"))
  dt[,t_r := floor(t/summary_time_window) * summary_time_window]
  dt[,var:=as.numeric(var)]
  summary_dt <- dt[,list(var=mean(var)),
                   by=c("t_r","condition",key(dt))]
  
  if(normalise_var_per_id){
    dt[,var:=(var-mean(var))/mean(var),by=c("condition",key(dt))]
  }
  
  summary_dt[,t_d:=t_r/(3600*24)]
  summary_dt[,row_name:=sprintf("%s@%s@%02d",condition,experiment_id,region_id)]
  p <- ggplot(summary_dt,aes(x=t_d,y=row_name,fill=var)) + geom_tile(alpha=1) +
    labs(title= sprintf("Overview of individual '%s' pattern over time",var),x="time (day)", y="Individual")+
    guides(fill=guide_legend(title=var))
  p
}
#' TODO
#' 
#' TODO
#' TODO...... . .............. ... . .. . ...... 

#' @export
ethogramPlot <- function(data,var,condition,summary_time_window=30*60,normalise_var_per_id=TRUE,error_bar=NULL){
  dt = copy(data)
  error_bar <- "sem"
  setnames(dt,c(var,condition),c("var","condition"))
  dt[,var:=as.numeric(var)]
  dt[,t_r := floor(t/summary_time_window) * summary_time_window]
  
  summary_dt <- dt[,list(var=mean(var)),by=c("t_r","condition",key(dt))]
  
  if(normalise_var_per_id){
    dt[,var:=(var-mean(var))/mean(var),by=c("condition",key(dt))]
  }
  
  if(!is.null(error_bar)){
    if(!error_bar %in% c("sd", "sem"))
      stop("error_bar should can be only one of NULL,'sd' or 'sem'")
    
    if(error_bar == "sd")
      errBarFun <- sd
    
    if(error_bar == "sem")
      errBarFun <- function(x){sd(x)/sqrt(length(x))}    
    
    summary_dt_all_animals <- summary_dt[,list(
      var=mean(var),
      err_var=errBarFun(var)),
      by=.(t_r,condition)]  
  }
  if(is.null(error_bar))
    summary_dt_all_animals <- summary_dt[,list(var=mean(var)),by=.(t_r,condition)] 
  
  summary_dt_all_animals[,t_d:=t_r/(3600*24)]
  
  p <- ggplot(summary_dt_all_animals, aes(t_d,var,colour=condition)) + geom_line() 
  
  p <- p + geom_ribbon(aes(ymin=var-err_var, ymax=var+err_var,fill=condition,colour=NULL),alpha=.3)
  
  p <- p + guides(fill=guide_legend(title=condition))+
    labs(x="time (day)", y=var)
  p
}


#' TODO
#' 
#' TODO
#' TODO...... . .............. ... . .. . ...... 

#' @export
sleepPlotPipeLine <- function(output,what, condition,summary_time_window=30*60,reference_hour=9.0,...){
  pdf(output,w=16,h=9)
  dev.off()
  
  dt <- loadPsvData(what, reference_hour=reference_hour, FUN=sleepAnnotation,...)  
  out <- list()
  out[[1]] <- overviewPlot(dt,"asleep", condition, summary_time_window)
  out[[2]] <- overviewPlot(dt,"activity", condition, summary_time_window)
  out[[3]] <- ethogramPlot(dt,"asleep", condition, summary_time_window)
  out[[4]] <- ethogramPlot(dt,"activity", condition, summary_time_window)
  pdf(output,w=16,h=9)
  lapply(out, print)
  dev.off()
  return(dt)
}
