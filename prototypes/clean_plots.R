rm(list=ls())



path <- loadSampleData("validation.db")
my_data <- loadPsvData(path,FUN=sleepAnnotation)


overviewPlot(activity,my_data,condition )
my_data[,sex:=ifelse(region_id %% 2==0,"m","f")]
overviewPlot2(activity,my_data,condition = sex)
p <- overviewPlot2(activity,my_data,condition = sex)
p + labs(title="MY own title")

#' @export

overviewPlot <- function(y,data,
                         condition=NULL,
                         summary_time_window=mins(30),
                         normalise_var_per_id=TRUE){
  
  
  dt = copy(as.data.table(data))  
  y_var_name <- deparse(substitute(y))
  setnames(dt,y_var_name,"y_var")
  dt[,t_r := floor(t/summary_time_window) * summary_time_window]
  dt[,y_var:=as.numeric(y_var)]
  c_var_name <- deparse(substitute(condition))
  
  if(c_var_name == "NULL")
    dt[,c_var:=TRUE]
  else
    setnames(dt, c_var_name,"c_var")
  
  if(normalise_var_per_id)
    dt <- na.omit(dt[,y_var:=as.vector(scale(y_var)),by=key(dt)])
  
  summary_dt <- dt[,list(y_var=mean(y_var)),
                   by=c("t_r","c_var",key(dt))]
  summary_dt[,t_d:=t_r/days(1)]
  
  if(c_var_name != "NULL"){
    summary_dt[,row_name:=sprintf("%s | %s | %02d",c_var,experiment_id,region_id)]
    y_lab <- sprintf("Individual (%s | experiment_id | region_id)", c_var_name)
  }
  else{
    summary_dt[,row_name:=sprintf("%s | %02d",experiment_id,region_id)]
    y_lab <- "Individual (experiment_id | region_id)"
  }
  
  p <- ggplot(summary_dt,aes(x=t_d,y=row_name,fill=y_var)) + geom_tile(alpha=1) +
    labs(title= sprintf("Overview of individual '%s' pattern over time",y_var),x="time (day)", y=y_lab)+
    guides(fill=guide_legend(title=y_var_name))
  p
}



#' TODO
#' 
#' TODO
#' TODO...... . .............. ... . .. . ...... 

#' @export
#ethogramPlot <- function(data,var,condition,summary_time_window=30*60,normalise_var_per_id=TRUE,error_bar=NULL){
ethogramPlot2 <- function(y,data,
                         condition=NULL,
                         summary_time_window=mins(30),
                         normalise_var_per_id=FALSE,
                         error_bar=NULL){
  
  dt = copy(as.data.table(data))  
  y_var_name <- deparse(substitute(y))
  setnames(dt,y_var_name,"y_var")
  dt[,t_r := floor(t/summary_time_window) * summary_time_window]
  dt[,y_var:=as.numeric(y_var)]
  c_var_name <- deparse(substitute(condition))
  
  if(c_var_name == "NULL")
    dt[,c_var:=TRUE]
  else
    setnames(dt, c_var_name,"c_var")
  
  if(normalise_var_per_id)
    dt <- na.omit(dt[,y_var:=as.vector(scale(y_var)),by=key(dt)])
  
  summary_dt <- dt[,list(y_var=mean(y_var)),
                     by=c("t_r","c_var",key(dt))]
  
  
  summary_dt[,t_d:=t_r/days(1)]
  
  if(!is.null(error_bar)){
    if(!error_bar %in% c("sd", "sem"))
      stop("error_bar should can be only one of NULL,'sd' or 'sem'")
    
    if(error_bar == "sd")
      errBarFun <- sd
    
    if(error_bar == "sem")
      errBarFun <- function(x){sd(x)/sqrt(length(x))}    
    
    summary_dt_all_animals <- summary_dt[,list(
      y_var=mean(y_var),
      err_var=errBarFun(y_var)),
      by=.(t_r,c_var)]  
  }
  if(is.null(error_bar))
    summary_dt_all_animals <- summary_dt[,list(y_var=mean(y_var)),by=.(t_r,c_var)] 
  
  summary_dt_all_animals[,t_d:=t_r/days(1)]
  
  if(c_var_name != "NULL"){
    p <- ggplot(summary_dt_all_animals, aes(t_d,y_var,colour=c_var)) + geom_line() 
    p <- p + guides(fill=guide_legend(title=c_var_name))
   }
  else{
    p <- ggplot(summary_dt_all_animals, aes(t_d,y_var)) + geom_line() 
  }
  
  
  if(!is.null(error_bar)){
    if(c_var != "NULL"){
    p <- p + geom_ribbon(aes(ymin=y_var-err_var, ymax=y_var+err_var,fill=c_var,colour=NULL),alpha=.3)
    }
    else{
      p <- p + geom_ribbon(aes(ymin=y_var-err_var, ymax=y_var+err_var),alpha=.3)
    }
  }
  p <- p + labs(x="time (day)", y=c_var_name)
  p
}



# path <- loadSampleData("validation.db")
# )
,FUN=sleepAnnotation)
my_data

ethogramPlot2(activity,my_data)
ethogramPlot2(activity,my_data,error_bar="sd")
ethogramPlot2(activity,my_data,sex,error_bar="sem")
my_data[,sex:=ifelse(region_id %% 2==0,"m","f")]

ethogramPlot2(asleep,my_data,sex,error_bar="sem",normalise_var_per_id = T)
ethogramPlot2(asleep,my_data,sex,error_bar="sem",normalise_var_per_id = F)


overviewPlot(activity,my_data,condition = sex,n=F)
overviewPlot(activity,my_data,n=F)
overviewPlot2(asleep,my_data,condition = sex,n=F)
p + labs(title="MY own title")

