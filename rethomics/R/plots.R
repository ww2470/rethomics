NULL
#@include
#' Displays, per individual, the temporal average of a variable of interest.
#' 
#' This function produces a tiled representation in which every row represents one individual (i.e. from a unique combination of region and experiement).
#' The x axis represents time in days.
#' The values of the variable of interest are represented by different colour intensity.
#'
#' @param y The variable of interest
#' @param data The data.table containing the data. It must have a column with the same name as y.
#' @param condition An optionnal grouping factor to order rows.
#' @param summary_time_window the width (in seconds) of the time window used to draw each pixel.
#' @param normalise_var_per_id whether each row is to be normalised, using \code{new_y = (y - mean(y))/sd(y)}.
#' @return A \code{ggplot} object that can be plotted directly or modified.

#' @examples
#' # Load sample data
#' data(sleep_validation)
#' my_data <- sleep_validation[,sleepAnnotation(.SD),by=key(sleep_validation)]
#' # No condition
#' p <- overviewPlot(activity,my_data)
#' print(p)
#' # We make a dummy condition, males are in every even region
#' my_data[,sex:=ifelse(region_id %% 2==0,"m","f")]
#' p <- overviewPlot(activity,my_data,condition = sex)
#' print(p)
#' # p is simply a ggplot object, so we can change things:
#' print(p + labs(title="MY own title"))
#' @seealso \code{\link{ethogramPlot}} To show trend by aggregating individuals over time. 
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

NULL
#@include
#' Displays the temporal and inter-individual average of a variable of interest.
#' 
#' This function produces a graph where y is the variable of interest and x, the time. 
#' It can be used to visualise temporal trends per groups of conditions. 
#' The response variable, y,  is grouped by time windows of defined size.
#'
#' @param y The variable of interest
#' @param data The data.table containing the data. It must have a column with the same name as y.
#' @param condition An optionnal grouping factor to order rows.
#' @param summary_time_window the width (in seconds) of the time window used to draw each pixel.
#' @param normalise_var_per_id whether each row is to be normalised (using new_x = {x - mean(x)}/sd(x)).
#' @param error_bar what type of error bar should be used. It should be one of \code{NULL},`sem' or `sd').
#' @return A \code{ggplot} object that can be plotted directly, or modified

#' @examples
#' # Load sample data
#' data(sleep_validation)
#' my_data <- sleep_validation[,sleepAnnotation(.SD),by=key(sleep_validation)]
#' # No condition
#' p <- ethogamPlot(activity,my_data)
#' print(p)
#' # We make a dummy condition, males are in every even region
#' my_data[,sex:=ifelse(region_id %% 2==0,"m","f")]
#' p <- ethogamPlot(activity,my_data,condition = sex)
#' print(p)
#' p <- ethogamPlot(activity,my_data,condition = sex,error_bar="sm")
#' 
#' # p is simply a ggplot object, so we can change things:
#' print(p + labs(title="MY own title"))
#' @seealso \code{\link{oveviewPlot}} To show per-individual patterns
#' @export
ethogramPlot <- function(y,data,
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
      errBarFun <- function(x){
        sd(x)/sqrt(length(x))
      }    
    
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
    if(c_var_name != "NULL"){
      p <- p + geom_ribbon(aes(ymin=y_var-err_var, ymax=y_var+err_var,fill=c_var,colour=NULL),alpha=.3)
    }
    else{
      p <- p + geom_ribbon(aes(ymin=y_var-err_var, ymax=y_var+err_var),alpha=.3)
    }
  }
  p <- p + labs(x="time (day)", y=y_var_name)
  p
}

# 
# A simple pipeline to d
# 
# TODO
# TODO...... . .............. ... . .. . ...... 
# 
# @export
# sleepPlotPipeLine <- function(output,what, condition,summary_time_window=30*60,reference_hour=9.0,...){
#   pdf(output,w=16,h=9)
#   dev.off()
#   
#   dt <- loadPsvData(what, reference_hour=reference_hour, FUN=sleepAnnotation,...)  
#   out <- list()
#   out[[1]] <- overviewPlot(dt,"asleep", condition, summary_time_window)
#   out[[2]] <- overviewPlot(dt,"activity", condition, summary_time_window)
#   out[[3]] <- ethogramPlot(dt,"asleep", condition, summary_time_window)
#   out[[4]] <- ethogramPlot(dt,"activity", condition, summary_time_window)
#   pdf(output,w=16,h=9)
#   lapply(out, print)
#   dev.off()
#   return(dt)
# }
