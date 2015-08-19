NULL
#@include
#' Displays, per individual, the temporal average of a variable of interest.
#' 
#' This function produces a tiled representation in which every row represents one individual (i.e. from a unique combination of region and experiment).
#' The x axis represents time in days.
#' The values of the variable of interest are represented by different colour intensities.
#'
#' @param y The variable of interest
#' @param data The data.table containing the data. It must have a column with the same name as y.
#' @param condition An optional grouping factor to order rows.
#' @param summary_time_window the width (in seconds) of the time window used to draw each pixel.
#' @param normalise_var_per_id whether each row is to be normalised, using \code{new_y = (y - mean(y))/sd(y)}.
#' @return A \code{ggplot} object that can be plotted directly or modified.

#' @examples
#' # Load sample data, it is already annotated for sleep, has sex=='male' or sex=="female"
#' data(sleep_sexual_dimorphism)
#' my_data <- sleep_sexual_dimorphism
#' # let us have a look of the max velocity as a measure of activity
#' p <- overviewPlot(max_velocity,my_data)
#' print(p)
#' # what about sleep amount?
#' p <- overviewPlot(asleep,my_data)
#' print(p)
#' # we can also group by condition. For instance by sex:
#' p <- overviewPlot(asleep,my_data,condition = sex)
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
    labs(title= sprintf("Overview of individual '%s' pattern over time",y_var_name),x="time (day)", y=y_lab)+
    guides(fill=guide_legend(title=y_var_name))
  p
}

NULL
#@include
#' Displays the temporal and inter-individual average of a variable of interest.
#' 
#' This function produces a graph where the variable of interest and time are on the y and x axes, respectively. 
#' It can be used to visualise temporal trends per groups of conditions. 
#' The response variable, y,  is grouped by time windows of defined size.
#'
#' @param y The variable of interest.
#' @param data The data.table containing the data. It must have a column with the same name as y.
#' @param condition An optional grouping factor to order rows.
#' @param facet_var An optional grouping factor to draw group in each row of a faceted plot
#' @param summary_time_window the width (in seconds) of the time window used to draw each ``pixel''.
#' @param normalise_var_per_id whether each row is to be normalised (using \code{new_x = (x - mean(x))/sd(x)}).
#' @param error_bar what type of error bar should be used see details.
#' @return A \code{ggplot} object that can be plotted directly, or modified.
#' @details 
#' At the moment, four types of error bars are supported:
#' \itemize{
#'  \item{`sd' }{The standard error}
#'  \item{`sem' }{The standard error of the mean (\emph{i.e.} \eqn{\frac{sd}{\sqrt{n}}})}
#'  \item{`gauss_ci' }{The gaussian 95\% confidence interval (\emph{i.e.} \eqn{1.96 \cdot{} \frac{sd}{\sqrt{n}}})}
#'  \item{`boot_ci' }{A standard 95\% bootstrap resampling confidence interval.
#'                   This is done over 5000 replicates. This can be quite \emph{slow}, but is often more statistically sound.}
#' }

#' @examples
#' data(sleep_sexual_dimorphism)
#' my_data <- sleep_sexual_dimorphism
#' # Fraction of animal asleep over time:
#' p <- ethogramPlot(asleep,my_data)
#' # We would like to show that per group:
#' p <- ethogramPlot(asleep,my_data,condition=sex)
#' print(p)
#' # We can also put error bars:
#' p <- ethogramPlot(asleep,my_data,condition=sex,error_bar="sem")
#' print(p)
#' # we can also use a condition to split data per row (ggplot faceting):
#' p <- ethogramPlot(asleep,my_data,condition=sex,facet_var=experiment_id,error_bar="sem")
#' print(p)
#' # p is simply a ggplot object, so we can change things:
#' print(p + labs(title="MY own title"))
#' # Let us play with several error bars:
#' p <- ethogramPlot(asleep,my_data,condition=sex,error_bar="sd")
#' p
#' p <- ethogramPlot(asleep,my_data,condition=sex,error_bar="sem")
#' p
#' p <- ethogramPlot(asleep,my_data,condition=sex,error_bar="gauss_ci")
#' p
#' # this one is a bit slow
#' p <- ethogramPlot(asleep,my_data,condition=sex,error_bar="boot_ci")
#' p
#' @seealso \code{\link{overviewPlot}} to show per-individual patterns
#' @export
ethogramPlot <- function(y,data,
                          condition=NULL,
                          facet_var=NULL,
                          summary_time_window=mins(30),
                          normalise_var_per_id=FALSE,
                          error_bar=NULL){
  
  dt = copy(as.data.table(data))  
  y_var_name <- deparse(substitute(y))
  setnames(dt,y_var_name,"y_var")
  dt[,t_r := floor(t/summary_time_window) * summary_time_window]
  dt[,y_var:=as.numeric(y_var)]
  c_var_name <- deparse(substitute(condition))
  f_var_name <- deparse(substitute(facet_var))
  
  if(c_var_name == "NULL")
    dt[,c_var:=TRUE]
  else
    setnames(dt, c_var_name,"c_var")
  
  if(f_var_name == "NULL")
    dt[,f_var:=TRUE]
  else
    setnames(dt, f_var_name,"f_var")
  
  if(is.numeric(dt[,c_var])){
    dt[,c_var = as.character(c_var)] 
    warning("Condition variable is a number.
             Converting it to a factor")
  }
   
  if(is.numeric(dt[,f_var])){
    dt[,c_var = as.character(f_var)] 
    warning("Faceting variable is a number. 
             Converting it to a factor")
  }
  
  if(normalise_var_per_id)
    dt <- na.omit(dt[,y_var:=as.vector(scale(y_var)),by=key(dt)])
  
  summary_dt <- dt[,list(y_var=mean(y_var)),
                   by=c("t_r","c_var","f_var",key(dt))]
  
  
  summary_dt[,t_d:=t_r/days(1)]
  
  if(!is.null(error_bar)){
    if(!error_bar %in% c("sd", "sem", "boot_ci", "gauss_ci"))
      stop("error_bar should can be only one of NULL,'sd, 'sem', 'gauss_ci' or 'boot_ci' ")
    
    if(error_bar == "sd")
      errBarFun <- plusMinusSd

    if(error_bar == "sem")
      errBarFun <-plusMinusSem
    
    if(error_bar == "gauss_ci")
      errBarFun <-gaussianCi
    
    if(error_bar == "boot_ci")
      errBarFun <- bootCi
    
    
    summary_dt_all_animals <- summary_dt[,c(
      y_var=mean(y_var),
      errBarFun(y_var)),
      by=.(t_r,c_var,f_var)]  
    
  }
  if(is.null(error_bar))
    summary_dt_all_animals <- summary_dt[,list(y_var=mean(y_var)),by=.(t_r,c_var,f_var)] 
  
  summary_dt_all_animals[,t_d:=t_r/days(1)]
  
  if(c_var_name != "NULL"){
    p <- ggplot(summary_dt_all_animals, aes(t_d,y_var,colour=c_var,fill=c_var)) + geom_line() 
  }
  else{
    p <- ggplot(summary_dt_all_animals, aes(t_d,y_var)) + geom_line() 
  }
  
  
  if(!is.null(error_bar)){
    if(c_var_name != "NULL"){
      p <- p + geom_ribbon(aes(ymin=lower, ymax=higher,colour=NULL),alpha=.3)
    }
    else{
      p <- p + geom_ribbon(aes(ymin=lower, ymax=higher),alpha=.3)
    }
  }
  
  p <- p + labs(title= sprintf("Average '%s' over time",y_var_name),x="time (day)", y=y_var_name)
  p <- p + guides(fill=guide_legend(title=c_var_name),
                  colour=guide_legend(title=c_var_name))
  
  if(f_var_name != "NULL"){
    p <- p + facet_grid(f_var ~ .)
  }
  p
}


plusMinusSd <- function(x){
  m <- mean(x)
  s <- sd(x)
  list(lower = m - s, higher = m +s)
}
plusMinusSem <- function(x){
  m <- mean(x)
  s <- sd(x)/sqrt(length(x)) 
  list(lower = m - s, higher = m +s)
}

gaussianCi <- function(x){
  m <- mean(x)
  s <- 1.96 * sd(x)/sqrt(length(x)) 
  list(lower = m - s, higher = m +s)
}


bootCi <- function(x,
                   r=5000,
                   ci=0.95){
  v <- replicate(r, mean(sample(x,replace=T)))
  ci <- quantile(v,c(1-ci,ci))
  out <-list(lower = ci[1],
             higher = ci[2])
  out
}
