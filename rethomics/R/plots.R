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
#' @param time_wrap the time (in seconds) used to wrap the data (see details).
#' @param time_unit_conversion a function to convert time in the x axis. typically, \code{days}, \code{hours} or \code{mins}.
#' @details \code{time_wrap} is typically used to express time relatively to the start of the the day.
#' In other words, it can help be used to pull all days together in one representative day. In this case, \code{time_wrap=hours(24)}`.
#' @return A \code{ggplot} object that can be plotted directly or modified.
#' 

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
#' ##### time wrapping example
#' data(dam_data)
#' # the original plot:
#' p <- overviewPlot(activity,dam_data)
#' p
#' # summarise/wrap activity in one `day'
#' p <- overviewPlot(activity,dam_data,time_wrap=hours(24))
#' p
#' ######expresses time in hours:
#' p <- overviewPlot(activity,dam_data, time_unit_conversion=hours)
#' p
#' 
#' @seealso \code{\link{ethogramPlot}} To show trend by aggregating individuals over time. 
#' @export
overviewPlot <- function(y,data,
                         condition=NULL,
                         summary_time_window=mins(30),
                         normalise_var_per_id=FALSE,
                         time_wrap=NULL,
                         time_unit_conversion=days){
  
  dt = copy(as.data.table(data))  
  y_var_name <- deparse(substitute(y))
  setnames(dt,y_var_name,"y_var")
  dt[,t_r := floor(t/summary_time_window) * summary_time_window]
  if(!is.null(time_wrap))
    dt[,t_r := t_r %% time_wrap]
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
  summary_dt[,t_d:=t_r/time_unit_conversion(1)]
  
  if(c_var_name != "NULL"){
    summary_dt[,row_name:=sprintf("%s | %s | %02d",c_var,experiment_id,region_id)]
    y_lab <- sprintf("Individual (%s | experiment_id | region_id)", c_var_name)
  }
  else{
    summary_dt[,row_name:=sprintf("%s | %02d",experiment_id,region_id)]
    y_lab <- "Individual (experiment_id | region_id)"
  }
  
  p <- ggplot(summary_dt,aes(x=t_d,y=row_name,fill=y_var)) + geom_tile(alpha=1) +
    labs(title= sprintf("Overview of individual '%s' pattern over time",y_var_name),x="time", y=y_lab)+
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
#' @param time_wrap the time (in seconds) used to wrap the data (see details).
#' @param time_unit_conversion a function to convert time in the x axis. typically, \code{days}, \code{hours} or \code{mins}.
#' @return A \code{ggplot} object that can be plotted directly, or modified.
#' @details 
#' \code{time_wrap} is typically used to express time relatively to the start of the the day.
#' In other words, it can help be used to pull all days together in one representative day. In this case, \code{time_wrap=hours(24)}`.
#' 
#' At the moment, four types of error bars (\code{error_bar}) are supported:
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
#' data(dam_data)
#' # Time, on the x axis, in hours via
#' p <- ethogramPlot(activity,
#'              dam_data,
#'              condition,
#'              error_bar = "sem",
#'              time_unit_conversion=hours # this is where you set time in hours
#'              )
#' p
#' # summarise/wrap data in one day
#' p <- ethogramPlot(activity,
#'              dam_data,
#'              condition,
#'              error_bar = "sem",
#'              time_wrap=days(1) # this argument does the job
#'              )
#' p
#' @seealso \code{\link{overviewPlot}} to show per-individual patterns
#' @export
ethogramPlot <- function(y,data,
                          condition=NULL,
                          facet_var=NULL,
                          summary_time_window=mins(30),
                          normalise_var_per_id=FALSE,
                          error_bar=NULL,
                          time_wrap=NULL,
                          time_unit_conversion=days){
  
  dt = copy(as.data.table(data))  
  y_var_name <- deparse(substitute(y))
  setnames(dt,y_var_name,"y_var")
  dt[,t_r := floor(t/summary_time_window) * summary_time_window]
  
  if(!is.null(time_wrap))
    dt[,t_r := t_r %% time_wrap]
  
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
  
  
  summary_dt[,t_d:=t_r/time_unit_conversion(1)]
  
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
  
  summary_dt_all_animals[,t_d:=t_r/time_unit_conversion(1)]
  
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
  
  p <- p + labs(title= sprintf("Average '%s' over time",y_var_name),x="time", y=y_var_name)
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



#' Put white and black bars under a plot to show Dark and Light phases.

#' @param pl A \code{ggplot} object to be annotated.
#' @param time_conversion_unit The time conversion function used in \code{pl}.
#' @param period The period, in seconds
#' @param offset A number, lower than the \code{period} to shift the bar left or right
#' @param size The hight of the bar. It is expressed in percent of the graph height
#' @return A \code{ggplot} object that can be plotted directly, or modified.
#' @examples
#' data(sleep_sexual_dimorphism)
#' my_data <- sleep_sexual_dimorphism
#' # Fraction of animal asleep over time:
#' p <- overviewPlot(asleep,my_data,condition=sex)
#' p <- makeLDAnnotation(p)
#' print(p)
#' p <- ethogramPlot(asleep,my_data,condition=sex,error_bar="sem")
#' p <- makeLDAnnotation(p)
#' print(p)
#' @export
makeLDAnnotation <- function(pl, time_conversion_unit=days,period=hours(24), offset=0, size=.02){
	panel_ranges <- ggplot_build(pl)$panel$ranges[[1]]
	
	min_y <- panel_ranges$y.range[1]
	max_y <- panel_ranges$y.range[2]
	middle_y <- panel_ranges$y.major_source[1]
	min_x <- time_conversion_unit(panel_ranges$x.range[1])
	max_x <- time_conversion_unit(panel_ranges$x.range[2])
	
	steps <- offsettedSeq(min_x, max_x, by=period/2, offset)
	
	
	steps <- steps / time_conversion_unit(1)
	
	if(length(steps) %% 2 == 0){
		step_mat_l <- matrix(steps,ncol=2,byrow=T)
		step_mat_d <- matrix(c(NA,steps[1:(length(steps) -1)]),ncol=2,byrow=T)
		}
	else{
		step_mat_l <- matrix(c(steps, NA),ncol=2,byrow=T)
		step_mat_d <- matrix(c(NA,steps),ncol=2,byrow=T)
		}
	
	step_mat_d <- na.omit(step_mat_d)
	step_mat_l <- na.omit(step_mat_l)
	
	
	
	top_annotation <- (min_y + middle_y)/2
	bottom_annotation <- top_annotation - (max_y -min_y) * size
	
	al <- annotate("rect", xmin=step_mat_l[,1], xmax=step_mat_l[,2], ymin=bottom_annotation, ymax= top_annotation  , fill=c("white"))
	ad <- annotate("rect", xmin=step_mat_d[,1], xmax=step_mat_d[,2], ymin=bottom_annotation, ymax= top_annotation  , fill=c("black"))
	s1 <- annotate("segment", x=min(steps), xend=max(steps), y=top_annotation, yend= top_annotation  , colour=c("black"))
	s2 <- annotate("segment", x=min(steps), xend=max(steps), y=bottom_annotation, yend= bottom_annotation  , colour=c("black"))
	
	pl + al + ad  + s1 + s2
}


offsettedSeq <- function(start, end, by,offset){
		second <- by * ceiling(start/by) + offset
		out <- seq(from = second, to=end, by=by)
		
		
		return(unique(c(start,out,end)))
		
}
