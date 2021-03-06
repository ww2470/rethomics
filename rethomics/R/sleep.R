#@include 
NULL
#' Determines whether an animal is asleep
#' 
#' This function uses a motion classifier to first decide whether an animal is moving during a given time window.
#' Then, it defines sleep as contiguous immobility for a minimal duration.
#'
#' @param data the data (i.e a data.table) from a \emph{single} region. It must contain, at least,
#' the columns `t`, `x` and `y`.
#' @param time_window_length The number of seconds to be used by the motion classifier. This corresponds to the sampling period of the output data.
#' @param min_time_immobile the minimal duration (in s) after which an immobile animal is scored as `asleep'.
#' @param motion_classifier_FUN the function used to classify movement.
#' @param ... extra arguments to be passed to \code{motion_classifier_FUN}
#' @return A data table similar to \code{data} with additional variables/annotations (i.e. `moving', `asleep').
#' @note The resulting data will only have one data point every \code{time_window_length} seconds.

#' @examples
#' # Let us load some sample data
#' data(tube_monitor_validation)
#' # We will start only with region 2:
#' dt_region2 <- tube_monitor_validation[region_id==2,]
#' sleep_dt <-  sleepAnnotation(dt_region2, masking_duration=0)
#' print(sleep_dt)
#' # We make a sleep `barecode'
#' ggplot(sleep_dt, aes(t,region_id,fill=asleep)) + geom_tile()
#' # A bit of data.table wizardry to apply that to each experiement and region:
#' sleep_dt <-  tube_monitor_validation[,
#'                                        sleepAnnotation(.SD, masking_duration=0),
#'                                        by=key(tube_monitor_validation)]
#' # The same bare code for all regions
#' ggplot(sleep_dt, aes(t,region_id,fill=asleep)) + geom_tile()
#' @seealso \code{\link{loadEthoscopeData}} to load data and optionally apply analysis on the fly.
#' @export
sleepAnnotation <- function(data,
                            time_window_length=10, #s
                            min_time_immobile=60*5, #s
                            motion_classifier_FUN=maxVelocityClassifierMasked,
                            ...
){ 
  d <- copy(data)
  ori_keys <- key(d)
  d <- curateSparseRoiData(d)
  if(nrow(d) <1)
    return(NULL)
  
  d[, t_round := time_window_length * floor(d[,t] /time_window_length)]  
  setkeyv(d, "t_round")
  d_small <- motion_classifier_FUN(d,...)
  
  # special variable "has interacted". We sum over it
  if("has_interacted" %in% colnames(d)){
    d_n_interations <- d[, .(n_interactions = sum(has_interacted)), by=key(d)]
    d_small <- d_small[d_n_interations]
    d[, has_interacted := NULL]
  }
  
  if(key(d_small) != "t_round")
    stop("Key in output of motion_classifier_FUN MUST be `t_round'")
  setnames(d_small,"t_round", "t")
  d$t <- NULL
  d_small <- d_small[unique(d,by=key(d))]
  
  t_out <- seq(from=d_small[1,t], to=d_small[.N,t], by=time_window_length)
  
  time_map <- data.table(t=t_out,key="t")
  missing_val <- time_map[!d_small]
  
  d_small <- d_small[time_map,roll=T]
  d_small[,is_interpolated := FALSE]
  d_small[missing_val,is_interpolated:=TRUE] 
  
  d_small[is_interpolated == T, moving := FALSE]
  d_small[,asleep := sleepContiguous(moving,1/time_window_length,min_valid_time = min_time_immobile)]
  setkeyv(d_small, ori_keys)
  na.omit(d_small)
}


NULL
#' Motion classifier based on maximum velocity.
#' 
#' Defines whether an animal is moving according to its subpixel velocity. 
#' It requires a variable named \code{xy_dist_log10x1000} in the .db file.
#' @param data the data.table containing behavioural features used for movement classification.
#' @param velocity_threshold velocity above which an animal is classified as `moving'.
#' @return a data table with the columns \code{moving} (logical, TRUE iff. motion was detected) and \code{t_round} (the `rounded' time). There is one row per rounded time point.
#' @seealso \code{\link{sleepAnnotation}} to apply this function to all subsequent time windows.
#' \code{\link{maxVelocityClassifierMasked}}, the emiprically corected FPS invariant version of this classifier.
#' @export
maxVelocityClassifierLegacy <- function(data,velocity_threshold=.006){
	d <- copy(data)
	d[,dt := c(NA,diff(t))]
	d[,velocity := 10^(xy_dist_log10x1000/1000)/dt ]
	#d[,max_velocity := 10^(xy_dist_log10x1000/1000)/dt ]
	d_small <- d[,.(
  	        max_velocity = max(velocity)
						), by="t_round"]

	d_small[, moving :=  ifelse(max_velocity > velocity_threshold, TRUE,FALSE)]
	d_small
	}

NULL

#' Motion classifier based on maximum velocity, but with a correction factor for variable FPS.
#' 
#' Defines whether an animal is moving according to its subpixel velocity. 
#' It requires a variable named \code{xy_dist_log10x1000} in the .db file.
#' @param data the data.table containing behavioural features used for movement classification.
#' @param velocity_correction_coef an empirical coefficient to correct velocity with respect to variable framerate
#' @param masking_duration the number of second when any movement is ignored after an interaction (velocity is set to 0).
#' @return a data table with the columns \code{moving} (logical, TRUE iff. motion was detected)
#' and \code{t_round} (the `rounded' time). 
#' There is one row per rounded time point.
#' Also we return the whether an animal has crossed a virtual midline, at x=0.5 (\code{beam_crosses}).
#' @seealso \code{\link{sleepAnnotation}} to apply this function to all subsequent time windows,
#' \code{\link{maxVelocityClassifierLegacy}}, the uncorected/unmasked version.
#' @export
maxVelocityClassifierMasked  <- function(data,velocity_correction_coef =3e-3 , masking_duration=6){
  d <- copy(data)
  d[,dt := c(NA,diff(t))]
  #d[,surface_change := xor_dist * 1e-3]
  d[,dist := 10^(xy_dist_log10x1000/1000) ]
  d[,velocity := dist/dt]
  
  a = velocity_correction_coef
  
  d[,beam_cross := abs(c(0,diff(sign(.5 - x))))]
  d[,beam_cross := as.logical(beam_cross)]
  
  if ("has_interacted" %in% colnames(d)){
    d[,interaction_id := cumsum(has_interacted)]  
    d[,
      masked := t < (t[1] + masking_duration),
      by=interaction_id]
    d[ ,velocity := ifelse(masked & interaction_id != 0, 0, velocity)]
    d[,beam_cross := !masked & beam_cross]
    d[,interaction_id := NULL]
    d[,masked := NULL]
  }
  else{
    if(masking_duration >0) 
      warning("Data does not contain an `has_interacted` column. Cannot apply masking!. Set `masking_duration=0` to ignore masking")
  }
  
  d[, velocity_corrected :=  velocity  * dt  /a]
  d_small <- d[,.(
    max_velocity = max(velocity_corrected[2:.N]),
    # dist = sum(dist[2:.N]),
    beam_crosses = any(beam_cross)
  ), by="t_round"]
  
  d_small[, moving :=  ifelse(max_velocity > 1, TRUE,FALSE)]
  d_small
}


NULL


#' Motion classifier based on beam crosses.
#' 
#' Defines whether an animal is moving. This is achieved by computing the number of crossed of
#' a "virtual beam" in the middle of its region (i.e. at x=0.5).
#' This emulate the type of data generated by DAM2.
#' @param data the data.table containing behavioural features used for movement classification.
#' @return a data table with the columns \code{moving} (logical, TRUE iff. motion was detected) and \code{t_round} (the `rounded' time). There is one row per rounded time point.
#' @seealso \code{\link{maxVelocityClassifierMasked}} to define movement by maximum velocity, which is more accurate, instead.
#' @export
virtualBeamCrossClassif <- function(data){
  d <- copy(data)
  d[,beam_cross := abs(c(0,diff(sign(.5 - x))))]
  d[,beam_cross := as.logical(beam_cross)]
  
  d_small <- d[,list(moving = any(beam_cross)), by="t_round"]
  
  d_small
}
NULL

activity <- function(x,y){
	comp = x + 1i*y
	distance <- c(0, abs(diff(comp)))
	return(distance)
}

half_angular_distance <- function(half_angle){
  half_angle
  abs_diff <- abs(diff(half_angle))
  
  da <- ifelse(abs_diff >=90,180 - abs_diff ,abs_diff)
  
  c(0,da)
}


sleepContiguous <- function(moving,fs,min_valid_time=5*60){
	min_len <- fs * min_valid_time
	r_sleep <- rle(!moving)
	valid_runs <-  r_sleep$length > min_len 
	r_sleep$values <- valid_runs & r_sleep$value
	inverse.rle(r_sleep)
}

#remove data points when the time series is too sparse
curateSparseRoiData <- function(
	data,
	window=60,#s
	min_points=20#
	){
  
	d <- copy(data)
	d[, t_w := window * floor(t/window)]
	sparsity <- d[, t_w := window * floor(t/window)]
	d[,sparsity := .N,by=t_w]
	d <- d[sparsity >min_points,]
	d$t_w <- NULL
	d$sparsity <- NULL
	d
	}
	


makeBoutDt <- function(x,sub_data){
  sdt <- copy(sub_data)
  sdt[,delta_t:= c(0,diff(sub_data[,t]))] 
  r <- rle(x)
  vals <-r$values
  r$values <- 1:length(r$values)
  
  bdt <- data.table(delta_t = sdt[,delta_t], bout_id=inverse.rle(r),key="bout_id")
  
  bout_times <- bdt[,list(length=sum(delta_t)),by="bout_id"]
  r$values <- vals
  out <- data.table(
    x = vals,
    length = bout_times[,length],
    start_time = cumsum(c(0,bout_times[1:.N-1,length])) + sdt[1,t]
  ) 
  var_name <- deparse(substitute(var))
  setnames(out,"x",var_name)
  out
}

NULL
#' Finds `bouts' in categorical time series.
#' 
#' This function is used to find contiguous regions of unique value in a -- potentially irregular -- univariate time series.
#' @param var the column variable to use in \code{data}
#' @param data a data.table
#' @return A data.table with columns for the unique value of the bout variable, bout start time, and bout length (ie. duration). 
#' Bout analysis will be performed by individual (data.table key), which adds additional columns. Their is one row for each bout.
#' @examples 
#' set.seed(1)
#' # 1000 points the first 500 points should have higher chance to be 1 than the last 500:
#' y_var <- round(c(runif(500,0,1),
#'                    runif(500,0,0.75)))
#' # first 500 point are for individual "A", next 500 points are for "B":
#' dt <- data.table( y = y_var,
#'              t = rep(1:500,2)*12,
#'              id = rep(c("A","B"),each=500),key="id")

#' bout_dt <- boutAnalysis(y,dt)
#' summary <- bout_dt[,
#'         .(n=.N,
#'           mean_duration=mean(length))
#'         ,by=c(key(bout_dt),"y")] 
#' print(summary)
#' @seealso \code{rle} to perform a run length transform manually
#' @export
boutAnalysis <- function(var,data){
  dt <- copy(as.data.table(data))
  var_name <- deparse(substitute(var))
  setnames(dt,var_name,"var")
  
  out <- dt[,makeBoutDt(var,.SD),by=c(key(dt))]
  
  setnames(out,"var",var_name)
  out
}
NULL
#' Determines whether an animal is asleep using beam crossing activity
#' 
#' Sleep as contiguous inactivity (absence of beam crossing) for a minimal duration.
#'
#' @param data the data (i.e a data.table) from a \emph{single} region. It must contain, at least,
#' the columns \code{t}, \code{x} and \code{y}.
#' @param time_window_length The number of seconds to be used by the motion classifier. This corresponds to the sampling period of the output data.
#' @param min_time_immobile the minimal duration (in s) after which an immobile animal is scored as `asleep'.
#' @return A data table similar to \code{data} with additional variables/annotations (i.e. `moving', `asleep').
#' @note The resulting data will only have one data point every \code{time_window_length} seconds.
#' @examples
#' # Let us load some sample data
#' data(dam_data)
#' dam_data[,
#'            sleepDAMAnnotation(.SD),
#'            by=key(dam_data)]
#'            
#' @seealso \code{\link{loadDAM2Data}} To load DAM2 data first/ apply this function to each animal.
#' @export
sleepDAMAnnotation <- function(
  data,
  time_window_length=60, #s
  min_time_immobile=60*5 # s
){
  d <- copy(data)
  if(nrow(d) <1)
    return(NULL)
  d[, moving:= activity > 0]
  d[, asleep := sleepContiguous(moving, 1/time_window_length, min_time_immobile)]
  d
}
NULL

#' Finds when an animal is `dead' and removes the all consecutive data
#'
#' In this context, death is defined by very long periods of immobility.
#'
#' @param data the data (i.e a data.table) from a \emph{single} region. It must contain, at least,
#' the columns \code{t}and \code{moving}.
#' @param max_immobile_live the longest duration an alive animal can remain immobile before being considered dead.
#' @return A data table similar to \code{data} where late time points have potentially been removed
#' @note Death is assumed to be irreversible. Therefore, if an animal is classified as dead, all subsequent data is is removed.
#' @examples
#' # Let us load some sample data
#' data(dam_data)
# we add a `moving` column to the data
#' dt <- dam_data[,
#'             sleepDAMAnnotation(.SD),
#'             by=key(dam_data)]
#' # let us have a look at the pattern of movement.
#' # Some animals (e.g. 06, 21, 24) died early.
#' overviewPlot(moving,dt,normalise_var_per_id = FALSE)
#' dt_curated <- dt[,curateDeadAnimals(.SD,hours(15)),by=key(dt)]
#' # Note that some data has been removed.
#' # Also, no data was there for region_id == 06, therefore, it is removed altogether
#' overviewPlot(moving, dt_curated, normalise_var_per_id = FALSE)
#' #####
#' # A simple way to compute total lifespan of each remaining animal:
#' lifespan_dt <- dt_curated[,
#'         .(lifespan = max(t) - min(t))
#'         ,by=key(dt_curated)]
#' @seealso \code{\link{sleepAnnotation}} and \code{\link{sleepDAMAnnotation}} to define movement and add a \code{moving} column.
#' @export
curateDeadAnimals <- function(data, max_immobile_live=hours(12)){
  
  if(!"moving" %in% colnames(data))
    stop("`data` must have a column names `moving`")
  death_dt <- copy(data)
  death_dt <- death_dt[moving==F]
  bout_dt <- makeBoutDt(data[,moving],data)
  bout_dt[,dead := length > max_immobile_live]
  dead_instances <- which(bout_dt[,dead])
  if(length(dead_instances) < 1)
    return(data)
  first_death_idx <- min(dead_instances)
  first_dead_time <- bout_dt[first_death_idx,start_time]
  data[t < first_dead_time]
}

