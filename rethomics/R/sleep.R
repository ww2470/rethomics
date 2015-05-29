#@include 
NULL
#' Determine whether an animal is asleep
#' 
#' This function uses a motion classifier to first define whether an animal is moving during a given time window.
#' Then, it defines sleep as contiguous immobility for a minimal duration.
#'
#' @param data the data (i.e a data.table) from a \emph{single} ROI. It must contain, at least,
#' the columns `t`, `x` and `y`.
#' @param time_window_length The number of seconds tu use in the motion classifier
#' @param min_time_immobile the minimal duration (in s) after which an immobile an animal is scored as `asleep'.
#' @return A data table similar to \code{data} with additionnal variables/annotations (i.e. `activity', `moving', `asleep').
#' @note The resulting data will only have one data point every \code{time_window_length} seconds.

#' @examples
#' # We load samples from the package data
#' file <- loadSampleData("validation.db")
#' # We would like only ROI #2 from this file
#' map <- data.frame(path=file, roi_id=2)
#' dt <- loadPsvData(map)
#' sleep_dt <-  sleepAnnotation(dt)
#' # A more likely scenario, we load ROIs 5 to 10, 
#' # apply sleep analysis in combination with loadPsvData.
#' # this means we apply the function to all rois just after they are being loaded.
#' map <- data.frame(path=file, roi_id=5:10)
#' dt <- loadPsvData(map,FUN=sleepAnalysis)
#' sleep_dt <- dt[,sleepAnnotation(.SD),by=key(dt)]
#' 
#' @seealso \code{\link{loadPsvData}} to load data and optionnaly apply analysis on the fly.
#' @export
sleepAnnotation <- function(data,
			time_window_length=10, #s
			min_time_immobile=60*5, #s
			motion_classifier_FUN=totalWlkdDistClassif,
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
	d_small <- unique(d_small[d])
	
	d_small[,t:=t_round]
	d_small$t_round <- NULL
	setkeyv(d_small,"t")
	
	t_out <- seq(from=d_small[1,t], to=d_small[.N,t], by=time_window_length)
	  
	d_small <- merge(d_small, data.table(t=t_out,key="t"),all=T)
  
	d_small[,moving := ifelse(
    is.na(moving), F, moving)]
  
	d_small[,asleep := sleep_contiguous(moving,1/time_window_length)]
	
  is_interpolated <- d_small[,is.na(x)]
  
	d_small <- d_small[,lapply(.SD,na.locf,na.rm=F)]
	d_small[,is_interpolated := is_interpolated]
	
	
	setkeyv(d_small, ori_keys)
	d_small
	}

#@include 
NULL
#' Define whether an animal is moving from behavioural data.
#' 
#' @seealso \code{\link{sleepAnnotation}} to apply this function to all subsequent time windows.
#' @export
totalWlkdDistClassif <- function(data,velocity_threshold=.005){
	d <- copy(data)
	d[,dt := c(NA,diff(t))]
	d[,surface_change := xor_dist * 1e-3]
	d[,max_velocity := 10^(xy_dist_1e6/1000)/dt ]

	d_small <- d[,list(
  	        surface_change = max(surface_change),
  	        max_velocity = max(max_velocity)
						), by="t_round"]
  
	d_small[, moving :=  ifelse(max_velocity > velocity_threshold, TRUE,FALSE)]
	d_small
	}

#@include 
NULL
#' Define whether an animal is moving from behavioural data.
#' 
#' @seealso \code{\link{sleepAnnotation}} to apply this function to all subsequent time windows.
#' @export
virtualBeamCrossClassif <- function(data){
  
  d <- copy(data)
  d[,beam_cross := abs(c(0,diff(sign(.5 - x))))]
  d[,beam_cross := as.logical(beam_cross)]
  
  d_small <- d[,list(moving = any(beam_cross)), by="t_round"]
  
  d_small
}


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


sleep_contiguous <- function(moving,fs,min_valid_time=5*60){
	min_len <- fs * min_valid_time
	r_sleep <- rle(!moving)
	valid_runs <-  r_sleep$length > min_len 
	r_sleep$values <- valid_runs & r_sleep$value
	inverse.rle(r_sleep)
}

#' remove data points when the time serie is too sparse
curateSparseRoiData <- function(
	data,
	window=60,#s
	min_points=20#
	){
	d <- copy(data)
	d[, t_w := window * floor(t/window)]
	sparsity <- d[, t_w := window * floor(t/window)]
	d[,sparsity := .N,by=t_w]
	d[,sparsity := .N,by=t_w]
	d <- d[sparsity >min_points,]
	d$t_w <- NULL
	d$sparsity <- NULL
	d
	}
	
