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
	d_small[,moving := ifelse(is.na(moving), F, moving)]
	d_small[,asleep := sleep_contiguous(moving,1/time_window_length)]
	
	d_small <- d_small[,lapply(.SD,na.locf,na.rm=F)]
	
	setkeyv(d_small, ori_keys)
	
	d_small
	}

#@include 
NULL
#' Define whether an animal is moving from behavioural data.
#' 
#' @seealso \code{\link{sleepAnnotation}} to apply this function to all subsequent time windows.
#' @export
totalWlkdDistClassif <- function(data,activity_threshold=.03){
	d <- copy(data)
	d[,activity := activity(x,y)]
	
	d[,rel_ar := (w - h)/w]
	d[,diff_rel_ar := c(0,abs(diff(rel_ar)))]
	#d[,rel_ar := pmin(rel_ar,c(0,rel_ar[1:(length(rel_ar)-1)]))]
	
	d[,vt1 := (w - h)/w]
	d[,vt2 := c(0,vt1[1:(length(vt1)-1)])]
  
	d[,phi_diff := half_angular_distance(phi)]
	d[ ,angular_activity:=sqrt(vt1^2 + vt2^2
                              + vt1*vt2*cos(pi*phi_diff/90))]
  print(d)
	
  d[,area_diff:=abs(c(0,diff(w*h)))/(w*h)]
# 	d[ ,angular_activity:=phi_diff * rel_ar]
  
	d_small <- d[,list(
						activity = sum(activity),
						angular_activity = sum(angular_activity),
						area_diff = sum(area_diff),
						ar_diff = sum(diff_rel_ar),
						ar_mean = sd(rel_ar) / mean(rel_ar),
						sd_activity = sd(activity),
						sd_angular_activity = sd(angular_activity),
						sd_ar_diff = sd(diff_rel_ar),
						sd_area_diff = sd(area_diff),
						max_ar_diff = max(diff_rel_ar),
						max_area_diff = max(area_diff),
						max_angular_activity = max(angular_activity/c(Inf,diff(t))),
						max_activity = max(activity/c(Inf,diff(t)))
						), by="t_round"]
	
	d_small[, moving :=  ifelse(activity > activity_threshold, TRUE,FALSE)]
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
	
