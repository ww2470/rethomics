#' Motion classifier for Ethocope
#' 
#' Defines whether an animal is moving according to:
#' 
#' * Validated and corrected subpixel velocity ([maxVelocityClassifierMasked]), the mostb rigorous. 
#' * Uncorrected subpixel velocity ([maxVelocityClassifierLegacy]). 
#' * Crossing a virtual beam in the middle of the region of interest ([virtualBeamCrossClassif]). 
#' 
#' [maxVelocityClassifierMasked] is the default movement classification for real-time ethoscope experiments.
#' It is benchmarked against human-generated ground truth.
#' 
#' @param data `data.table` containing behavioural features used for movement classification.
#' It must have the columns `xy_dist_log10x1000`(for computing subpixel velocity), `x`(beam cross), `t` and `t_round` 
#' (time points belonging to the same scoring window share the same `t_round`).
#' @param velocity_correction_coef an empirical coefficient to correct velocity with respect
#'  to variable framerate.
#' @param masking_duration the number of second when any movement is ignored (velocity is set to 0) after 
#' a stimulus is delivered (aka interaction).
#' @param velocity_threshold Uncorrected velocity above which an animal is classified as `moving' (for the legacy version).
#' @return a data table with the columns:
#' * `moving` Logical, TRUE iff. motion was detected.
#' * `t_round` The "rounded" time. 
#' * `beam_crosses` The number of beam crosses
#' (when the animal crosses x = 0.5 -- that is the midpoint of the region of interest) within the time window
#' * `max_velocity` The maximal velocity within the time window.
#' @details 
#'  These functions are *rarely used directly*, but rather used by [sleepAnnotation].
#' @seealso
#' * Tutorial for sleep analysis with ethoscopes \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
#' * [sleepAnnotation] that requieres a motion classifier
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
    beam_crosses = sum(beam_cross)
  ), by="t_round"]
  
  d_small[, moving :=  ifelse(max_velocity > 1, TRUE,FALSE)]
  d_small
}

#' @export
#' @rdname maxVelocityClassifierMasked
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


#' @export
#' @rdname maxVelocityClassifierMasked
virtualBeamCrossClassif <- function(data){
  d <- copy(data)
  d[,beam_cross := abs(c(0,diff(sign(.5 - x))))]
  d[,beam_cross := as.logical(beam_cross)]
  
  d_small <- d[, .(moving = any(beam_cross)), by="t_round"]
  
  d_small
}