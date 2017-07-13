#' Prepare a ggplot object to represent behavioural data
#' 
#' This function summarises a variable of interest 
#' in order to subsequently represent it over time (using a panel of plotting functions).
#' @param variable Variable of interest.
#' @param data `data.table` containing the data. It must have a column with the same name as `variable`.
#' @param mapping Default list of aesthetic mappings to use for plot.
#' @param summary_FUN Method (function) used to summarise `variable` over time (typically, the mean). 
#' @param summary_time_window Width (in seconds) of the time window to compute a summary on.
#' @param time_wrap Time (in seconds) used to wrap the data (see details).
#' @param time_offset Time offset (i.e. phase, in seconds) when using `time_wrap`.
#' @param ... Additional arguments to be passed to [ggplot2::ggplot()]
#' @details `time_wrap` is typically used to express time relatively to the start of the the day.
#' In other words, it can help be used to pull all days together in one representative day. 
#' In this case, `time_wrap=hours(24)`. 
#' Instead of representing data from the start of the day, it can be done from any offset, using `time_offset`.
#' For instance,  `time_offset= hours(12)` puts the circadian reference (ZT0) in the middle of the plot.
#' @return A initial plot object that can be further edited.
#' @examples
#' # we start by making a to dataset with 20 animals
#' query <- data.table(experiment_id="toy_experiment",
#'                    region_id=1:20, 
#'                    condition=c("A","B"))
#' dt <- toyActivityData(query,3)
#' # We build a plot object with nothing inside (just the axis)
#' pl <-  ggetho(asleep,     # the variable of interest
#'          dt,              # our original data
#'          aes(
#'              x=t,         # we have the time on the x axis
#'              y=asleep     # and asleep on the y axis
#'              )     
#'          ) 
#' pl
#' # Sometimes, the variable of interest in not on the y.
#' # When we do not provide a y axis,
#' # ggetho will make a ID fo each animal and display them on separate rows
#' pl <-  ggetho(asleep,dt, aes(x=t)) 
#' pl
#' @seealso
#' * [stat_pop_etho] To show trend by aggregating individuals over time.
#' * TODO to see each individual on a "tyle-style" plot
#' @export
#' @author Quentin Geissmann (\email{qgeissmann@@gmail.com})
ggetho <- function(variable, 
                    data, 
                    mapping,
                    summary_FUN = mean,
                    summary_time_window = mins(30),
                    #time_conversion=hours,
                    time_wrap=NULL,
                    time_offset=NULL,
                    # todo add time wrap offset / double plotting
                    ...){
  
  if(!is.null(time_offset))
    stop("Not implemented") #todo
  # todo! large memory overhead here!
  dd <- copy(data)
  k <- key(dd)
  
  y_char=as.character(substitute(variable))
  
  dd[, ..t.. := floor(t /summary_time_window) * summary_time_window]
  if(!is.null(time_wrap))
    dd[, ..t.. := ..t.. %% time_wrap]
  
  setnames(dd,y_char, "..y..")
  sdt <- dd[, .(..y.. = summary_FUN(..y..)), by=c(k, "..t..")]
  sdt[,..t.. := hms::as.hms(..t..) ]
  setkeyv(sdt,c("..t..",k))
  setnames(sdt,c("..y..","..t.."), c(y_char, "t"))
  
  mapping_list <- lapply(mapping, as.character) 
  
  if(!"y" %in% names(mapping_list)){
    aes_y <- mapping_list$y
    # todo check /warn if fill exist and is different!
    mapping_list$fill = y_char
    sdt[, experiment_id...region_id := as.factor(sprintf("%s...%02d",substr(experiment_id,1,26),region_id))]
    mapping_list$y = "experiment_id...region_id"
  }
  else if(mapping_list$y !=y_char)
    # todo check /warn if fill exist and is different!
    mapping_list$fill = y_char
  
  mapping = do.call(aes_string,mapping_list)
#  return(list(sdt,mapping))
  ggplot(sdt, mapping,...)
}
