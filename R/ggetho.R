#' Prepare a ggplot object to represent behavioural data
#' 
#' This function summarises a variable of interest 
#' in order to subsequently represent it over time (using a panel of plotting functions).
#' 
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
#' # We build a plot object with **nothing inside** (just the axis)
#' # we want to show proportion of time sleeping  on the y axis:
#' pl <-  ggetho(dt, aes(y=asleep))   
#' pl
#' # Sometimes, the variable of interest in not on the y axis, but on z axis (colour scale).
#' # When we do not provide a y axis,
#' # ggetho will make a ID fo each animal and display them on separate rows
#' pl <-  ggetho(dt, aes(z=asleep)) 
#' pl
#' # this one is the same type, but groups the animals by condition 
#' pl <-  ggetho(dt, aes(z=asleep,y=condition)) 
#' pl 
#' # we want to summarise (wrap) data along a circadian day:
#' pl <-  ggetho(dt, aes(y=asleep), time_wrap=hours(24))   
#' pl
#' @seealso
#' * [stat_pop_etho] To show population trend by aggregating individuals over time
#' * [stat_tile_etho] To show variable of interest as colour intensity
#' * [stat_ld_annotations] To show light and dark phases on the plot
#' * Tutorial for this function \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
#' @export
#' @author Quentin Geissmann (\email{qgeissmann@@gmail.com})
ggetho <- function(data, 
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
  #todo check argument types!!
  
  mapping_list <- lapply(mapping, as.character) 
  aes_names <- names(mapping_list)

  
  has_colour = "colour" %in% aes_names
  has_fill = "fill" %in% aes_names
  
  # if has only colour Xor fill deffined
  if( xor(has_fill, has_colour)){
     col = c(mapping_list$fill, mapping_list$colour)[[1]]
     mapping_list$fill <- col
     mapping_list$colour <- col
  }
  
  if(!"x" %in% aes_names)
    mapping_list$x = "t"
  
  if("z" %in% aes_names)
    var_of_interest = mapping_list$z
  else if("y" %in% aes_names)
    var_of_interest = mapping_list$y
  else
    stop("Either `y` or `z` should be provided as variable of interest")
  
  
  # todo! large memory overhead here!
  dd <- copy(data)
  k <- key(dd)

  dd[, ..t.. := floor(t /summary_time_window) * summary_time_window]
  if(!is.null(time_wrap))
    dd[, ..t.. := ..t.. %% time_wrap]
  
  setnames(dd, var_of_interest, "..voi..")
  sdt <- dd[, .(..voi.. = summary_FUN(..voi..)), by=c(k, "..t..")]
  sdt[,..t.. := hms::as.hms(..t..) ]
  setkeyv(sdt,c("..t..",k))
  setnames(sdt,c("..voi..","..t.."), c(var_of_interest, "t"))
  
  # when no `y`` is provided, the default is to have a 
  # discrete/factor axis with individuals as rows
  if(!"y" %in% aes_names){
    sdt[, experiment_id...region_id := as.factor(sprintf("%s...%02d",substr(experiment_id,1,26),region_id))]
    mapping_list$y = "experiment_id...region_id"
  }
  
  mapping = do.call(aes_string, mapping_list)
  out <- ggplot(sdt, mapping,...)
  if(!is.null(time_wrap))
    return( out + scale_x_time(limits=c(0, time_wrap)))
  out
}





