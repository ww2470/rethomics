#' Compute and display a population aggregate for a behavioural variable of interest
#' 
#' Compute an aggregate -- and assotiated error bars -- per population.

#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}.
#' If specified and \code{inherit.aes = TRUE} (the default), it is combined with the default mapping at the top level of the plot.
#' @param data Default list of aesthetic mappings to use for plot.
#' @param summary_FUN Method (function) used to summarise \code{variable} over time (typically, the mean). 
#' @param summary_time_window Width (in seconds) of the time window to compute a summary on.
#' @param time_conversion Method (function) to convert time (from second) in the x axis. Typically, \code{days}, \code{hours} or \code{mins}.
#' @param time_wrap Time (in seconds) used to wrap the data (see details).
#' @param time_offset Time offset (i.e. phase, in seconds) when using \code{time_wrap}.
#' @details \code{time_wrap} is typically used to express time relatively to the start of the the day.
#' In other words, it can help be used to pull all days together in one representative day. 
#' In this case, \code{time_wrap=hours(24)}. 
#' Instead of representing data from the start of the day, it can be done from any offset, using \code{time_offset}.
#' For instance,  \code{time_offset= hours(12)} puts the circadian reference (ZT0) in the middle of the plot.
#' @return A \code{ggplot} object that can be further edited.
#' 

#' @examples
#' #todo
#' @seealso \code{\link{stat_pop_ethod}} To show trend by aggregating individuals over time. 
#' @export
stat_pop_etho <- function(mapping = NULL, data = NULL,
                          geom = "smooth", position = "identity",
                          ...,
                          method = mean_se,
                          method.args = list(),
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPopEtho,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      method.args = method.args,
      ...
    )
  )
}


StatPopEtho <- ggproto("StatPopEtho", Stat,
                       compute_group = function(data, scales,method, method.args = list()) {
                         data <- as.data.table(data)
                         foo <- function(y){
                           all_args <- append(list(y), method.args)
                           do.call(method, all_args)
                         }
                         out <- data[,foo(y)
                                     ,by="x"]
                         out
                       },
                       required_aes = c("x", "y")
)

bootCi <- function(y,
                   r=5000,
                   ci=0.95){
  v <- replicate(r, mean(sample(y,replace=T)))
  ci <- quantile(v,c(1-ci,ci))
  out <-list(y = mean(y),
             ymin = ci[1],
             ymax = ci[2])
  out
}

