#@include
#' Compute and display a population aggregate for a behavioural variable of interest
#' 
#' @family layers
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::stat_smooth
#' @param method Function used to compute the aggregate and error bars. 
#' It should return (`y`, `ymin` and `ymax`). 
#' The default is [ggplot2::mean_se], which computes the mean + or - standard error.
#' [boot_ci] can be used instead to generate bootstrap confidence interval.
#' @examples 
#' # we start by making a to dataset with 20 animals
#' query<- data.table(experiment_id="toy_experiment",
#'                    region_id=1:20, 
#'                    condition=c("A","B"))
#' print(query)
#' dt <- toyActivityData(query,seed=3)
#' # We build a plot object
#' pl <-  ggetho(asleep, dt, aes(x=t, y=asleep))
#' # A standard plot of the whole population:
#' pl + stat_pop_etho()              
#' # We can also split by condition, and display the two population on different facets:
#' pl + stat_pop_etho() + facet_grid(condition ~ .)
#' 
#' # Instead, we can use different colour for separate conditions:
#' pl <-  ggetho(asleep, dt, aes(x=t, y=asleep, fill=condition, colour=condition))
#' pl + stat_pop_etho()              
#' # sometimes we want to aggreate several days of data to one circadian day (i.e. time wrapping)
#' pl <-  ggetho(moving, 
#'               dt,
#'               aes(x=t, y=moving),
#'               time_wrap=hours(24)
#'               )
#' pl + stat_pop_etho()            
#' @seealso  
#' * [ggetho] to generate a plot object
#' * Tutorial for this function \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
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


#' Bootstrap confidence interval
#' 
#' Compute the mean of a variable, and the quantiles after bootstrap resampling.
#' 
#' @param y Numeric vector
#' @param r Number of replicates to draw.
#' @param ci Confidence interval to draw from the empirical distribution.
#' @export
boot_ci <- function(y,
                   r=5000,
                   ci=0.95){
  v <- replicate(r, mean(sample(y,replace=T)))
  ci <- quantile(v,c(1-ci,ci))
  out <-list(y = mean(y),
             ymin = ci[1],
             ymax = ci[2])
  out
}

