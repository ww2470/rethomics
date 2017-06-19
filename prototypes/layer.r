rm(list=ls())
library(rethomics)
library(plotly)
data(sleep_sexual_dimorphism)

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


StatPopEtho <- ggproto("StatSmooth", Stat,
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


dt <- sleep_sexual_dimorphism[t < 15e4]

ggetho <- function(data, mapping, 
                   simplify_FUN = mean,
                   summary_time_window = mins(30),
                   time_conversion=hours,
                   time_wrap=NULL,
                   ...){
  
  mapping = lapply(mapping, as.character)
  x_char <- as.character(mapping$x)
  y_char <- as.character(mapping$y)
  fill_char <- as.character(mapping$fill)
  
  dd <- copy(data)
  k <- key(dd)
  dd[, ..t.. := floor(t /summary_time_window) * summary_time_window]
  if(!is.null(time_wrap))
    dd[, ..t.. := ..t.. %% time_wrap]
  
  # no y specified -> we use the key as y for raster/tyle plots
  if(length(y_char) == 0){
    dd[, ..y.. := as.factor(sprintf("%s.%02d",substr(experiment_id,1,26),region_id))]
    #assert fill is defined here
    setnames(dd,fill_char,"..fill..")
    sdt <- dd[, .(..fill.. = simplify_FUN(..fill..)), by=c("..y..",k, "..t..")]
    
    setnames(sdt,c("..y..","..t..", "..fill.."), c("experiment_id.region_id", x_char, fill_char))
    mapping$y = "experiment_id.region_id"
    
  }
  else{
    setnames(dd,y_char, "..y..")
    sdt <- dd[, .(..y.. = simplify_FUN(..y..)), by=c(k, "..t..")]
    setnames(sdt,c("..y..","..t.."), c(y_char, x_char))
  }
  
  sdt[,t := t / time_conversion(1)]
  #rebuild mappling with aes_string and do.call here
  
  mapping = do.call(aes_string,mapping)
  ggplot(sdt, mapping, ...)
}


# pl <- ggetho(dt, aes(t, y=asleep), time_wrap = days(1))
# pl
# + geom_raster()  #+  facet_grid(sex ~.)
# 

pl <- ggetho(dt, aes(t,asleep,colour=sex, fill=sex)) +
   stat_pop_etho(method=mean_se) 
pl

pl <- ggetho(dt, aes(t,asleep,colour=sex, fill=sex))

pl <- ggetho(dt, aes(t,colour=sex, fill=sex))

pl + geom_raster(aes(y=interaction(region_id,experiment_id,sex), fill=asleep))




