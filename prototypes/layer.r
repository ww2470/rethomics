rm(list=ls())
library(rethomics)
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


dt <- sleep_sexual_dimorphism[ t < 7e4]



ggetho <- function(data, mapping, 
                   simplify_FUN = mean,
                   summary_time_window = mins(30),
                   time_conversion=hours,
                   time_wrap=NULL,
                   ...){
  x_char <- as.character(mapping$x)
  y_char <- as.character(mapping$y)
  fill_char <- as.character(mapping$fill)
  
  dd <- copy(data)
  k <- key(dd)
  
  # no y specified -> we use the key as y for raster/tyle plots
  if(length(y_char) == 0)
    dd[, ..y.. := paste(experiment_id,region_id,sep="|")]
  else
    setnames(dd,y_char, "..y..")
  
  dd[, ..t.. := floor(t /summary_time_window) * summary_time_window]
  sdt <- dd[, .(..y.. = simplify_FUN(..y..)), by=c(k, "..t..")]
  
  setnames(sdt,c("..y..","..t.."), c(y_char, x_char))
  sdt[,t := t / time_conversion(1)]
  #rebuild mappling with aes_string and do.call here
  #ggplot(sdt, mapping, ...)
}

#ggetho(dt,aes(fill=asleep)) +
  

ggetho(dt,aes(fill=asleep)) +
  geom_tile(aes(y=region_id, fill=asleep))



ggetho(dt, aes(asleep,colour=sex, fill=sex)) +
            stat_pop_etho(method=mean_se) +  facet_grid(sex ~ machine_name)






