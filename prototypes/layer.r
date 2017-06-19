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


ggetho2 <- function(variable, 
                    data, 
                    mapping,
                    simplify_FUN = mean,
                    summary_time_window = mins(30),
                    time_conversion=hours,
                    time_wrap=NULL,
                    # todo add time wrap offset / double plotting
                    ...){
  
  dd <- copy(data)
  k <- key(dd)
  
  y_char=as.character(substitute(variable))

  dd[, ..t.. := floor(t /summary_time_window) * summary_time_window]
  if(!is.null(time_wrap))
    dd[, ..t.. := ..t.. %% time_wrap]
  
  
  setnames(dd,y_char, "..y..")
  sdt <- dd[, .(..y.. = simplify_FUN(..y..)), by=c(k, "..t..")]
  setnames(sdt,c("..y..","..t.."), c(y_char, "t"))
  sdt[,t := t / time_conversion(1)]
  
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
  ggplot(sdt, mapping,...)
}


dt <- sleep_sexual_dimorphism[t < 25e4]
pl1 <- ggetho2(asleep, dt, aes(t), time_wrap = days(1)) +
       geom_raster() # sortby

pl2 <- ggetho2(asleep, dt, aes(t, y=region_id, region_id), time_wrap = days(1)) +
       geom_raster()

dt <- dt[!(machine_name == "GGSM-004" & t > hours(20))]
pl3 <- ggetho2(asleep, dt, aes(t,asleep,colour=sex)) +
       stat_pop_etho(aes(),method=mean_se) + facet_grid(. ~ machine_name)



pl3

ldAnnotation <- function(x, period=1, phase=0){
  
  if(!(phase <= period))
    stop("Phase should be lower or equal to period!")
  left <- min(x)
  right <- max(x)
  p2 <- period/2
  
  out <- phase + floor(seq(from= left, to= right, by=p2) /p2) * p2
  out <- data.table(xmin=out, xmax=out+p2)
  out[1,xmin := min(x)]
  out[.N,xmax := max(x)]
  return(out)
  
}

StatLDAnnotation <- ggproto("StatLDannotation", Stat,
                           default_aes = aes(colour = "black", size = 0.5, linetype = 1,
                                             alpha = .66),
                           setup_params = function(data, params){
                              
                              out <- ldAnnotation(data$x,params$period,params$phase)
                              if(params$y != "auto")
                                out[,y:=params$y]
                              else
                                stop("Not IMPLEMENTED")
                              if(params$height != "auto")
                                out[,height:=params$height]
                              else
                                stop("Not IMPLEMENTED")
                                
                              
                              out[,ymin:= y-height/2]
                              out[,ymax:= y+height/2]
                              
                              out[, fill:=rep(params$ld_colours, length.out=.N)]
                              params$ld_boxes <-out
                              print(params)
                           },
                           
                           compute_group = function(data, scales,ld_colours, ld_boxes,y,
                                                    height,phase,period,...) {
                             ld_boxes
                           },
                          required_aes = c("x"),
                           draw_key = draw_key_polygon
)



GeomRectLD <- ggproto("GeomLD", GeomRect,
                           default_aes = aes(colour = "black",fill="blue", size = 0.5, linetype = 1,
                                             alpha = .66))


stat_ld_annotation <- function (mapping = NULL,
                                data = NULL,
                                position = "identity",
                                ld_colours = c("white", "black"),
                                y = "auto",
                                height = "auto",
                                period = 1,
                                phase = 0,
                                ..., 
                                na.rm = FALSE,
                                show.legend = FALSE,
                                inherit.aes = FALSE) 
{
  layer(data = data, mapping = mapping, stat = StatLDAnnotation, geom = GeomRectLD, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ld_colours=ld_colours, y=y,height=height,
                      phase=phase, period=period,ld_boxes=NULL, ...))
}

pl3 + stat_ld_annotation(aes(x=t),colour="black", y=-.1, height=.05, period=24, phase=0)

d
#pl3 + stat_ld_annotation(data=d,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t),inherit.aes = F)



pl4 <- ggetho2(asleep, dt, aes(t,asleep)) +
  stat_pop_etho(aes(),method=mean_se) 

grid:::rectGrob()

?ggproto
