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


ldAnnotation <- function(x, period=1, phase=0){
  if(!(abs(phase) <= period))
    stop("Phase should be lower or equal to period!")
  left <- min(x)
  right <- max(x)
  p2 <- period/2
  box_pos <- p2 * floor(seq(from=left-p2, to=right+p2, by=p2) /p2) + phase %%period
  ld <- ifelse(((box_pos - phase) %% period)/p2, "L","D")
  out <- data.table(ld=ld, xmin=box_pos, xmax=box_pos + p2)
  out <- out[left < xmax & right >xmin]
  out[1, xmin := left]
  out[.N, xmax := right]
  out  
}

StatLDAnnotation <- ggproto("StatLDannotation", Stat,
                            default_aes = aes(colour = "black", size = 0.5, linetype = 1,
                                              alpha = .66),
                            setup_params = function(data, params){
                              out <- ldAnnotation(data$x,params$period,params$phase)
                              if(params$ypos != "auto")
                                out[,ypos:=params$ypos]
                              else{
                                range <- max(data$y) - min(data$y)
                                out[,ypos := min(data$y) - range * .05]
                              }
                              if(params$height != "auto")
                                out[,height:=params$height]
                              else{
                                range <- max(data$y) - min(data$y)
                                out[, height :=  range * .02]
                              }
                              out[,ymin:= ypos-height/2]
                              out[,ymax:= ypos+height/2]
                              params$ld_boxes <-out
                              params
                            },
                            
                            compute_group = function(data, scales,ld_colours, ld_boxes,ypos,
                                                     height,phase,period,...) {
                              ld_boxes
                            },
                            
                            finish_layer = function(data, params) {
                              data$fill <- params$ld_colours[(data$ld=="L")+1]
                            },
                            required_aes = c("x","y"),
                            draw_key = draw_key_polygon
)



GeomRectLD <- ggproto("GeomLD", GeomRect,
                      default_aes = aes(colour = "black",fill="blue", size = 0.5, linetype = 1,
                                        alpha = .66))


stat_ld_annotation <- function (mapping = NULL,
                                data = NULL,
                                position = "identity",
                                ld_colours = c("white", "black"),
                                ypos = "auto",
                                height = "auto",
                                period = 1,
                                phase = 0,
                                ..., 
                                na.rm = FALSE,
                                show.legend = FALSE,
                                inherit.aes = TRUE) 
{
  layer(data = data, mapping = mapping, stat = StatLDAnnotation, 
        geom = GeomRectLD,
        #geom = GeomRect, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ld_colours=ld_colours, ypos=ypos,height=height,
                      phase=phase, period=period,ld_boxes=NULL, ...))
}




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

pl3 + 
  stat_ld_annotation(colour="black",  period=24, phase=0)
