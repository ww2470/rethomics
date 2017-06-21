#@include
#' Compute and display light/dark annotations onto a plot object
#' 
#' This function is used to show ight and dark phases as boxes on a plot.
#' 
#' @family layers
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' #' @param ld_colours Character vector of length 2 naming the colours for light and dark phases, respectively.
#' The default is white and black.
#' @param ypos,height The position and height of the annotation on the y axis. 
#' The defaults, "auto" will put the labels below any data.
#' @param period,phase the period and phase of the LD cycle. 
#' The units are the same as on the x axis (). #todo chould be in s
#' @examples 
#' # we start by making a to dataset with 20 animals
#' query <- data.table(experiment_id="toy_experiment",
#'                    region_id=1:20, 
#'                    condition=c("A","B"))
#' dt <- toyActivityData(query,3)
#' # We build a plot object
#' pl <-  ggetho(asleep, dt, aes(x=t, y=asleep)) + stat_pop_etho()
#' pl + stat_ld_annotations(aes(t), period=24)
#' pl + stat_ld_annotations(aes(t), period=22)
#' @seealso  Useful links:
#' * [ggetho] to generate a plot object
#' * Tutorial for this function \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
#' @export
stat_ld_annotations <- function (mapping = NULL,
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
                              data
                            },
                            required_aes = c("x","y"),
                            draw_key = draw_key_polygon
)



GeomRectLD <- ggproto("GeomLD", GeomRect,
                      default_aes = aes(colour = "black",fill="blue", size = 0.5, linetype = 1,
                                        alpha = .66))

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

