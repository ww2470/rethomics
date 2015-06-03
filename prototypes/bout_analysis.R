#data(sleep_validation)
#my_data <- sleep_validation[,sleepAnnotation(.SD),by=key(sleep_validation)]


makeBoutDt <- function(x,sub_data){
  sdt <- copy(sub_data)
  sdt[,delta_t:= c(0,diff(sub_data[,t]))]
  
  r <- rle(x)
  vals <-r$values
  r$values <- 1:length(r$values)
  
  bdt <- data.table(delta_t = sdt[,delta_t], bout_id=inverse.rle(r),key="bout_id")
  
  bout_times <- bdt[,list(length=sum(delta_t)),by="bout_id"]
  r$values <- vals
  out <- data.table(
    x = vals,
    length = bout_times[,length],
    start_time = cumsum(bout_times[,length]) + sdt[1,t]
  ) 
  var_name <- deparse(substitute(var))
  setnames(out,"x",var_name)
  out
}


boutAnalysis <- function(var,data){
  dt <- copy(as.data.table(data))
  var_name <- deparse(substitute(var))
  setnames(dt,var_name,"var")
  
  out <- dt[,makeBoutDt(var,.SD),by=c(key(dt))]
  
  setnames(out,"var",var_name)
  out
}

d <- boutAnalysis(moving, dt)



