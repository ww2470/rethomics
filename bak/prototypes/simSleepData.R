rm(list=ls())

sleepContiguous <- function(moving,fs,min_valid_time=5*60){
  min_len <- fs * min_valid_time
  r_sleep <- rle(!moving)
  valid_runs <-  r_sleep$length > min_len 
  r_sleep$values <- valid_runs & r_sleep$value
  inverse.rle(r_sleep)
}





activityPropensity <- function(t, scale=1, rate=mins(1)){
  a <- 1 - abs(sin(2*pi*t/days(1)))
  a <- (.1 + a^3 *0.8) * rate
  a <- a * scale
  delta_t <- diff(t[1:2])
  a <- a * delta_t
  a
}


simulateAnimalActivity <- function(max_t=days(5), sampling_period=10, method=activityPropensity,...){
  t <- seq(from=0, to = max_t, by=sampling_period)
  propensity <- method(t,...)
  moving <- propensity > runif(length(t))
  asleep <- sleepContiguous(moving, 1/sampling_period)
  dt <-data.table(t=t, moving=moving, asleep=asleep)
  dt
}


toyActivityData <- function(query=NULL, seed=1, rate_range=1/c(60,10),...){
  set.seed(seed)
  
  if(is.null(query))
    query<- data.table(experiment_id="toy_experiment", region_id=1)
  runif(1,rate_range[1], rate_range[2])
  out <- query[,simulateAnimalActivity(rate=runif(.N,rate_range[1], rate_range[2]),...),by=names(query)]
  setkeyv(out, names(query))
}

# make sleep
query<- data.table(experiment_id="toy_experiment", region_id=1:20, condition=c("A","B"))
dt <- toyActivityData(query,3)



ggetho(asleep, dt, aes(t, asleep,colour=condition, fill=condition)) + stat_pop_etho()

