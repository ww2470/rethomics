#' Retrieves DAM2 data from continuous files
#' 
#' Uses a query mechanism to get data from a DAM2 array. 
#' This is useful when using the default behaviour of Trikinetics software
#' where data is simply appended to a single long file per monitor.
#' 
#' @param query Formatted query used to request data (see detail).
#' @param FUN Optional function to transform the data from each "region" (i.e. a `data.table`) immediately after is has been loaded. 
#' @param ... Extra arguments to be passed to `FUN`
#' @return A `data.table` where every row is an individual measurement. 
#' That is an activity at a unique time (`t`, in second) in a unique channel (`region_id`),
#'  and from a unique date/experiment (`experiment_id`).
#'  For each different combination of `start_date` and `file` in the query, an
#' individual `experiment_id` is generated.
#' @details `query` must be a `data.table`.
#' Conceptually, each row of the query describes the conditions in one channel (when `region_id` is specified),
#'  or in each monitor (when it is not).
#' It should have  the following columns:
#' * `path` the location of your data file (e.g. `"C:/User/me/Desktop/Monitor3.txt"`)
#' * `start_date` the first day of the requested experiment (e.g. `"2014-12-28_10-00-00"`).
#'     Importantly, the time component (i.e. `"10-00-00"`in this example) is used as a reference time.
#'     Here, ZT0 will be `10:00:00` in the DAM file.
#' * `stop_date`  the last day of the requested experiment (e.g. `"2014-12-30"`).
#' * `region_id`` the channel (between 1 and 32) in what the animal was in (e.g. `20`). 
#'     This is an optional column. If not provided, all 32 channels are loaded with the same conditions.
#' * `...` arbitrary columns to associate conditions/treatments/genotypes/... to the previous columns
#' 
#' @examples
#' \dontrun{
#' # This is a simple, single sample dam2 file:
#' sample_file <- getSampleDataPath('misc/DAMfile.dam')
#' query = data.table(path=sample_file,
#'                    # note the time (10:00) is added as reference time
#'                  start_date="2015-07-02_10-00-00", 
#'                  stop_date="2015-07-07",
#'                  region_id=c(1:32),condition=rep(letters[1:2],each=16))
#' print(query)
#' dt <- loadDAM2Data(query)
#' ethogramPlot(activity,dt,condition) + scale_x_continuous(breaks=0:10/2)
#' dt <- loadDAM2Data(query,FUN= sleepDAMAnnotation)
#' ethogramPlot(asleep,dt,condition) + scale_x_continuous(breaks=0:10/2)
#' }
#' @seealso  
#' * Tutorial for this function \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
#' * What queries are \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
#' * The structure of the resuting data \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
#' @export
loadDAM2Data <- function(query, FUN=NULL, ...){
  tz=""
  q <- copy(as.data.table(query))
  cn <- colnames(q)
  if(!("path" %in% cn & "start_date" %in% cn & "stop_date" %in% cn )){
    stop("query MUST have at least thre columns names `path`, `start_date` and `stop_date`")
  }
  q[, experiment_id := paste(start_date,basename(path),sep="_")]
  if(!"region_id" %in% cn)
    q <- q[q[,.(region_id=1:32),by=experiment_id]]
  setkey(q,experiment_id)
  
  wrapLoadDAM <- function(p,srtd,stpd,tz){
    out <- loadSingleDAM2File(p,srtd,stpd,tz=tz)
    out[,t:= t]
    out
  }
  
  out <- unique(q,by=key(q))[,
                             wrapLoadDAM(path,start_date, stop_date,tz),
                             by=experiment_id]
  
  q$path <- NULL
  setkeyv(out,c("experiment_id","region_id"))
  
  final_key <- copy(colnames(q))
  
  setkeyv(q,final_key)
  q[, t0:=dateStrToPosix(start_date, tz)]
  out <- merge(q,out)
  
  out[, t:=as.numeric(t-t0,units='secs')]
  out$t0 <- NULL
  q$t0 <- NULL
  
  setkeyv(out,final_key)
  
  if(!is.null(FUN)){
    out <- out[, FUN(.SD,...),by=key(out)]
  }
  
  setkeyv(out, union(key(out),colnames(q)))
  out
}

NULL
#' Read a text file formatted as DAM2 into a single data table
#'
#' This function is used to load data from DAM2 devices as a `data.table`.
#'
#' @param FILE the name of the input file.
#' @param start_date the starting date formated as "yyyy-mm-dd" or "yyyy-mm-dd_hh-mm-ss"
#' @param stop_date the last day of the experiment. Same format as \code{start_date}
#' @param tz the time zone of the computer saving the file. By default, \code{tz} is taken from the computer running this function
#' @param verbose whether to print progress (a logical).
#' @return a data table with an activity (number of beam crosses) variable, a region_id (channel) variable and a posix time stamp.
#' @examples
#' \dontrun{
#' FILE <- "Monitor53.txt"
#' out <- loadSingleDAM2File(FILE)
#' #histogram of x marginal distribution
#' hist(out[roi_id == 1, x], nclass=100)
#' }
#' \dontrun{
#' # More realistic example where we have experimental conditions, and
#' we want to resample data at 1.0Hz.
#' # First, the conditions:
#' conditions <- cbind(roi_id=1:32, expand.grid(treatment=c(T,F), genotype=LETTERS[1:4]))
#' print(conditions)
#' }
#' @noRd
loadSingleDAM2File <- function(FILE, 
                               start_date=-Inf,
                               stop_date=+Inf,
                               tz = "",
                               verbose=TRUE
){
  # 1 load time stamps
  # 1 load time stamps
  if(verbose)
    print(sprintf("Reading %s.",FILE))
  dt <- fread(FILE, select=2:4, header = FALSE)
  dt[,datetime:=paste(V2,V3, sep=" ")]
  dt[,t:=as.POSIXct(strptime(datetime,"%d %b %y %H:%M:%S",tz=tz))]
  min_date <- dateStrToPosix(start_date,tz)
  max_date <- dateStrToPosix(stop_date,tz)
  
  # if time is not in date, we add a day
  # TODO
  # ifelse(parseDateStr(stop_date)$has_time, max_date,max_date +hours(24))
  
  if(max_date < min_date)
    stop("`max_date` MUST BE greater than `min_date`")
  valid_dt <- dt[,.(
    valid = (t >= min_date & t < max_date & V4 ==1),
    idx = 1:.N,
    t=t
  )]
  valid_dt <- valid_dt[valid == T]
  
  first = min(valid_dt[,idx]) 
  last = max(valid_dt[,idx])     
  
  #2 check time stamps
  if(nrow(valid_dt) < 1){
    stop("There is apparently no data in this range of dates")
  }
  
  valid_dt[,diff_t := c(NA,diff(t))]
  valid_dt <- na.omit(valid_dt)
  sampling_periods <- valid_dt[,.(n=.N),by=diff_t]
  if(nrow(sampling_periods) > 1){
    warning(sprintf("The sampling period is not always regular in %s.
                    Some reads must have been skipped.",FILE))
    #fixme show a table of samplig rates
  }
  
  
  # 1 find duplicated time stamps.
  valid_dt[,t_str := as.character(t)]
  setkeyv(valid_dt,"t_str")
  n_dups <- sum (duplicated(valid_dt))
  
  if(n_dups > 0){
    warning(sprintf("Some of the dates are repeated between successive measument in %s.",FILE))
  }
  
  if(n_dups > 50){
    stop("More than 50 duplicated dates entries in the queries file.
         This is a likely instance of the recording computer changing time 
         (e.g. between winter and summer time)")
  }
  
  if(any(sampling_periods[,diff_t] < 0)){
    stop("Come measument appear to have been recorded 'before' previous measuments.
         It looks as if the recording computer went back in time!")
  }
  
  # 3 actually load the file
  DAM_COL_NAMES <- c("idx", "day_month_year", "time","status", sprintf("channel_%02d", 1:32))
  dt_list <- fread(FILE, drop=5:10, header = FALSE,
                   skip = first-1, nrows = last-first+1)
  setnames(dt_list,DAM_COL_NAMES)
  dt_list <- dt_list[status ==1]
  dt_list[,datetime:=paste(day_month_year,time, sep=" ")]
  dt_list[,t:=as.POSIXct(strptime(datetime,"%d %b %y %H:%M:%S",tz=tz))]
  setkeyv(dt_list,"datetime")
  dt_list <- unique	(dt_list, by=key(dt_list))
  #clean table from unused variables (idx,time, datetime...)
  dt_list[,time:=NULL]
  dt_list[,datetime:=NULL]
  dt_list[,idx:=NULL]
  dt_list[,day_month_year:=NULL]
  dt_list[,status:=NULL]
  
  out <- as.data.table(melt(dt_list,id="t"))
  
  roi_value <- function(channel_string){
    s <- strsplit(channel_string,"_")
    num <- as.integer(sapply(s,function(x) x[2]))
    return(num)
  }
  
  #get the values on activity
  setnames(out,"value", "activity")
  #out[,activity:=value]
  out[,region_id:=roi_value(as.character(variable))]
  out[,variable := NULL]
  return(out)
  }
NULL
