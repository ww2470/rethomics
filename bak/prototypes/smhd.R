SECONDS_PER_MINUTE <- 60
MINUTES_PER_HOUR <- 60
HOURS_PER_DAY <- 24
SECONDS_PER_HOUR <- MINUTES_PER_HOUR * SECONDS_PER_MINUTE
SECONDS_PER_DAY <- HOURS_PER_DAY * SECONDS_PER_HOUR

setClass("smhd", contains = "numeric")


smhd <- function(seconds, minutes=0, hours=0, days=0){
  new("smhd", as.numeric(  seconds+
                           minutes * SECONDS_PER_MINUTE+
                           hours * SECONDS_PER_HOUR + 
                           days * SECONDS_PER_DAY))
}

a_days <- function(x) {
  trunc(as.numeric(x) / SECONDS_PER_DAY)
}

a_hours <- function(x) {
  trunc(as.numeric(x) / SECONDS_PER_HOUR)
}

hour_of_day <- function(x) {
  abs(a_hours(x) - a_days(x) * HOURS_PER_DAY)
}

a_minutes <- function(x) {
  trunc(as.numeric(x) / SECONDS_PER_MINUTE)
}

minute_of_hour <- function(x) {
  abs(a_minutes(x) - a_hours(x) * MINUTES_PER_HOUR)
}

a_seconds <- function(x) {
  trunc(as.numeric(x))
}

second_of_minute <- function(x) {
  abs(a_seconds(x) - a_minutes(x) * SECONDS_PER_MINUTE)
}

split_seconds <- function(x) {
  as.numeric(x)
}

split_second_of_second <- function(x) {
  abs(split_seconds(x) - a_seconds(x))
}


as.character.smhd <- function(x, ...) {
  sprintf("@%d",x)
}


format_two_digits <- function(x) {
  formatC(x, width = 2, flag = "0")
}

format.smhd <- function(x, ...) {
  format(as.character(x), justify = "right")
}

print.smhd <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

format_split_seconds <- function(x) {
  split_second <- split_second_of_second(x)
  out <- format(split_second, scientific = FALSE)
  digits <- max(min(max(nchar(out) - 2), 6), 0)
  out <- formatC(split_second, format = "f", digits = digits)
  gsub("^0", "", out)
}




as.character.smhd <- function(x, ...) {
  ifelse(is.na(x), "NA", paste0(
    ifelse(x < 0, "-", ""),
    abs(a_days(x)), "d ",
    format_two_digits(hour_of_day(x)), ":",
    format_two_digits(minute_of_hour(x)), ":",
    format_two_digits(second_of_minute(x)),
    format_split_seconds(x)))
  
}

format.smhd <- function(x, ...) {
  format(as.character(x), justify = "right")
}

print.smhd<- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}


as.smhd.character <- function(x, ...) {
    as.difftime(as.character(x), format = "%H:%M:%OS", units = "secs")
}

a <- smhd(1:200 )
a <- a + 1
dt <- data.table(t=a, y=rnorm(200))
dt[, .(t+days(1))]
dt[t > days(2)+hours(3)]
ggplot(dt, aes(t,y))
smhd(1.1)
