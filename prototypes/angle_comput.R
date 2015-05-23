library(data.table)

half_angular_distance <- function(half_angle){
  half_angle
  abs_diff <- abs(diff(half_angle))
  
  da <- ifelse(abs_diff >=90,180 - abs_diff ,abs_diff)

  c(0,da)/90
  }

phi <- runif(100,0,180)

h <- runif(100,0.015,.02)
w <- runif(100,0.02,.04)

arr <-1- h/w


dt <- data.table(h,w,phi,arr)

dt[ ,dphi:=half_angular_distance(phi)]
dt[ ,angular_activity:=dphi * arr]
dt
