rm(list=ls()); graphics.off()
library(rethomics)
MY_RESULT_DIR <- "/data/psv_results"
OUT_NAME <- "sleep_sexual_dimorphism.RData"
query <- fread("sleep_sexual_dimorphism.csv")
path_query <- buildEthoscopeQuery(MY_RESULT_DIR, query)
dt <- loadEthoscopeData(
  # What files/conditions to fecth
  path_query, 
  # We apply directly the function to annotate sleep event. This will make the data smaller (1point/10s)
  FUN=sleepAnnotation, 
  # Importantly, we define the refence hour (zt0) at 09:00, GMT (i.e. 10:00 in British Summer Time)
  reference_hour=9.0, 
  cache=TRUE,
  n_cores=4
)

dt$w <- NULL
dt$h <- NULL
dt$phi <- NULL
dt$xor_dist <- NULL
dt$mlog_L_x1000 <- NULL
xy_dist_log10x1000 <- NULL

sleep_sexual_dimorphism <- dt
save(sleep_sexual_dimorphism,file=OUT_NAME,compress = "xz")
