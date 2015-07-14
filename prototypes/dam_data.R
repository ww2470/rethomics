rm(list=ls())
library(rethomics)


query = data.table(start_date="2015-05-04", stop_date = "2015-05-08", machine_id=c("M009","M010","M011"), sex=c("m_f","m_0","m_m"))
print(query)
dt <- fetchDAMData("/data/dailyData/", query,reference_hour = 10.0, FUN=sleepDAMAnnotation)

ethogramPlot(asleep,dt[t > days(2.5) & t<days(3.5)],sex,error_bar = "sem")
overviewPlot(moving,dt,sex,normalise_var_per_id = F)

library(devtools)
install_github("gilestrolab/rethomics", subdir="rethomics")
