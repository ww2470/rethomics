rm(list=ls()); graphics.off()
library(rethomics)
OUT_NAME <- "dam_data.RData"
sample_file <- system.file('data/DAMfile.txt', package="rethomics")

query = data.table(path=sample_file,
                  # note the time (10:00) is added as reference time
                start_date="2015-07-02_10-00-00", 
                stop_date="2015-07-07",
                region_id=c(1:32),condition=rep(letters[1:2],each=16))
dt <- loadDAM2Data(query)
#ethogramPlot(asleep,dt )
dam_data <- dt
save(dam_data,file=OUT_NAME,compress = "xz")

