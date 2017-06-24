rm(list=ls())
library(rethomics)
library(ggplot2)

path <- rep(c("/data/psv_results/00076dfce6e94dee9bb1a845281b086e/GGSM-007/2015-05-08_16-28-08/2015-05-08_16-28-08_00076dfce6e94dee9bb1a845281b086e.db",
              "/data/psv_results/00086dfce6e94dee9bb1a845281b086e/GGSM-008/2015-05-08_16-30-02/2015-05-08_16-30-02_00086dfce6e94dee9bb1a845281b086e.db"),
            each=32)

region_id <- 1:32
sex <- rep(c("female","male"),length.out=16)
sex <- c(sex,rev(sex))
map <- data.table(path,region_id,sex)
print(map)

my_data <- sleepPlotPipeLine("/tmp/sleep_out.pdf",map,"sex")