rm(list=ls())
library(rethomics)

sample_files <- c("sample_1.db", "sample_2.db","sample_3.db", "validation.db")
# l1 <- lapply(sample_files, function(f){
#     file <- loadSampleData(f)
#     dt <- loadPsvData(file)
#   })

l2 <- lapply(sample_files, function(f){
  file <- loadSampleData(f)
  dt <- loadPsvData(file, FUN=sleepAnnotation,reference_hour=9.0)
})