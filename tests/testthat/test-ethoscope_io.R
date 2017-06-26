library(rethomics)
library(testthat)

# test_that("test db file listing/parsing works", {
#   sample_dir <- getSampleDataPath(get_sample_dir =  T)
#   result_dir <- paste(sample_dir,"ethoscope",sep="/")
#   rethomics:::checkDirExists(result_dir)
#   expect_silent({files <- buildEthoscopeQuery(result_dir)})
#   expect_equal(nrow(files), 3)
#   })
# 
# 
# test_that("Test that buildEthoscopeQuery works with simple query file",{
#   sample_dir <- getSampleDataPath(get_sample_dir =  TRUE)
#   result_dir <- paste(sample_dir,"ethoscope",sep="/")
#   
#   query <- data.table(
#         machine_name = c("E_029","E_014", "E_014"),
#         date = c("2016-01-25","2016-01-25", "2016-02-17")
#         )
#   expect_silent({query_path <- buildEthoscopeQuery(result_dir, query)})
#   expect_true("path" %in% colnames(query_path))
#   })
# 
# test_that("Test that buildEthoscopeQuery warns when no file present",{
#   sample_dir <- getSampleDataPath(get_sample_dir =  TRUE)
#   result_dir <- paste(sample_dir,"ethoscope",sep="/")
#   
#   query <- data.table(
#     machine_name = c("E_029","E_014", "E_014"),
#     date = c("2016-01-25","2016-01-25", "2016-02-19")
#   )
#   ### We do not have a file for the last entry!
#     expect_warning({query_path <- buildEthoscopeQuery(result_dir, query)}, "No result for machine_name == E_014")
#   # the last row was ignored
#   expect_equal(nrow(query_path), 2)
# })
