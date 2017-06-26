library(rethomics)

test_that("Check directory function throughs error if a directory does not exist", {
  expect_error(rethomics:::checkDirExists("/test/doesNotExist"), "The directory .* does not exist")
})


test_that("Check directory function does NOT through error if a directory exists", {
  dir <- getwd()
  expect_silent(rethomics:::checkDirExists(dir))
})




test_that("Check columns function throughs an error if a column does NOT exist", {
  df <- data.frame(a=1:3, b=3:5,c=19,z_1=NA)
  expect_error(rethomics:::checkColumns("d", df), "The following columns are needed, but not found: d")
  expect_error(rethomics:::checkColumns(c("c","d"), df), "The following columns are needed, but not found: d")
  
})
# 


test_that("Check columns function does NOT through ant error if all columns do exist", {
  df <- data.frame(a=1:3, b=3:5,c=19,z_1=NA)
  expect_silent(rethomics:::checkColumns("a", df))
  expect_silent(rethomics:::checkColumns("b", df))
  expect_silent(rethomics:::checkColumns("c", df))
  expect_silent(rethomics:::checkColumns( "z_1", df))
  expect_silent(rethomics:::checkColumns(c("b","a","z_1","c"), df))
})


test_that("Dates are parsed correctly from str", {
  posix = 1444435200
  date_str = "2015-10-10"
  datetime <- rethomics:::parseDateStr(date_str, "GMT")
  expect_false(datetime$has_time)
  expect_equal(as.integer(datetime$date),posix)
  
  datetime <- rethomics:::parseDateStr(date_str, "Europe/London")
  
  expect_false(datetime$has_time)
  expect_equal(as.integer(datetime$date + 3600),posix)
  expect_error(rethomics:::parseDateStr(date_str, "BST"), "This time zone does not exist.*")
  
})



