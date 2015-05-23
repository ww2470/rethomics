data(sleep_validation)
my_data <- sleep_validation[,sleepAnnotation(.SD),by=key(sleep_validation)]

my_data