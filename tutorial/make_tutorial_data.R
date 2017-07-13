library(rethomics)
DST_DIR <- paste(TUTO_DATA_DIR, "ethoscope", "RData", sep="/")
ETHOSCOPE_DATA <- paste(TUTO_DATA_DIR, "ethoscope", "results", sep="/")
rethomics:::checkDirExists(DST_DIR)
rethomics:::checkDirExists(ETHOSCOPE_DATA)
ETHOSCOPE_QUERY <- paste(TUTO_DATA_DIR, "ethoscope", "queries", "query.csv", sep="/")
DST_FILE <- paste(DST_DIR, "ethoscope_tuto_data.RData", sep="/")
q <- fread(ETHOSCOPE_QUERY)
q <- buildEthoscopeQuery(ETHOSCOPE_DATA,q)
raw_data_exple_dt <- loadEthoscopeData(q[region_id <= 4],reference_hour = 9.00, max_time = days(1.5))

tables()
save(raw_data_exple_dt, file = DST_FILE)







