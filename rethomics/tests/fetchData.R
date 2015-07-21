library(rethomics)
MY_DATA_DIR <- "/data/psv_results/"
query <- data.table(date="2015-06-02",
                    machine_name=c("GGSM-001","GGSM-003"),
                    region_id = rep(1:10,each=2))

a <- fetchPsvResultFiles(MY_DATA_DIR,query)
print(a)

