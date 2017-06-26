library(rethomics)
copyCreateDir <-function(src,dst){
  dir.create(dirname(dst), recursive=T)
  file.copy(src, dst)
}

copyDataForQuery <- function(query,
                             result_dir,
                             target_dir)
{
  
  q <- buildEthoscopeQuery(result_dir=result_dir, query)
  to_dl <- q[,.(src = unique(path))]
  to_dl[,dst := gsub(result_dir,target_dir,src)]
  to_dl[,id := 1:nrow(to_dl)]
  to_dl[,copyCreateDir(src,dst),by=id]
  q
}
q <- "/home/quentin/comput/sleep_analysis_experiments-git/ethoscope_paper/Z_preliminary_results/20160321_dyn_sleep_csIso_noArtefact/query.csv"
q <- fread(q)
q2 <- q[sdi==0, .(date,machine_name, region_id,sex, status)]
copyDataForQuery(q2, "/data/ethoscope_results/", "/tmp/tutorial_query")

write.csv(q2, "/tmp/query.csv",row.names = F)
  