
library(rethomics)

query <- data.table(
  machine_name=c("GGSM-006","GGSM-007","GGSM-008"),
  condition=c("mf","mm","mock"),
  time=c("2015-05-21"))

map <- fetchPsvResultFiles("/data/psv_results/",query)
dt <- loadPsvData(map,FUN=sleepAnnotation, reference_hour = 9.0)
print(xtabs( ~ region_id + machine_name,dt))

dt
overviewPlot(dt,"moving","condition")
overviewPlot(dt,"asleep","condition")
ethogramPlot(dt,"activity","condition")
ethogramPlot(dt,"asleep","condition")
ethogramPlot(dt,"moving","condition")



query <- data.table(
  machine_name=c("GGSM-006","GGSM-007","GGSM-008"),
  condition=c("mf","mm","mock"),
  time=c("2015-05-19"))

map <- fetchPsvResultFiles("/data/psv_results/",query)
dt_baseline <- loadPsvData(map,FUN=sleepAnnotation, reference_hour = 9.0)
print(xtabs( ~ region_id + machine_name,dt_baseline))

overviewPlot(dt_baseline,"moving","condition")
overviewPlot(dt_baseline,"asleep","condition")
ethogramPlot(dt_baseline,"activity","condition")
ethogramPlot(dt_baseline,"asleep","condition")
ethogramPlot(dt_baseline,"moving","condition")




map<- data.table(path=rep(c(
                      "/data/psv_results/00056dfce6e94dee9bb1a845281b086e/GGSM-005/2015-05-06_15-50-48/2015-05-06_15-50-48_00056dfce6e94dee9bb1a845281b086e.db",
                      "/data/psv_results/00066dfce6e94dee9bb1a845281b086e/GGSM-006/2015-05-06_15-51-39/2015-05-06_15-51-39_00066dfce6e94dee9bb1a845281b086e.db")
                      ,each=32),
                    region_id=rep(1:32,length.out=64))

map[,infected:=ifelse(region_id>16,F,T)]


dd <- loadPsvData(map,FUN=sleepAnnotation,reference_hour = 9)




overviewPlot(dd,"activity","infected",normalise_var_per_id = F)


# quick and lifespan analysis. TODO curate data first
ls_dt <- dd[,list(lifespan_day=max(t)/(24*3600)),
            by=c(key(dt),"infected")]
ggplot(ls_dt, aes(x=infected, y=lifespan_day,fill=infected)) + geom_boxplot()

ethogramPlot(dd,"activity","infected")

dd_cur <- subset(dd, region_id != 25 | experiment_id != "2015-05-06_15-51-39_00066dfce6e94dee9bb1a845281b086e.db")

ethogramPlot(dd_cur,"activity","infected")
ethogramPlot(dd_cur,"asleep","infected")


p <- ggplot(dd_cur[asleep==T,], aes(x_rel, colour=sex)) + geom_density()

dd_cur[,x_rel:=ifelse(region_id > 16, 1-x,x)]

ggplot(dd_cur[asleep==T & t < 4*24*3600,], aes(x_rel, colour=infected)) + geom_density()
ggplot(dd_cur[asleep==F,], aes(x_rel, colour=infected)) + geom_density()

ggplot(
  dd[region_id == 25 & experiment_id == "2015-05-06_15-51-39_00066dfce6e94dee9bb1a845281b086e.db",],
  aes(t,x)) + geom_line()
  
dd_i25 <- dd[region_id == 25 & experiment_id == "2015-05-06_15-51-39_00066dfce6e94dee9bb1a845281b086e.db",]
day <- function(nd){
  return(3600*24*nd)
}
ggplot(dd_i25[t> day(4)],
       aes(x, colour=infected)) + geom_density()
mean(dd_i25[t> day(4), w])

#sleepPlotPipeLine()