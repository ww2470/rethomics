library(rethomics)
sample_file <- system.file('data/DAMfile.txt', package="rethomics")
query = data.table(path=sample_file,
                    # note the time (10:00) is added as reference time
                  start_date="2015-06-25_10-00-00", 
                  stop_date="2015-07-13",
                  region_id=c(1:32),condition=rep(letters[1:2],each=16))
print(query)
dt <- queryDAMFiles(query)

ethogramPlot(activity,dt)

dt[,t_in_day := t%% days(1)]
dt[,day := floor(t/days(1))]
dt2 <- copy(dt)
dt2[,day := day-1]

dt2[,t_in_day := t_in_day + days(1)]
dt <- dt[day<max(day)]
dd <- rbind(dt, dt2)
dd <- dd[day>-1]

dd[, day_str := sprintf("day\n%03d",day)]
dd <- dd[, .(activity = mean(activity)),by=c("t","t_in_day","day_str","day")]

ggplot(dd,aes(t_in_day/hours(1),ymax=activity, ymin=min(activity))) +
  geom_ribbon() + 
  facet_grid(day_str ~ .) + scale_x_continuous(name="time (h)",breaks = 0:8 * 6)+
  scale_y_continuous(name="activity", breaks=NULL)
  #scale_y_continuous(name="beam crosses")
  

