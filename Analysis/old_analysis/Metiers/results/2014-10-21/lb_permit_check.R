# logbook permits?
lb_2010 <- read.csv("/Users/efuller/1/CNH/Data/Catch/LBK_2010_woc_samhouri.csv",stringsAsFactors=F, skip = 2)


lb_2010$year <- paste0(20,str_sub(lb_2010$ticket_date, -2))
lb_2010$trip_id <- paste0(lb_2010$ftid, lb_2010$year)

met_merge <- unique(select(tickets, trip_id, metier))

lb_met <- merge(lb_2010, met_merge, all.x = T, by = "trip_id")

# lookes like i only have groundfish logbooks. so wouldn't be able to use this for permits anyway. 
