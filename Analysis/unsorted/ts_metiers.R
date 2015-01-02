# making a time-series for a port, look at Newport

tickets <- readRDS(
  "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/tickets.RDS"
  )

newport <- subset(tickets, pcid=="NEW")
length(unique(newport$veid)) # 603 vessels

vessels <- unique(newport$veid)

# are these vessels landing only in newport?
landing_port <- function(vessel_id){
  v1 <- subset(tickets, veid==vessel_id)
  v1_returns <- unique(v1[,c("trip_id", "pcid")])
  percent_return <- table(v1_returns$pcid)/sum(table(v1_returns$pcid)) # only newport
  newport_return <- percent_return[which(names(percent_return)=="NEW")]
  return(newport_return)
}

percent_returns <- vector()

for(i in 1:length(vessels)){
  percent_returns[i] <- landing_port(vessels[i])
  cat(i," ")
}

# some, not all. meh, not going to worry about it
library(igraph)

# make layout for all nodes, then connections for all
mat <- matrix(ncol=length(unique(newport$metier)), nrow=length(unique(newport$metier)), 1)
diag(mat) <- 0
# each timestep, node size is the number of trips landed that day. 
g <- graph.adjacency(mat,mode = "undirected")
plot(g)

library(plyr)
met_ts <- ddply(newport, .(tdate, metier), summarize, num_trips = length(unique(trip_id)))
met_ts$tdate <- as.POSIXlt(met_ts$tdate, format="%d-%b-%y",tz="US/Pacific")
library(ggplot2)
ggplot(met_ts, aes(x=tdate, y =num_trips, group = metier, colour=factor(metier)))+ geom_line(stat="identity") + theme(legend.position="none")

met_ts$week <- strftime(met_ts$tdate, format="%U")
met_ts <- met_ts[order(met_ts$tdate),]
met_ts$week_id <- paste(met_ts$week, strftime(met_ts$tdate, format="%Y"),sep="-")

met_ts$tdate <- as.character(met_ts$tdate)
week_ts <- ddply(met_ts, .(week_id, metier), summarize, trips = sum(num_trips), date = unique(tdate)[1])
week_ts$date <- strptime(week_ts$date, format="%Y-%m-%d",tz="US/Pacific")
week_ts <- week_ts[order(week_ts$date),]
ggplot(week_ts, aes(x=date, y =trips, group = metier, color=factor(metier)))+ geom_bar(stat="identity",position="dodge") + theme_minimal() + theme(legend.position="none")
