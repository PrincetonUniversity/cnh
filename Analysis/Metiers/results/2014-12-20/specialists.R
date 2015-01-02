# have not adjusted for 2009 dollars, need to do that for this. 

yrdf = readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/yrdf2.RDS")
tickets = readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/tickets.RDS")

# want profit per trip. then take mean profit per trip for generalists and specialists 

# specialists are in yrdf, only participate in 1 fishery

specialists_id <- yrdf[[2]]$drvid[which(yrdf[[2]]$mean_num_fisheries==1)]
length(specialists_id) # 1175 specialists
generalists_id <- yrdf[[2]]$drvid[which(yrdf[[2]]$mean_num_fisheries>1)]
length(generalists_id) # 2490 generalists

spec <- subset(tickets, drvid %in% specialists_id)
gen <- subset(tickets, drvid %in% generalists_id)

# go by vessel, return mean profit per trip 
spec.trip <- ddply(spec, .(drvid, trip_id), summarize, profit=sum(ppp*landed_wt))
spec.mean.trip <- ddply(spec.trip, .(drvid), summarize, median_trip_profit = median(profit))

gen.trip <- ddply(gen, .(drvid, trip_id), summarize, profit=sum(ppp*landed_wt))
gen.mean.trip <- ddply(gen.trip, .(drvid), summarize, median_trip_profit=median(profit))

hist(spec.mean.trip$median_trip_profit, col="dodgerblue",bor="steelblue",breaks=50,freq=F,add=T)
hist(gen.mean.trip$median_trip_profit, col="tomato",bor="indianred",breaks=30,freq=F)

plot(density(gen.mean.trip$median_trip_profit),col="tomato",lwd=2)
lines(density(spec.mean.trip$median_trip_profit),col="dodgerblue",lwd=2)
boxplot(gen.mean.trip$median_trip_profit[which(gen.mean.trip$median_trip_profit>0)],spec.mean.trip$median_trip_profit[which(spec.mean.trip$median_trip_profit>0)],log = "y",col=c("tomato","dodgerblue"),border=c("indianred","steelblue"),pch=19,cex=.5,names=c("generalists","specialists"))
# many more outliers for 
