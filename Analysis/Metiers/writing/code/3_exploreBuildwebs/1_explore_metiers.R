# comparing classifications of major species versus clusters. 

# load metiers
#----
# load data
filtered_ftl <- readRDS("code/1_cleaningData/filtered_ftl.RDS")

files <- list.files("code/2_defineMetiers/") # load 2010 predicted metiers
pred_files <- files[grep("2010cluster_key.txt",files)]
class_files <- files[grep("2010p",files)]
predicteds <- do.call(rbind, lapply(paste0("code/2_defineMetiers/",pred_files), read.csv))
classifieds <- do.call(rbind, lapply(paste0("code/2_defineMetiers/", class_files), readRDS))
predicteds$ftid <- paste0(predicteds$ftid, 2010)
predicteds$node <- NULL
colnames(predicteds) <- c("trip_id", "metier")
colnames(classifieds) <- c("trip_id", "metier")

metiers <- rbind(classifieds, predicteds)

# merge metiers
tickets <- merge(filtered_ftl, metiers, by = "trip_id")
length(unique(tickets$trip_id)) == length(unique(metiers$trip_id))

tickets$metier <- paste(tickets$grgroup, tickets$metier, sep="_")

saveRDS(tickets, "code/3_exploreBuildwebs/tickets.RDS")
#----
# find major species in each catch
#----
library(plyr)
# by both revenue and volume
maj <- ddply(tickets, .(trip_id), summarize, m.rev = modified[which.max(adj_revenue)], m.vol = modified[which.max(landed_wt)]) # takes awhile

# append to tickets that have both cluster ID and major ID
trips <- unique(tickets[,c("trip_id","metier","grgroup")])
trips <- merge(trips, maj, by ="trip_id")

trips$m.rev <- paste(trips$m.rev, trips$grgroup, sep="_")
trips$m.vol <- paste(trips$m.vol, trips$grgroup, sep="_")
saveRDS(trips, "code/3_exploreBuildwebs/trips.RDS")
#----
# calculate ARI
#----
library(e1071)
table(trips$m.rev, trips$metier)
classAgreement(table(trips$metier, trips$m.rev))$crand

#----
# visualize fisheries
#----
# rows are fisheries, columns are by revenue
percents <- table(trips$metier, trips$m.rev)/rowSums(table(trips$metier,trips$m.rev))
library(vegan);library(RColorBrewer)
dca <- decorana(percents)
tabasco(decostand(percents,"log"), dca,col=c("white",rev(colorRampPalette(brewer.pal(9,"Blues"))(10))),cexRow=.25,cexCol=.6, add.expr=list(abline(v=7,col="grey"), text(x = 15, y = 375, "tls_1\nsalmon troll"), abline(v=48, col="grey")))

saveRDS(percents, "code/new_things/percents.RDS")

#----
#bipartite networks
#----
melt_catch <- melt(tickets, measure.vars = "landed_wt", id.vars = c("modified","metier"))
d_catch <- dcast(melt_catch,  modified~metier, fun.aggregate = sum)
web <- d_catch
row.names(web) <- web[,1]
web <- web[,-1]
web <- as.matrix(web)
plotweb(web)

# take top 10 by volume
plotweb(web[,names(sort(colSums(web),decreasing=T)[1:10])],arrow="both",text.rot=90)

saveRDS(web, "code/3_exploreBuildwebs/web.RDS")

#----
# participation networks
#----
