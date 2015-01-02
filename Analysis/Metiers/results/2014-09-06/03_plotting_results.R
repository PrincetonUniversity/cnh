# merge model runs together

#---- choose number of targets ----
library(devtools)
#install_github("noamross/noamtools")
library(noamtools)
library(topicmodels)
library(tm)
library(Rmpfr)
library(plyr)
library(XML)
library(stringi)
library(dplyr)
library(reshape2)
library(slam)
library(LDAvis)
library(LDAtools)
library(ggplot2)
setwd("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-09-06/topic_model_runs/")

files.in <- dir("model_runs/")

models = list()

for(i in 1:length(files.in)){
  models[[i]] <- readRDS(paste("model_runs/",files.in[i], sep=""))  
}
ks = 2:(length(files.in)+2)
ks <- ks[-which(ks==24)] # missing 24
burnin <- 1000
iter <- 1000 
keep <- 50 

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

logLiks <- lapply(models, function(L)  L@logLiks[-c(1:(burnin/keep))])
hm <- sapply(logLiks, function(h) harmonicMean(h))
k = sapply(models, function(L) sum(length(L@beta) + length(L@gamma)))
AICs = -2*hm + 2*k

AIC_df <- data.frame(hm = hm, AICs = AICs, ks = ks)

library(scales)
plot(AIC_df$ks, -1*(AIC_df$AICs),type="o",pch=19, bty="n", xlab="Number of targets", ylab = "Relative parsimony of model (negative AIC)", col=alpha("steelblue", .75), lwd=3)


opt <- models[which.min(AICs)][[1]]
top.opt = ks[which.min(AICs)]

#---- make LDAvis -----
# Extract the 'guts' of the optimal model
doc.id <- opt@wordassignments$i
token.id <- opt@wordassignments$j
topic.id <- opt@wordassignments$v
vocab <- opt@terms

# Get the phi matrix using LDAviz
dat <- getProbs(token.id, doc.id, topic.id, vocab, K = max(topic.id), sort.topics = "byTerms")

  # phi.hat = each row is the probability distributions for a topic
  # theta.hat = proportion of topics for each trip, each row is a trip. each column is a topic
phi <- t(dat$phi.hat)
# NOTE TO SELF: these things have to be numeric vectors or else runVis() will break...add a check in check.inputs
token.frequency <- as.numeric(table(token.id))
topic.id <- dat$topic.id
topic.proportion <- as.numeric(table(topic.id)/length(topic.id))

# order in visualization is by the topics that occur most frequently across all trips. 

# Run the visualization locally using LDAvis
z <- check.inputs(K=max(topic.id), W=max(token.id), phi, token.frequency, vocab, topic.proportion)
json <- with(z, createJSON(K=max(topic.id), phi, token.frequency, 
                           vocab, topic.proportion, dist.measure = "JS"))



#runShiny(phi, token.frequency, vocab, topic.proportion)
serVis(json, out.dir="astoria_lda_", open.browser = FALSE)

# Cluster targets
#------ 
library(cluster)
k = 1:100

cluster_results <- list()

# transform data into hellinger
library(vegan)
trip_trans <- decostand(dat$theta.hat, method = "hellinger")

for(i in 1:max(k)){
  cluster_results[[i]] <- clara(trip_trans, k[i], samples = 100, sampsize = 200)
  cat(i)
}

avg_sil <- rep(NA, max(k))
for(i in 2:max(k)){
  avg_sil[i] <- cluster_results[[i]]$silinfo$avg.width
}

plot(avg_sil,type='o',cex=.7,pch=19, bty="n", xlab="Number of clusters", ylab="Average silhouette width",lwd=2)
text(k, avg_sil, k, col="white", cex=.6, font=2)
which.max(avg_sil)

# 33 has the max
cluster_sol <- cluster_results[[which.max(avg_sil)]]$medoids
sol_trips <- table(cluster_results[[which.max(avg_sil)]]$clustering)

library(reshape2)
df_sol <- melt(cluster_sol)
colnames(df_sol) <- c("cluster","target", "proportion")
library(ggplot2)
df_sol$target <- factor(df_sol$target)
paint = "black"
ggplot(df_sol, aes(x=target, y=proportion)) + geom_bar(stat="identity",fill=paint) + facet_wrap(~cluster)+ theme_bw() + theme(strip.background = element_blank(), axis.text.x = element_text(size = 3)) 
# Make network between targets and gear
#----
# now question is: which gears go with which cluster?

ftl <- read.csv("/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors=F)

gear <- subset(ftl, ftid %in% rownames(trip_majority), select = c(ftid,grid))
gear <- unique(gear)

gear_ordered <- data.frame(ftid= as.numeric(rownames(trip_majority)))
gear_ordered <- merge(gear_ordered, gear, by = "ftid")

cluster_gear <- data.frame(cluster = cluster_results[[which.max(avg_sil)]]$clustering, gear = gear_ordered$grid, ftid=gear_ordered$ftid)

cluster_all <- merge(cluster_gear, data.frame(ftid=ftl$ftid, veid=ftl$veid,stringsAsFactors=F), by = "ftid", all.x = T, all.y =F)
cluster_small <- unique(cluster_all)


fishery_types <- table(cluster_small$gear, cluster_small$cluster)


# plot gear graph
#-----
library(igraph)

fish_edge <- melt(fishery_types)
fish_edge <- subset(fish_edge, value > 10)
colnames(fish_edge) <- c("from","to","weight")

fisheries <- graph.data.frame(fish_edge, directed = FALSE)
E(fisheries)$weight <- fish_edge$weight

paint <- rep("white",40)

gear_paint <- colorRampPalette(tail(head(brewer_pal(palette = "YlOrBr")(9),-2),-2))(10)
gear_paint <- sample(gear_paint)

fish_paint <- colorRampPalette(tail(brewer_pal(palette = "YlGnBu")(9),-3))(30)
fish_paint <- sample(fish_paint)

paint[1:10] <-gear_paint
paint[11:40] <- fish_paint
V(fisheries)$color <- paint
V(fisheries)$type <- c(rep(FALSE,10), rep(TRUE, 30))

par(bg="grey")
plot(fisheries,axes=F, edge.width = fish_edge$weight/27, vertex.label.color = "white", vertex.frame.color=paint, vertex.label.font = 2, layout=layout.bipartite,edge.color="slategrey",vertex.label.cex = .6, asp=0, ylim=c(-5,5),vertex.size=8)

# get rid of ONT and nodes 
sub_fisheries <- delete.vertices(fisheries, c("ONT","24","19","21","17","26","22","24","18","20","23","25","32","30","29","33","27","28"))
V(sub_fisheries)$type <- c(rep(FALSE, 9), rep(TRUE, 14))
new_paint <- paint[c(1:9, 11:24)]
new_edge <- subset(fish_edge, from!="ONT")
V(sub_fisheries)$color <- new_paint
par(bg="white")
plot(sub_fisheries, layout=layout.bipartite(sub_fisheries,maxiter=500), vertex.label.color="white", vertex.frame.color=new_paint, edge.width = new_edge$weight/25, edge.color="slategrey",vertex.label.cex = 1, asp=0, ylim=c(-5,5),vertex.size=13, vertex.label.font=2)
# Make network of fisheries - including all ONT groups
# ---- 
# now network of fisheries. there are how many fisheries total (edges?)
length(E(fisheries)) # 37, but more than 10 of those are the weird sardine stuff. 

# ---- network, including weird sardines

# which gear/cluster combinations are not frequent enough to be considered?

all_trips <- cluster_small
all_trips$fishery <- paste(all_trips$gear, all_trips$cluster, sep="_")

# subset to just trips included. these are 
fisheries_included <- paste(fish_edge$from, fish_edge$to, sep="_")

used_trips <- subset(all_trips, fishery %in% fisheries_included)

# collapse ONT into one
  used_trips$fishery[grep("ONT",used_trips$fishery)] <- "ONT"
  # change fisheries included to match
  fisheries_included[grep("ONT",fisheries_included)] <-"ONT"\
  fisheries_included <- unique(fisheries_included)

ves_trips <- table(used_trips$veid, used_trips$fishery)

fishery_mat <- matrix(ncol=length(fisheries_included), nrow=length(fisheries_included))
colnames(fishery_mat) <- colnames(ves_trips)
rownames(fishery_mat) <- colnames(ves_trips)

for(i in 1:ncol(ves_trips)){ # for each fishery except the last one
  fish_i <- colnames(ves_trips)[i]
  cat("j: \n")
  for(j in i:ncol(ves_trips)){
    fish_j <- colnames(ves_trips)[j]
    num_ves = length(which(which(ves_trips[,i]>0) %in% which(ves_trips[,j] > 0)))
    
    mat_row <- which(rownames(fishery_mat)==fish_j)
    mat_col <- which(colnames(fishery_mat)==fish_i)
    
    fishery_mat[mat_row, mat_col] <- num_ves
    cat(j," ")
  }
  cat("\n")
}

g<-graph.adjacency(fishery_mat,weighted=T, diag=F, mode = "lower")
l <- layout.fruchterman.reingold(g, niter=500, area=vcount(g)^2, repulserad=vcount(g)^2)
plot(g, edge.width = E(g)$weight, layout = l)

# any cliques?
lc <- largest.cliques(g)
V(g)$label <- V(g)$name
g.lc <- induced.subgraph(g, lc[[1]])
plot(g.lc, edge.width = E(g.lc)$weight, layout = layout.spring)
  # woudl be nice to color these in larger graph. 

# see this stackoverflow answer for overview
# http://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph

# communities using the fast_greedy model
fgg <- fastgreedy.community(g)

# membership using infomap
ifm <- infomap.community(g)
membership(ifm)
communities(ifm)

# membership using random walk
walk <- walktrap.community(g)
membership(walk)
cbind(membership(ifm),membership(walk), membership(fgg))
V(g)$size <- diag(fishery_mat)

# color by walktrap community
paint <- brewer_pal(type="qual", palette = "Dark2")(5)
V(g)$membership <- walk$membership
V(g)[membership==1]$color <- paint[1]
V(g)[membership==1]$frame.color <- paint[1]
V(g)[membership==2]$color <-paint[2]
V(g)[membership==2]$frame.color <-paint[2]
V(g)[membership==3]$color <- paint[3]
V(g)[membership==3]$frame.color <-paint[3]
V(g)[membership==4]$color <-paint[4]
V(g)[membership==4]$frame.color <-paint[4]
V(g)[membership==5]$color <- paint[5]
V(g)[membership==5]$frame.color <- paint[5]

par(mai=rep(0,4))
plot(g, vertex.color=V(g)$color, edge.width = E(g)$weight, vertex.size=sqrt(V(g)$size)*3, vertex.frame.color=V(g)$frame.color, vertex.label.color="black",layout=l)


# trips over time
trip_date <- unique(subset(ftl, ftid %in% used_trips$ftid, select = c(ftid, tdate)))

used_trips <- merge(used_trips, trip_date, by = "ftid", all.x=T, all.y = F, sort=F)
by_day <- ddply(used_trips, .(tdate, fishery), summarize, nt = length(unique(ftid)))

by_day$tdate <- as.POSIXlt(by_day$tdate, format="%d-%b-%y")
by_day$fishery <- as.factor(by_day$fishery)

community <- data.frame(community = membership(walk), fishery = names(membership(walk)))
rownames(community) <- NULL
          
by_day <- merge(by_day, community, by = "fishery", all.x = T, all.y = F)
by_day$community <- factor(by_day$community)

# color by group
names(paint) <- levels(by_day$community)
colScale <- scale_colour_manual(name = "community", values = paint)
ggplot(by_day, aes(x = tdate, y = nt, colour = community)) + geom_line(size = 1.5) + theme_bw() + colScale + ylab("Trips landed") + xlab("")

#----
# interested in particular in groundfish group. The flat fish, roller trawls clusters. 
bottom_fish <- induced.subgraph(g, V(g)[membership==2])
plot(bottom_fish, edge.width=E(bottom_fish)$weight, vertex.size = V(bottom_fish)$size)

V(bottom_fish)
day_bottom <- subset(by_day, fishery %in% tail(V(bottom_fish)$name,-1))

ggplot(day_bottom, aes(x = tdate, y = nt, colour = fishery)) + geom_line(size=2) + theme_bw() + xlab("") + ylab("Number of trips")
#####
# what about beta diversity among fisheries. do most people do most things? 
head(used_trips)

beta_data <- table(used_trips$fishery, used_trips$veid)
beta <- vegdist(beta_data, binary = TRUE, diag = T) # beta - Sørensen's dissimilarity

bray <- vegdist(beta_data, method = "bray",diag=T) # bray-curtis. 1 is no similarity, 0 is complete. 

dis_association <- as.matrix(bray)
similarity <- abs(1-dis_association)
similar_network <- graph.adjacency(similarity, weighted = TRUE, mode = "undirected",diag=FALSE)
par(mai=rep(0,4))
V(similar_network)$color <- V(g)$color
V(similar_network)$frame.colr <- V(g)$frame.color
V(similar_network)$size <- apply(beta_data, 1, function(x) length(which(x > 0)))
plot(similar_network, edge.width = E(similar_network)$weight*30,layout=l,vertex.frame.color=V(g)$frame.color, vertex.size = V(similar_network)$size/1.15)

heatmap(association, col=c("white", brewer.pal(9, "Blues")))
