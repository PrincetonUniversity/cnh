# infomap on network of astoria trips

library(igraph)
library(vegan)
library(reshape2)

ftl <- read.csv("/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors=F)

# let's subset to Astoria tickets from 2012
astoria_trips <- subset(ftl, pcid%in% c("AST", "NEW") & grid =="CPT",select=c("spid", "ftid","landed_wt", "grid", "veid"))

# select relevant data, construct trip matrix
melt_astoria <- melt(astoria_trips, variables = c("spid","ftid"), measure.vars = "landed_wt")
cast_astoria <- dcast(melt_astoria, ftid ~ spid, fun.aggregate = sum)
rownames(cast_astoria) <- cast_astoria$ftid
cast_astoria$ftid <-NULL

headtrip_mat <- as.matrix(cast_astoria)
# 4000 takes 44 seconds
# 5000 takes 72 seconds
# 6000 takes 105 seconds
# 7000 takes 171 seconds


bc <- as.matrix(vegdist(headtrip_mat, "bray"))
bc <- abs(bc-1)

bc_net <- graph.adjacency(bc, mode="undirected", weighted = TRUE)

system.time({
ic <- infomap.community(bc_net, e.weights = E(bc_net)$weight)
})
cg <- contract.vertices(bc_net, membership(ic))
E(cg)$weight <- 1
cg2 <- simplify(cg, remove.loops=TRUE)

# give names to clusters
par(bg="#253494", mai=rep(0,4))

vertex.type <- function(i){
  catch <- cast_astoria[sample(V(cg2)$name[[i]], 3), ]
  if(is.null(dim(catch[,which(colSums(catch)>0)]))){
    return(which.max(colSums(catch)))
  }else{
    return(catch[,which(colSums(catch)>0)])
  }
}

#gf - RLT, FTS only, astoria & newport
V(cg2)$name <- c("general","lingcod","small_crab","halibut","pop2","nusp","petrale","darkblotched","canary","sable","yellowtail","cabezon","chinook")

#gf - RLT, FTS only astoria
#V(cg2)$name <- c("general","ling_crab","small_crab","POP2","halibut","NUSP", "petrale","yellowtail_plus","sml_cnr1","darkblotched","cabezon", "small_sable","big_cnr1")

#2013
#V(cg2)$name <- 
#2012
#V(cg2)$name <- c("crab", "pelagics", "groundfish", "chinook", "whiting", "shrimp", "albacore", "sablefish", "lingcod", "halibut", "few_crab", "hagfish", "coho", "few_sable", "other_crab")
#2011
#V(cg2)$name <- c("crab","whiting","chinook","groundfish","albacore", "shrimp","pelagics","sablefish","hagfish","dogfish","lingcod","few_crab","halibut","chum","yellowtail")
#2010
#V(cg2)$name <- c("crab", "groundfish","chinook","pelagics","albacore","shrimp","whiting","dogfish_arrowtooth_yellowtail","razor_clam","sablefish","pop2_rockfish","petrale","nusp_rockfish","lingcod","halibut","canary","anchovies","darkblotched", "pacific_cod")
#2009
#V(cg2)$name <- c("crab","groundfish","pelagics","albacore","coho","whiting","shrimp","dogfish","sablefish","chinook","rockfish_pop2","razor_clam","nusp_rockfish","lingcod","darkblotched_rockfish","hagfish","thornyhead","cabezon","halibut","dover")

V(cg2)$size <- table(membership(ic))
par(bg="steelblue")

l <- layout.fruchterman.reingold(cg2, niter=500, area=vcount(cg2)^2, repulserad=vcount(cg2)^2)


plot(cg2, vertex.size = log(V(cg2)$size)*3, vertex.label = V(cg2)$name, edge.width = log(E(cg2)$weight), layout = l, vertex.frame.color="white", edge.color="#a1dab4", vertex.color="#41b6c4", vertex.label.color="#ffffcc", vertex.label.font=2, edge.label = E(cg2)$weight)

# what is the gear distribution by node?

# communities(ic) give me the rows in the bc matrix which gives me the ftid. 

# assign target cluster to each trip
astoria <- astoria_trips
astoria$target <- rep(NA, nrow(astoria))
for(i in 1:length(communities(ic))){
astoria$target[which(astoria_trips$ftid %in% rownames(bc[communities(ic)[[i]],]))] <- V(cg2)$name[[i]]
}

# now make bipartite network for gear
astoria_types <- table(astoria$target, astoria$grid)
astoria_gear <- melt(astoria_types)
astoria_gear <- subset(astoria_gear, value > 10)
colnames(astoria_gear) <- c("from","to","weight")

fisheries <- graph.data.frame(astoria_gear, directed = FALSE)
E(fisheries)$weight <- astoria_gear$weight
V(fisheries)$type <- c(rep(TRUE,length(unique(astoria_gear$from))), rep(FALSE, length(unique(astoria_gear$to))))
par(mai=rep(0,4), bg="white")
plot(fisheries, edge.width = log(E(fisheries)$weight), layout=layout.bipartite, ylim=c(-1,1),vertex.label.cex=.55, edge.label = E(fisheries)$weight)

# now make participation network. 
astoria$fishery <- paste(astoria$target, astoria$grid, sep="_")
# restrict to fisheries used
used_fisherys <- unique(paste(astoria_gear$from, astoria_gear$to, sep="_"))
used_trips <- subset(astoria, fishery %in% used_fisherys)
by_fishery <- table(used_trips$veid, used_trips$fishery)

fishery_mat <- matrix(ncol=length(used_fisherys), nrow=length(used_fisherys))
colnames(fishery_mat) <- colnames(by_fishery)
rownames(fishery_mat) <- colnames(by_fishery)

for(i in 1:ncol(by_fishery)){ # for each fishery except the last one
  fish_i <- colnames(by_fishery)[i]
  cat("j: \n")
  for(j in i:ncol(by_fishery)){
    fish_j <- colnames(by_fishery)[j]
    num_ves = length(which(which(by_fishery[,i]>0) %in% which(by_fishery[,j] > 0)))
    
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

# looking for communities of vessels 

ic_gear <- infomap.community(g)
library(RColorBrewer)
paint <- brewer.pal(6,"Dark2")
V(g)$membership <- ic_gear$membership
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
V(g)[membership==6]$color <- paint[6]
V(g)[membership==6]$frame.color <- paint[6]
par(mai=rep(0,4))

V(g)$size <- colSums(by_fishery)

plot(g, vertex.color=V(g)$color, edge.width = E(g)$weight, vertex.size=log(V(g)$size)*3, vertex.frame.color=V(g)$frame.color, vertex.label.color="black",layout=l)

## looking into groundfish catch clusters
gf <- subset(astoria, target == "groundfish")

# select relevant data, construct trip matrix
melt_gf <- melt(gf, variables = c("spid","ftid"), measure.vars = "landed_wt")
cast_gf <- dcast(melt_gf, ftid ~ spid, fun.aggregate = sum)
rownames(cast_gf) <- cast_gf$ftid
cast_gf$ftid <-NULL

headtrip_mat_gf <- as.matrix(cast_gf)

bc_gf <- as.matrix(vegdist(headtrip_mat_gf, "bray"))
bc_gf <- abs(bc_gf-1)

bc_net_gf <- graph.adjacency(bc_gf, mode="undirected", weighted = TRUE)

system.time({
  ic_gf <- infomap.community(bc_net_gf, e.weights = E(bc_net_gf)$weight)
})
cg_gf <- contract.vertices(bc_net_gf, membership(ic_gf))
E(cg_gf)$weight <- 1
cg2_gf <- simplify(cg_gf, remove.loops=TRUE)

plot(cg2_gf, vertex.label="")
