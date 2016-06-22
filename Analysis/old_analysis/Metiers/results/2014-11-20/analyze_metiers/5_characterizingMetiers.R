# characterizing metiers

# loading and setting up data
#----
library(stringr); library(vegan); library(reshape2); library(scales); library(plyr); library(RColorBrewer)
setwd("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/")

# load data
filtered_ftl <- readRDS("filtered_ftl.RDS")

# load metiers
files <- list.files(path="classify/") # load predicted metiers
files_ten <- files[grep("2010p",files)] # just 2010 metiers
predicteds <- do.call(rbind, lapply(paste0("classify/",files_ten), readRDS))
other_files <- list.files(path="define/")
other_files_key <- other_files[grep("2010cluster_key", other_files)]

classifieds <- do.call(rbind, lapply(paste0("define/", other_files_key), read.csv))
classifieds$ftid <- paste0(classifieds$ftid, 2010)
classifieds$node <- NULL
colnames(classifieds) <- c("trip_id", "metier")
colnames(predicteds) <- c("trip_id", "metier")

metiers <- rbind(classifieds, predicteds)

# merge metiers
tickets <- merge(filtered_ftl, metiers, by = "trip_id")
tickets$metier <- paste(tickets$metier, tickets$grgroup,sep="_")
length(unique(tickets$trip_id)) == length(unique(metiers$trip_id))
# should be TRUE

# remove filtered_ftl so it can't get used in any functions and confuse things. 
# only tickets will be properly filtered to remove bad veids
rm(filtered_ftl)

# can remove year from metier, because now they are all the same across years
saveRDS(tickets, "analyze_metiers/tickets.RDS")
#----
# characterizing data
#----
# number of metiers each year
over_years <- ddply(tickets, .(year), summarize, num_metiers = length(unique(metier)))
range(over_years$num_metiers)

# for each metier, what are the number of trips and vessels?
neffort <- ddply(tickets, .(metier, year), summarize, ntrips = length(unique(trip_id)), nves = length(unique(veid)))

neffort <- neffort[order(neffort$nves, decreasing = T),]

# for each metier, what are the commonly caught species?
# define as for all trips of this metier, what is the species that is most often in the majority of catches?
# so for each species in each metier, how many trips is it the majority?

characterize_metiers <- function(metier_choice, data = tickets){
  cat("subsetting catch data\n")
  catch_data <- subset(tickets, metier == metier_choice, select = c("modified","landed_wt", "trip_id","veid","ppp"))
  
  # find max species by trip.
  trips <- unique(catch_data$trip_id)
  max_species <- rep(NA, length(trips))
  
  cat("calculate maximum species per trip\n")
  max_species <- ddply(catch_data, .(trip_id), summarize, species = modified[which.max(landed_wt)], .progress = "text")


#   # count max species
#   barplot(log(sort(table(max_species$species), decreasing=T)), bor=F, las=2, cex.names=.75,ylab = "log number of trips", xlab="secies", col ="#f46d43")

  # for each metier, what are the main ports?
  metier_trips <- subset(tickets, trip_id %in% catch_data$trip_id, select = c("trip_id", "grid","pcid", "year"))
  tls_12 <- unique(metier_trips)
  grid = sort(table(metier_trips$grid), decreasing = T)
  pcid = sort(table(metier_trips$pcid), decreasing = T)
 
  
  return(list(catch_data = catch_data, max_species = max_species, grid = grid, pcid = pcid))
}

mets <- as.character(unique(neffort$metier))
met_data <- list()
for(i in 1:length(mets)){
  met_data[[mets[i]]] <- characterize_metiers(mets[i])
}

# build dataframe
  df <- data.frame(Metier = names(met_data))
  df$Major_species <- NA
  df$Major_gear <- NA
  df$CA <- NA
  df$OR <- NA
  df$WA <- NA
  df$AK <- NA
  df$At_sea <- NA
  df$CP_MS <- NA
  df$Other_ports <- NA
  df$number_trips <- NA
  df$multi_species <- NA
  df$number_vessels <- NA
  
  other_ports <- c("DFO", "NWAFC")

# load common names
  spid <- read.csv("../../writing/code/data/spid.csv", stringsAsFactors=F)
  grid <- read.csv("../../writing/code/data/grid.csv", stringsAsFactors=F)
  pcid <- read.csv("../../writing/code/data/pcid.csv", stringsAsFactors = F)

for(i in 1:length(met_data)){
  df$Metier[i] <- names(met_data[i])
  df$number_trips[i] <- with(met_data[[i]], length(unique(catch_data$trip_id)))
  df$number_vessels[i] <- with(met_data[[i]], length(unique(catch_data$veid)))
  
### find major species
#     it's hard to guess targeted species from this data. I would expect though
#     that targeted species are either very abundant in the catch, or high value, or both.
#     would like to construct an index that weights the relative abundance, the relative price
#     to give a "likely targeted" score. haven't done this yet.  

    tabs <- sort(table(met_data[[i]]$max_species$species), decreasing = T)
    freqsp <- round(tabs/sum(tabs)*100)
    
    # first look for species that make up more than 50% of trips
    maj_species <- names(freqsp[which(freqsp > 50)])
    df$multi_species[i] <- "no"
    # if none, then anything over 19% of majority catch
    if(length(maj_species)==0){
      maj_species <- names(freqsp[which(freqsp >=19)])
      df$multi_species[i] <- "yes"
    }
    
    # find common names
    maj_com <- tolower(spid$common_name[which(spid$SPID %in% maj_species)])
    maj_com <- gsub(pattern = "nom. ","", x = maj_com)
  
    # assign to df
    df$Major_species[i] <- paste(maj_com,collapse=", ")
  
#### find major gear types
    gear_tab <- with(met_data[[i]], round(100* grid/sum(grid)))
    maj_gear <- names(gear_tab[which(gear_tab>50)])
    if(length(maj_gear)==0){
      maj_gear <- names(gear_tab[which(gear_tab>=19)])
    }
    gear <- tolower(grid$Short.Name[which(grid$GRID %in% maj_gear)])
    
    df$Major_gear[i] <- paste(gear, collapse = ", ")

#### find which ports
  # CA
  by_port <- table(pcid$Agency[which(pcid$Pcid %in% with(met_data[[i]], names(pcid)))])
  df$CA[i] <- round(by_port["CDFG"]/sum(by_port)*100)
  df$OR[i] <- round(by_port["ODFW"]/sum(by_port)*100)
  df$WA[i] <- round(by_port["WDFW"]/sum(by_port)*100)
  df$AK[i] <- round(by_port["ADFG"]/sum(by_port)*100)
  df$At_sea[i] <- round(by_port["AFSC"]/sum(by_port)*100)
  df$CP_MS[i] <- round(by_port["AKR"]/sum(by_port)*100)
  op <- round(by_port[which(names(by_port) %in% other_ports)]/sum(by_port)*100)
  df$Other_ports[i] <- ifelse(length(op)==0, NA, op)
}

df <- df[order(df$number_trips, decreasing = T),]

# can get rid of non CA/OR/WA columns, nothing in them
df$Other_ports <- NULL
df$AK <- NULL
df$At_sea <- NULL
df$CP_MS <- NULL
row.names(df) <- NULL

saveRDS(df, "analyze_metiers//metier_descrp.RDS")
#----
# building participation networks
#----
library(igraph)

# calculate the bray-curtis again to look at vessel composition across fisheries. A fishery is very similar if the same vessels participate in the same way. 
  # need a metier by veid matrix to run vegdist on
  
define_participationPlot <- function(year_choose, port=NA){
  if(is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose),]}
  if(!is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$pcid==port),]
  }
  m_by_v <- melt(yr_tickets, id.vars = c("metier","veid"), measure.vars = "trip_id")
  cast_mv <- dcast(m_by_v, metier~veid, length)
  rownames(cast_mv) <- cast_mv$metier
  cast_mv <- cast_mv[,-1]
  # remove any metiers that aren't used at all
  if(any(rowSums(cast_mv)==0)){
    cast_mv <- cast_mv[-which(rowSums(cast_mv)==0),]}
    dist_trips <- vegdist(decostand(cast_mv,"hellinger"),"euclidean")
  bc_veid <- as.matrix(max(dist_trips)-dist_trips)
   
  g <- graph.adjacency(bc_veid,weighted = TRUE, mode="undirected", diag = FALSE)
  par(mai=rep(0,4))
  V(g)$size <- rowSums(cast_mv)

  strong_edges <- E(g)$weight
  strong_edges[strong_edges<=0.02] <- 0
  g_s <- g
  E(g_s)$weight <- strong_edges  

  im_gs <- infomap.community(g_s)
    
  paint <- colorRampPalette(brewer.pal( 8, "Dark2"))(length(im_gs))
  V(g_s)$color <- paint[im_gs$membership]

  l <- layout.fruchterman.reingold(g_s,niter=500,area=vcount(g)^2.3,repulserad=vcount(g)^2.8)

  plot(g_s, edge.width = E(g_s)$weight*20, layout = l, vertex.size = 4, vertex.label.color = "black", vertex.label.cex = .5, vertex.label.family="sans", vertex.label.degree=-pi/2, vertex.label.dist=.2)
  return(g_s)
}

gs_09 <- define_participationPlot(2009)
gs_10 <- define_participationPlot(2010)
gs_11 <- define_participationPlot(2011)
gs_12 <- define_participationPlot(2012)
gs_13 <- define_participationPlot(2013)

pre_ITQ <- define_participationPlot(c(2009,2010))
post_ITQ <- define_participationPlot(c(2012,2013))
gs_all <- define_participationPlot(c(2009, 2010, 2011, 2012,2013))

graphs <- list(gs_09, gs_10, gs_11, gs_12, gs_13, pre_ITQ, post_ITQ, gs_all)

saveRDS(graphs, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/particp_graph.RDS")

#----
# # alternative participation network: whether trips in both fisheries happen
# 
# mat <- matrix(nrow=length(mets), ncol=length(mets))
# colnames(mat) <- mets
# rownames(mat) <- mets
# 
# for(i in 1:nrow(mat)){
#   cat(i,":\n")
#   for(j in i:ncol(mat)){
#     sub_i <- tickets$veid[which(tickets$metier==rownames(mat)[i])]
#     sub_j <- tickets$veid[which(tickets$metier==colnames(mat)[j])]
#     mat[i,j] <- length(union(sub_i, sub_j))
#     cat(j," ")
#   }
#  cat("\n")
# }
# 
# g2 <- graph.adjacency(mat, weighted=TRUE, mode="undirected", diag = FALSE)
# V(g2)$size <- diag(mat)
# plot(g2, edge.width = log(E(g2)$weight)/10, layout = layout.fruchterman.reingold, vertex.size = 5)
#----
# participation networks by port
#----
# do participation networks by port
port_popularity <- ddply(tickets, .(pcid), summarize, num_Ves = length(unique(veid)))
port_popularity <- port_popularity[order(port_popularity$num_Ves, decreasing = T),]

# first will look at newport

gs09NEW <- define_participationPlot(2009, "NEW")
gs09COS <- define_participationPlot(2009, "COS")
gs09AST <- define_participationPlot(2009, "AST")
gs09SF <- define_participationPlot(2009, "SF")
gs09BFG <- define_participationPlot(2009, "BRG")
gs09WPT <- define_participationPlot(2009, "WPT")
gs09LWC <- define_participationPlot(2009, "LWC")
gs09BDG <- define_participationPlot(2009, "BDG")
gs09PRN <- define_participationPlot(2009, "PRN")
gs09TLL <- define_participationPlot(2009, "TLL")

top_ports <- list(gs09NEW, gs09COS, gs09AST, gs09SF, gs09BFG, gs09WPT, gs09LWC, gs09BDG, gs09PRN, gs09TLL)
saveRDS(top_ports, "analyze_metiers/top_ports.RDS")

#----
# calculating centrality
#----

# will do it for POT_12 by port
degree <- unlist(lapply(top_ports, degree))
degree_df <- data.frame(degree = degree, metier = names(degree))

# color by sample size
counting <- as.data.frame(table(degree_df$metier))
paint <- colorRampPalette(brewer.pal(8,"Spectral"))(max(counting$Freq))
counting$color <- paint[counting$Freq]

meds <- with(degree_df,reorder(metier, -degree, fun=median))
by_row <- data.frame(meds=levels(meds))  

coloring <-merge(by_row, counting[,c("Var1","color")], by.x="meds",by.y="Var1", sort=F)
  
ggplot(degree_df, aes(y=degree, x = reorder(metier, -degree, fun=median))) + geom_boxplot(fill = coloring$color,colour=coloring$color) +theme_minimal() + xlab("metier") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(degree_df, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/degree.RDS")
saveRDS(coloring, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/coloring.RDS")

#-----
# time series
#-----
# # albacore: TLS_2
# # salmon : TLS_1
# # shrimp : TWS_1
# # dover: TWL_1
# 
# # trips by day
# trl <- subset(tickets, metier %in% c( "TLS_2","TLS_1"))
# trl$date <- paste(trl$year, trl$month, trl$day, sep="-")
# 
# by_day <- ddply(trl, .(date, metier), summarize, trips = length(unique(trip_id)))
# by_day$date <- as.POSIXlt(by_day$date)
# ggplot(by_day, aes(x = date, y = trips, group = metier, color = metier)) + geom_bar(stat="identity",position="dodge") + theme_minimal()
# 
# # could you try to figure out what predicts high volume of trips in a particular metier? for example, day of year could be one, week, month, but also price in other fisheries, number of vessels in other fisheries, prices in other fisheries that are lagged