# prep landings data: add county to each port for new version of pcid ----
library(sp)
library(maps)
library(maptools)

ports_df <- read.csv("/Users/efuller/Desktop/CNH/processedData/spatial/ports/all_ports.csv", stringsAsFactors = FALSE)

latlon2county <- function(ports){
  # ht: http://stackoverflow.com/a/13474437/3137323
  counties <- map('county',fill=TRUE, col='transparent', plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs, proj4string = CRS("+proj=longlat + datum=wgs84"))
  
  pointsSP <- SpatialPoints(ports[-which(is.na(ports$lat)),c("lon","lat")], proj4string = CRS("+proj=longlat + datum=wgs84"))
  
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)

  # because on coastline, can't use over(). Instead find nearest polygon, which means we project
  # use an equidistance projection to maintain true distances
  pr_counties <- spTransform(counties_sp, CRS("+proj=aeqd +lat_0=41.30817205480597 +lon_0=-123.4149169921875"))
  pr_points <- spTransform(pointsSP, CRS("+proj=aeqd +lat_0=41.30817205480597 +lon_0=-123.4149169921875"))
  
  library(rgeos)
  county.df <- ports[-which(is.na(ports$lat)),]
  county.df$county <- NA
  for(i in 1:nrow(county.df)){
    county.df$county[i] <- countyNames[which.min(gDistance(pr_points[i,], pr_counties, byid=TRUE))]
  }
  
  return(county.df)
}

ports <- latlon2county(ports = ports_df); rm(ports_df)

# merge state, county scale data with ticket data 
tickets_df <- readRDS("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/tickets.RDS")

library(dplyr)
ports <- rename(ports, pcid = Pcid)
trips <- left_join(tickets_df, ports) %>%
  group_by(trip_id) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T), lbs = sum(landed_wt, na.rm = T), 
            tdate = unique(tdate), metier.2010 = unique(metier.2010), pcid = unique(pcid),
            state = unique(state), county = unique(county), year = unique(year), drvid = unique(drvid))

# build networks ----
define_participationPlot <- function(year_choose, scale, ID, tickets){
  library(reshape2); library(igraph); library(RColorBrewer)
  descrp <- read.csv("/Users/efuller/Desktop/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv", stringsAsFactors = FALSE)
  
    if(scale == "port"){
      yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$pcid %in% ID),]
    } 
  if(scale == 'county'){
      yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$county %in% ID),]
  }
  if(scale == 'state'){
    yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$state %in% ID),]
  }
  if(scale == 'coast'){
    yr_tickets <- tickets[which(tickets$year %in% year_choose),]
  }
  
  m_by_v <- melt(yr_tickets, id.vars = c("metier.2010","drvid"), measure.vars = "trip_id")
  m_by_v <- unique(m_by_v)
  if(nrow(m_by_v)<2) return(NA)
  cast_mv <- dcast(m_by_v, metier.2010~drvid, length) # calculate number of trips
  rownames(cast_mv) <- cast_mv$metier.2010
  cast_mv <- cast_mv[,-1, drop=FALSE]
    
    # want to calculate total number of vessels that do each fishery, and then 
    # make connections be what proportion does both
    # total number is row sums. And percentage that does both, relative to base 
    # nodes 
    
    # so symmetric matrix, of metiers, entries number of boats do both. 
    # Divide rows by row/sums to get directon. then each entry is the proprotion 
    # of boats that do both, relative to row-metier.
    
    met_mat <- matrix(data = 0, nrow = nrow(cast_mv), ncol = nrow(cast_mv))
    colnames(met_mat) <- rownames(cast_mv)
    rownames(met_mat) <- rownames(cast_mv)
    for(i in 1:nrow(met_mat)){
      for(j in 1:ncol(met_mat)){
        met_mat[i,j] <- sum(apply(cast_mv, MARGIN = 2, function(x) ifelse(x[i] & x[j]>0, 1, 0)))
      }
    }
    
    # make directed network, divide by diagonals
    met_mat <- met_mat/diag(met_mat)
    
    # put diagonals back in - number of vessels
    diag(met_mat) <- apply(X = cast_mv, MARGIN = 1, function(x) length(which(x>0)))
    
    # make network
    g <- graph.adjacency(met_mat,weighted = TRUE, mode="directed", diag = FALSE)
    V(g)$size <- diag(met_mat)
    
    contain_metier <- which(descrp$Metier %in% tolower(V(g)$name))
    # each major gear is a color, and then color ramp within gear between light and dark color
    # pot = reds
    # tws = pinks
    # tls = yellows
    # msc = purples
    # hkl = greens
    # twl = oranges
    # net = blues
    
    descrp$paint <- NA
    
    # find number of pot gears
    n.gear = length(grep("pot",descrp$Metier))
    paint = rev(colorRampPalette(brewer.pal(9, "Reds"))(n.gear))
    descrp$paint[grep("pot",descrp$Metier)] <- paint
    
    n.gear = length(grep("tws",descrp$Metier))
    paint = colorRampPalette(c("#FA9FB5","#E7298A"))(n.gear)
    descrp$paint[grep("tws",descrp$Metier)] <- paint
    
    n.gear = length(grep("tls",descrp$Metier))
    paint = colorRampPalette(c("#FFEDA0","#FED976"))(n.gear)
    descrp$paint[grep("tls",descrp$Metier)] <- paint
    
    n.gear = length(grep("msc",descrp$Metier))
    paint = colorRampPalette(brewer.pal(9, "Purples"))(n.gear)
    descrp$paint[grep("msc",descrp$Metier)] <- paint
    
    n.gear = length(grep("hkl",descrp$Metier))
    paint = rev(colorRampPalette(brewer.pal(9, "Greens"))(n.gear))
    descrp$paint[grep("hkl",descrp$Metier)] <- paint
    
    n.gear = length(grep("twl",descrp$Metier))
    paint = colorRampPalette(brewer.pal(9, "Oranges"))(n.gear)
    descrp$paint[grep("twl",descrp$Metier)] <- paint
    
    n.gear = length(grep("net",descrp$Metier))
    paint = colorRampPalette(brewer.pal(9, "Blues"))(n.gear)
    descrp$paint[grep("net",descrp$Metier)] <- paint
    
    cn <- data.frame(cn = paste(descrp$Major_species[contain_metier], descrp$Major_gear[contain_metier],sep="\n"), metier = descrp$Metier[contain_metier], paint = descrp$paint[contain_metier], stringsAsFactors = FALSE)
    cn <- cn[match(tolower(V(g)$name), cn$metier),] # reorder to match
    
    g_s <- g
    V(g_s)$name <- cn$cn
    
    V(g_s)$color <- cn$paint
    
    l <- layout.fruchterman.reingold(g_s,niter=500)
    
    #plot(g_s, edge.width = E(g_s)$weight*3, layout = l, vertex.size = log(V(g_s)$size)*4, vertex.label.color = "grey30", vertex.label.cex = .75, vertex.label.family="sans", vertex.frame.color = NA, edge.curved=TRUE, edge.arrow.size = .25, main = port, edge.color = "black")
    
    return(g_s)
  }


# make port networks
port_list <- list()
pcids <- unique(trips$pcid)
for(p in 1:length(pcids)){
  port_list[[p]] <- define_participationPlot(year_choose = 2009:2013, ID = pcids[p], scale = "port", tickets = trips)
}
names(port_list) <- pcids
# remove NAs
port_list <- port_list[-which(is.na(port_list))]

# make county networks 
county_list <- list()
counties <- unique(trips$county)
for(c in 1:length(counties)){
  county_list[[c]] <- define_participationPlot(year_choose = 2009:2013, ID = counties[c], scale = "county", tickets = trips)
}
names(county_list) <- counties
if(any(is.na(county_list))){
  county_list <- county_list[-which(is.na(county_list))]
}
# make state networks
state_list <- list()
states <- unique(trips$state)
for(s in 1:length(states)){
  state_list[[s]] <- define_participationPlot(year_choose = 2009:2013, ID = states[s], scale = 'state', tickets = trips)
}
names(state_list) <- states
if(any(is.na(state_list))){
  state_list <- state_list[-which(is.na(state_list))]
}

saveRDS(port_list, "/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/port_list.RDS")
saveRDS(county_list, "/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/county_list.RDS")
saveRDS(state_list, "/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/state_list.RDS")

# calculate fishery statistics ----
