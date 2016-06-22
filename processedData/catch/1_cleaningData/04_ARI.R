# compare classification results using 2010 versus 2012 as base year
if(!exists("tickets")) cat("need to load tickets.RDS")

# compute adjusted rand index for all data ----
library(mclust)
ARI_total10_12 <- with(unique(tickets[,c("trip_id","metier.2010","metier.2012")]), adjustedRandIndex(metier.2010, metier.2012)) 
ARI_total10_06 <- with(unique(tickets[,c("trip_id","metier.2010","metier.2006")]), adjustedRandIndex(metier.2010, metier.2006)) 
ARI_total12_06 <- with(unique(tickets[,c("trip_id","metier.2012","metier.2006")]), adjustedRandIndex(metier.2012, metier.2006)) 

#save(ARI_total,file =  "/Users/efuller/1/CNH/processedData/catch/1_cleaningData/ARI_total.Rdata")

# by year and gear ----
  gears = c("TLS","TWL","TWS","NET","POT","HKL","MSC", "DRG")
  years = 2006:2015
  ari_1012 <- matrix(ncol = length(years),nrow = length(gears), data = NA)
  colnames(ari_1012) <- years
  rownames(ari_1012) <- gears
  for(g in 1:length(gears)){
    for(y in 1:length(years)){
      ari_1012[g, y] = with(subset(tickets, year == years[y] & grgroup == gears[g]), adjustedRandIndex(metier.2010, metier.2012))
    }
  }
  
  ari_0612 <- matrix(ncol = length(years),nrow = length(gears), data = NA)
  colnames(ari_0612) <- years
  rownames(ari_0612) <- gears
  
  for(g in 1:length(gears)){
    for(y in 1:length(years)){
      ari_0612[g, y] = with(subset(tickets, year == years[y] & grgroup == gears[g]), adjustedRandIndex(metier.2010, metier.2012))
    }
  }
  
  saveRDS(by_year, "/Users/efuller/1/CNH/processedData/catch/1_cleaningData/by_year.RDS")

# examine hkl ----
# 2012 is top, 2010 is bottom (2012), rows are bottom (2012).  not sure
  # line width is proportional to number of trips
  library(bipartite)
with(unique(tickets[grep("HKL", tickets$metier.2010),c("drvid","metier.2010","metier.2012")]),
       plotweb(table(metier.2010, metier.2012)) )
  
  # try making the one trips zeros
  bar <- with(unique(tickets[grep("HKL", tickets$metier.2010),c("drvid","metier.2010","metier.2012")]),table(metier.2010, metier.2012)) 
  
  bar[bar < 5] <- 0 
  saveRDS(bar, "/Users/efuller/1/CNH/processedData/catch/1_cleaningData/hkl_disagree_clean.RDS")
  plotweb(bar)
  # highlight major disagreements, removes any connections with fewer than 5 trips (so will remove small realized fisheries and or disagreements that had fewer than 5 trips misclassified.)
  
  # line width is proportional to amount of biomass
  library(reshape2)
  melt.df <- melt(tickets[grep("HKL", tickets$metier.2010),], id.vars = c("metier.2010", "metier.2012"), measure.vars = "adj_revenue")
  cast.df <- dcast(melt.df, metier.2010 ~ metier.2012, fun.aggregate = sum)
  
  row.names(cast.df) <- cast.df$metier.2010
  cast.df$metier.2010 <- NULL
  plotweb(cast.df)
  
  
  # hkl_2 in both 2010 and 2012
    trips <- tickets$trip_id[which(tickets$metier.2010 == "HKL_2" & tickets$metier.2012 == "HKL_2")]
    
    sp.total <- subset(tickets, trip_id %in% trips) %>%
      group_by(modified) %>%
      summarize(total.lbs = sum(landed_wt, na.rm = T), 
                total.revenue = sum(adj_revenue, na.rm = T)) %>%
      arrange(-total.revenue, -total.lbs)
    library(ggplot2)
    ggplot(sp.total[1:30,], aes(x = modified, y = total.revenue)) + 
      geom_bar(stat='identity') + theme_classic()
    ggplot(sp.total[1:30,], aes(x = modified, y = total.lbs)) + 
      geom_bar(stat='identity') + theme_classic()
    # dominated by black rockfish
  
  # hkl_6 in 2010, hkl_2 in 2012
    trips <- tickets$trip_id[which(tickets$metier.2010 == "HKL_6" & tickets$metier.2012 == "HKL_2")]
    
    sp.total <- subset(tickets, trip_id %in% trips) %>%
      group_by(modified) %>%
      summarize(total.lbs = sum(landed_wt, na.rm = T), total.revenue = sum(adj_revenue, na.rm = T)) %>%
      arrange(-total.revenue, -total.lbs)
    library(ggplot2)
    ggplot(sp.total[1:30,], aes(x = modified, y = total.revenue)) + geom_bar(stat='identity') + theme_classic()
    ggplot(sp.total[1:30,], aes(x = modified, y = total.lbs)) + geom_bar(stat='identity') + theme_classic()
    # dominated by grass rockfish, cabezon, and black and yellow
  
    # hkl_4 in 2010, hkl_2 in 2012
    trips <- tickets$trip_id[which(tickets$metier.2010 == "HKL_4" & tickets$metier.2012 == "HKL_2")]
    
    sp.total <- subset(tickets, trip_id %in% trips) %>%
      group_by(modified) %>%
      summarize(total.lbs = sum(landed_wt, na.rm = T), total.revenue = sum(adj_revenue, na.rm = T)) %>%
      arrange(-total.revenue, -total.lbs)
    library(ggplot2)
    ggplot(sp.total[1:30,], aes(x = modified, y = total.revenue)) + geom_bar(stat='identity') + theme_classic()
    ggplot(sp.total[1:30,], aes(x = modified, y = total.lbs)) + geom_bar(stat='identity') + theme_classic()
    # dominated by gopher and brown rockfish, lingcod, and black and yellow, cabezon
  
    # hkl_8 in 2010, hkl_2 in 2012
    trips <- tickets$trip_id[which(tickets$metier.2010 == "HKL_8" & tickets$metier.2012 == "HKL_2")]
    
    sp.total <- subset(tickets, trip_id %in% trips) %>%
      group_by(modified) %>%
      summarize(total.lbs = sum(landed_wt, na.rm = T), total.revenue = sum(adj_revenue, na.rm = T)) %>%
      arrange(-total.revenue, -total.lbs)
    library(ggplot2)
    ggplot(sp.total[1:30,], aes(x = modified, y = total.revenue)) + geom_bar(stat='identity') + theme_classic()
    ggplot(sp.total[1:30,], aes(x = modified, y = total.lbs)) + geom_bar(stat='identity') + theme_classic()
    # dominated by vermilion
    
    # hkl_5 in 2010, hkl_2 in 2012
    trips <- tickets$trip_id[which(tickets$metier.2010 == "HKL_5" & tickets$metier.2012 == "HKL_2")]
    
    sp.total <- subset(tickets, trip_id %in% trips) %>%
      group_by(modified) %>%
      summarize(total.lbs = sum(landed_wt, na.rm = T), total.revenue = sum(adj_revenue, na.rm = T)) %>%
      arrange(-total.revenue, -total.lbs)
    library(ggplot2)
    ggplot(sp.total[1:30,], aes(x = modified, y = total.revenue)) + geom_bar(stat='identity') + theme_classic()
    ggplot(sp.total[1:30,], aes(x = modified, y = total.lbs)) + geom_bar(stat='identity') + theme_classic()
    # dominated by lingcod. 
    
    
# which gear groups have more/less for 2010/2012
    length(unique(tickets$metier.2010[grep("POT",tickets$metier.2010)]))
    length(unique(tickets$metier.2012[grep("POT",tickets$metier.2012)]))
    
    length(unique(tickets$metier.2010[grep("NET",tickets$metier.2010)]))
    length(unique(tickets$metier.2012[grep("NET",tickets$metier.2012)]))
    
    length(unique(tickets$metier.2010[grep("TWL",tickets$metier.2010)]))
    length(unique(tickets$metier.2012[grep("TWL",tickets$metier.2012)]))
    
    length(unique(tickets$metier.2010[grep("TWS",tickets$metier.2010)]))
    length(unique(tickets$metier.2012[grep("TWS",tickets$metier.2012)]))
    
    length(unique(tickets$metier.2010[grep("HKL",tickets$metier.2010)]))
    length(unique(tickets$metier.2012[grep("HKL",tickets$metier.2012)])) # way more in 2012
    
    length(unique(tickets$metier.2010[grep("TLS",tickets$metier.2010)]))
    length(unique(tickets$metier.2012[grep("TLS",tickets$metier.2012)]))
    
    length(unique(tickets$metier.2010[grep("MSC",tickets$metier.2010)]))
    length(unique(tickets$metier.2012[grep("MSC",tickets$metier.2012)]))
    