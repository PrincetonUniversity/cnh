# the hkl 2010 and 2012 are still weird. seems robust. why is that..?

library(e1071)

agreement <- function (gear, year) {
  df10 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/classify/2010p",gear, year,".RDS"))
  df12 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/classify/2012p",gear, year,".RDS"))
  
  df10$predicted_metier <- paste0(df10$predicted_metier,"_10")
  df12$predicted_metier <- paste0(df12$predicted_metier,"_12")
  
  predicted_df <- merge(df10, df12, by = "trip_id")
  table(predicted_df$predicted_metier.x, predicted_df$predicted_metier.y)
}

classAgreement(agreement("HKL",2009))$crand # super low. 

agreement("HKL",2009)
# looks like 1_10 and 1_12 doing it
classAgreement(agreement("HKL",2009)[-1,][,-1])
# but still not great.. i would take it though

# in 1_12 I have 2_10, 3_10, 5_10, 6_10, and 8_10, what are those?
gear = "HKL"
year = 2009
df10 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/classify/2010p",gear, year,".RDS"))
df12 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/classify/2012p",gear, year,".RDS"))

df10$predicted_metier <- paste0(df10$predicted_metier,"_10")
df12$predicted_metier <- paste0(df12$predicted_metier,"_12")

predicted_df <- merge(df10, df12, by = "trip_id")
tickets <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-19/define_metiers/filtered_ftl.RDS")
tickets_09 <- merge(tickets, predicted_df, by = "trip_id")

library(reshape2)

trip_comp <- function(year10_met, year12_met,tick = tickets_09){
  trip_comp <- vector("list")
  trip_comp$tickets <- subset(tick, predicted_metier.x == year10_met &
                                predicted_metier.y == year12_met)
  
  trip_comp$num_trips <- length(unique(trip_comp$tickets$trip_id))
  
  # what gears?
  ot_gears <- with(trip_comp, unique(tickets[,c("grid", "trip_id")]))
  trip_comp$num_gears <- with(trip_comp, table(tickets$grid))
  
  # what species?
  melt_ot <- with(trip_comp, melt(tickets, id.vars=c("modified","trip_id"), 
                                  measure.vars = "landed_wt"))
  cast_ot <- with(trip_comp, dcast(melt_ot, trip_id ~ modified, 
                                   fun.aggregate = sum))
  rownames(cast_ot) <- cast_ot$trip_id; cast_ot$trip_id <- NULL
  
  trip_comp$total_catches <- sort(colSums(cast_ot),decreasing=T)
  
  # what's the maximum catch for each species?
  max_sp <- apply(cast_ot,1, which.max)
  max_sp <- sort(table(max_sp),decreasing=T)
  names(max_sp) <- colnames(cast_ot[as.numeric(names(max_sp))])
  trip_comp$majority_species <- max_sp
  
  barplot(trip_comp$total_catches, bor=F,las=2,cex.names=.75, 
          main = "total caught")
  barplot(trip_comp$majority_species, bor=F, las=2, cex.names=.75, 
          main="majority of catch")
  return(trip_comp)
}

twoten <- trip_comp(year10_met="2_10",year12_met="1_12")
threeten <- trip_comp(year10_met="3_10",year12_met = "1_12")
fiveten <- trip_comp(year10_met="5_10",year12_met = "1_12")
sixten <- trip_comp(year10_met="6_10",year12_met = "1_12")
eightten <- trip_comp(year10_met="8_10",year12_met = "1_12")

all_trips <- list(twoten = twoten, threeten = threeten, fiveten = fiveten, sixten = sixten, eightten = eightten)

par(mfrow=c(1,5))
lapply(all_trips, function(x)barplot(x$majority_species,bor=F, col="steelblue",las=2,cex.names=.85))


# notes:
# maybe it's the relative amounts of cabezon versus lingcod in 2010 and 2012.

# look at 2010 trips and their clusters
df10 <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/define/HKL2010cluster_key.txt")
tickets_10 <- merge(tickets, df10, by = "ftid")
tickets_10 <- subset(tickets_10, year == 2010) #ftid not unique by year

# look at majority species. 

all_trip_comp <- function(tick){
  trip_comp <- vector("list")
  trip_comp$tickets <- tick
  
  trip_comp$num_trips <- length(unique(trip_comp$tickets$trip_id))
  
  # what gears?
  ot_gears <- with(trip_comp, unique(tickets[,c("grid", "trip_id")]))
  trip_comp$num_gears <- with(trip_comp, table(tickets$grid))
  
  # what species?
  melt_ot <- with(trip_comp, melt(tickets, id.vars=c("modified","trip_id"), 
                                  measure.vars = "landed_wt"))
  cast_ot <- with(trip_comp, dcast(melt_ot, trip_id ~ modified, 
                                   fun.aggregate = sum))
  rownames(cast_ot) <- cast_ot$trip_id; cast_ot$trip_id <- NULL
  
  trip_comp$total_catches <- sort(colSums(cast_ot),decreasing=T)
  
  # what's the maximum catch for each species?
  max_sp <- apply(cast_ot,1, which.max)
  max_sp <- sort(table(max_sp),decreasing=T)
  names(max_sp) <- colnames(cast_ot[as.numeric(names(max_sp))])
  trip_comp$majority_species <- max_sp
  
  barplot(trip_comp$total_catches, bor=F,las=2,cex.names=.75, 
          main = "total caught")
  barplot(trip_comp$majority_species, bor=F, las=2, cex.names=.75, 
          main="majority of catch")
  return(trip_comp)
}
ten <- all_trip_comp(tickets_10)

# look at 2012 trips and their clusters. 
