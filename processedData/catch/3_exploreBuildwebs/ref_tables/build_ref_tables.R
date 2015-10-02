library(dplyr)
library(stringr)
library(vegan)
library(reshape2)
# load data ----
tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/tickets.RDS")
tickets <- rename(tickets, metier = metier.2010)

# reference table for metiers ---- 
neffort <- tickets %>%
  group_by(metier, year) %>%
  summarize(ntrips = length(unique(trip_id)), nves = length(unique(drvid))) %>%
  arrange(nves) 

# for each metier, what are the commonly caught species?
# define as for all trips of this metier, what is the species that is most often in the majority of catches?
# so for each species in each metier, how many trips is it the majority?

characterize_metiers <- function(metier_choice, data = tickets){
  #cat("subsetting catch data\n")
  catch_data <- subset(tickets, metier == metier_choice, select = c("modified","landed_wt", "trip_id","drvid","ppp","drvid"))
  
  # find max species by trip.
  trips <- unique(catch_data$trip_id)
  max_species <- rep(NA, length(trips))
  
  #cat("calculate maximum species per trip\n")
  max_species <- catch_data %>%
    group_by(trip_id) %>%
    summarize(species = modified[which.max(landed_wt)])
  
  
  #   # count max species
  #   barplot(log(sort(table(max_species$species), decreasing=T)), bor=F, las=2, cex.names=.75,ylab = "log number of trips", xlab="species", col ="#f46d43")
  
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
spid <- read.csv(
  "/Users/efuller/1/CNH/processedData/catch/1_cleaningData/spid.csv", 
  stringsAsFactors=F)
grid <- read.csv(
  "/Users/efuller/1/CNH/Analysis/Metiers/data/grid.csv", 
  stringsAsFactors=F)
pcid <- read.csv(
  "/Users/efuller/1/CNH/Analysis/Metiers/results/2015-01-09/code/data/pcid.csv", 
  stringsAsFactors = F)

for(i in 1:length(met_data)){
  df$Metier[i] <- names(met_data[i])
  df$number_trips[i] <- with(met_data[[i]], length(unique(catch_data$trip_id)))
  df$number_vessels[i] <- with(met_data[[i]], length(unique(catch_data$drvid)))
  
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

df$Metier <- tolower(df$Metier)

write.csv(df, "/Users/efuller/1/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv",row.names = FALSE)

# make strategy reference table ----
# what is the majority fishery (> .5)
# list the rest out the rest with percentages in parentheses
# how many total vessels across all five years

define_strategy <- function(cid, df,graph = "no"){
  s1 <- subset(df, cluster == cid)
  s1$VY <- paste0(s1$drvid,"_",s1$year)
  
  fishery_income <- ddply(s1, .(VY, metier), summarize, adj_revenue = sum(adj_revenue))
  melt_income <- melt(fishery_income, id.vars = c("VY","metier"), measure.vars = "adj_revenue")
  d_income <- dcast(melt_income, VY ~ metier, fun.aggregate = sum)
  rownames(d_income) <- d_income$VY
  d_income$VY <- NULL
  
  # calculate diversity for each vessel
  if(ncol(d_income)==1){ # this is because if there's one column, diversity flips it and changes the margin to 2 (by column). Problem, because really there's only one species. So it should be zero. 
    income_div = 0
    med_income_div = 0
    sd_income_div = 0
  }else{
    income_div <- diversity(d_income, index = "simpson")
    med_income_div <- median(income_div)
    sd_income_div <- sd(income_div)
  }
  ratio_var <- sd_income_div/med_income_div

  # so the number of people which caught the fishery, and the proportion of people who had it in their combo. 
    d_income[d_income==0] <-NA # for counting purposes
    # number of people that have it in their combo
  strat_combo <- data.frame(strategy = colnames(d_income), stringsAsFactors=FALSE)
  strat_combo$percent_vessels <- NA
  for(i in 1:nrow(strat_combo)){
    strat_combo$percent_vessels[i] <- round(length(which(!is.na(d_income[,which(colnames(d_income)==strat_combo$strategy[i])])))/nrow(d_income),2)
  }
  strat_combo <- subset(strat_combo, percent_vessels > 0)
  strat_combo <- strat_combo[order(strat_combo$percent_vessels, decreasing = T),]
  # find number of vessels
  vids <- NA
  for(i in 1:nrow(d_income)){
    vids[i] <- unlist(strsplit(rownames(d_income)[i],"_"))[1]
  }
  vids <- unique(vids)
  if(graph == "yes"){
    barplot(strat_combo$percent_vessels, names.arg = strat_combo$strategy, las=2, main = paste("strategy", cid,"\n",length(vids), "total vessels"))
  }
  
  return(fisheries_comp = list(
    strat_combo=strat_combo, 
    n_vessels = length(vids), 
    max_fishery = ifelse(any(strat_combo$percent_vessels >=.5), strat_combo$strategy[which.max(strat_combo$percent_vessels)], NA),
    median.income.diversity = med_income_div,
    sd.income.diversity = sd_income_div,
    ratio.sd_med = ratio_var
    )
    )
}

# build table
  # set up table columns
    strategies <- unique(tickets$cluster)
    strat_df <- data.frame(strategies=strategies, stringsAsFactors=FALSE)
    strat_df$major_fishery <- NA
    strat_df$n_vessels <- NA
    strat_df$other_fisheries <- NA
    strat_df$median.income.div <- NA
    strat_df$sd.income.div <- NA
    strat_df$ratio.sd_med <- NA
  # run through rows, filling them in 
    for(i in 1:length(strategies)){
      outs <- define_strategy(cid = strategies[i], df = tickets)
      strat_df$major_fishery[i] <- outs$max_fishery
      strat_df$n_vessels[i] <- outs$n_vessels
      strat_df$median.income.div[i] <- outs$median.income.diversity
      strat_df$sd.income.div[i] <- outs$sd.income.diversity
      strat_df$ratio.sd_med[i] <- outs$ratio.sd_med
      
      string_list <- vector('list',nrow(outs$strat_combo))
      for(j in 1:length(string_list)){
        string_list[[j]] <- paste0(outs$strat_combo[j,1]," (",outs$strat_combo[j,2]*100,"%)")
      }
        strat_df$other_fisheries[i] <- str_c(unlist(string_list),collapse=", ")
    }

strat_df$type <- ifelse(strat_df$median.income.div==0, "specialist", ifelse(is.na(strat_df$sd.income.div),NA,"generalist"))

write.csv(strat_df, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_exploreBuildwebs/ref_tables/strategies.csv",row.names=FALSE)
