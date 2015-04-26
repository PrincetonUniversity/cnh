# dahl sector implementation
library(reshape2)
library(plyr)
# load data ----
tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/fisheries_participation_profiles/tickets_plus.RDS")

# some tickets round_wt has NAs, make them the landed weight if that happens
tickets$round_wt[which(is.na(tickets$round_wt))] <- tickets$landed_wt[which(is.na(tickets$round_wt))]

any(is.na(tickets$round_wt)) # should be FALSE

# merge the management groups to them
spid <- read.csv("/Users/efuller/1/CNH/processedData/catch/spid.csv")
spid_names <- spid[,c("SPID","mgmt_grp")]
tickets <- merge(tickets, spid_names, by.x = "modified", by.y = "SPID", 
                 all.x = TRUE, all.y = FALSE)

# make nearshore species list (see sector descriptions)
# Nearshore species list. (Source: M. Bellman, Heery, E., Jannot, J., and Majewski, J. 2010. Explicit Retrieval and Processing of PacFIN Data Used in Total Mortality Estimation. FRAM, NWFSC, NMFS.)

near_shore <- c("BISC", "KLPG", "BLCK", "KLPR", "BLK1","NSHR","NUSR", "BLU1","BLUR", "OLV1", "BRW1", "OLVE", "BRWN", "QLB1", "BSCL", "QLBK", "BYEL", "RCK2", "BYL1", "RCK7", "CBZ1","RCK9","CBZN", "RSCL", "CHN1", "SCOR","CHNA", "SCR1", "CLC1", "SHP1","CLCO", "SHPD","COP1", "SSHR", "COPP","￼SSRD", "GPH1","SSRS","GPHR", "SUSR","GRAS","TRE1","GRS1","TREE","KGL1","UDNR", "KLP1", "UGLG","USHR")

# global options ----
# should all be false
  # Year ≥ 1994
  any(tickets$year<1994)
  # Catch is from a PFMC area
  any(tickets$area=="")
    # are unknown areas - what to do with them? 
    length(which(tickets$area=="UNKN"))/nrow(tickets) # 16% of trips from unknown areas
  # Sectors 1-15 PARGRP=C
  # Sectors 16-19 PARGRP=I
  unique(tickets$pargrp)
  # Sectors 01, 02, and 16 AGID = N (as noted)
  # Sectors 03-15, 17-19 AGID = W, O, C
  unique(tickets$agid)

tickets$dahl_sector <- NA

# sector definitions ----
# sector 1: Whiting Catcher Processor Sector
s1 <- function(tickets) {
#   AGID =N
#   Gear Group is TWL
#   Valid trawl endorsement
#   DRVID=PROC
  tickets$dahl_sector <- ifelse(tickets$agid == "N" & tickets$grgroup == "TWL" & 
                         tickets$drvid == tickets$processorid & 
                         grepl("T", tickets$gr_endors) & is.na(tickets$dahl_sector), "s1", NA)
  return(tickets)
}

# sector 2: Whiting Mothership Sector
s2 <- function(tickets) {
#   AGID =N
#   Gear Group is TWL
#   Valid trawl endorsement
#   DRVID not equal to PROC
  tickets$dahl_sector <- ifelse(tickets$agid == "N" & tickets$grgroup == "TWL" & 
                         tickets$drvid != tickets$processorid & 
                         grepl("T", tickets$gr_endors) & 
                         is.na(tickets$dahl_sector), 1, NA)
  return(tickets)
}

# sector 3: Shoreside Whiting Sector (Shoreside IFQ Trawl)
s3 <- function(tickets){
#   Removal type (ftl.removal_type) not R (research)
#   Whiting (PWHT) RWT >= 50% total vessel-day-gear RWT
#   Gear Group is TWL
#   Valid trawl endorsement
  
  # find all trips that have TWL, valid trawl endorsement and not research, 
  # then test for whiting condition
  
  possible_whiting <- subset(tickets, removal_type != "R"  & 
                               grepl("T", gr_endors) & grgroup=="TWL" & 
                               is.na(dahl_sector))
  melt_df <- melt(possible_whiting, id.vars = c("trip_id", "modified"), 
                  measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ modified, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id
  cast_df$trip_id <- NULL
  
  cast_df <- cast_df/rowSums(cast_df)
  
  whiting_trips <- rownames(cast_df)[which(cast_df$PWHT>=.5)]
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% whiting_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% whiting_trips)] <- "s3"
    return(tickets)
  }else{
      warning("some sectors being overwritten")
    }
}

# sector 4: Shoreside Nonwhiting Trawl Sector (Shoreside IFQ Trawl)
# takes forever to run
s4 <- function(tickets){
  # check trip conditions and then look for species weights
  #   Removal type (ftl.removal_type) not E or R (EFP, research)
  #   Gear Group is TWL
  #   Valid trawl endorsement
  possible_s4 <- subset(tickets, !(removal_type %in% c("E","R")) & 
                          grgroup == "TWL" & grepl("T", gr_endors) &
                          is.na(dahl_sector))
  
  #   Pink shrimp, ridgbeback prawn, or spot prawn (PHSP, RPRW, SPRW) RWT < 100 lbs
  shrmps <- unique(possible_s4$trip_id[which(possible_s4$modified %in% c("PSHP","RPRW","SPRW") 
                                             & possible_s4$round_wt >=100)])
  if(length(shrmps)>0){
    possible_s4 <- possible_s4[-which(possible_s4$trip_id %in% shrmps),]
  }
  
  melt_df <- melt(possible_s4, id.vars = c("trip_id", "modified"), 
                  measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ modified, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id; cast_df$trip_id <- NULL
  cast_df <- cast_df/rowSums(cast_df)
  
  # Whiting (PWHT) RWT < 50% total vessel-day-gear RWT
  cast_df <- cast_df[which(cast_df$PWHT<.5),]
  
  #   Nonwhiting groundfish (sp.mgrp=GRND and sp.spid <> PWHT) RWT >= 50% total vessel-day-gear RWT
  poss_cols <- spid$SPID[which(spid$mgmt_grp=="GRND")] 
  poss_cols <- poss_cols[-which(poss_cols=="PWHT")]
  cols <- which(colnames(cast_df) %in% poss_cols)
  
  cast_df <- cast_df[which(rowSums(cast_df[,cols])>=.5),]
  
  # Groundfish (sp.mgrp=GRND) RWT > California halibut (CHLB, CHL1) RWT
  all_gf_cols <- which(colnames(cast_df) %in% spid$SPID[which(spid$mgmt_grp=="GRND")])
  gf_sums <- rowSums(cast_df[,all_gf_cols])
  
  ca_hal <- which(colnames(cast_df) %in% c("CHLB","CHL1"))
  
  if(length(ca_hal)>1){
    ca_hal_sums <- rowSums(cast_df[,ca_hal])
  }else{
    ca_hal_sums <- cast_df[,ca_hal]
  }
  cast_df <- cast_df[which( gf_sums > ca_hal_sums),]
  s4_trips <- rownames(cast_df)
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s4_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s4_trips)] <- "s4"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  }
  
  trips <- unique(possible_s4$trip_id)
  pb <- txtProgressBar(min = 1,max = length(trips))
  
  for(i in 1:length(trips)){
    setTxtProgressBar(pb, i)
    
    focal <- subset(possible_s4, trip_id == trips[i])
    
    # check for whiting
    if(any(focal$modified == "PWHT" &&
           sum(focal$round_wt[which(focal$modified=="PWHT")]) / sum(focal$round_wt) >=.5
           )
       ){
      possible_s4$s4[i] <- 0
      next
    }
    
    # check for groundfish
    if(sum(focal$round_wt[which(focal$mgmt_grp=="GRND" & focal$modified!="PWHT")])/sum(focal$round_wt) <.5){
      possible_s4$s4[i] <- 0
      next
    }
    # check to make sure there's more groundfish than CA halibut
    if(sum(focal$round_wt[which(focal$mgmt_grp=="GRND" & focal$modified!="PWHT")]) < sum(focal$round_wt[which(focal$modified %in% c("CHLB", "CHL1"))])){
      possible_s4$s4[i] <- 0
      next
    }
    
    # if made it through, then it's s4!
    possible_s4$s4[which(possible_s4$trip_id == trips[i])] <- 1
  }

  # find those trips that fall into s4

  trip_s4s <- unique(subset(possible_s4, s4 == 1, select = "trip_id"))
  tickets$s4 <- 0
  tickets$s4[which(tickets$trip_id %in% trip_s4s$trip_id)] <- 1
  return(tickets)
}

# sector 5: Nearshore Sector (Limited Entry)
# this is super weird, it's not sablefish fixed gear limited entry, it's nearshore caught with fixed gear...?
s5 <- function(tickets){
#   Gear Group is HKL or POT
#   Removal type (ftl.removal_type) not E or R (EFP, research)
#   Valid longline endorsement or pot gear endorsement.
#   IFQ landing flag (ifq_landing) = 'N'

  possible_s5 <- subset(tickets, grgroup %in% c("HKL","POT") & 
                          !(removal_type %in% c("E","R")) & 
                          grepl("P|L", gr_endors) & is.na(dahl_sector) & 
                          ifq_landing != "Y")

#   Sablefish RWT = 0
  drop_trips <- unique(possible_s5$trip_id[
    which(possible_s5$modified=="SABL" & possible_s5$round_wt>0)
    ])
  
  possible_s5 <- possible_s5[-which(possible_s5$trip_id %in% drop_trips),]

  #   Sum of nearshore species vessel-day-gear RWT >0
  
  melt_df <- melt(possible_s5, id.vars = c("trip_id", "modified"), 
                  measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ modified, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id; cast_df$trip_id <- NULL
  cast_df <- cast_df/rowSums(cast_df)
  
# Nearshore rockfish species as defined by WCGOP spid list see above
  nrshr_cols <- which(colnames(cast_df) %in% near_shore)
  cast_df <- cast_df[which(rowSums(cast_df[,nrshr_cols])>0),]
#   Groundfish (sp.mgrp=GRND) >= 50% of total vessel-day-gear RWT.
  grnd_cols <- which(colnames(cast_df) %in% spid$SPID[which(spid$mgmt_grp=="GRND")])
  cast_df <- cast_df[which(rowSums(cast_df[grnd_cols])>=.5),]
# This is equivalent to Groundfish (sp.mgrp=GRND) >= nongroundfish (all other sp.mgrp) by weight.

  s5_trips <- rownames(cast_df)
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s5_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s5_trips)] <- "s5"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  }
}

# sector 6: Nearshore Sector (Open Access)
s6 <- function(tickets){
#   Gear Group = HKL or POT
#   Removal type (ftl.removal_type) not E or R (EFP, research)
#   No valid longline endorsement or pot gear endorsement
#   IFQ landing flag (ifq_landing) = 'N'
  
  possible_s6 <- subset(tickets, grgroup %in% c("HKL","POT") & 
                          !(removal_type %in% c("E","R")) & 
                          !(grepl("P|L", gr_endors)) & ifq_landing != "Y" & 
                          is.na(dahl_sector))

  #   Groundfish (sp.mgrp=GRND) >= 50% of total vessel-day-gear RWT
  # find trips which have groundfish in them
  grnd_trips <- unique(possible_s6$trip_id[which(possible_s6$mgmt_grp=="GRND")])
  possible_s6 <- possible_s6[which(possible_s6$trip_id %in% grnd_trips), ]
  
  # find trips that have at least 50% groundfish in them
  melt_df <- melt(possible_s6, id.vars = c("mgmt_grp","trip_id"), measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ mgmt_grp, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id
  cast_df$trip_id <- NULL
  cast_df <- cast_df/rowSums(cast_df)
  
  # find trips that have at least 50% groundfish
  trip_keep <- rownames(cast_df)[which(cast_df$GRND>=.5)]
  possible_s6 <- subset(possible_s6, trip_id %in% trip_keep)
    
  #   Sablefish RWT = 0
  drop_trips <- unique(possible_s6$trip_id[which(possible_s6$modified=="SABL" & 
                                                   possible_s6$round_wt>0)])
  possible_s6 <- possible_s6[-which(possible_s6$trip_id %in% drop_trips),]
  
  #   Sum of nearshore species vessel-day-gear RWT >0
  melt_df <- melt(possible_s6, id.vars = c("modified","trip_id"), measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ modified, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id
  cast_df$trip_id <- NULL
  cast_df <- cast_df/rowSums(cast_df)
  
  nrshr_cols <- which(colnames(cast_df) %in% near_shore)
  cast_df <- cast_df[which(rowSums(cast_df[,nrshr_cols])>0),]
  
  s6_trips <- rownames(cast_df)
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s6_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s6_trips)] <- "s6"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  }
  
}

# sector 7: Non Nearshore Sector (Limited Entry)
s7 <- function(tickets){
#   Gear Group is HKL or POT
#   Removal type (ftl.removal_type) not E or R (EFP, research)
#   Valid longline endorsement or pot gear endorsement
#   IFQ landing flag (ifq_landing) = 'N'  

  possible_s7<- subset(tickets, grgroup %in% c("HKL","POT") & 
                          !(removal_type %in% c("E","R")) & 
                          grepl("P|L", gr_endors) & ifq_landing != "Y" &
                         is.na(dahl_sector))

  #   Sum of nearshore species RWT =0
  #find trips with nearshore species
  if(any(possible_s7$modified %in% near_shore & possible_s7$round_wt > 0 )){
  nearshore_trips <- unique(possible_s7$trip_id[
    which(possible_s7$modified %in% near_shore)
    ])
  possible_s7 <- possible_s7[-which(possible_s7$trip_id %in% nearshore_trips),]
  }
  
  #   Sablefish RWT >0
  # find trips that have sablefish greater than zero
  sable_trips <- unique(possible_s7$trip_id[which(possible_s7$modified=="SABL" &
                                                    possible_s7$round_wt>0)])
  possible_s7 <- possible_s7[which(possible_s7$trip_id %in% sable_trips),]

  #   Groundfish (sp.mgrp=GRND) >= 50% of total vessel-day-gear RWT
  # table and find trips that have at least 50% groundfish
  melt_df <- melt(possible_s7, id.vars = c("trip_id","mgmt_grp"), measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ mgmt_grp, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id; cast_df$trip_id <- NULL
  cast_df <- cast_df/rowSums(cast_df)
  s7_trips <- rownames(cast_df)[which(cast_df$GRND>=.5)]
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s7_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s7_trips)] <- "s7"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  }
  }

# sector 8: Non Nearshore Sector (Open Access)
s8 <- function(tickets){
#   Gear Group = HKL or POT
#   Removal type (ftl.removal_type) not E or R (EFP, research)
#   No valid longline endorsement or pot gear endorsement
#   IFQ landing flag (ifq_landing) = 'N'
  possible_s8 <- subset(tickets, grgroup %in% c("HKL","POT") & 
                          !(removal_type %in% c("E","R")) & 
                          !(grepl("P|L", gr_endors)) & ifq_landing != "Y" & 
                          is.na(dahl_sector))

#   Sum of nearshore species RWT =0
  #find trips with nearshore species
  if(any(possible_s8$modified %in% near_shore & possible_s8$round_wt>0)){
    nearshore_trips <- unique(possible_s8$trip_id[
      which(possible_s8$modified %in% near_shore & possible_s8$round_wt >0 )
      ])
    possible_s8 <- possible_s8[-which(possible_s8$trip_id %in% nearshore_trips),]
  }
#   Sum of sablefish vessel-day-gear RWT >0
  # find trips that have sablefish greater than zero
  sable_trips <- unique(possible_s8$trip_id[which(possible_s8$modified=="SABL" 
                                                  & possible_s8$round_wt>0)])
  possible_s8 <- possible_s8[which(possible_s8$trip_id %in% sable_trips),]
  
#   Groundfish (sp.mgrp=GRND) >= 50% of total vessel-day-gear RWT
  # table and find trips that have at least 50% groundfish
  melt_df <- melt(possible_s8, id.vars = c("trip_id","mgmt_grp"), measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ mgmt_grp, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id; cast_df$trip_id <- NULL
  cast_df <- cast_df/rowSums(cast_df)
  
  s8_trips <- rownames(cast_df)[which(cast_df$GRND>=.5)]
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s8_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s8_trips)] <- "s8"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  }
  
  return(tickets)
}

# sector 9: Non Nearshore Non Sablefish Sector (Limited Entry)
s9 <- function(tickets){
#   Gear Group is HKL or POT
#   Removal type (ftl.removal_type) not E or R (EFP, research)
#   IFQ landing flag (ifq_landing) = 'N'
#   Valid longline endorsement or pot gear endorsement
  possible_s9<- subset(tickets, grgroup %in% c("HKL","POT") & 
                         !(removal_type %in% c("E","R")) & 
                         grepl("P|L", gr_endors) & ifq_landing != "Y" &
                         is.na(dahl_sector))
  
#   Sum of nearshore species RWT =0
  #find trips with nearshore species remove them
  if(any(possible_s9$modified %in% near_shore & possible_s9$round_wt>0)){
    nearshore_trips <- unique(possible_s9$trip_id[
      which(possible_s9$modified %in% near_shore)
      ])
    possible_s9 <- possible_s9[-which(possible_s9$trip_id %in% nearshore_trips),]
  }
  
#   Sablefish RWT = 0
  if(any(possible_s9$modified == "SABL" & possible_s9$round_wt>0)){
    sable_trips <- unique(possible_s9$trip_id[
      which(possible_s9$modified == "SABL" & possible_s9$round_wt >0 )
      ])
    possible_s9 <- possible_s9[-which(possible_s9$trip_id %in% sable_trips),]
  }
#   Groundfish (sp.mgrp=GRND) >= 50% of total vessel-day-gear RWT
  # table and find trips that have at least 50% groundfish
  melt_df <- melt(possible_s9, id.vars = c("trip_id","mgmt_grp"), measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ mgmt_grp, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id; cast_df$trip_id <- NULL
  cast_df <- cast_df/rowSums(cast_df)
  s9_trips <- rownames(cast_df)[which(cast_df$GRND>=.5)]
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s9_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s9_trips)] <- "s9"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  }
}

# sector 10: Non Nearshore Non Sablefish Sector (Open Access)
s10 <- function(tickets){
#   Gear Group = HKL or POT
#   Removal type (ftl.removal_type) not E or R (EFP, research)
#   IFQ landing flag (ifq_landing) = 'N'
#   No valid longline endorsement or pot gear endorsement
  possible_s10 <- subset(tickets, grgroup %in% c("HKL","POT") & 
                         !(removal_type %in% c("E","R")) & 
                        !(grepl("P|L", gr_endors)) & ifq_landing != "Y" & 
                          is.na(dahl_sector))

#   Sum of nearshore species RWT =0
  #find trips with nearshore species remove them
  if(any(possible_s10$modified %in% near_shore & possible_s10$round_wt>0)){
    nearshore_trips <- unique(possible_s10$trip_id[
      which(possible_s10$modified %in% near_shore & possible_s10$round_wt > 0)
      ])
    possible_s10 <- possible_s10[-which(possible_s10$trip_id %in% nearshore_trips),]
  }
  
#   Sablefish =0
  #   Sablefish RWT = 0
  if(any(possible_s10$modified == "SABL" & possible_s10$round_wt > 0)){
    sable_trips <- unique(possible_s10$trip_id[
      which(possible_s10$modified == "SABL" & possible_s10$round_wt > 0)
      ])
    possible_s10 <- possible_s10[-which(possible_s10$trip_id %in% sable_trips),]
  }
  
#   Groundfish (sp.mgrp=GRND) >= 50% of total vessel-day-gear RWT
  # table and find trips that have at least 50% groundfish
  melt_df <- melt(possible_s10, id.vars = c("trip_id","mgmt_grp"), measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ mgmt_grp, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id; cast_df$trip_id <- NULL
  cast_df <- cast_df/rowSums(cast_df)
  s10_trips <- rownames(cast_df)[which(cast_df$GRND>=.5)]
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s10_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s10_trips)] <- "s10"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  }
  
}

# sector 11: Non Fixed Gear Directed Open Access Sector
s11 <- function(tickets){
#   Gear Group = NET or TLS
#   Removal type (ftl.removal_type) not E or R (EFP, research)
#   IFQ landing flag (ifq_landing) = 'N'
possible_s11 <- subset(tickets, grgroup %in% c("NET","TLS") & 
                         !(removal_type %in% c("E","R")) & ifq_landing != "Y"
                       & is.na(dahl_sector))
  
    # Groundfish (sp.mgrp=GRND) RWT >= 50% of total vessel-day-gear RWT
    # table and find trips that have at least 50% groundfish
    melt_df <- melt(possible_s11, id.vars = c("trip_id","mgmt_grp"), measure.vars = "round_wt")
    cast_df <- dcast(melt_df, trip_id ~ mgmt_grp, fun.aggregate = sum)
    rownames(cast_df) <- cast_df$trip_id; cast_df$trip_id <- NULL
    cast_df <- cast_df/rowSums(cast_df)
    s11_trips <- rownames(cast_df)[which(cast_df$GRND>=.5)]
    
    # make sure no sectors have already been assigned
    no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s11_trips)]))
    
    if(no_NA){
      tickets$dahl_sector[which(tickets$trip_id %in% s11_trips)] <- "s11"
      return(tickets)
    }else{
      warning("some sectors being overwritten")
    }
}

# sector 12: Incidental Open Access Sector
s12 <- function(tickets){
#   Removal type (ftl.removal_type) not E or R (EFP, research)
#   Gear Group = HKL or POT or NET or TLS
#   No valid longline endorsement or pot gear endorsement
#   IFQ landing flag (ifq_landing) = 'N'
  possible_s12 <- subset(tickets, grgroup %in% c("HKL","POT","NET","TLS") &
                           !(removal_type %in% c("E","R")) & !(grepl("P|L", gr_endors))
                         & ifq_landing != "Y" & is.na(dahl_sector))
  
  #   Groundfish (sp.mgrp=GRND) RWT > 0 and <50% of total vessel-day-gear RWT
  # table and find trips that have at least 50% groundfish
  melt_df <- melt(possible_s12, id.vars = c("trip_id","mgmt_grp"), measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ mgmt_grp, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id; cast_df$trip_id <- NULL
  cast_df <- cast_df/rowSums(cast_df)
  s12_trips <- rownames(cast_df)[which(cast_df$GRND<.5 & cast_df$GRND>0)]
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s12_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s12_trips)] <- "s12"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  }
}

# sector 20: Shoreside IFQ Nontrawl Sector
s20 <- function(tickets){
  #   Gear Group not TWL
  #   Removal type (ftl.removal_type) not E or R (EFP, research) IFQ landing flag (ifq_landing) = 'Y'
  possible_s20 <- subset(tickets, ifq_landing == "Y" & grgroup != "TWL" & 
                           is.na(dahl_sector) & !(removal_type %in% c("E","R")))
  
  s20_trips <- unique(possible_s20$trip_id)
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s20_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s20_trips)] <- "s20"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  }

}

# sector 13: Exempted Trawl Sector (With Groundfish Landings)
s13 <- function(tickets){
#   Removal type (ftl.removal_type) not E or R (EFP, research)
#   Gear group is TWL or TWS
#   No valid trawl endorsement
  possible_s13 <- subset(tickets, !(removal_type %in% c("E","R")) & 
                           grgroup %in% c("TWL","TWS") & 
                           !(grepl("T",gr_endors)) & is.na(dahl_sector))
  
#   Groundfish (sp.mgrp=GRND) RWT > 0
  gf_trips <- unique(possible_s13$trip_id[which(possible_s13$mgmt_grp == "GRND" 
                                                & possible_s13$round_wt>0)])
  possible_s13 <- possible_s13[which(possible_s13$trip_id %in% gf_trips),]

#   California halibut (CHLB, CHL1) RWT > 0 or pink shrimp, ridgbeback prawn, or spot prawn (PSHP, RPRW, SPRW) RWT => 100 lbs
  ca_hal_trips <- unique(possible_s13$trip_id[
    which(possible_s13$modified %in% c("CHLB", "CHL1") & possible_s13$round_wt> 0)
    ])
  shrp_trp <- unique(possible_s13$trip_id[   
      which(possible_s13$modified %in% c("PSHP", "RPRW", "SPRW") & possible_s13$round_wt>=100)
    ])
  
  s13_trips <- c(ca_hal_trips, shrp_trp)
  
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s13_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s13_trips)] <- "s13"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  }
}

# sector 14: EFP and Miscellaneous Sector
s14 <- function(tickets){
  #   Groundfish (sp.mgrp=GRND) RWT > 0
  s14_trips <- unique(tickets$trip_id[which(tickets$mgmt_grp=="GRND" & 
                                            is.na(tickets$dahl_sector) &
                                            tickets$round_wt > 0)])
  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s14_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s14_trips)] <- "s14"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  } 
  
}

# sector 15: Commercial Nongroundfish Sector
s15 <- function(tickets){
#   groundfish (sp.mgrp=GRND) RWT = 0 
#   IFQ landing flag (ifq_landing) = 'N'
  possible_s15 <- subset(tickets, ifq_landing != "Y" & is.na(dahl_sector))
  remove_gf <- unique(possible_s15$trip_id[which(possible_s15$mgmt_grp=="GRND" 
                                                 & possible_s15$round_wt>0)])
  if(length(remove_gf)>0){
    s15_trips <- unique(possible_s15$trip_id[-which(possible_s15$trip_id %in% remove_gf)])
  }else{
      s15_trips <- unique(possible_s15$trip_id)
  }

  # make sure no sectors have already been assigned
  no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s15_trips)]))
  
  if(no_NA){
    tickets$dahl_sector[which(tickets$trip_id %in% s15_trips)] <- "s15"
    return(tickets)
  }else{
    warning("some sectors being overwritten")
  } 
  
}

# sector 16: Treaty Mothership Whiting Sector
any(tickets$agid=="N") # don't need it

# sector 17: Treaty Shoreside Whiting Sector - not sure about what makes something a treaty
s17 <- function(tickets){
#   Whiting (PWHT) RWT >= 50% total vessel-day-gear RWT 
#   Gear Group is TWL
  possible_s17 <- subset(tickets, grgroup == "TWL" & is.na(dahl_sector))
  melt_df <- melt(possible_s17, id.vars = c("trip_id","modified"), 
                  measure.vars = "round_wt")
  cast_df <- dcast(melt_df, trip_id ~ modified, fun.aggregate = sum)
  rownames(cast_df) <- cast_df$trip_id; cast_df$trip_id <- NULL
  cast_df <- cast_df/rowSums(cast_df)
  s17_trips = rownames(cast_df)[which(cast_df$PWHT>=.5)]
  if(length(s17_trips > 0)){
    # make sure no sectors have already been assigned
    no_NA <- all(is.na(tickets$dahl_sector[which(tickets$trip_id %in% s17_trips)]))
    
    if(no_NA){
      tickets$dahl_sector[which(tickets$trip_id %in% s15_trips)] <- "s17"
      return(tickets)
    }else{
      warning("some sectors being overwritten")
    } 
  }else{
    return(tickets)
  }
}

# sector 18: ... waiting on understanding what indicates treaty sector



# filter tickets ----
s1_mark <- s1(tickets)
s2_mark <- s2(s1_mark)
s3_mark <- s3(s2_mark)
s4_mark <- s4(s3_mark)
s5_mark <- s5(s4_mark)
s6_mark <- s6(s5_mark)
s7_mark <- s7(s6_mark)
s8_mark <- s8(s7_mark)
s9_mark <- s9(s8_mark)
s10_mark <- s10(s9_mark)
s11_mark <- s11(s10_mark)
s12_mark <- s12(s11_mark)
s20_mark <- s20(s12_mark)
s13_mark <- s13(s20_mark)
s14_mark <- s14(s13_mark)
s15_mark <- s15(s14_mark)
s17_mark <- s17(s15_mark)

saveRDS(s17_mark, "/Users/efuller/1/CNH/processedData/catch/dahl_sector/tickets_dahl_sector.RDS")


# compare to my metiers ----
# basic questions
#   - what metiers are left out of sectors?
#   - how do my metiers compare to the sectors?
# should make a network where nodes are metiers/sectors and connections are the percentage of metiers.. or sectors? or do both each with own arrow?

trip_cats <- unique(s17_mark[,c("trip_id","metier","cluster","dahl_sector","drvid")])

# make an association matrix in which each elements is the percentage of the row that is categorized by the column. rows are metiers, columns are sectors

associate_mat <- matrix(nrow = length(unique(trip_cats$metier)), ncol = length(unique(trip_cats$dahl_sector)))
rownames(associate_mat) <- unique(trip_cats$metier)
colnames(associate_mat) <- unique(trip_cats$dahl_sector)

for(i in 1:nrow(associate_mat)){
    row_met <- subset(trip_cats, metier == rownames(associate_mat)[i])
    total_trips <- nrow(row_met)
  for(j in 1:ncol(associate_mat)){
#     associate_mat[i,j] <- length(
#       (which(row_met$dahl_sector==colnames(associate_mat)[j])))/total_trips
     associate_mat[i,j] <- length(
       (which(row_met$dahl_sector==colnames(associate_mat)[j])))
  }
}

library(bipartite)
plotweb(associate_mat)

# are at least 50 metiers that have fewer than 200 trips. drop those for visual clarity?
hist(rowSums(associate_mat), breaks = 500,xlim = c(0,1000))

plotweb(associate_mat[which(rowSums(associate_mat)>5000),], text.rot = 90)


# just lok at groundfish
plotweb(associate_mat[c("HKL_3","HKL_2","TWL_1","POT_4","HKL_1","TWL_3","HKL_4","HKL_6","HKL_8","POT_13","TWL_7","TWL_11"),])

# what else are TWL_11 doing?
twl_11_vessels <- unique(trip_cats$drvid[which(trip_cats$metier == "TWL_11")])

twl_11_trips <- subset(trip_cats, drvid %in% twl_11_vessels)
round(table(twl_11_trips$drvid, twl_11_trips$metier)/rowSums(table(twl_11_trips$drvid, twl_11_trips$metier)),3)
#  hm, twl_11 doesn't look like it's a target fishery

# what about sector 6, are these people pretty segregated?
s6_vessels <- unique(trip_cats$drvid[which(trip_cats$dahl_sector == "s6")])

s6_trips <- subset(trip_cats, drvid %in% s6_vessels)
percentage_mets <- round(table(s6_trips$drvid, s6_trips$metier)/rowSums(table(s6_trips$drvid, s6_trips$metier)),3)
which(colnames(percentage_mets)=="HKL_3")
hist(percentage_mets[,15])
which(colnames(percentage_mets)=="HKL_6")
hist(percentage_mets[,18])
which(colnames(percentage_mets)=="HKL_2")
hist(percentage_mets[,10])

# hm none of these tend to be done in isolation.. 

# remove s14, and anything that 100% s14
non_msc <- associate_mat[-which(rowSums(associate_mat[,-1])<55),]
non_msc[which(non_msc<30)] <- 0
plotweb(non_msc, text.rot=90)

?# troubleshooting how come HKL_1 it shows up so often in s14 and s10 and s12, not s8 and s7? 
hkl_1 <- subset(s17_mark,metier=="HKL_1")
hkl_1_trips <- unique(hkl_1[,c("trip_id","metier","cluster","dahl_sector")])
sort(table(hkl_1_trips$dahl_sector),decreasing = T)

head(which(hkl_1_trips$dahl_sector=="s14"))
focal1 <- subset(hkl_1, trip_id==hkl_1_trips$trip_id[6]) 
focal2 <- subset(hkl_1, trip_id==hkl_1_trips$trip_id[7])
# these two s14s have rockfish, which would explain why they're not in s7 or s8. 

head(which(hkl_1_trips$dahl_sector=="s10"))
(focal3 <- subset(hkl_1, trip_id==hkl_1_trips$trip_id[77]))
(focal4 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[78]))
# neither of these have sablefish in them. 
(focal5 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[85]))
# all seem to have a lot of blackgill (blgl). First two are pole, last one is longline

(focal6 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[109]))
# this has shortspine in them, relates to number 5 and is longline. 
# also a lot of them are cluster 4

(focal7 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[118]))
# again a ton of black gill, is VHL this time for gear and cluster 4. 

# looking just at s14. 
melt_df <- melt(subset(hkl_1, dahl_sector == "s14"), id.vars = c("trip_id","modified"), measure.vars = "round_wt")
cast_df <- dcast(melt_df,trip_id ~ modified, fun.aggregate = sum)
rownames(cast_df) <- cast_df$trip_id; cast_df$trip_id <- NULL
cast_df <- cast_df/rowSums(cast_df)
which(colnames(cast_df)=="SABL")
which(colnames(cast_df)=="BLGL")
plot(density(cast_df[,67]) # bimodal
plot(density(cast_df[,7])) # mostly zero
# look at blgl when sable < .75
plot(density(cast_df[which(cast_df[,67]<.75),7]))
# look at blgl when sable < .5
plot(density(cast_df[which(cast_df[,67]<.5),7]))
# look at blgl when sable < .25
plot(density(cast_df[which(cast_df[,67]<.25),7]))
# look at blgl when sable < .05
plot(density(cast_df[which(cast_df[,67]<.05),7]))

# look at blgl when sable < .05
plot(density(cast_df[which(cast_df[,67]==0),7]))

# hm so maybe not blgl. what's in the catch when sablefish is 0
plot(density(cast_df[which(cast_df$SABL==0),"SSPN"]))
plot(density(cast_df$SSPN))
# it's shortspine perhaps

# but what's in the catch when both SSPN and SABLE not there, LSPN?
plot(density(cast_df[which(cast_df$SABL==0 & cast_df$SSPN == 0), "LSPN"]))
plot(density(cast_df[which(cast_df$SABL==0 & cast_df$SSPN == 0), "BLGL"]))
# sometimes
plot(density(cast_df[which(cast_df$SABL==0 & cast_df$SSPN == 0), "NUSP"]))
plot(density(cast_df[which(cast_df$SABL==0 & cast_df$SSPN == 0), "GRDR"]))
plot(density(cast_df[which(cast_df$SABL==0 & cast_df$SSPN == 0), "PHLB"]))
plot(density(cast_df[which(cast_df$SABL==0 & cast_df$SSPN == 0), "PHLB"]))
plot(density(cast_df[which(cast_df$SABL==0 & cast_df$SSPN == 0), "PTRL"]))
plot(density(cast_df[which(cast_df$SABL==0 & cast_df$SSPN == 0), "DBRK"]))
plot(density(cast_df[which(cast_df$SABL==0 & cast_df$SSPN == 0), "NUSP"]))

# but then why is there so much sablefish in s14?
round(sort(colSums(cast_df[which(cast_df$SABL>.5),])),3)

# find these rows
s14_sable_trips <- rownames(cast_df)[which(cast_df$SABL>.5)]
subset(hkl_1, trip_id == s14_sable_trips[1])$round_wt/sum(subset(hkl_1, trip_id == s14_sable_trips[1])$round_wt)
# I guess 

# how about s12
head(which(hkl_1_trips$dahl_sector=="s12"))
(focal8 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[1], select = c("modified","round_wt","mgmt_grp","grid","grgroup","cluster")))
# majority is pacific halibut but has sable
(focal9 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[391], select = c("modified","round_wt","mgmt_grp","grid","grgroup","cluster")))
# majority is pacific halibut but has sable
(focal10 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[433], select = c("modified","round_wt","mgmt_grp","grgroup","grid","cluster")))
# again has sable but majority is pacific halibut
(focal11 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[526], select = c("modified","round_wt","mgmt_grp","grgroup","grid","cluster")))
# again sable but majority is pacific halibut

# what gets put in s15?
head(which(hkl_1_trips$dahl_sector=="s15"))
(focal12 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[1846], select = c("modified","round_wt","mgmt_grp","grid","grgroup","cluster","metier")))
# angel_shark, pole

(focal13 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[4004], select = c("modified","round_wt","mgmt_grp","grid","grgroup","cluster","metier")))
# some serious tuna on a pole

(focal13 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[4004], select = c("modified","round_wt","mgmt_grp","grid","grgroup","cluster","metier")))

(focal14 <- subset(hkl_1,trip_id == hkl_1_trips$trip_id[4016], select = c("modified","round_wt","mgmt_grp","grid","grgroup","cluster","metier")))
# ca halibut and misc

# conclusion, really weird things and probably extremely rare are getting lumped into the sablefish group. 