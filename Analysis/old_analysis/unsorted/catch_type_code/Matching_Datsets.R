# Goal: Subset VMS data for pink shrimp, and vessels for which I have obseverer data/observer & ownership data, respectively. 

# load VMS data
  VMS <- read.csv("/wrk2/efuller/NOAA/Data_Analysis/VMS/Code/processed_data/VMS_woDups.csv")
  
  # load Observer data
  nhObs <- read.csv("/wrk2/efuller/NOAA/Data_Analysis/Obs/Data/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv")
  
# load Ownership data
  Owners <- read.csv("/wrk2/efuller/NOAA/Data_Analysis/Ownership/csvVersions/Linked_IFQ_Vessel_Accounts_01-27-14_EFmod_VesselAccounts_Exact.csv")
  
########### 
## local ##
###########
  
#Owners <- read.csv("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Ownership/csvVersions/Linked_IFQ_Vessel_Accounts_01-27-14_EFmod_VesselAccounts_Exact.csv")
#nhObs <- read.csv("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Obs/Data/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv")
#VMS <- readRDS("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/VMS/Code/processed_data/VMSdata.RDS")

  
## Pink shrimp vessels

# Find observed vessels in pink shrimp fishery
    shrimps <- subset(nhObs, sector == "Pink Shrimp")
    uniqueShrimps <- unique(shrimps$CG_NUM)    # 74 vessels
  # Subset VMS data to those pink shrimp vessels
    tracks <- unique(VMS$Doc_Number)
    captured <- length(uniqueShrimps[which(uniqueShrimps %in% tracks)]) #all
    shrimpVMS <- VMS[VMS$Doc_Number %in% uniqueShrimps,]
    length(unique(shrimpVMS$Doc_Number)) == captured   # should be TRUE
  
  # Save dataset
    write.csv(shrimpVMS,"/wrk2/efuller/NOAA/Data_Analysis/VMS/Code/processed_data/shrimpVMS.csv")
  
# Vessels we have ownership and obsever data for
    # Find vessels in ownership dataset and observerer dataset
      owned <- unique(Owners$VESSEL.DOC..)
      captured <- length(which(owned %in% unique(nhObs$CG_NUM)))
      ownedObs <- nhObs[nhObs$CG_NUM %in% owned,]
      length(unique(ownedObs$CG_NUM)) == captured # should be TRUE if worked
    # Find VMS data
      uniqueOwnedObs <- unique(ownedObs$CG_NUM)
      captured <- length(uniqueOwnedObs[which(uniqueOwnedObs %in% tracks)]) # missing 1 from owned/obs
      ownedObsVMS <- VMS[VMS$Doc_Number %in% uniqueOwnedObs,]
      length(unique(ownedObsVMS$Doc_Number)) == captured   # should be TRUE

    # Write csv
      write.csv(ownedObsVMS,"/wrk2/efuller/NOAA/Data_Analysis/VMS/Code/processed_data/ownedObsVMS.csv")
    
