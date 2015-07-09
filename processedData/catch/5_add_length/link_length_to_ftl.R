# add length to catch data
# load state and federal data, load catch data. 

# load data ----
# state registration
  sv <- read.csv("/Users/efuller/1/CNH/rawData/Catch/vessel_registration/SV_2009-2013_woc_141210_two.csv", stringsAsFactors = FALSE, skip = 2)
  colnames(sv)[1] <- "year"

# federal registration
  cg1 <- read.csv("/Users/efuller/1/CNH/rawData/Catch/vessel_registration/CG_2009-2012_woc_141210_three.csv", stringsAsFactors = FALSE, skip = 2)
  cg2 <- read.csv("/Users/efuller/1/CNH/rawData/Catch/vessel_registration/CG_2013_woc_141210_three.csv", stringsAsFactors = FALSE, skip = 2)
  
  cg <- rbind(cg1, cg2)
  # lots of blank rows in there
  rm(cg1, cg2)

# load catch data
  ftl <- readRDS("/Users/efuller/1/CNH/processedData/catch/4_dahl_sector/tickets_dahl_sector.RDS")
  
# find vessel-year in each data set - have many different ids ----
  
  # catch data
  ftl_vessels <- unique(ftl[,c("drvid","veid", "year")])
  
  # cg data
    # any cg have more than one record in a year?
      library(dplyr)
      in_year_count <- cg %>%
        group_by(vessel_id, pubyr) %>%
        summarize(num_entries = length(length)) %>%
        filter(pubyr>2000)
      
      in_year_count[which(in_year_count$num_entries!=1),] # 5 entries
      # but note the pubyr. I bet more than this one got messed up. 
      
      # find all pubpyrs not between 2009-2013, 
      cg[which(cg$pubyr < 2009 | cg$pubyr > 2013),]
      # looks weird. will drop
      
      cg <- cg[-which(cg$pubyr < 2009 | cg$pubyr > 2013),]
      
      # recount
      in_year_count <- cg %>%
        group_by(vessel_id, pubyr) %>%
        summarize(num_entries = length(length)) %>%
        filter(pubyr>2000)
      
      # retest - none, great. 
      in_year_count[which(in_year_count$num_entries!=1),]
      # can move on
  cg_vessels <- unique(cg[,c("hull_number","vessel_id","vessel_name","pubyr","length","horsepower","hp_main_astern","breadth","depth")])
  
  # sv data
  sv$year <- as.integer(sv$year) # NAs are real, remove
  sv <- sv[-which(is.na(sv$year)),]
    # check for more than one entry in a year
      in_year_count <- sv %>%
        group_by(svid, year) %>%
        summarize(num_entries = length(length)) 
      
      tail(in_year_count[which(in_year_count$num_entries!=1),])
      # see lots, and that's because the same entry shows up in both places. And have different names there. And differ in some measures.
      # i.e. 
      subset(sv, svid == "WN8337RG" & year == 2013)
   
      # many of these are similar, maybe take the average of any numeric values. But want to save the vessel name that's listed. 
      # because the names differ based on states, will retain both copies for the moment
       sv_vessels <- unique(sv[,c("year","svid","plate","name","length","weight","horsepower","charterboat")])
       
# combining vessel length data ----
  
  # figure out which id is most relevant?
    any(sv_vessels$svid %in% cg_vessels$vessel_id) # looks good
    any(sv_vessels$svid %in% cg_vessels$hull_number) # expected FALSE, good
    
    sv_cg <- merge(sv_vessels, cg_vessels, by.x = c("svid","year"), by.y = c("vessel_id", "pubyr"), all.x = TRUE, all.y = TRUE)
    
    # looks messy
      head(sv_cg)
    # but also like it's working
      sv_cg[head(which(!is.na(sv_cg$name) & !is.na(sv_cg$vessel_name))),]
      
    # now try to link with ftl data
      length(which(sv_cg$svid %in% ftl_vessels$drvid))
      length(which(sv_cg$svid %in% ftl_vessels$veid)) 
      # twice as many in drvid as in veid. Will go with that, 
      # also Brad told me to to use drvid, veid is more ambiguous. 
      
    # subset to vessel length data that is in ftl
      length_data <- sv_cg[which(sv_cg$svid %in% ftl_vessels$drvid),]
      # how much does this drop
      nrow(length_data)/nrow(sv_cg)
      # a lot, but it looks a lot cleaner
      head(length_data)
      tail(length_data)
      
    # now that I have length data, I should quality check. Biggest/smallest?
      range(length_data$length.x, na.rm = T)
      range(length_data$length.y, na.rm = T)
      
      # state is much bigger, but that big boat not registered in cg. 
      # and is a forage fish seiner and crabber, reasonable?
    
      head(subset(ftl, drvid == "1240646", select = c("spid","landed_wt","adj_revenue","pcid")),20)
      
      range(length_data$horsepower.x, na.rm = T)
      range(length_data$horsepower.y, na.rm = T)
      
    # the vessel with 2000 hp is a 107 foot boat, fair enough?
      length_data[which(length_data$horsepower.x==2000),]
      
    # and is a whiting boat, probably a processor
      sort(table(subset(ftl, drvid == "518937", select = c("spid","landed_wt","adj_revenue","pcid", "grid","grgroup"))$spid),decreasing = TRUE)[1:10]
      
    # goal is to link length data to ftl within a year. 
    # but many vessels have multiple entries for length/hp information
    # can have different measures between state and CG and then between different states
    
    # will take most informed combination for each vessel in a year
    # and take average of all entries, removing NAs
      
    length_ref <- length_data %>%
      group_by(svid, year) %>%
      summarize(len = mean(length.x, length.y, na.rm = TRUE, trim = 0),
                hp = mean(horsepower.x, horsepower.y, hp_main_astern,trim = 0, na.rm = TRUE),
                weight = mean(weight, na.rm = T, trim = 0),
                breadth = mean(breadth, na.rm = T, trim = 0),
                depth = mean(depth, na.rm = T, trim = 0)
                )
    
    # check to make sure I've collapsd all records
    any(duplicated(length_ref[,c("svid","year")])) # should be FALSE
    
    # merge with ftl data
    ftl_len <- merge(ftl, length_ref, by.x = c("drvid","year"), by.y = c("svid","year"), all.x = TRUE, all.y = FALSE)
    
    # remove the extra year column
    ftl_len$year.y <- NULL
    # rename year.x
    colnames(ftl_len)[which(colnames(ftl_len)=="year.x")] <- "year"
    saveRDS(ftl_len, file = "/Users/efuller/1/CNH/processedData/catch/5_add_length/ftl_len.RDS")
    
    
    