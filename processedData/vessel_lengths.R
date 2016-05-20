CG1 <- read.csv("rawData/Catch/vessel_registration/CG_2013_woc_141210_three.csv",stringsAsFactors=F, skip = 2)
CG2 <- read.csv("rawData/Catch/vessel_registration/CG_2009-2012_woc_141210_three.csv",stringsAsFactors = F, skip = 2)
cg <- rbind(CG1,CG2); rm(CG1, CG2)

# CG lengths
library(dplyr)
lengths <- cg %>% filter(vessel_id!="") %>% group_by(vessel_id, vessel_name) %>% 
  filter(pubyr == max(pubyr)) %>%
  dplyr::select(pubyr, length) %>% summarize(pubyr = unique(pubyr), 
                                      length = mean(length, na.rm =T))
  
# state lengths
sv <- read.csv("rawData/Catch/vessel_registration/SV_2009-2013_woc_141210_two.csv",stringsAsFactors=F, skip = 2)
colnames(sv)[1] <- "year"

# take most recent
full_lengths <- sv %>% group_by(svid) %>% filter(year == max(year)) %>% 
  dplyr::select(year, length) %>% summarize(year = as.numeric(unique(year)), 
                                            length = mean(length,na.rm=T)) %>%
  full_join(lengths, by = c("svid" = "vessel_id", "year" = "pubyr")) %>%
  group_by(svid) %>%
  filter(year == max(year)) %>%
  gather(length_type, length, -svid, -year) %>%
  filter(!is.na(length)) %>%
  summarize(length = mean(length, na.rm=T)) %>%
  rename(drvid = svid)

write.csv(full_lengths, file = "processedData/vessel_lengths.csv",row.names = FALSE)
