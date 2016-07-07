library(dplyr)

tickets <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")
#tickets <- tickets %>% filter(drvid !="NONE") # don't drop dredges

# check for ZZZ drvids
if(length(grep("ZZZ",tickets$drvid))>0){
  warning('remove ZZZ drvids')
}

# some stats
length(unique(tickets$drvid))
length(unique(tickets$trip_id))
sum(tickets$adj_revenue)
sum(as.numeric(tickets$pounds))
length(unique(tickets$modified))

# table 1----
# get latitude
all_ports <- read.csv("processedData/spatial/ports/all_ports.csv", 
                      stringsAsFactors = F) %>%
  rename(pcid = Pcid)
tickets <- left_join(tickets, all_ports, by = 'pcid') 

# add DOY
tickets$tdate <- as.Date(tickets$tdate, format="%d-%b-%y")
tickets$doy <- as.numeric(format(tickets$tdate, "%j"))

# get length
st_len <- read.csv("processedData/vessel_lengths.csv")
tickets <- left_join(tickets, st_len, by = 'drvid')

# are 12 trips that are poorly categorized (same trip_id but different states and different days of the year. Drop for now.)

bad_trip <- tickets %>% select(trip_id, lat) %>% distinct() %>% group_by(trip_id) %>% summarize(n_lat = length(unique(lat)))
drp_trips <- bad_trip$trip_id[which(bad_trip$n_lat>1)]

# summarize!

t1a <- tickets %>% 
  filter(!(trip_id %in% drp_trips)) %>%
  group_by(metier.2010, trip_id, modified, lat, length, doy) %>% 
  summarize(rev = sum(adj_revenue, na.rm=T)) %>%
  group_by(metier.2010, trip_id, lat, length,doy) %>%
  mutate(percent_rev = rev/sum(rev), 
         multsp = ifelse(length(which(percent_rev>.1))>1, 1, 0)) %>%
  group_by(metier.2010, trip_id) %>%
  summarize(rev = sum(rev), lat = unique(lat), length = unique(length),
            doy = unique(doy), multsp = unique(multsp)) %>%
  ungroup()

# to find top fisheries by revenue
t1b <- t1a %>%
  group_by(metier.2010) %>%
  summarize(rev = sum(rev), 
            percent_multsp = sum(multsp)/length(trip_id), 
            min_doy = min(doy, na.rm=T), max_doy = max(doy, na.rm=T),
            min_lat = min(lat,na.rm=T), max_lat = max(lat, na.rm=T),
            min_len = min(length, na.rm=T), max_len = max(length, na.rm=T)) %>%
  arrange(-rev)

t1b$cumulative_rev = cumsum(t1b$rev/sum(t1b$rev))

# function to find ranges ---
find_stats <- function(metier, min_lat=NA, max_lat=NA, min_len=NA, 
                       max_len=NA, min_doy=NA, max_doy=NA, df = t1a){
  rel_df <- df %>% filter(metier.2010 == metier)
  
  # getting latitude
  if(!is.na(min_lat)){
    lat_range = with(rel_df, length(which(lat>=min_lat & lat <=max_lat))/nrow(rel_df))
  }else{
    lat_range = quantile(rel_df$lat, c(0.025, 0.975), na.rm=T)
  }
  
  # getting multspecies
  multsp = length(which(rel_df$multsp==1))/nrow(rel_df)
  
  # getting day range
  if(!is.na(min_doy)){
    doy_range = with(rel_df, length(which(doy>=min_doy & doy <=max_doy))/nrow(rel_df))
  }else{
    doy_range = quantile(rel_df$doy, c(0.025, 0.975), na.rm=T)
  }
  
  # getting length range
  if(!is.na(min_len)){
    len_range = with(rel_df, length(which(length>=min_len & length<=max_len))/nrow(rel_df))
  }else{
    len_range = quantile(rel_df$length, c(0.025, 0.975), na.rm=T)
  }
  
  return(list(lat = round(lat_range,1), multsp = round(multsp,3), 
              doy = doy_range, len = len_range))
}


find_stats(metier="TWL_1", min_lat = 35.4, max_lat = 49, min_len = 35, 
           max_len = 95)
find_stats(metier="TWS_1", min_lat = 35.8, max_lat = 49,min_doy = 91, 
           max_doy = 305, min_len = 38, max_len = 105)

find_stats(metier="TWL_2", min_lat = 34.05, max_lat = 37.4, min_len = 29, max_len = 71)

find_stats(metier="POT_1")
find_stats(metier="NET_2")
find_stats(metier="TLS_2")
find_stats(metier="DRG_1")
find_stats(metier="HKL_1")
find_stats(metier="TLS_1")
find_stats(metier="NET_3")
find_stats(metier="POT_2")
find_stats(metier="POT_1")
find_stats(metier="POT_5")
find_stats(metier="NET_1")
find_stats(metier="POT_4")
find_stats(metier="HKL_12")

# examine fisheries with winter seasons by eye
t1a %>% filter(metier.2010=="POT_1") %>% ggplot(aes(x=doy)) + geom_histogram()
t1a %>% filter(metier.2010=="NET_2") %>% ggplot(aes(x=doy)) + geom_histogram()
