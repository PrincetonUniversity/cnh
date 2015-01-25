# forage fish for Jameal

# following vessel headers
# Vessel
# Year
# Species 
# Proportion of landings 
# Proportion of revenue 
# Simpsons diversity 
# Effective connectance 
# Total annual landings 
# Total annual revenue

spid <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/data/spid.csv",stringsAsFactors=FALSE)

# Whitebait smelt (Allosmerus elongatus), Surf smelt (Hypomesus pretiosus), no species level
# so is just unspecified 

# also some UNSP. SQUID in tickets, want that? also unsp. mackerel, want that? and atka mackerel

forage <- c("SMLT", "PHRG","SARY","NANC","MSQD","PWHT","CMCK","JMCK","PSDN")

forage_table <- subset(spid, SPID %in% forage,select=-c(X,complex, complex2,complex3,complex4))
forage_table <- as.data.frame(sapply(forage_table, tolower)) 

# by vessel, by year, by forage species, find total amount landed, find total amount of revenue,
library(plyr)
tickets <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2015-01-09/code/3_exploreBuildwebs/tickets.RDS")

# find forage trips
forage_tickets <- subset(tickets, spid %in% forage)

catches <- ddply(forage_tickets, .(drvid, year, spid), summarize, lbs = sum(round_wt), dollars= sum(adj_revenue))

# find those vessels total landings per year
f_trips <- unique(forage_tickets$drvid)

all_trips <- subset(tickets, drvid %in% f_trips)
total_catch <- ddply(all_trips, .(drvid, year), summarize, total_lbs = sum(round_wt))

merged_catches <- merge(catches, total_catch, by = c("drvid","year"), all.x=TRUE,all.y=FALSE)

# merge with diversity to find vessel's yearly diversity, total amount of revenue and landings, get proportions
yrdf <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2015-01-13/yrdf.RDS")

merged_df <- merge(merged_catches, yrdf[[1]][,c("drvid","year","yr_revenue","simpsons")],by=c("drvid","year"),all.x=TRUE)

merged_df$prop_lb = merged_df$lbs/merged_df$total_lbs
merged_df$prop_dollars = merged_df$dollars/merged_df$yr_revenue

write.csv(merged_df, "/Users/efuller/1/CNH/Analysis/forage_landings/forage_boats.csv")
write.csv(forage_table,"/Users/efuller/1/CNH/Analysis/forage_landings/spid_interest.csv")