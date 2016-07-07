# Figure 3

library(dplyr)
library(ggplot2)
library(tidyr)

tickets <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")

# get common names
dscrp <- read.csv('processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv',
                  stringsAsFactors = FALSE)
dscrp$name <- paste(dscrp$Major_species, dscrp$Major_gear)
dscrp <- rename(dscrp, metier.2010 = Metier)

tickets <- left_join(tickets, dscrp[,c("metier.2010","name")])

# figure 3a ----
tickets$tdate <- as.Date(tickets$tdate, format = "%d-%b-%y")
tickets$doy <- as.numeric(format(tickets$tdate, "%j"))

# subset to black rockfish, dungness crab, groundfish trawl, chinook troll, 
# albacore troll, market squid, halibut

fig3a <- tickets %>% 
  filter(metier.2010 %in% c("POT_1","HKL_2","TWL_1","NET_2",
                            "TLS_1","TLS_2","HKL_12", "TWS_1")) %>%
  group_by(name, metier.2010, tdate) %>%
  summarize(n_trips = length(unique(trip_id))) %>%
  ggplot(aes(x=tdate, y = n_trips)) + geom_bar(stat='identity') +
  facet_wrap(~name, scale='free', ncol = 1) + theme_classic() + 
  theme(axis.line.x=element_line(size=.5, color='black'),
        axis.line.y=element_line(size=.5, color='black'),
        strip.background = element_rect(size = 0)) + 
  xlab("date landed") + ylab("number of trips")

ggsave(fig3a, filename='Analysis/new_analysis/metiers/tables_figures/fig3a.png',
       height = 10, width = 5, dpi = 300)

# figure 3b ----
# look at spiny lobster versus pot1
all_ports <- read.csv("processedData/spatial/ports/all_ports.csv", stringsAsFactors = FALSE) %>%
  rename(pcid=Pcid)


tickets <- tickets %>% left_join(all_ports[,c("pcid","lat")], by = 'pcid')
tickets$bin_lat <- cut(tickets$lat, breaks = seq(32,49, .2))


fig3b <- tickets  %>%
  filter(metier.2010 %in% c("POT_1", "POT_2"), !is.na(lat)) %>%
  group_by(trip_id,bin_lat, name) %>%
  summarize(rev = sum(adj_revenue)) %>%
  ggplot(aes(x = bin_lat, y = rev, fill =name)) + geom_bar(stat='identity') +
  scale_fill_manual(values=c("grey20", "grey80")) + 
  theme_classic() + coord_flip() + xlab("latitude") +
  ylab("revenue ($)") +  scale_y_reverse() + 
  theme(axis.line.x=element_line(color='black', size = .5),
        axis.line.y = element_line(color='black', size =.5),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.title.y=element_blank())

ggsave(fig3b, filename='Analysis/new_analysis/metiers/tables_figures/fig3b.png',
       height = 10, width = 5, dpi = 300)

load("processedData/spatial/2_coastline.Rdata")
png(filename="Analysis/new_analysis/metiers/tables_figures/fig3b_map.png", 
   height = 960, width = 400)
par(mai=rep(0,4), bg='transparent')
plot(WC, col='black')
dev.off()
