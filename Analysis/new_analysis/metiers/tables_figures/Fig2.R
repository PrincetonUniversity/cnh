# Figure 2
# make matrix where columns are fishery, rows are species. Do only top 15 fisheries (90% of revenue)
library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan)

tickets <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")
#tickets <- tickets %>% filter(drvid !="NONE") # don't drop dredges

# fig2b ----
# to find top fisheries by revenue
t1b <- tickets %>%
  group_by(metier.2010) %>%
  summarize(rev = sum(adj_revenue)) %>%
  ungroup() %>%
  arrange(-rev)

t1b$cumulative = cumsum(t1b$rev)/sum(t1b$rev)
top_90 <- t1b[1:15,] # lazy, based on visual inspection

# make table of common names
spid <- read.csv("processedData/catch/spid.csv", stringsAsFactors = FALSE)
spid$X <- NULL
spid <- rename(spid, modified = SPID)
spid$common_name <- tolower(spid$common_name)

dscrp <- read.csv('processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv',
                  stringsAsFactors = FALSE)
dscrp$name <- paste(dscrp$Major_species, dscrp$Major_gear)
dscrp <- rename(dscrp, metier.2010 = Metier)

sp_table <- tickets %>% 
  filter(metier.2010 %in% top_90$metier.2010) %>%
  group_by(metier.2010, modified) %>%
  summarize(rev = sum(adj_revenue)) %>%
  group_by(metier.2010) %>%
  mutate(percent_rev=rev/sum(rev)) %>%
  filter(percent_rev>.05) %>%
  left_join(dscrp[,c("metier.2010", "name")], by = 'metier.2010') %>%
  left_join(spid[,c("modified", "common_name")], by = 'modified') %>%
  ungroup() %>%
  select(-rev, -modified, -metier.2010) %>%
  spread(name, percent_rev, fill = 0)
sp_table <- as.data.frame(sp_table)
rownames(sp_table) = sp_table$common_name
sp_table$common_name <- NULL

plotree <- hclust(vegdist(sp_table), "average")
## Automatic reordering of clusters
png(filename = "Analysis/new_analysis/metiers/tables_figures/fig2b.png", 
    height = 500, width = 500)
tabasco(sp_table, plotree, col = gray(level=seq(0,1,.001)), margins = c(20,20))
dev.off()

# fig2a -----
# total revenue v. number of trips
fig2a <- tickets %>% group_by(metier.2010) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T),
            n_trips = length(unique(trip_id))) %>%
  ggplot(aes(x = n_trips, y = revenue)) + geom_point() +
  scale_x_log10() + scale_y_log10() + theme_classic() +
  xlab("Number of trips") + ylab("total revenue") +
  theme(axis.line.x = element_line(color='black', size=.75),
        axis.line.y = element_line(color='black', size=.75)) 
  
ggsave(fig2a, filename = "Analysis/new_analysis/metiers/tables_figures/fig2a.png",
       width = 4, height = 4, units = 'in', dpi = 300, device = 'png')
