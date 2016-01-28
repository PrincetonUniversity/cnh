# comparing Dan's analysis to my own
d10 <- readRDS("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/tickets_2010.RDS")

# species groups ----
sp <- read.csv("/Users/efuller/Downloads/woc_species_group_lookup_table4.csv", stringsAsFactors = FALSE)
library(dplyr)
d10 <- left_join(d10, sp)

# interested to see agreement between their gear groups and my metiers
library(bipartite)
plotweb(table(d10$metier,d10$specgrp2))
with(subset(d10, grgroup == "POT"), plotweb(table(metier,specgrp2)))
with(subset(d10, grgroup == "TLS"), plotweb(table(metier,specgrp2)))
with(subset(d10, grgroup == "TWL"), plotweb(table(metier,specgrp2)))
with(subset(d10, grgroup == "NET"), plotweb(table(metier,specgrp2)))
with(subset(d10, grgroup == "MSC"), plotweb(table(metier,specgrp2)))
with(subset(d10, grgroup == "HKL"), plotweb(table(metier,specgrp2)))

# find which vessels are in Dan's sample
d.boats <- read.csv("/Users/efuller/1/CNH/Analysis/metiers/Holland_Kasperski/Emma GF Trawl Fleet .csv", stringsAsFactors = FALSE)
d.boats <- rename(d.boats, drvid = VESSEL_NUM)
d10$present = ifelse(d10$drvid %in% d.boats$drvid, 1, 0)

# calculate dan's diversity 
library(vegan)
dk_div <- subset(d10, present == 1) %>%
  group_by(drvid, year, specgrp2) %>%
  summarize(dk_revenue = sum(adj_revenue, na.rm = T)) %>%
  group_by(drvid, year) %>%
  summarize(dk_ESI = exp(diversity(dk_revenue, index = 'shannon')))

# calculate my diversity
both_div <- subset(d10, present ==1 ) %>%
  group_by(drvid, year, metier) %>%
  summarize(ef_revenue = sum(adj_revenue, na.rm = T)) %>%
  group_by(drvid, year) %>%
  summarize(ef_ESI = exp(diversity(ef_revenue, index = 'shannon'))) %>%
  left_join(dk_div)

library(ggplot2)
ggplot(both_div, aes(x = ef_ESI, y= dk_ESI)) + geom_abline(a = 0, b = 1, lty = 1, size =1.5, color = 'grey' ) + geom_point() + theme_classic() 

# so generally looks good, but some that I find have 0 diversity that dk_ESI find more. Which are these?



# sidebar, using this method, do i still see the effect? - yep! ----
group_div <- subset(d10, year!=2011) %>%
  mutate(post = ifelse(year>2011, 1, 0)) %>%
  group_by(drvid, post, metier) %>%
  summarize(met_rev = sum(adj_revenue, na.rm = T)) %>%
  group_by(drvid, post) %>%
  summarize(ESI = exp(diversity(met_rev, index = 'shannon')))

# have to take out boats that are not present for before and after
group_div <- subset(group_div, !(drvid %in% names(which(table(group_div$drvid)<2))))
  
group_div <- group_div %>%
  group_by(drvid) %>%
  mutate(delta_esi = diff(ESI)) %>%
  filter(post == 0)

ifq_yes <- unique(d10$drvid[which(d10$ifq_landing=="Y")])
group_div$ifq <- ifelse(group_div$drvid %in% ifq_yes, 1, 0)
group_div$ifq <- as.factor(group_div$ifq)

hake_yes <- unique(d10$drvid[which(d10$metier=="TWL_4" & d10$modified=="PWHT")])
group_div$hake <- ifelse(group_div$drvid %in% hake_yes, 1, 0)
group_div$hake <- as.factor(group_div$hake)

ggplot(subset(group_div, hake == 0), aes(x = ifq, y = delta_esi)) + geom_boxplot()

summary(lm(delta_esi ~ ESI + ifq, group_div))
# ok back to comparisons - investigate the weird ef_ESI = 1 and dk_ESI > 1 ----
head(both_div[(which(both_div$ef_ESI==1 & both_div$dk_ESI > 1)),])

# how many vessels?
length(unique(both_div$drvid[(which(both_div$ef_ESI==1 & both_div$dk_ESI > 1))]))
hist(table(both_div$drvid[(which(both_div$ef_ESI==1 & both_div$dk_ESI > 1))]),col = 'grey')

# look at 2 vessels that are present for each year
names_to_look <- names(which(table(both_div$drvid[(which(both_div$ef_ESI==1 & both_div$dk_ESI > 1))])==5))

checkout <- subset(d10, drvid == names_to_look[1]) %>%
  select(trip_id, metier, specgrp2) %>%
  distinct()

# so relative frequencies a bit messed up
plotweb(table(checkout$metier, checkout$specgrp2))
# example of a twl_1 boat that is found to do more species

checkout <- subset(d10, drvid == names_to_look[2]) %>%
  select(trip_id, metier, specgrp2) %>%
  distinct()

# so relative frequencies a bit messed up
plotweb(table(checkout$metier, checkout$specgrp2))
# same thing

names_to_look <- names(which(table(both_div$drvid[(which(both_div$ef_ESI==1 & both_div$dk_ESI > 1))])==4))

checkout <- subset(d10, drvid == names_to_look[1]) %>%
  select(trip_id, metier, specgrp2) %>%
  distinct()

# so relative frequencies a bit messed up
plotweb(table(checkout$metier, checkout$specgrp2))
# again the TWL_1 thing. 

# makes me think that holland and kasperski method might end up combining effects of species diversity within a trip and fishery diversity. for my results, you see people landing the same metier, but different combinations of species

# what about using their measure of diversity, do i still see effects ----
dk_div <- subset(d10, year!=2011) %>%
  mutate(post = ifelse(year>2011, 1, 0)) %>%
  group_by(drvid, post, specgrp2) %>%
  summarize(group_rev = sum(adj_revenue, na.rm = T)) %>%
  group_by(drvid, post) %>%
  summarize(ESI = exp(diversity(group_rev, index = 'shannon')))

# have to take out boats that are not present for before and after
dk_div <- subset(dk_div, !(drvid %in% names(which(table(dk_div$drvid)<2))))

dk_div <- dk_div %>%
  group_by(drvid) %>%
  mutate(delta_esi = diff(ESI)) %>%
  filter(post == 0)

ifq_yes <- unique(d10$drvid[which(d10$ifq_landing=="Y")])
dk_div$ifq <- ifelse(dk_div$drvid %in% ifq_yes, 1, 0)
dk_div$ifq <- as.factor(dk_div$ifq)

ggplot(dk_div, aes(x = ifq, y = delta_esi)) + geom_boxplot()

summary(lm(delta_esi ~ ESI + ifq, dk_div))
# hm, also get increase in ifq using their approach. 
# what about not controlling for pre diversity
summary(lm(delta_esi ~ ifq, dk_div)) # oh very interesting. it reverses. this is because IFQ vessels are more diversified anyway. 
