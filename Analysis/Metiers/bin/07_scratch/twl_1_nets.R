# are boats that drop TWL_1 but don't add. Are they in less diverse networks?

len_before$no_adds <- ifelse(len_before$n_new == 0, 1, 0)
boxplot(edge_density ~ no_adds, subset(len_before, after == 0))

boxplot(edge_density ~ no_adds, subset(len_before, after == 0 & hake == 0 ))
with(subset(len_before, after == 0), table(no_adds, max_port))
     
# don't seem to be focused in any particular network. something at vessel level?

boxplot(eff.shannon_2010 ~ no_adds, subset(len_before, after == 0 & hake == 0))
t.test(eff.shannon_2010 ~ no_adds, subset(len_before, after == 0 & hake == 0))
# maybe those that are adding are less diverse boats. 

# what percentage of revenue was due to TWL_1 before?
percent_before <- subset(dat, drvid %in% len_before$drvid) %>%
  filter(year < 2011) %>%
  group_by(drvid, metier.2010) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T)) %>%
  group_by(drvid) %>%
  mutate(percent.rev = revenue/sum(revenue)) %>%
  filter(metier.2010 == "TWL_1")

len_before <- left_join(len_before, percent_before)
boxplot(percent.rev ~ no_adds, subset(len_before, after == 0 & hake == 0))
# those that didn't add had a lower percentage of revenue 

boxplot(percent.rev ~ after, subset(len_before, hake == 0))
# those that stayed in were getting much more from trawl anyway