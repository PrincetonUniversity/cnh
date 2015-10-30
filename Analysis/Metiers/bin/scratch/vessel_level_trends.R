# what about length distribution of TWL_1 before and after catch shares?

len_before <- subset(dat, drvid %in% unique(div_dat$drvid)) %>%
  filter(year < 2011, metier.2010 == "TWL_1") %>%
  dplyr::select(drvid, year, adj_revenue) %>%
  group_by(drvid) %>%
  summarize(rev = sum(adj_revenue, na.rm = T)) %>%
  left_join(dplyr::select(div_landings, drvid, len, lat, hake, 
                          single.fishery_2010, delta.shannon_2010, 
                          eff.shannon_2010))

len_after <- subset(dat, drvid %in% unique(div_dat$drvid)) %>%
  filter(year > 2011, metier.2010 == "TWL_1") %>%
  dplyr::select(drvid, year, adj_revenue) %>%
  group_by(drvid) %>%
  summarize(rev = sum(adj_revenue, na.rm = T))


# big drop in the number of boats landing after catch shares, goes from 113 to 69. 
# doesn't seem to be a length shift. but is there a latitudinal one?

# find average latitude of landing prior to catch shares
len_before$after <- 0
len_before$after[which(len_before$drvid %in% len_after$drvid)] <-  1

boxplot(rev ~ after, subset(len_before, hake ==0 & single.fishery_2010==0)) # low income boats dropped
boxplot(len ~ after, subset(len_before, hake == 0 & single.fishery_2010==0)) # low income boats dropped

lm4 <- glm(after ~ len, subset(len_before, hake==0 & single.fishery_2010==0), family = "binomial") 
lm5 <- glm(after ~ len + rev, subset(len_before, hake==0 & single.fishery_2010==0), family = "binomial") 

plot(rev ~ len, subset(len_before, hake==0 & single.fishery_2010==0))
lm6 <- lm(rev ~ len, subset(len_before, hake==0 & single.fishery_2010==0))
abline(lm6)

boxplot(eff.shannon_2010 ~ after, subset(len_before, hake == 0 & single.fishery_2010==0))
# much less diverse trawlers after ITQs, yet ITQs increase trawlers diversity. 
# so see if trawlers diversity increases if participate

boxplot(delta.shannon_2010 ~ after,  subset(len_before, hake == 0 & single.fishery_2010==0))
t.test(delta.shannon_2010 ~ after,  subset(len_before, hake == 0 & single.fishery_2010==0))

plot(eff.shannon_2010 ~ len, subset(len_before, hake == 0 & single.fishery_2010==0))
abline(lm(eff.shannon_2010 ~ len, subset(len_before, hake == 0 & single.fishery_2010==0)))
plot(eff.shannon_2010 ~ len, len_before)

lm7 <- glm(after ~ len + eff.shannon_2010, 
           subset(len_before, hake == 0 & single.fishery_2010==0),
           family = "binomial")
library(arm)
display(lm(eff.shannon_2010 ~ len, subset(len_before, hake == 0 & single.fishery_2010==0)))

lm8 <- glm(after ~ eff.shannon_2010, 
           subset(len_before, hake == 0 & single.fishery_2010==0),
           family = "binomial")

# what it looks like is that small trawlers left, and more diverse boats left. 
# but those trawlers that stayed got more diverse. 

boxplot(lat ~ after, subset(len_before, hake == 0 & single.fishery_2010==0))
# doesn't look like a huge latitudinal shift. 

# relationships between diversity and latitude?
plot(eff.shannon_2010 ~ lat, subset(len_before, hake == 0 & single.fishery_2010==0))
# maybe mild - but really none. 
display(lm(eff.shannon_2010 ~ lat, subset(len_before, hake == 0 & single.fishery_2010==0)))

# what about length
plot(len ~ lat, subset(len_before, hake == 0 & single.fishery_2010==0))
display(lm(len~ lat, subset(len_before, hake == 0 & single.fishery_2010==0)))
# maybe as you go north you get bigger boats, but mild. not significant and low R2
summary(lm(len~ lat, subset(len_before, hake == 0 & single.fishery_2010==0)))

with(subset(len_before, hake == 0 & single.fishery_2010==0 & after ==0), plot(density(eff.shannon_2010), col='grey', lwd = 3))
with(subset(len_before, hake == 0 & single.fishery_2010==0 & after == 1), lines(density(eff.shannon_2010), col='steelblue',lwd=3))

# did those small trawlers compensate in effort after switching out of ITQs?
# make same amount of money?
library(tidyr)
rev_compare <-  subset(div_dat, drvid %in% len_before$drvid) %>%
  group_by(drvid, post.itq) %>%
  summarize(total.rev = sum(annual_revenue)) %>%
  group_by(drvid) %>%
  summarize(delta.revenue = diff(total.rev))

len_before <- merge(len_before, rev_compare)

with(subset(len_before, hake == 0 & single.fishery_2010==0 & after ==0), hist(delta.revenue))
with(subset(len_before, hake == 0 & single.fishery_2010==0 & after ==0), t.test(delta.revenue))

# looks like revenue is pretty much the same before and after. maybe slightly increased?

with(subset(len_before, hake == 0 & single.fishery_2010==0 & after ==1), hist(delta.revenue))
with(subset(len_before, hake == 0 & single.fishery_2010==0 & after ==1), t.test(delta.revenue))
# but revenue definitely increased (on average) for boats in ITQs. 

# so seems like boats didn't leave fishery, so must have reallocated effort to non
# groundfish trawl fishery. so the question is, can you predict where they will go?
# seems like past history of fishing will be predictive. and that the only way the
# network would matter, was if they entered a totally new fishery from what they
# had done previously. If that was the case, I'd predict that it was one it was
# connected to the fishery it was previously in. 

# so to look at this, did any vessels do a new fishery? so find the fisheries 
# before and after for each vessel that exited TWL_1, anything in the post but
# not in the prior?

new_fish <- subset(dat, drvid %in% len_before$drvid[which(len_before$after==0)]) %>%
  filter(year!=2011) %>%
  dplyr::select(drvid, metier.2010, year) %>%
  distinct() %>%
  mutate(post.itq = ifelse(year >2011, 1, 0)) %>%
  dplyr::select(drvid, metier.2010, post.itq) %>%
  distinct()

boats <- unique(new_fish$drvid)
new_entries <- data.frame(drvid = boats, stringsAsFactors = FALSE)
new_entries$any_new <- 0
for(i in 1:length(boats)){
  before_fisheries <- subset(new_fish, drvid == boats[i] & post.itq==0)
  after_fisheries <- subset(new_fish, drvid == boats[i] & post.itq==1)
  new_entries$any_new[i] <- ifelse(any(!(after_fisheries$metier.2010 %in% before_fisheries$metier.2010)),length(which(!(after_fisheries$metier.2010 %in% before_fisheries$metier.2010))),0)
}

# about half had a new fishery. some are likely bycatch (like pot 13). 
# most entered one new fishery, 6 entered >2. 
# look at one that entered 4

i <- which(new_entries$any_new==3)

# so would predict that those new entries are fisheries closely connected. and that the ability to enter new fisheries is related to your majority port network. 

# so would be multi-nomial regression: prob of going into each of those fisheries is based on strength of connection to each of those fisheries at your home port. 
# so for each boat's 2010 fisheries, what's the shortest path(?)  between all other fisheries. woudl predict that shortest path is negatively related to probability of adding it to portfolio. 
# not totally sure how to formulate this. 

# and would be probability of entering new fishery would be a function of your centrality/flexibility of port network. 
# so for each boat, logit model, probability of adding new fishery is positively related to networks centrality/flexibility. 
# so for each boat find it's majority port. then make the network for that port prior to 2010 (just with subset data - should try with all data too at some point). 
# Then calculate statistics. Then fit model. 



# hm, did people who left groundfish add fisheries more than those that stayed in fisheries? or what's the average turnover in fisheries across?

pairwise_bx <- function(x){
  mat <- as.matrix(dplyr::select(spread(x, metier.2010, revenue), -post.itq, -drvid))
  mat[which(is.na(mat))] <- 0
  return(as.vector(as.numeric(vegdist(mat, method = "bray"))))
}

# want to compute pair wise BC between vessel comp before and after
pairwise_bc <- subset(dat, drvid %in% unique(div_dat$drvid) & year!=2011) %>%
  mutate(post.itq = ifelse(year %in% c(2012, 2013), 1, 0)) %>%
  group_by(drvid, post.itq, metier.2010) %>%
  summarize(revenue = sum(adj_revenue))

# bc i cannot figure out how to do this in dplyr
ids <- data.frame(drvid = unique(pairwise_bc$drvid), stringsAsFactors = FALSE)
ids$bc <- NA

for(i in 1:nrow(ids)){
  subs <- subset(pairwise_bc, drvid == ids$drvid[i])
  ids$bc[i] <- pairwise_bx(subs)
}

# link to before_len
len_before <- left_join(len_before, ids)

boxplot(bc ~ after, len_before, xlim = c(.5,3.5), ylim = c(0,1)) # so boats that leave TWL_1 are more different (BC is bigger)

# what about other boats?
with(subset(ids, !(drvid %in% len_before$drvid)), boxplot(at = 3, bc,add=T,names = "rest of fleet"))

# number of fisheries added? - redo new entries to count all this stuff, and extend to entire fleet
new_fish <- subset(dat, drvid %in% div_dat$drvid) %>%
  filter(year!=2011) %>%
  dplyr::select(drvid, metier.2010, year) %>%
  distinct() %>%
  mutate(post.itq = ifelse(year >2011, 1, 0)) %>%
  dplyr::select(drvid, metier.2010, post.itq) %>%
  distinct()

boats <- unique(new_fish$drvid)
new_entries <- data.frame(drvid = boats, stringsAsFactors = FALSE)
new_entries$n_new <- 0; new_entries$n_lost <- 0; 
new_entries$n.before <- 0; new_entries$n.after <- 0
for(i in 1:length(boats)){
  before_fisheries <- unique(subset(new_fish, drvid == boats[i] & post.itq==0)$metier.2010)
  after_fisheries <- unique(subset(new_fish, drvid == boats[i] & post.itq==1)$metier.2010)
  new_entries$n_new[i] <- length(which(!(after_fisheries %in% before_fisheries)))
  new_entries$n_lost[i] <- length(which(!(before_fisheries %in% after_fisheries)))
  new_entries$n.before[i] <- length(before_fisheries)
  new_entries$n.after[i] <- length(after_fisheries)
}

# add the len_before data
foo <- left_join(new_entries, len_before)
foo$after <- foo$after+1 # to index properly. so after = 2 is itq, after = 1 is twl_1 but no itq, after = 0 is rest of boats
foo$after[is.na(foo$after)] <- 0
boxplot(n_new ~ after, subset(foo,after %in% c(1,2)))

boxplot(n_lost ~ after, subset(foo,after %in% c(1,2)))

foo$delta.fisheries <- foo$n.before-foo$n.after

boxplot(delta.fisheries ~ after, subset(foo,after %in% c(1,2))) # 
boxplot(delta.fisheries ~ after, foo) 
t.test(delta.fisheries ~ after, subset(foo,after %in% c(1,2))) # 
# non ifq trawl boats added more fisheries than ifq

t.test(delta.fisheries ~ after, subset(foo,after %in% c(0,1))) # 
# and non ifq boats added more fisheries than rest of fleet. 

# so question is: can we predict what fisheries these boats added?

# figure out port breakdown for each of these boats in len_before set

port_break <- list()
for(i in 1:nrow(len_before)){
  port_break[[i]] <- subset(dat, drvid == len_before$drvid[i] & year < 2011) %>%
    group_by(pcid) %>%
    summarize(revenue = sum(adj_revenue)) %>%
    mutate(per = round(revenue/sum(revenue), 4)*100)
}

maxes <- sapply(port_break, function(x) max(x$per))
hist(maxes)
length(which(maxes<=50)) # 2
maxes[maxes<=50] # practically 50. so will use majority ports. could also use this as explanatory variable for how well prediction works.

len_before$max_port <- sapply(port_break, function(x) x$pcid[which.max(x$per)])
len_before$max_percent <-sapply(port_break, function(x) max(x$per))

boxplot(max_percent ~ after, subset(len_before,hake==0)) # TWL_1 had more ports
# something about spatial diversity versus fishery diversity? 
# would expect them maybe to be negatively related. do fewer things, go to more ports?

# calculate port diversity (revenue distribution across fisheries)
port_div <- subset(dat, drvid %in% div_dat$drvid) %>%
  filter(year < 2011) %>%
  group_by(drvid, pcid) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T)) %>%
  group_by(drvid) %>%
  summarize(ESI_port = exp(diversity(revenue, index = 'shannon'))) %>%
  left_join(div_landings)

with( subset(port_div, hake == 0), plot(eff.shannon_2010, ESI_port, log="xy", pch = 19, cex = .5))

with( subset(port_div, hake == 0 & drvid %in% len_before$drvid), plot(eff.shannon_2010, ESI_port, pch = 19, cex = .5))
# no relationship really. wonder if it's because low diversity, at some level, probably collates those boats that are very successful and those that are very not successful. so would like to
# maybe standardize for revenue (but that also relates to length). so want those boats that are most successful for their size

lm_port <- lm(ESI_port ~ eff.shannon_2010 + len + annual.revenue, subset(port_div, hake == 0))
# so these things are significant, but effects very small. not really an obvious story here. R2 very low too. 

# ok so going back to len_before, make participation plot. want to see if we can predict if a vessel added a new fishery after ITQs based on p
# so about half of the non-ifq boats added at least one new fishery. 

len_before <- left_join(len_before, new_entries)
len_before <- left_join(len_before, port_div)

boxplot(ESI_port ~ after, subset(len_before,hake==0))

library(bipartite)
# for a given port, need to make bipartite web, weighted network by percentage of revenue from each fishery
max_ports <- unique(len_before$max_port)
for(i in 1:length(max_ports)){
  port_web <- subset(dat, drvid %in% div_dat$drvid & pcid == max_ports[i] & year < 2011) %>%
    group_by(drvid, metier.2010) %>%
    summarize(revenue = sum(adj_revenue, na.rm = T)) %>%
    group_by(drvid) %>%
    mutate(percent.revenue = revenue/sum(revenue)) %>%
    dplyr::select(-revenue) 
  
  bar <- graph_from_edgelist(as.matrix(port_web[,c(1,2)]), directed = FALSE)
  E(bar)$weight <- 1-port_web$percent.revenue # because weight is distances, so closer to one, closer it is 
  plot(bar)
  
  # find port boats 
  port_boats <- len_before$drvid[which(len_before$max_port == max_ports[i] & len_before$n_new > 0)]
  for(j in 1:length(port_boats)){
    ves_id <- port_boats[j]
    fish_in <- unique(dat$metier.2010[which(dat$drvid==ves_id & dat$year < 2011)])
    fish_amount <- subset(dat, drvid == ves_id) %>%
      filter(year < 2011) %>%
      group_by(metier.2010) %>%
      summarize(revenue = sum(adj_revenue, na.rm = T)) %>%
      mutate(percent_revenue = revenue/sum(revenue))
    
    v_id <- which(V(bar)$name==ves_id) # choose a boat
    dp <- as.data.frame(distances(bar, v = v_id, to = grep("_",V(bar)$name), mode = "in", weights = E(bar)$weight))
    # (but would have to be one not already in)
    # drop others that in
    dp <- dp[,-which(colnames(dp) %in% fish_in)]
    pred_fish <- names(dp)[which.min(dp)]
    
    # actual fish 
    before_fisheries <- unique(subset(new_fish, drvid ==ves_id & post.itq==0)$metier.2010)
    after_fisheries <- unique(subset(new_fish, drvid == ves_id & post.itq==1)$metier.2010)
    actual_fish = after_fisheries[which(!(after_fisheries %in% before_fisheries))]
    
    rand_fish <- names(dp)[sample(1:length(dp),size = 1)]
    
    cat("ves ",j,": pred -",any(actual_fish %in% pred_fish), actual_fish,": ", pred_fish," rand - ",any(actual_fish %in% rand_fish), "\n")
  }
  cat("port ", i,": \n")
}

# bigboat v. small boat

# alternatively, look at whether boats add new fisheries as a function of the network property?
# or diversity of vessels for whether they added or not new fisheries?

