# uses av_dens done on all tickets to make comparison

holland.boat <- read.csv("/Users/efuller/Desktop/Emma GF Trawl Fleet Diversificaiton.csv", stringsAsFactors = FALSE, row.names = NULL)

length(which(unique(holland.boat$drvid) %in% unique(ftl$drvid)))/length(unique(holland.boat$drvid))

length(which(unique(holland.boat$drvid) %in% av_dens$drvid)) 

write.csv(data.frame(drvid = unique(holland.boat$drvid)[which(!(unique(holland.boat$drvid) %in% unique(ftl$drvid)))]), file = "/Users/efuller/Desktop/missing_drvids.csv", row.names = FALSE)


holland.boat$drvid <- as.character(holland.boat$drvid)
sub.data <- left_join(av_dens, holland.boat)
sub.data <- subset(sub.data, !is.na(HHI))

# calculate average diversity before and after catch shares using dan's diversity values
sub.data <- av_dens %>%
  left_join(holland.boat) %>%
  filter(!is.na(HHI) & Year != 2011) %>%
  mutate(post.ifq = ifelse(Year>2011, 1, 0)) %>%
  group_by(drvid, post.ifq) %>%
  summarize(pre.simpsons = unique(pre.simpsons), 
            pre.shannons = unique(pre.shannons), 
            delta.simpsons = unique(delta.simpsons), 
            delta.shannons = unique(delta.shannons), 
            ifq = unique(ifq), hake = unique(hake), 
            single_fishery = unique(single_fishery), 
            mean.hhi = mean(HHI, na.rm = T), 
            mean.SI = mean(SI, na.rm = T), 
            mean.ESI = mean(ESI, na.rm = T), holland.ITQ = unique(IFQ)) %>%
  group_by(drvid) %>%
  mutate(delta.hhi = diff(mean.hhi), delta.SI = diff(mean.SI), delta.ESI = diff(mean.ESI)) %>%
  filter(post.ifq == 0)

  ggplot(sub.data, aes(x = delta.simpsons, y = delta.ESI)) + geom_point(aes(color = ifq), size = 3) + theme_classic()
  ggplot(sub.data, aes(x = delta.simpsons, y = delta.SI)) + geom_point(aes(color = ifq), size = 3) + theme_classic()
  ggplot(sub.data, aes(x = delta.simpsons, y = delta.hhi)) + geom_point(aes(color = ifq), size = 3) + theme_classic()
  
  ggplot(subset(sub.data, hake == 0), aes(x = delta.shannons, y = delta.ESI)) + geom_point(aes(color = ifq), size = 3) + theme_classic()
  ggplot(subset(sub.data, hake == 0), aes(x = delta.shannons, y = delta.SI)) + geom_point(aes(color = ifq), size = 3) + theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
  ggplot(subset(sub.data, hake == 0), aes(x = delta.shannons, y = delta.hhi)) + geom_point(aes(color = ifq), size = 3) + theme_classic()
  
  ggplot(sub.data, aes(x = delta.shannons, y = delta.hhi)) + geom_point(aes(color = ifq), size = 3) + theme_classic() + facet_grid(~ifq) 
  ggplot(sub.data, aes(x = delta.shannons, y = delta.SI)) + geom_point(aes(color = ifq), size = 3) + theme_classic() + facet_grid(~ifq) 
  ggplot(sub.data, aes(x = delta.shannons, y = delta.ESI)) + geom_point(aes(color = ifq), size = 3) + theme_classic() + facet_grid(~ifq) 
  
  # ancova: continuous variable is simpsons, response is total revenue by vessel. categorical factor is before/after catch shares. do just ifq (affected by catch shares). Jameal would expect no relationship before catch shares (or maybe a negative one.) would e ~ pre-post + H' + [ppxh], effect of simpsons on revenue depends whether you're talking pre and post catch shares
  
  # broad agreement, but some general scatter in bottom right and top left quadrants which would lead to opposite conclusions.
  # look at those for which delta.SI and delta.shannons differ in sign
  sub.data[which(sub.data$delta.SI < 0 & sub.data$delta.shannons > 0),]
  # so for all vessels in this data frame, tabulate the fisheries before and after catch shares. 
  
  ves <- subset(ftl, drvid == "OR001UA") %>%
    select(trip_id,metier,year, adj_revenue) %>%
    distinct() %>%
    filter(year != 2011) %>%
    mutate(post.itq = ifelse(year>2011, 1, 0)) %>%
    group_by(post.itq, metier) %>% 
    summarize(num.trips = length(unique(trip_id)), revenue = sum(adj_revenue)) %>%
    group_by(post.itq) %>%
    mutate(total.rev = sum(revenue), percent.rev = revenue/total.rev)
  
# noticing that things get "more even" and therefore more diverse when overall landings decline. So what if we calculate total revenue before and after and look at delta revenue to see how that relates to diversity. 
  sub.data <- av_dens %>%
    left_join(holland.boat) %>%
    filter(!is.na(HHI) & Year != 2011) %>%
    mutate(post.ifq = ifelse(Year>2011, 1, 0)) %>%
    group_by(drvid, post.ifq) %>%
    summarize(pre.simpsons = unique(pre.simpsons), 
              pre.shannons = unique(pre.shannons), 
              delta.simpsons = unique(delta.simpsons), 
              delta.shannons = unique(delta.shannons), 
              ifq = unique(ifq), hake = unique(hake), 
              single_fishery = unique(single_fishery), 
              mean.hhi = mean(HHI, na.rm = T), 
              mean.SI = mean(SI, na.rm = T), 
              mean.ESI = mean(ESI, na.rm = T), holland.ITQ = unique(IFQ)) %>%
    group_by(drvid) %>%
    mutate(delta.hhi = diff(mean.hhi), delta.SI = diff(mean.SI), delta.ESI = diff(mean.ESI)) %>%
    filter(post.ifq == 0)

  
# some differences on whether they landed ITQ
sub.data[which(sub.data$ifq!=sub.data$ITQ),]

# dan's boats 
dan.div <- subset(av_dens, drvid %in% holland.boat$VESSEL_NUM)
ggplot(dan.div, aes(x = ifq, y = delta.shannons)) + geom_boxplot() + theme_grey()
ggplot(dan.div, aes(x = ifq, y = delta.simpsons)) + geom_boxplot() + theme_grey()

summary(lm(delta.shannons ~ ifq, dan.div))
summary(lm(delta.shannons ~ ifq + pre.shannons, subset(dan.div, hake == 0 & single_fishery == 0))) # no effect of ifqs unless you drop hake and single fisheries

# many of these boats are single fishery and/or hake
length(which(dan.div$single_fishery==1))
ggplot(subset(dan.div, single_fishery==1 | hake == 1), aes(x = delta.simpsons)) + geom_bar() + theme_grey() # but doesn't look like directional. 
ggplot(subset(dan.div, single_fishery==1), aes(x = delta.simpsons)) + geom_bar() + theme_grey() # that's because single fisheries push in positive direction
ggplot(subset(dan.div, hake==1), aes(x = delta.simpsons)) + geom_bar() + theme_grey() # and hake looks much less diverse. 

# suggests that the strategy a vessel uses will change effect of itqs. 
ggplot(subset(dan.div, single_fishery==0 & hake == 0 & ifq == 1), aes(x = ifq, y = delta.simpsons)) + geom_boxplot() + theme_grey() # generally ifq boats got more diverse. 

# how many of these boats do I exlcude because hake??
length(which(dan.div$hake == 1)) # I drop 31 of these boats because of hake
ggplot(subset(dan.div, hake == 1), aes(x = delta.simpsons)) + geom_bar() 
# these guys are generally negative in my book


# match Dan's HHI with my delta.simpsons and delta.shannons
ggplot(subset(dan.div, hake == 0 & single_fishery == 0), aes(x = delta.SI, y = delta.shannons)) + geom_point() + theme_grey()
