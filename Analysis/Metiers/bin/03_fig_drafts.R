# making figures
rm(list=ls())

# load data ----
tickets <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_landings_data.RDS")
vessel_stats <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_stats.RDS")

# figure 2 ----
# making histograms by management zone
# load ports with lat/lon
library(dplyr)
library(vegan)

ports <- read.csv("/Users/efuller/Desktop/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
ports <- rename(ports, pcid = Pcid)

# plot
library(ggplot2)  
library(cowplot)

coastwide <- ggplot(vessel_stats[which(vessel_stats$alaska==0 & !is.na(vessel_stats$type)),], aes(x = type)) + geom_bar() + ylab("number of vessels") + background_grid(major = "xy", minor = "none")

by_zone <- ggplot(vessel_stats[which(vessel_stats$alaska==0 & !is.na(vessel_stats$type) & !is.na(vessel_stats$zone)),], aes(x = type)) + geom_bar() + facet_wrap(~zone, ncol = 1, scales = "free_y") + ylab('number of vessels') + background_grid(major = "xy", minor = "none")

div_hist <- ggplot(subset(vessel_stats[which(!is.na(vessel_stats$zone) & vessel_stats$alaska==0),], vessel_stats$eff.shannon_2010>1), aes(x = eff.shannon_2010)) + geom_histogram(aes(y = ..count..)) + facet_wrap(~zone, ncol = 1,scales = "free_y") + xlab("effective shannon diversity of revenue") + ylab('number of vessels') + background_grid(major = "xy", minor = "none")

all_plot <- plot_grid(coastwide, by_zone, div_hist, labels = c("A","B","C"), ncol = 3, rel_widths = c(1,1,1.75))

save_plot("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_2.pdf", 
          all_plot,base_height = 8, base_width = 12)

# figure 3 ----
# make port networks and measure degree, betwenness
port_df <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/port_stats.RDS")

source("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/02_define_participationPlot.R")

port_df <- port_df[order(port_df$ic_pre, decreasing = TRUE),]

pdf("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_3c.pdf",width = 16, height = 4)
  par(bg="transparent", cex = .75,cex=1.2)
  barplot(port_df$ic_pre, bor = F, ylab = "average fishery connectance", xlab="port",names=port_df$pcid,cex.names=.5)
dev.off()

# aux ploting - drop nodes with < 3 vessels
gp <- function(g_s){
  g=delete.vertices(g_s,which(V(g_s)$size<3))
  plot(g, edge.width = E(g)$weight*3, layout = layout.fruchterman.reingold, vertex.size = log(V(g)$size)*4, vertex.label.color = "grey30", vertex.label.cex = .75, vertex.label.family="sans", vertex.frame.color = NA, edge.curved=TRUE, edge.arrow.size = .25, edge.color = "black")
}

sb <- define_participationPlot(year=2009:2010, port = "SB", tickets = tickets)
erk <- define_participationPlot(year=2009:2010, port = "ERK", tickets = tickets)

crs <- define_participationPlot(year=2009:2010, port = "CRS", tickets = tickets)
new <- define_participationPlot(year=2009:2010, port = "NEW", tickets = tickets)
ast <- define_participationPlot(year=2009:2010, port = "NEW", tickets = tickets)
mnt <- define_participationPlot(year=2009:2010, port = "MNT", tickets = tickets)
mos <- define_participationPlot(year=2009:2010, port = "MOS", tickets = tickets)
wpt <- define_participationPlot(year=2009:2010, port = "WPT", tickets = tickets)
nea <- define_participationPlot(year=2009:2010, port = "NEA", tickets = tickets)
orf <- define_participationPlot(year=2009:2010, port = "ORF", tickets = tickets)
bdn <- define_participationPlot(year=2009:2010, port = "BDN", tickets = tickets)
alb <- define_participationPlot(year=2009:2010, port = "ALB", tickets = tickets)

load("/Users/efuller/Desktop/CNH/processedData/spatial/2_coastline.Rdata")
par(bg="transparent")
plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="NEA")], ports$lat[which(ports$pcid=="NEA")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="WPT")], ports$lat[which(ports$pcid=="WPT")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="AST")], ports$lat[which(ports$pcid=="AST")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="NEW")], ports$lat[which(ports$pcid=="NEW")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="CRS")], ports$lat[which(ports$pcid=="CRS")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="ERK")], ports$lat[which(ports$pcid=="ERK")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="MNT")], ports$lat[which(ports$pcid=="MNT")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="SB")], ports$lat[which(ports$pcid=="SB")],col='dodgerblue',pch=19)

pdf("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_3a.pdf",width = 8, height = 8)
  par(bg="transparent")
  g=delete.vertices(sb,which(V(sb)$size<3))
  plot(g, edge.width = E(g)$weight*1, 
       layout = layout.circle, vertex.size = log(V(g)$size)*2, 
       vertex.label.color = "black", vertex.label.cex = .5, 
       vertex.label.family="sans", vertex.frame.color = NA, 
       edge.curved=TRUE, edge.arrow.size = .05, edge.color = "black")
dev.off()

pdf("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_3b.pdf",width = 8, height = 8)
  par(bg="transparent")
  g = delete.vertices(nea, which(V(nea)$size<3))
  plot(g, edge.width = E(g)$weight*1, 
       layout = layout.circle, vertex.size = log(V(g)$size)*2, 
       vertex.label.color = "black", vertex.label.cex = .5, 
       vertex.label.family="sans", vertex.frame.color = NA, 
       edge.curved=TRUE, edge.arrow.size = .05, edge.color = "black")
  dev.off()
  

# figure 4 ----
# making linear regression

# individual model
lm1 <- lm(delta.eff.shannon_2010 ~ eff.shannon_2010, subset(vessel_stats, alaska == 0 & both.periods==1 & c.halibut==0 & ifq_flag != "itq entrant"))
lm2 <- lm(delta.eff.shannon_2010 ~  eff.shannon_2010 + ifq_flag, subset(vessel_stats, alaska == 0 & both.periods==1 & ifq_flag != "itq entrant: general fleet" & c.halibut == 0))

lm3 <- lm(delta.eff.shannon_2010 ~ ifq_flag, subset(vessel_stats, alaska == 0 & both.periods==1 & ifq_flag != "itq entrant: general fleet" & c.halibut == 0))
lm4 <- lm(delta.eff.shannon_2010 ~ ifq_flag + eff.shannon_2010 + ifq_flag*eff.shannon_2010, subset(vessel_stats, alaska == 0 & both.periods==1 & ifq_flag != "itq entrant: general fleet"  & c.halibut == 0))
lm5 <- lm(delta.eff.shannon_2010 ~ eff.shannon_2010 + ifq_flag + overall.lat + len,subset(vessel_stats, alaska == 0 & both.periods==1 & ifq_flag != "itq entrant: general fleet"  & c.halibut == 0))

with(subset(vessel_stats, ifq_flag=="general fleet" & alaska == 0 & c.halibut == 0), t.test(eff.shannon_2010, eff.shannon_2010 + delta.eff.shannon_2010))

with(subset(vessel_stats, ifq_flag=="itq stay on" & alaska == 0 & c.halibut == 0), t.test(eff.shannon_2010, eff.shannon_2010 + delta.eff.shannon_2010))

# comparing general fleet and itq stay on starting diversity
t.test(vessel_stats$eff.shannon_2010[which(vessel_stats$ifq_flag=="itq stay on")], vessel_stats$eff.shannon_2010[which(vessel_stats$ifq_flag=="general fleet")])

# port model
port_df <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/port_stats.RDS")

# look before and after in ports
t.test(port_df$ic_pre,port_df$ic_post)
#with(subset(port_df, ifq==1), t.test(ic_pre, ic_post))

lm_port1 <- lm(ic_delta ~ ic_pre, subset(port_df, ifq_flag!="itq entrant: general landings"))
lm_port <- lm(ic_delta ~  ic_pre + ifq_flag, subset(port_df, ifq_flag!="itq entrant: general landings"))
lm_port3 <- lm(ic_delta ~ ifq_flag, subset(port_df, ifq_flag!="itq entrant: general landings"))
lm_port4 <- lm(ic_delta ~ ic_pre + ifq_flag + ic_pre*ifq_flag, subset(port_df, ifq_flag!="itq entrant: general landings"))

# plot best fit regression
library(ggplot2); library(cowplot); library(RColorBrewer)
vessel_stats$ifq_flag <- as.factor(vessel_stats$ifq_flag)
levels(vessel_stats$ifq_flag) <- c("general fleet", "general fleet entrant", "general fleet exit", "itq entrant", "itq entrant: general fleet", "catch shares participant", "limited entry exit", "limited entry complete exit") 
vessel_stats$ifq_flag <- factor(vessel_stats$ifq_flag, rev(levels(vessel_stats$ifq_flag)))
lm_reg =  ggplot(subset(vessel_stats, alaska==0 & c.halibut == 0 & ifq_flag!="itq entrant: general fleet" & both.periods == 1),aes(x=eff.shannon_2010, y = delta.eff.shannon_2010, color=ifq_flag))  + geom_point(aes(color=ifq_flag),size=1) + stat_smooth(aes(group=ifq_flag), method = 'lm',lwd=1.25)  + xlab(expression("H"['pre'])) + ylab(expression(paste(Delta,"H"))) + scale_color_manual(name='',values = brewer.pal(3, "Dark2"))

ggsave(lm_reg, file="/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_5.pdf",width=8, height=6)

# alternative is below

# sim to get effects 
library(arm)
lm2.sim <- sim(lm2,n=10000)
coef.lm2.sim <- coef(lm2.sim)
# calc estimates
sigma.lm2.sim <- sigma.hat(lm2.sim)

lm_port.sim <- sim(lm_port1,n=10000)
coef.lm_port.sim <- coef(lm_port.sim)
sigma.lm2.sim <- sigma.hat(lm_port.sim)

library(tidyr)
ifq_effect <- as.data.frame(cbind(coef.lm2.sim[,2:4], coef.lm_port.sim[,2]))
colnames(ifq_effect) <- c("a","b","c","d")
ifq_effect <- gather(ifq_effect) %>%
  dplyr::rename(ifq_flag = key,coefficient = value) %>%
  group_by(ifq_flag) %>%
  summarize(mean = mean(coefficient), 
            max_ci = quantile(coefficient, .975), 
            min_ci = quantile(coefficient,0.025))


effect.size <- ggplot(ifq_effect, aes(x = ifq_flag, y = mean)) + geom_point(size =10) + geom_errorbar(aes(ymax = max_ci, ymin = min_ci, width = 0)) + scale_x_discrete(labels=c(
    a = expression(paste("vessel: H"["pre"])), 
    b = paste("vessel: catch share participant"),
    c = paste("vessel: limited entry exit"), 
    d = expression(paste("port: C"["pre"])))) + geom_hline(yintercept=0) + xlab("") + ylab(expression(paste(Delta, "diversity")))

ggsave(effect.size, file="/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_6.pdf",width=12,height=9)
  
ifq_effect <- as.data.frame(cbind(coef.lm2.sim[,2:4], coef.lm_port.sim[,2]))
colnames(ifq_effect) <- c("a","b","c","d")
ifq_effect <- gather(ifq_effect)

ggplot(ifq_effect, aes(x=key, y = value)) + geom_boxplot(outlier.size = 0, fill = "grey") + geom_hline(yintercept  = 0) + xlab("level") + ylab(expression(paste(Delta,"diversity")) ) + xlab("coefficient value") 

save_plot("/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/figures/fig_4.pdf", effect.size)

boxplot(coef.lm2.sim[,2],outline=FALSE,ylim=c(-3,3)) + abline(h=0,lwd=2)
boxplot(coef.lm_port.sim[,3],outline=FALSE,ylim=c(-3,3)) + abline(h=0,lwd=2)

# look at intercepts
boxplot(cbind(coef.lm2.sim[,3:4], coef.lm_port.sim[,1]),bty='n',names=c('stay on', 'ifq exit', 'port level'), ylab = 'Change in diversity', col='grey',ylim=c(-3,3), outline=FALSE, bty="n")
abline(h = 0, lwd = 2)

# try dropping mean_degree prior since not large effect
lm_port2 <- lm(delta.degree ~ has.ifq, port_df)

# alternative: predict
library(reshape2)
data <- data.frame(x=c(0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4),
                   y=c(0, 0, 0, 3, 1, 1, 1, 2, 2, 1, 4, 4),
                   group=c(rep(1, 6), rep(2, 4), rep(3, 2)))
model <- lm(mass0 ~ group + treatment, data)


# supplementary figures ----


  # figure S3
# time series of landings
by_day <- tickets %>%
  filter(year < 2011) %>%
  group_by(tdate, metier.2010) %>%
  summarize(n.trips = length(unique(trip_id)))

by_day$tdate <- as.Date(by_day$tdate, format = "%d-%b-%y")
by_day$fishery <- ifelse(by_day$metier.2010 == "HKL_2", "black rockfish hook & line",
                  ifelse(by_day$metier.2010 == "NET_1", "market squid seine",
                  ifelse(by_day$metier.2010 == "TWL_1", "groundfish trawl",
                  ifelse(by_day$metier.2010 == "POT_1","dungeness crab pots",
                  ifelse(by_day$metier.2010 == "TWS_1", "pink shrimp trawl", NA)))))

my_dat <-as.data.frame(subset(by_day, metier.2010 %in% c("HKL_2","NET_1","TWS_1","POT_1","TWL_1")))
my_dat$fishery <- factor(my_dat$fishery)
myColors <- toupper(c("#268bd2","#cb4b16","#859900","#2aa198","#6c71c4"))
names(myColors) <- levels(my_dat$fishery)
library(ggplot2)
colScale <- scale_colour_manual(name = "fishery",values = myColors)

# p <- ggplot(my_dat, aes(x = tdate, y = n.trips, colour=fishery)) + geom_bar(stat='identity') + facet_wrap(~fishery, ncol = 1, scales = "free_y") + theme_minimal() + xlab("date landed") + ylab('number of trips landed')

# p + colScale +guides(colour=FALSE)

ggplot(my_dat, aes(x = tdate, y = n.trips)) + geom_bar(stat='identity') + facet_wrap(~fishery, ncol = 1, scales = "free_y") + theme_minimal() + xlab("date landed") + ylab('number of trips landed')


# figure S4
# west coast map with landings
ports <- read.csv("/Users/efuller/Desktop/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
ports <- rename(ports, pcid = Pcid)

ex_space <- subset(tickets, metier.2010 %in% c("TWL_1","TWS_1")) %>%
  filter(year < 2011) %>%
  group_by(metier.2010, trip_id) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T), pcid = unique(pcid)) %>%
  left_join(ports)

ex_space$fishery <- ifelse(ex_space$metier.2010 == "HKL_2", "black rockfish hook & line",
                    ifelse(ex_space$metier.2010 == "NET_1", "squid seine",
                    ifelse(ex_space$metier.2010 == "TWL_1", "groundfish trawl",
                    ifelse(ex_space$metier.2010 == "POT_1","dungeness crab pots",
                    ifelse(ex_space$metier.2010 == "TWS_1", "pink shrimp trawl", NA)))))

ex_space$fishery <- as.factor(ex_space$fishery)

ggplot(ex_space, aes(x = lat)) + geom_bar(aes(weight=revenue, fill = fishery),position="dodge",binwidth=1)+ ylab("revenue ($)") + xlab("latitude")  + theme_classic() + scale_y_sqrt()  + scale_fill_manual(values = c("grey40","grey80")) +  scale_y_continuous(trans = "reverse") + coord_flip() + theme(plot.background =  element_rect(fill="transparent"), panel.background = element_rect(fill="transparent"), legend.background = element_rect(fill='transparent'))

load("/Users/efuller/Desktop/CNH/processedData/spatial/2_coastline.Rdata")
plot(WC, col = 'black',bg="transparent", axes=TRUE)

