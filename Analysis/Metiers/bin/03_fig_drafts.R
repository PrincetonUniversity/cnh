# making figures
rm(list=ls())
library(ggplot2);library(ggthemes);library(cowplot)
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

coastwide <- ggplot(vessel_stats[which(vessel_stats$alaska==0 & !is.na(vessel_stats$type)),], aes(x = type)) + geom_bar() + ylab("number of vessels")  + theme_pander() + theme(panel.grid = element_blank())

by_zone <- ggplot(vessel_stats[which(vessel_stats$alaska==0 & !is.na(vessel_stats$type) & !is.na(vessel_stats$zone)),], aes(x = type)) + geom_bar() + facet_wrap(~zone, ncol = 1, scales = "free_y") + ylab('number of vessels')  + theme_pander() + theme(panel.grid = element_blank())

div_hist <- ggplot(subset(vessel_stats[which(!is.na(vessel_stats$zone) & vessel_stats$alaska==0),], vessel_stats$eff.shannon_2010>1), aes(x = eff.shannon_2010)) + geom_histogram(aes(y = ..count..)) + facet_wrap(~zone, ncol = 1,scales = "free_y") + xlab("effective shannon diversity of revenue") + ylab('number of vessels') + theme_pander() + theme(panel.grid = element_blank())

all_plot <- plot_grid(coastwide, by_zone, div_hist, labels = c("A","B","C"), ncol = 3, rel_widths = c(1,1.2,1.4))

ggplot2::ggsave( "/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_2.pdf", 
          all_plot,width = 11.4, height = 7, units = "cm" ,scale=2.5, dpi = 300)

# figure 3 ----
# make port networks and measure degree, betwenness
port_df <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/port_stats.RDS")

source("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/02_define_participationPlot.R")

port_df <- port_df[order(port_df$ic_pre, decreasing = TRUE),]

pdf("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_3c.pdf",width = 8, height = 2)
  par(bg="transparent", cex = .75,cex=1.2)
  paint = rep("normal",nrow(port_df))
  paint[port_df$pcid %in% c("SB","ERK","OAK")] <- "example"
  ggplot(port_df, aes(x = reorder(pcid, -ic_pre), fill=factor(paint), y = ic_pre)) + 
           geom_bar(stat='identity') + xlab("port") + 
    ylab("port connectance") + theme(axis.text.x  = element_text(size=5, angle=90)) + scale_fill_manual(values = c("grey20","grey50")) + theme_pander()
dev.off()

# aux ploting - drop nodes with < 3 vessels
gp <- function(g_s){
  g=delete.vertices(g_s,which(V(g_s)$size<3))
  plot(g, edge.width = E(g)$weight*3, layout = layout.fruchterman.reingold, vertex.size = log(V(g)$size)*4, vertex.label.color = "grey30", vertex.label.cex = .75, vertex.label.family="sans", vertex.frame.color = NA, edge.curved=TRUE, edge.arrow.size = .25, edge.color = "black")
}


sb <- define_participationPlot(year=2009:2010, port = "SB", tickets = tickets)
erk <- define_participationPlot(year=2009:2010, port = "ERK", tickets = tickets)
oak <- define_participationPlot(year=2009:2010, port = "OAK", tickets = tickets)

png("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_3a.png",
    width = 4, height = 4, res = 300, units = "in")
  par(bg="transparent",oma=rep(0,4),mai=rep(0,4))
  g=delete.vertices(sb,which(V(sb)$size<3))
  plot(g, edge.width = E(g)$weight*5, 
       layout = layout.circle, vertex.size = log(V(g)$size)*5, 
       vertex.label.color = "black", vertex.label.cex = .5, 
       vertex.label.family="sans", vertex.frame.color = NA, vertex.label="",
       edge.curved=TRUE, edge.arrow.size = .05, edge.color = alpha("black",.25))
dev.off()

png("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_3b.png",
    width = 4, height = 4, res = 300, units = "in")
  par(bg="transparent",oma=rep(0,4),mai=rep(0,4))
  g = delete.vertices(erk, which(V(erk)$size<3))
  plot(g, edge.width = E(g)$weight*5, 
       layout = layout.circle, vertex.size = log(V(g)$size)*5, 
       vertex.label.color = "black", vertex.label.cex = .5, 
       vertex.label.family="sans", vertex.frame.color = NA, vertex.label="",
       edge.curved=TRUE, edge.arrow.size = .05, edge.color = alpha("black",.25))
  dev.off()
  
  
  png("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_3d.png",
      width = 4, height = 4, res= 300, units = "in")
  par(bg="transparent",oma=rep(0,4),mai=rep(0,4))
  g = delete.vertices(oak, which(V(oak)$size<3))
  plot(g, edge.width = E(g)$weight*3, 
       layout = layout.circle, vertex.size = log(V(g)$size)*10, 
       vertex.label.color = "black", vertex.label.cex = .5, 
       vertex.label.family="sans", vertex.frame.color = NA,vertex.label="",
       edge.curved=TRUE, edge.arrow.size = .05, edge.color = alpha("black",.25))
  dev.off()
  

# figure 4 ----
# making linear regression

# individual model
lm1 <- lm(delta.eff.shannon_2010 ~ eff.shannon_2010, 
          subset(vessel_stats, alaska == 0 & 
                   both.periods==1 & c.halibut==0 & 
                   ifq_flag != "itq entrant"))

lm2 <- lm(delta.eff.shannon_2010 ~  eff.shannon_2010 + ifq_flag, 
          subset(vessel_stats, alaska == 0 & 
                   both.periods==1 & 
                   ifq_flag != "itq entrant: general fleet" & 
                   c.halibut == 0))

# sim to get effects 
  library(arm)
  lm2.sim <- sim(lm2,n=10000)
  coef.lm2.sim <- coef(lm2.sim)
  
  vessel_effects <- as.data.frame(coef.lm2.sim)
  colnames(vessel_effects) <- c("intercept","eff.shannon_2010",
                                "stay_on","LE_exit")
  
  ifq_effect <- vessel_effects %>%
    dplyr::select(-eff.shannon_2010) %>%
    mutate(stay_on = stay_on + intercept, LE_exit = LE_exit + intercept) %>%
    gather() %>%
    dplyr::rename(ifq_flag = key,coefficient = value) %>%
    group_by(ifq_flag) %>%
    summarize(mean = mean(coefficient), 
              max_ci = quantile(coefficient, .975), 
              min_ci = quantile(coefficient,0.025))
  
  ifq_effect$ifq_flag = factor(ifq_effect$ifq_flag,labels = 
                                 c("general\nparticipants",
                                   "limited entry\nexit",
                                   "catch share\nparticipant"), ordered = TRUE)

# plot
ggplot(ifq_effect, aes(x = ifq_flag, y = mean)) + geom_point(size =7) + 
  ylim(c(0,1.2)) + 
  geom_errorbar(aes(ymin = min_ci, ymax = max_ci), width = 0) + 
  theme_pander() + ylab(expression(paste(Delta,"H"))) + 
  xlab("") + theme(panel.grid=element_blank()) + ylim(c(0,1.3))

ggplot2::ggsave("Analysis/Metiers/bin/05_figures/fig5.pdf",width=4, height = 4, 
                units="in",scale=1)

# port model
fp = "/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/port_stats.RDS"
port_df <- readRDS(fp)
lm_port1 <- lm(ic_delta ~ ic_pre, 
               subset(port_df, ifq_flag!="itq entrant: general landings"))

lm_port <- lm(ic_delta ~  ic_pre + ifq_flag, 
              subset(port_df, ifq_flag!="itq entrant: general landings"))

# sim to get effects 
lm_port.sim <- sim(lm_port,n=10000)
coef.lm_port.sim <- coef(lm_port.sim)

port_effect <- as.data.frame(coef.lm_port.sim)
colnames(port_effect) <- c("intercept","ic_pre","LE_exit","general_fleet")

port_effect <- port_effect %>%
  dplyr::select(-ic_pre) %>%
  mutate(stay_on = stay_on + intercept, LE_exit = LE_exit + intercept) %>%
  gather() %>%
  dplyr::rename(ifq_flag = key,coefficient = value) %>%
  group_by(ifq_flag) %>%
  summarize(mean = mean(coefficient), 
            max_ci = quantile(coefficient, .975), 
            min_ci = quantile(coefficient,0.025))

levels(ifq_effect$ifq_flag) = c("general fleet"," catch share\nparticipant", 
                                "limited entry\nexit")

# gather and plot



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