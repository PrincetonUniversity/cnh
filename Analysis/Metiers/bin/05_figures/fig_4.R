# plotting model coefficients
library(dplyr)
library(ggplot2)
library(tidyr)
library(arm)
library(RColorBrewer)

# load vessel level data ----
vessel_stats <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_stats.RDS")

# vessel models ----
lm1_ca <- lm(delta.eff.shannon_2010 ~ eff.shannon_2010, subset(vessel_stats, alaska == 0 & both.periods==1 & c.halibut==0 & ifq_flag != "itq entrant"))
lm2_ca <- lm(delta.eff.shannon_2010 ~  eff.shannon_2010 + ifq_flag, subset(vessel_stats, alaska == 0 & both.periods==1 & ifq_flag != "itq entrant: general fleet" & c.halibut == 0))

lm3_ca <- lm(delta.eff.shannon_2010 ~ ifq_flag, subset(vessel_stats, alaska == 0 & both.periods==1 & ifq_flag != "itq entrant: general fleet" & c.halibut == 0))
lm4_ca <- lm(delta.eff.shannon_2010 ~ ifq_flag + eff.shannon_2010 + ifq_flag*eff.shannon_2010, subset(vessel_stats, alaska == 0 & both.periods==1 & ifq_flag != "itq entrant: general fleet"  & c.halibut == 0))
lm5_ca <- lm(delta.eff.shannon_2010 ~ eff.shannon_2010 + ifq_flag + overall.lat + len,subset(vessel_stats, alaska == 0 & both.periods==1 & ifq_flag != "itq entrant: general fleet"  & c.halibut == 0))

    # checking CA Halibut inclusion
        lm1 <- lm(delta.eff.shannon_2010 ~ eff.shannon_2010, 
                     subset(vessel_stats, alaska == 0 & both.periods==1  & 
                              ifq_flag != "itq entrant"))
        
        lm2 <- lm(delta.eff.shannon_2010 ~  eff.shannon_2010 + ifq_flag, 
                     subset(vessel_stats, alaska == 0 & both.periods==1 & 
                              ifq_flag != "itq entrant: general fleet"))
        
        lm3 <- lm(delta.eff.shannon_2010 ~ ifq_flag, 
                     subset(vessel_stats, alaska == 0 & both.periods==1 & 
                              ifq_flag != "itq entrant: general fleet"))
        
        lm4 <- lm(delta.eff.shannon_2010 ~ ifq_flag + eff.shannon_2010 + 
                       ifq_flag*eff.shannon_2010, 
                     subset(vessel_stats, alaska == 0 & both.periods==1 & 
                              ifq_flag != "itq entrant: general fleet"))
        
        lm5 <- lm(delta.eff.shannon_2010 ~ eff.shannon_2010 + ifq_flag + 
                       overall.lat + len,
                     subset(vessel_stats, alaska == 0 & both.periods==1 & 
                              ifq_flag != "itq entrant: general fleet"))

# sim to get effects ----
  lm2.sim <- sim(lm2,n=10000)
  coef.lm2.sim <- coef(lm2.sim)
  
  itqs_effects <- coef.lm2.sim[,c(1,3,4)]
  colnames(itqs_effects) <- c("general_fleet","ifq_participants","limited_entry_exits")
  itqs_effects <- as.data.frame(itqs_effects)
  itqs_effects$ifq_participants <- itqs_effects$general_fleet + itqs_effects$ifq_participants
  itqs_effects$limited_entry_exits <- itqs_effects$general_fleet + itqs_effects$limited_entry_exits
  
  ifq_effect <- gather(itqs_effects) %>%
    dplyr::rename(ifq_flag = key,coefficient = value) %>%
    group_by(ifq_flag) %>%
    summarize(mean = mean(coefficient), 
              max_ci = quantile(coefficient, .975), 
              min_ci = quantile(coefficient,0.025))
  

# vessel level plot ----
effect.size <- 
  ggplot(ifq_effect, aes(x = ifq_flag, y = mean)) + geom_point(size = 4.5) + theme_light() + 
  geom_errorbar(aes(ymin = min_ci, ymax = max_ci, width = 0)) + 
  scale_x_discrete(labels=c(
    general_fleet = expression(paste("general fleet")), 
    ifq_participants = paste("catch share participant"),
    limited_entry_exits = paste("limited entry exit"))) + 
  geom_hline(yintercept=0) + xlab("") + 
  ylab(expression(paste(Delta, "H"))) + 
  ylim(c(0,1.2)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border=element_blank()) + geom_vline(xintercept = .41)

ggsave("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_4a.pdf",plot = effect.size, width=4.75,height=4.5)

# plot best fit regression
vessel_stats$ifq_flag <- as.factor(vessel_stats$ifq_flag)
levels(vessel_stats$ifq_flag) <- c("general fleet", "general fleet entrant", "general fleet exit", "itq entrant", "itq entrant: general fleet", "catch shares participant", "limited entry exit", "limited entry complete exit") 
vessel_stats$ifq_flag <- factor(vessel_stats$ifq_flag, rev(levels(vessel_stats$ifq_flag)))
lm_reg =  ggplot(subset(vessel_stats, alaska==0  & ifq_flag!="itq entrant: general fleet" & both.periods == 1),aes(x=eff.shannon_2010, y = delta.eff.shannon_2010, color=ifq_flag))  + geom_point(aes(color=ifq_flag),size=1) + stat_smooth(aes(group=ifq_flag), method = 'lm',lwd=1.25)  + xlab(expression("H"['pre'])) + ylab(expression(paste(Delta,"H"))) + scale_color_manual(name='',values = brewer.pal(3, "Dark2"))

# ggsave(lm_reg, file="/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_5.pdf",width=8, height=6)

# port models ----
port_df <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/port_stats.RDS")

# look before and after in ports
lm_port1 <- lm(ic_delta ~ ic_pre, subset(port_df, ifq_flag!="itq entrant: general landings"))
lm_port <- lm(ic_delta ~  ic_pre + ifq_flag, subset(port_df, ifq_flag!="itq entrant: general landings"))
lm_port3 <- lm(ic_delta ~ ifq_flag, subset(port_df, ifq_flag!="itq entrant: general landings"))
lm_port4 <- lm(ic_delta ~ ic_pre + ifq_flag + ic_pre*ifq_flag, subset(port_df, ifq_flag!="itq entrant: general landings"))

# sim to get effects

lm_port.sim <- sim(lm_port,n=10000)
coef.lm_port.sim <- coef(lm_port.sim)

port_effects <- coef.lm_port.sim[,c(1,3,4)]
colnames(port_effects) <- c("ifq_participants","limited_entry_exits", "general_fleet")
port_effects <- as.data.frame(port_effects)
port_effects$limited_entry_exits <- port_effects$ifq_participants + port_effects$limited_entry_exits
port_effects$general_fleet <- port_effects$general_fleet + port_effects$ifq_participants

port_effect <- gather(port_effects) %>%
  dplyr::rename(ifq_flag = key,coefficient = value) %>%
  group_by(ifq_flag) %>%
  summarize(mean = mean(coefficient), 
            max_ci = quantile(coefficient, .975), 
            min_ci = quantile(coefficient,0.025))

levels(port_effect$ifq_flag) <- levels(port_effect$ifq_flag)[c(3,1,2)]

port.plot <- 
  ggplot(port_effect, aes(x = ifq_flag, y = mean)) + geom_point(size = 4.5) + theme_light() + 
  geom_errorbar(aes(ymin = min_ci, ymax = max_ci, width = 0)) + 
  scale_x_discrete(labels=c(
    general_fleet = expression(paste("general ports")), 
    ifq_participants = paste("catch share ports"),
    limited_entry_exits = paste("limited entry exit ports"))) + 
  geom_hline(yintercept=0) + xlab("") + 
  ylab(expression(paste(Delta, "C"))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border=element_blank()) + geom_vline(xintercept = .41)

ggsave("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/fig_4b.pdf",plot = port.plot, width=4.75,height=4.5)

