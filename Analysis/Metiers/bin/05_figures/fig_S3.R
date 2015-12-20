# density plot for port diversity versus vessel diversity

# load port data
ports <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/port_stats.RDS")
vessels <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_stats.RDS")

# link these together, take majority port by revenue
landings <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_landings_data.RDS")

library(dplyr)
port_link <- landings %>%
  group_by(drvid, pcid) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T)) %>%
  filter(revenue > 0) %>%
  mutate(percent.rev = revenue/sum(revenue)) %>%
  filter(revenue == max(revenue)) %>%
  dplyr::select(-revenue) %>%
  left_join(vessels) %>%
  dplyr::select(drvid, pcid, eff.shannon_2010, delta.eff.shannon_2010) %>%
  left_join(ports) %>%
  filter(!is.na(ic_pre)) %>%
  filter(!is.na(eff.shannon_2010))
library(MASS)
k <- kde2d( port_link$eff.shannon_2010, port_link$ic_pre)
library(RColorBrewer)
png("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/S_h_v_c_pre.png",width = 5, height = 5, units = "in", res = 500, bg= "transparent")
#image(k, col= colorRampPalette(rev(brewer.pal(9, "Greys")))(10000), bty = "n", ylab=expression(C[pre]), xlab = expression(H[pre]), )
plot( port_link$eff.shannon_2010,port_link$ic_pre,cex=.45, col="black",pch=3,bty="n",ylab=expression(C[pre]),  xlab = expression(H[pre]))
abline(lm(ic_pre ~ eff.shannon_2010, port_link))
dev.off()

plot(ic_pre ~ before.nves, port_link, cex=.45, pch = 3, bty="n")
plot(eff.shannon_2010~ before.nves, port_link, cex=.45, pch = 3, bty="n")

abline(lm(ic_pre ~ before.nves, port_link))
display(lm(ic_pre ~ before.nves, port_link))

d <- kde2d(port_link$ic_delta, port_link$delta.eff.shannon_2010)
image(d)
contour(d, add = T,col='grey10')
points(port_link$ic_delta, port_link$delta.eff.shannon_2010,cex=.15, col="steelblue")

