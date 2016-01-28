# plotting what looks like a track
library(scales); library(RColorBrewer)
# load data 
  shrimps <- read.csv("anonymized_groundtruthed.csv",stringsAsFactors=F)

# just look at one shrimp vessel
  ves1 <- subset(shrimps, vessel_id==1)

# getting a vector to color by fishing (paint = 3), observed but not fishing (paint = 2), not observed (paint=1)
paint = rowSums(ves1[,c("observed","fishing")])
paint = paint+1
pal <- brewer.pal(3, "Set1")

# plotting
  plot(ves1$Longitude, ves1$Latitude, type='l',asp=1,cex=.25, col=alpha("black",.25))
  points(ves1$Longitude, ves1$Latitude, pch=20, cex=.05, col=pal[paint])

# zoom in because colors are hard to see
  plot(ves1$Longitude, ves1$Latitude, type='l',asp=1, col=alpha("black",.25),xlim=c(-1,0),ylim=c(.5,1.4))
  points(ves1$Longitude, ves1$Latitude, pch=20, cex=.5, col=pal[paint])

# ggplot does a slightly better job of coloring segments of line, also let's change alpha so we can see the fishing bits 
alpha_level = c(0.05, .5, 1)
ggplot(ves1, aes(x=Longitude, y = Latitude, color = paint)) + geom_path(alpha=alpha_level[paint]) + geom_point(alpha=alpha_level[paint]) + theme_bw()

# looks like there's some jumping from one port to another.. not sure about that. maybe just screen to make sure no speeds are ridiculous, like more than 100 kmph?