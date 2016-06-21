library(vegan)

# how many vessels have participated?
participate <- apply(by_fishery > 0, 1, sum)

major_fisheries <- by_fishery


trips.norm <- decostand(by_fishery, "normalize")
trips.ch <- vegdist(trips.norm, "euc")
trips.ch.ward <- hclust(trips.ch, method="ward.D2")
trips.chwo <- reorder.hclust(trips.ch.ward, trips.ch)

freq <- apply(by_fishery > 0, 2, sum)

nf <- layout(matrix(1), widths = lcm(10), heights = lcm(5), respect = FALSE)

tabasco(decostand(by_fishery,"log"), trips.chwo,col=rev(c("white",brewer.pal(9, "Blues"))), margins=c(0,0), cexCol = .15)


dist_fishery <- vegdist(by_fishery)
heatmap(as.matrix(dist_fishery))
h_dist <- hclust(dist_fishery, method="ward.D")

plot(h_dist, cex=.3)
