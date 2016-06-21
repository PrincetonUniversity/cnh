# applying Shannon Weiner index to fishery by year. 

require(vegan); require(boot)
H <- diversity(BCI)

F <- table(tickets$year, tickets$c8)
f <- diversity(F)
plot(f,type="o")

gf <- subset(tickets,c8==6, select=veid)
gf <- gf[!duplicated(gf),]

# subset tickets to only vessels which participate in groundfish

gf_tick <- subset(tickets, veid %in% gf)

Fgf <- table(gf_tick$year, gf_tick$c8)
fgf <- diversity(Fgf)
plot(fgf,type="o")

foo <- sample(F[1,], 10, replace=T)
