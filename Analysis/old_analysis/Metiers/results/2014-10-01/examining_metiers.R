# examining metiers
library(ggplot2); library(plyr); library(grid)
# Loading Data
#----
wd <- "/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-01/metier_lists/"
setwd(wd)

# loading all metier lists, formatting
  files <- list.files()
  tables <- lapply(files, read.csv, header = TRUE, stringsAsFactors = FALSE)
  
  # because each year has it's own numbering system, i should append the year to each dataframe
  years <- substring(files, 4, 7)
  tables <- mapply(cbind, tables, "year"=years, SIMPLIFY=F)
  
  combined.df <- do.call(rbind, tables)
  combined.df$X <- NULL
  combined.df$metier <- paste(combined.df$metier, combined.df$year, sep="_")
  
# merging with fish tickets
  ftl <- read.csv("/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors = F)
  metiers <- merge(ftl, combined.df, by = c("ftid", "year"))
  # it appears that ftid are not unique across years. so need to match to both ftid and year. luckily the metiers are already split up by year, so this won't affect how the trip totals are assigned. 

saveRDS(metiers, file="metiers.RDS")

# still don't have three years worth of pot metiers (2009, 2011, 2013). 
#----
# Plotting
#----
# there are a maximum of 501 different metiers possible (so far), assuming no overlap across years
length(unique(metiers$metier))

plot_species <- function(grgroup, year_check){

  # subset to 2009
  m09 <- subset(metiers, year==year_check)
  length(unique(m09$metier))
  
  # look at TLS first
  tls09 <- m09[grep(pattern = grgroup, m09$metier),]
  if(nrow(tls09)==0){
    message("there are no metiers of that gear type in this year")
    return("sry")
  }
  
  # figure out how to parse out the plot
  num_facets <- length(unique(tls09$metier))
  cols <- num_facets/2
  if(cols%%1!=0){rows=cols+.5}

  obs <- ddply(.data=tls09, .(metier), summarize, 
               n_trips=paste("trips =",length(unique(ftid))), 
               x_trips = length(unique(spid))-length(unique(spid))/5, 
               y_trips = max(landed_wt)-max(landed_wt/6),
               n_veids=paste("vessels =", length(unique(veid))), 
               x_vessels = length(unique(spid))-length(unique(spid))/5,
               y_vessels = max(landed_wt)-max(landed_wt/3))
  
# this webpage might help in plotting, for now hammering in the text
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/

  ggplot(tls09, aes(x = factor(spid), y = landed_wt, fill = factor(spid), colour = factor(spid))) + geom_boxplot() + 
    facet_wrap(~metier, nrow = cols, scales = "free") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    geom_text(data=obs, aes(x = x_trips, y = y_trips,label = n_trips), colour="black", inherit.aes=FALSE, parse=FALSE) + 
    theme(legend.position="none") + 
    geom_text(data=obs, aes(x = x_vessels, y = y_vessels, label = n_veids), colour="black", inherit.aes=FALSE, parse=FALSE)
}

plot_species("TWS", 2010)
