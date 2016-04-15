rm(list=ls())
require(plyr); require(dplyr); require(zoo)
load("tickets.Rdata")

# load vessel lenghts

CG <- read.csv("/Volumes/NOAA_Data/CNH/Data/Catch/cg_2009-2013_2014-03-21.csv",stringsAsFactors=F)
colnames(CG) <- tolower(colnames(CG))

# just the info I need: veid, length

len <- select(CG, vid, len)
len <- len[order(len$vid),]
len <- len[!duplicated(len),]

########### Exploring CG length data, not required for replication ######################### 
          # some vessel ids are duplicated, which means they have different values for length
          dup_id <- which(duplicated(len$vid) | duplicated(len$vid, fromLast=TRUE) == TRUE)
          dup_len <- len[dup_id,]
          
          # are all vessels getting bigger?
          change_len <- ddply(dup_len, .(vid), summarise, num_mes = mean(diff(len)))
          
          # some surprises, for example a boat that drops by 300 feet
          subset(CG, vid==as.character(change_len[which(change_len$num_mes==-300),]))[15:18,1:9]
          
          # another grows by 63
          subset(CG, vid==as.character(change_len[which(change_len$num_mes==63),]))[8:12,1:9]
########################################################################################

# according to NOAA people, I think that sometimes the vessel actually changes, but other times errors are caught and applied moving forwards. So I'll take the most recent length report for each vessel possible. There are 42 vessels with changing length. For them I need to find the 2013 entry. Is there a 2013 entry for all 42 vessels?

# make date
pubdate <- paste(CG$pubyr, CG$pubmo, sep=" ")
date <- as.yearmon(pubdate, '%Y %b')
CG$date <- date

most_recent <- ddply(CG, .(vid), summarize, recent = max(date))
colnames(most_recent)[2] <- "date"

# CG lengths
CG_len <- select(CG, vid, date, len)
lengths <- merge(most_recent, CG_len, by = c("vid","date"))

# now do state vessel registration

sv <- read.csv("/Volumes/NOAA_Data/CNH/Data/Catch/sv_2009-2013_2014-03-21.csv",stringsAsFactors=F)
colnames(sv) <- tolower(colnames(sv))
# just select things I need

svlen <- select(sv, svid, len)
svlen <- svlen[order(svlen$svid),]
svlen <- svlen[!duplicated(svlen),]


# do have duplicate records for length, so will take most recent
any(duplicated(svlen$svid)) # TRUE = replicates

# remove NAs from length
sv <- sv[!is.na(sv$len),]

svmost_recent <- ddply(sv, .(svid), summarize, year = max(year))

# sv lengths
sv_len <- select(sv, svid, year, len, agid)
sv_len <- sv_len[!duplicated(sv_len),]
svlengths <- merge(svmost_recent, sv_len, by = c("svid","year"))

################# exploring duplicates not necessary for replication #############
      # something's duplicated. why? because multiple length reports for same year. and realize that they're the listings of the same boat from different states. 
      
      foo_dup <- svlengths[which(duplicated(svlengths$svid) | duplicated(svlengths$svid, fromLast = TRUE)),]
      
      # and some boats are listed in both CG and state vessel registration. And in the case below, it looks like Washington has the correct boat length
      
      subset(CG, vid=="1074005")
      subset(sv, svid=="1074005")
      
      # should look to see how often oregon versus washington are correct about boats in both SV and CG datasets. 
      
      # find boats that are duplicated and in CG
      
      dup_CG <- subset(CG, vid %in% unique(foo_dup$svid), select = c(vid, len))
      dup_CG <- dup_CG[!duplicated(dup_CG),]
      dup_CG <- dup_CG[order(dup_CG$vid),]
      
      dup_CG[which(duplicated(dup_CG$vid) | duplicated(dup_CG$vid, fromLast = TRUE)),]
      # about half
      length(unique(dup_CG$vid))/length(unique(foo_dup$svid))
      
      # and find differences between the coast gaurd and either number that the state's have:
      subset(sv, svid=="547210")
      subset(CG, vid=="547210")
      
      # this is also an example where the length changed. But becasue the netwt changed as well, i bet that this boat was remodeled. Rather than just an error. 
########################################################################

# for now, will use CG when present, and then oregon. because they just seem more on top of things. 

# dups

dsv <- svlengths[duplicated(svlengths$svid) | duplicated(svlengths$svid, fromLast = TRUE),]
length(unique(dsv$svid))
nrow(subset(dsv, agid=="O")) # means I can just use Oregon licenses. 

dsv <- subset(dsv, agid=="O")

# make dataset with no duplicates, then add in the filtered duplicated boats back after choosing the oregon version of their records
no_dups_sv <- svlengths[- which(duplicated(svlengths$svid) | duplicated(svlengths$svid, fromLast = TRUE)),]
no_dups_sv <- rbind (no_dups_sv, dsv)

# now merge coast guard and state vessels, but only vessels that are not already in coast guard database. 
nonCG <- subset(no_dups_sv, !(svid %in% lengths$vid))

state_length <- data.frame(vid = nonCG$svid, len = nonCG$len, stringsAsFactors = F, record = rep("sv", nrow(nonCG)))
lengths$record <- rep("cg",nrow(lengths))
# total length data
total_lengths <- rbind(state_length, lengths[,c(1,3,4)])

# now merge with tickets veid
colnames(total_lengths)[1] <- "veid"

tickets_len <- merge(tickets, total_lengths, by="veid")

# loose about half of the vessels in original dataset. 
length(unique(tickets_len$veid))/length(unique(tickets$veid))

# looks like all vessels covered: 
any(is.na(tickets_len$len)) # FALSE, nice

# looking at vessel characteristics only
vchar <- select(tickets_len, veid, len, c8)
vchar <- vchar[!duplicated(vchar),]
hist(vchar$len, main="vessel length", col="grey",bor="darkgrey",breaks=50, freq=F)
lines(density(vchar$len),col="indianred",lwd=3)

# look at length and cluster relationship 
par(mfrow=c(2,4))
for(i in 1:8){
  hist(subset(vchar, c8==i)$len[subset(vchar, c8==i)$len < 500],
       xlim=c(0,150), 
       col="grey",bor="darkgrey",
       main=paste("vessel length for cluster ",i, sep=" "),
       xlab="length (ft)",
       freq=F,
       sub=paste(nrow(subset(vchar,c8==i)), "vessels",sep=" "))
  lines(density(subset(vchar, c8==i)$len),lwd=4,col="indianred")
  abline(v=60,lty=3,col="dark blue",lwd=3)
}

save(tickets_len,file="tickets_len.Rdata")
