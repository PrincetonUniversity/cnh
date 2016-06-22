# searching for fishing strategies within a year of trips. Steps:
#   1. Get lengths and vessel IDs, add to tickets/top level clustering
#   2. hclust() yearly trip profile
#   3. Color hclust() plot by length (where possible)

############## first get lengths ############

    rm(list=ls())
    require(plyr); require(dplyr); require(zoo); require(RColorBrewer)
    load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-06-21/tickets.Rdata")
    
    # laod vessel lenghts
    
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
    
    ############ now do state vessel registration #########
    
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
    colnames(total_lengths)[1] <- "veid"
    
    # merge with tickets, but retain tickets for which I Have no length data
    tickets_length <- merge(tickets, total_lengths, by="veid",all.x = TRUE)
    
    # remove weird vessels, namely '***' and UNKNOWN
    tickets_length <- subset(tickets_length, veid!="UNKNOWN")
    tickets_length <- tickets_length[-grep('[*]',tickets_length$veid),]

    # generate total catch with vessel length info for 2009

    # subset to just 2009
    catch09 <- subset(tickets_length, year==2009)
    
    by_vessel <- data.table(catch09)
    setkey(by_vessel, veid)
    catch <- ddply(by_vessel, .(veid,c8), summarize, num_trips = length(c8))
    
    # transform: rows are vessel/category combintations, column are cluster that trip was landed in
    catch <- as.data.table(catch)
    total_catch <- dcast.data.table(catch, veid ~ c8, fun=sum)
    
    # making a reference table of this that has length attached
    ref_tab <- as.data.frame(total_catch)
    
    len_vec <- select(tickets_length, veid, len)
    len_vec <- len_vec[!duplicated(len_vec),]

    ref_tab <- merge(ref_tab,len_vec)

############### hclust ################################

# first try without taking proportion

hc <- hclust(dist(total_catch[,-1,with=F]))

# color by labels

myColorRamp <- function(colors, values) {
  v <- (values - min(values,na.rm=T))/diff(range(values,na.rm=T))
  x <- colorRamp(colors)(v)
  x[is.na(x)] <- 0
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}
paint <- myColorRamp(c("red",'blue'),ref_tab$len)

plot(hc)
ColorDendrogram(hc, y = paint,branchlength=250)
    