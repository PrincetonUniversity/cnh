# Adjust income for 2009 (remove inflation), drop vessels with less than $10,000 median income
incomeFilter <- function(){
  # script does two things: adjusts for 2009 prices
  # calculates median annual income and removes vessels which have less than median_income.
  # can set median income value however

  library(reshape2); library(plyr); library(dplyr); library(scales); 
  library(quantmod); library(lubridate); library(data.table)

  # load data----
  ftl <- fread('rawData/Catch/PacFIN pull April 2017/pacfin_pull_04_2017_2009_2016_compFT.csv', 
               stringsAsFactors = F)
  ftl <- as.data.frame(ftl)
  colnames(ftl) <- tolower(colnames(ftl))
  
  # check names of removal types
  # http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/list_cl_removal_type.txt
  unique(ftl$removal_type_code)
  
  # reassign to legacy columns,
  # keep only commercial fisheries (drop scientific, tribal, etc.)
  ftl2 <- ftl %>% 
    filter(removal_type_code %in% c('C', 'D'))
  ftl2.1 <- plyr::rename(ftl, c(vessel_num = 'drvid', 
                             fish_ticket_id = 'trip_id',
                             orig_pacfin_species_code = 'spid'))
  
  expect_false(any(is.na(ftl2.1$drvid)))

  # Find annual income, adjust to 2009 ----
  # Adjust income to 2009 using CPI 
  # (see here http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package)
  getSymbols("CPIAUCSL", src="FRED", auto.assign=TRUE) # get CPI. 
  # The CPI inflation calculator at the BLS uses the latest monthly value for a given year. 
  annual.cpi <- CPIAUCSL[.indexmon(CPIAUCSL)==11] 
  # it's two lagged... but want jan for each year

  cf <- annual.cpi/as.numeric(annual.cpi['2009']) #using 2009 as the base year
  # need to change cf from december to january 
  cf <- as.data.frame(cf)
  cf$pacfin_year = strftime(as.Date(row.names(cf)),"%Y")
  ftl3 <- merge(ftl2.1, cf, by = "pacfin_year", all=FALSE)
  ftl3$adj_revenue <- ftl3$exvessel_revenue / ftl3$CPIAUCSL

return(ftl3)
}