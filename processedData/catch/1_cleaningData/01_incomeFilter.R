# Adjust income for 2009 (remove inflation), drop vessels with less than $10,000 median income
incomeFilter <- function(){
  # script does two things: adjusts for 2009 prices
  # calculates median annual income and removes vessels which have less than median_income.
  # can set median income value however

  library(reshape2); library(plyr); library(dplyr); library(scales); 
  library(quantmod); library(lubridate); library(data.table)

  # load data----
  ftl <- fread('rawData/Catch/pacfin_pull_09_2016/2006_2016_compFT.csv', 
               stringsAsFactors = F)
  ftl <- as.data.frame(ftl)
  colnames(ftl) <- tolower(colnames(ftl))
  
  # reassign to legacy columns,
  # keep only commercial fisheries (drop scientific, tribal, etc.)
  ftl <- ftl %>% 
    filter(removal_type_code %in% c('C', 'D'))
  ftl <- plyr::rename(ftl, c(vessel_num = 'drvid', 
                             fish_ticket_id = 'trip_id',
                             orig_pacfin_species_code = 'spid'))
  
  expect_false(any(is.na(ftl$drvid)))

  # Find annual income, adjust to 2009 ----
  # Adjust income to 2009 using CPI 
  # (see here http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package)
  getSymbols("CPIAUCSL", src="FRED") # get CPI. 
  # The CPI inflation calculator at the BLS uses the latest monthly value for a given year. 
  annual.cpi <- CPIAUCSL[.indexmon(CPIAUCSL)==11] 
  # it's two lagged... but want jan for each year

  cf <- annual.cpi/as.numeric(annual.cpi['2009']) #using 2009 as the base year
  # need to change cf from december to january 
  cf <- as.data.frame(cf)
  cf$pacfin_year = strftime(as.Date(row.names(cf)),"%Y")
  ftl <- merge(ftl, cf, by = "pacfin_year", all=FALSE)
  ftl$adj_revenue <- ftl$exvessel_revenue / ftl$CPIAUCSL

return(ftl)
}