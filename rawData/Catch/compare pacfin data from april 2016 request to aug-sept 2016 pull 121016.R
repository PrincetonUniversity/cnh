setwd("~/Documents/cnh/rawData/Catch/2006-2015_pacfin_data")

mydir <- dir()[2:11]

df1314 <- read.csv(mydir[8], header=TRUE)
head(df1314)
df1415 <- read.csv(mydir[9], header=TRUE)
df1516 <- read.csv(mydir[10], header=TRUE)

df14 <- rbind(subset(df1314, YEAR == 2014), subset(df1415, YEAR == 2014))
df15 <- rbind(subset(df1415, YEAR == 2015), subset(df1516, YEAR == 2015))

length(which(df14$SPID == "DCRB"))
#64690

nrow(subset(df14, SPID=="DCRB"))

# are there duplicate records?
any(duplicated(df14)) # yes
any(duplicated(df15)) # yes

# drop duplicates
new_dat <- df14[-which(duplicated(df14)),]
nrow(new_dat)
nrow(subset(new_dat, SPID=="DCRB"))
# 45057
table(new_dat$FLEET, new_dat$SPID)

new_dat <- df15[-which(duplicated(df15)),]
nrow(new_dat)
nrow(subset(new_dat, SPID=="DCRB"))
# 36206
new_dat$TDATE2<- as.Date(new_dat$TDATE, "%d-%b-%y")
new_dat$month <- as.numeric(format(new_dat$TDATE2, '%m'))
table(new_dat$FLEET, new_dat$SPID == "DCRB", new_dat$month)

new_dat$FLEET<- as.character(new_dat$FLEET)

# are there unknown vessels and tribal landings?
# Fleet type (limited entry = "LE", open access = "OA", tribal Indian = "TI", research = "R", unknown = "XX") 

new_dat2 <- subset(new_dat, FLEET == "LE" | FLEET == "OA")
nrow(new_dat2)

which(new_dat2$SPID == "DCRB")

nrow(subset(new_dat2, SPID=="DCRB"))

head(new_dat2)
unique(new_dat2$FLEET)
unique(new_dat$FLEET)
new_dat$FLEET[1]
