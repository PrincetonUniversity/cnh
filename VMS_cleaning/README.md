This directory is for figuring out the process of cleaning and organizing the VMS data. There are problems I'm discovering as we go along. These include

+ Duplicates (either entire rows duplicated, or the vessel being recorded in the same place and the same time)
+ Vessel names associated with more than one ID number, ID numbers associated with more than one vessel name. 
+ VMS pings that happen much more rapidly than every hour

Other things to check/organize: 

+ Are inferred speeds reasonable? (i.e. distance and time between doesn't mean a vessel travelled > 700mph to get there)
+ Flag for beginning and ends of trips (enter and leave ports). Possibly we need to build a new dataset that is a trip log that could be matched with the observer or catch data
+ Flag trips which leave the EEZ 
