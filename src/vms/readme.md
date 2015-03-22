01_processing_VMS.R does the following

+ reads in a fixed-width file to a data.frame
+ converts degrees-minutes-seconds to decimal degrees
+ converts the date.time to `POSIXct` format
+ converts avg.speed and avg.direction to numeric
+ renames the columns with lower-case and periods for spaces
+ saves to an RDS file (don’t need to continue changing classes of vectors this way)

02_cleaning_data.R

+ takes the processed `.RDS` files and collates them into a giant data frame (it takes a long time)
+ removes any points with vessel ID and date.time information that is duplicated (i.e. the same vessel at the same time in -possibly- two different places).
	+ *Important* this removes *all* the duplicates, not just one of the two duplicated points.
	+ This is because I found that duplicated partners are both error-prone, and it’s not obvious how to figure out which of the duplicated partners is the “good” data point.
	+ To be conservative, I remove them all
+ Find onland points by overlaying `2_coastline.Rdata`. See that file for how that polygon is created
+ remove any points that have longitude outside of the range -180,180
+ split vms data by vessel doc.num and save as `vessel_tracks*.RDS`. This is important because can’t break by vessel name. There is more than one vessel with the same name (i.e. Pioneer).
+ Processes `vessel_tracks*.RDS` one by one to remove sequential on-land points. Just leave book-end onland points.
+ save each vessel track as `v_#####.RDS` where `#####` is the vessels `doc.num`. 

03_matchMetier.R

+ loads tickets, vms tracks (from `/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/02_cleaned`)
+ go through each track to see if `doc.num` matches `drvid` in landings ticket
+ if it does, make sure that at least one VMS observation is within the range of landing dates (i.e. one vessel I have VMS for in 2013-2014, but by then is in alaska and the landings data I have for it are 2009-2010)
+ save each track that matches above criteria in `/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/03_matchedMetier/` with prefix `v_` and then doc.number as `.RDS` file.
+ for each of these tracks I subset landings and find all the unique days on which they land. This is different than landings because often there is more than one landing per day.
+ to address multiple landings in a day, there are two possible trip_id fields (`trip_id1` and `trip_id2`).
+ go through each unique day (starting at last), make sure that it’s in the VMS data, and assign all points before it as that trip.
+ by repeating this to the earliest trip, I successively overlay new trip ids as they happen.
+ if there are two trip ids, it goes into second trip_id slot.
+ then after finished laying down trip_ids, merge with landings data to get the two metiers
+ combine all tracks into a single dataset that now has trip id, metier plus vms data. (kept trip-id in case I want other meta-data associated with that trip. gear for example)
+ save as `/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/04_with_trip/VMS_catch.RDS`
