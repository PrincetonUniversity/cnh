# Guide to Data
This is raw data that is related to VMS data. This includes the raw VMS data, port data, and other declaration information. 

`VMS*.log` files are the raw VMS data that was given to me (Emma) by Jameal. They include the vessel names, vessel IDs, speed, direction, lat/lon, and declaration. There is at least one file that is filled with `SQL` errors, and this gives an error when processed by `format_data.sh`

`Declarations.csv` provide key for delcaration ids, provides ID and description of declaration. From Jameal

`West_Coast_Vessels.csv` provide the key for ship number, vessel ID, and vessel name. From Jameal.

`wc_Fishing_Communities.kmz`; `wc_fishing_communities.csv`; displays the locations of fishing communities and fishing ports identified by the National Marine Fisheries Service Northwest Fisheries Science Center Publication,  Community Profiles of West Coast and North Pacific Fisheries, and in the Pacific Fishery Information Network (PacFIN) database maintained by the Pacific States Marine Fisheries Commission. Found [here](http://www.pcouncil.org/habitat-and-communities/area-closures/). The `csv` file is made by removing the community names and lat/lons and storing them in a comma delimited format. 
