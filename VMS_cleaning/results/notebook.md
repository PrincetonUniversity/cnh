# Notebook of work on VMS_cleaning

## 2014-03-02: Cleaning VMS data
`author: Emma`

### Basic formatting
The `format_data.R` does the following

1. Takes `/VMS_cleaning/data/VMS.txt` as the input
2. Strips whitespace
3. Converts the latitude and longitude from minutes/seconds to decimal degrees
4. Matches the vessel ID and doc ID number using `West_Coast_Vessels.csv`
5. Adds a description of declarations using `Declarations.csv`
6. Outputs `/VMS_cleaning/results/2014-03-02/VMS_woDups.csv`

`VMS_woDups.csv` should be the default data file to use for VMS data analysis until I hear from Alan Haynie about the methods he uses to filter and clean his VMS data. 

However, the `format_data.R` doesnt' seem to completely drop the duplicated name/timestamp duplicates. When I attempt to classify a VMS track as a adehabitat time series it tells me there are duplicates, and indeed I can still find them. So need to reinvestigate the duplicate part of this code. Have reinvestigated: satisfied that I'm successfully marking all points that are duplicated (by searching for non-unique vessel names and time stamps), and then dropping the extra copies. 

Remaining issuess

+ I haven't spent time carefully deciding which duplicated values to drop, on qualitative inspection, the lat/longs look extremely close to one another
+ I have only searched with `Vessel_Name` and `Date_Time` fields. This should be repeated with the `Ship_Number` and `Doc_Number` as there look to be multiple vessel names associated with these ID numbers and that will change how duplicates are recognized. 

### Duplicated vessel names and IDs
Problem: there are a number of vessel names associated with multiple coast gaurd ID numbers and vice versa. Looks like I need to be careful how I'm sorting. There's an open screen detached in ursus that has the VMS data all processed to the duplication removal point. Return to that. 

`parseDifNumName.R` examines this problem. Find that the duplicates between IDs and names will be found differently depending on which I use to search. This is still unresolved and needs more work. But is related to the above problem of removing duplicates. 

## 2014-03-02: Examining Duplicate Data
`duplicate_data.R` 
