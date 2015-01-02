# Guide to formatting raw VMS data
#### Author: Emma; Date: 2014-03-01; Last-Modified: 2014-03-4, Emma Fuller
This provides a walkthrough of how the VMS data goes from the `.log` files to `VMS.txt` found in `/VMS_cleaning/data/`. To run this analysis navigate to `/CNH/src/` and type  `sh format_data.sh` which does what this file describes. The end product is `VMS.txt`, which is the entire VMS dataset transformed into a comma-delimited text file. It still needs processing in `R` before ready for analysis. 

In summary `format_data.sh` does the following

1. Changes a fixed-width format into a comma-delimited one. This is flexible and can count the number of fields present.
2. Combines all individual `.log` fixed-width files into one text file called `VMS.txt`.
3. Converts the minutes/seconds lat/lon measurements into seperate fields so R can convert them to decimal degrees

## Detailed Notes

**NOTE**: Some of the data files have fields of different lengths (35 characters instead of 50 for the first field). Now instead of assigning the field lengths I'm finding the first dashed row, finding the length of each field (have to add 1 to this length since the comma I will add will add one character) and then using those variables for the new field widths. This code is as follows

```
linenumber=$(grep -nm1 '\---' test | cut -f1 -d:)

# length of fields
first=$(sed -n "${linenumber}p" test | awk '{ print length($1);}')
second=$(sed -n "${linenumber}p" test | awk  '{ print length($2);}')
second=`expr $second + 1`
third=$(sed -n "${linenumber}p" test | awk  '{ print length($3);}')
third=`expr $third + 1`
fourth=$(sed -n "${linenumber}p" test | awk  '{ print length($4);}')
fourth=`expr $fourth + 1`
fifth=$(sed -n "${linenumber}p" test | awk  '{ print length($5);}')
fifth=`expr $fifth + 1`
sixth=$(sed -n "${linenumber}p" test | awk  '{ print length($6);}')
sixth=`expr $sixth + 1`
seventh=$(sed -n "${linenumber}p" test | awk  '{ print length($7);}')
seventh=`expr $seventh + 1`
eighth=$(sed -n "${linenumber}p" test | awk  '{ print length($8);}')
eighth=`expr $eighth + 1`
........
awk -v FIELDWIDTHS="$first $second $third $fourth $fifth $sixth $seventh $eighth" -v OFS=',' '{$1=$1 ""; print }'  test >  try
```

still will remove the header lines (lines starting with 'VESSEL' and '---'); need to split the latitude and longitude up and replace degree symbol with comma; replace periods in date_time with dash. 

Find are remove header lines (lines that start with Vessel or ----)

```
grep -v '^VESSEL NAME\|---\|NAME ' try > temp
```

Remove empty lines (lines with characters less than 2)

```
sed '/.\{2\}/!d'  temp > stripped
```

replace degrees with commas

```
perl -i -pe 's/\xB0/,/g' stripped
```

strip whitespace (greater than 2)

```
perl -p -i -e 's/ +/ /g' stripped
```

replace periods with dashes only in date and time column (7th)

```
awk -F, 'BEGIN{OFS=","}{gsub("[.]","-",$7)}1' stripped > dashed
```

**Addition:** To help with diagnosing data import problems I'm appending a column that gives the file name so I can quickly target raw data that are not properly formatted after my R import script. This adds a column to the file with the filename. 

```
sfx=$(echo "$f")
sed "s/$/ ,$sfx/" dashed  > named
```
to loop across all files I've written a bash script called `format_data.sh` which is in `/CNH/src/`. To use it, navigate to the directory and type

```
sh format_data.sh
```

This will go into `CNH/Data/VMS/` make the fixed width files comma-delimited and then concatenate into a single file called `VMS.txt` and put that in `CNH/Analysis/Data` and clean up any intermediate files. 
There is still a lingering problem that this way someone leaves blank lines where the headers used to be. Can still find and clean that easily with R, but not ideal.
