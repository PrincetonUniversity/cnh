Original file is `wc_Fishing_Communities.kml`. This is a Google Earth file. Can read it directly into R, using `getKMLcoordinates()` in `mapdata` library, but wasn't importing names. Instead just used bash to extract names, latitude and longitude to a text file using following shell script.


get out anything between <name>..</name> tags (in this case port names)
```
sed -n 's%.*<name>%%; s%</name>.*%%p' wc_Fishing_Communities.kml > ports.txt 
```
have to remove the first 3 lines of this file since it has header info in it

```
tail -n +3 ports.txt > ports_temp.txt
```
then do same, but for longitude and latitude

```
sed -n 's%.*<longitude>%%; s%</longitude>.*%%p' wc_Fishing_Communities.kml > long.txt


sed -n 's%.*<latitude>%%; s%</latitude>.*%%p' wc_Fishing_Communities.kml > lat.txt
```

combine the files together
```
paste ports_temp.txt long.txt lat.txt> wc_fishing_communities.txt

```
These are a list of fishing communities identified by NOAA. The original technical report [is here](http://www.nwfsc.noaa.gov/assets/25/499_01082008_153910_CommunityProfilesTM85WebFinalSA.pdf). 
