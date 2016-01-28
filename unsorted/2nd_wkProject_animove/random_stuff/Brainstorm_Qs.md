# 2 week project

Q1: Where does fishing occur?
Q2: What is unique about fishing areas?

4th order RSF: Look for shifts in behavior, ideally looking to see different types of movement, whether they correspond to "fishing", "traveling", "searching" behavior. What enviornmental covariates best predict distribution of presence and behavior. How is this related to catch value/volume/composition. 

Data available

+ GPS locations approximately every hour for > 1000 fishermen over 5 years
+ Catch composition per trip (volume, composition, value)
+ Observer data: can confirm whether some behavior associated with fishing?

Methods ideas

+ Behavior categorization:
	+ BCPA to look for breakpoints? Then random forest to see if there are distinctive behavioral classes? If time, are there habitat covariates that are related to behavorial classes?
	+ What about dynamic brownian bridges?
+ Environmental covariates
	+ RSF to relate fishing locations to biophysical characteristics
	+ Consider individual, seasonal effects

Motivation

Management is managing people, not fish. When systems change (climate change, new management), would like to predict how resource users will shift. ie, would like to predict how fishermen effort will change.

-> Are fishing foraging strategies predictable?
-> Hypothesize it will depend on ecology, gear, management
-> Test: systematic variation across prey ecologies, gear, management regimes

Required: Quantify variation


Main questions

+ How to deal with long term visits to port? Scale of analysis should be within trip, or across trips? 


Q: What environmental covariates are associated with "habitat use" by vessels? How does this vary with individuals?

Data available:

- GPS locations approximately every hour for > 1000 fishermen over 5 years
- Substrate (hard, soft, mixed, inferred rock)
- Bathymetry
- Modeled species abundance for 6 focal species (Darkblotched rockfish, greenstriped rockfish, yelloweye rockfish, petrale sole, sablefish, longspine thornyhead) and 5 additional ones (dover sole, lingcod, shortspine thornyhead, pacific ocean perch, chilipepper rockfish)
	+ modeled based on depth, bottom temperature, sediment grain size, distance to rocks
	+ possibly included SST and chlorophyll a
	+ Gives probability of occurrance (0-1) and mean predicted abundance (kg/ha)
	+ But also doesn't cover the entire EEZ, just shelf area.. So not sure I can use it?
	+ Also not sure if it's by year?
- MPAs (areas where fishing is restricted, important for making null sets)
- ROMS: temperature, SST, other temps? But looks like NWFSC often uses MODIS Aqua for chlorophyll a, SST, so perhaps should do that. 
- Non-fishery impacts from Halpern et al. 2009
	+ Atmospheric pollution
	+ Inorganic pollution
	+ Organic pollution
	+ Ocean-based pollution
	+ Nutrient input
	+ Sediment decrease
	+ Sediment increase
	+ Ocean acidification

Data prep 
+ How to make sure that I'm not biasing results by having layers that don't cover all places?
+ How to make sure all are comparable?
+ How to do null sets
+ How to add variation among individuals?