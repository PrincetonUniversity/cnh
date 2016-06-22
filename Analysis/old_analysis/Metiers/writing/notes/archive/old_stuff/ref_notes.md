# relevant literature

**@kasperski2013income**:

+ motivations
  - growing literature suggests that fishermen should adopt portfolio approaches to manage species composition of catch to achieve the lowest variance in income for any level of expected return (2, 32-38).
  - climate change and ocean acidification is expected to change the geographic range and relative productivity of individual fisheries and may increase the volatility in productivity as well (43), the importance of diversification as a risk-reduction strategy may increase.
+ methods
  - evaluate trends in diversification over time for vessels participating in the EEZ off the WC and AK
  - analyze the relationship between diversification and variation in revenues at the vessel level.
  - construct indices of gross income diversification for individual vessels
    - use the Herfindahl-Hirschman Index (HHI, 44-45) also known as the Simpons diversity index (46). Index of concentration where higher numbers correspond to increased concentration and thus less diversification.
$$H = \sum^{s_j}_{i=1}\sum^4_{j=1}p^2_{ij}$$
where $p_{ij}$ is the percentage (ranging from 0-100) of an individuals total gross revenue derived from species group $i$ in region $j$.
    - calculate the scores for individual vessels.
    - calculate the average HHI for vessels
    - Data has annual landings and revenues from 1981 to 2010 by species, port and vessel from all commercial fisheries in the US off WC and AK.
    - Analysis is based off of 30,757 vessels for which have vessel characteristics and at least two years of documented landings and revenues.
    - Include vessels with average annual revenues greather than $5000 (adjusted to 2005 values) to exclude vessels for which fishing was not an important source of income.
    - quantify variation in year to year revenues as the coefficient of variation of annual revenues.
    - look for relationship between average HHI and CV of annual revenues.
    - Several authors suggest nonlinear relationship between risk and diversification (54,55) and nonparametric fitting of the data suggest a concave relationship between CV and HHI so use a simpel quadratic functional form and estimate the following linear regression model
    $$CV = \alpha_0+\alpha_1\bar{HHI}^2+\epsilon$$
    where $CV$ represents the coefficient of variation of gross revenues for each vessel, $\bar{HHI}$ is the mean value of $HHI$ for each vessel over the period it is included in the sample, and $\epsilon$ is the normally distributed error term with mean zero.
    - control for variation in income due to inflation by deflating annual income using the Bureau of Economic Analysis price index for personal consumption expenditures, using 2005 as a base year.  
    - examine relationship between CV and diversity both across vessels and within vessels. Look within vessels by creating time series fo different lenghts and estimating the CV of gross revenues on diversification and diversification squared using individual vessel fixed effects.
+ results
  - average HHI for all vessels exhibits no significant change or trend in diversification in time.
  - average HHI for vessels still active in 2010 shows a significant trend of increasing concentration
  - Find that vessels that were fishing in 1981 and are still in the fishery as 2010 are less diversified but still more diversified than the overal fleet fishing in 2010.
  - average levels and trends of diversification differ for vessels with different levels of average revenues. There is no significant trend in HHI for vessel with average income between $5000 and $25000. Vessels with average revenue between $25,000 and $100,000 tend to be more diversified than vessels with lower revenues, and there is a significant decrease in diversification of fishing revenue from 1981 to late 1990s and then leveling off. Vessels with > $1000,000 in average revenues are still more diversified in average revenues but exhibit a significant trend of decreasing diversification, particularly from 1991 onward.
  - smaller vessels (< 40 ft) tend to be less diversified than larger vessels.
  - medium and larger vessels are much more diversified than the smaller vessels.
  - after ITQ implementation in AK halibut and sablefish fisheries there was a jump in concentration for the vessels involved in those fisheries.
  - also an increase in concentration for WC groundfish fleet after introduction of more stringent catch and effort controls on rockfish species.
  - annecdotal evidence suggests that the effect of 2011 ITQs in groundfish made the fleets more diversified.  
  - confirm a decreasing trend in diversification over the last few decades
  - show that diversification is correlated with a reduction in the variation of revenues
  - also examine the ratio o fthe minimum revenues to mean revenues for a vessel. This min/mean ratio is a measure of the potential for a very bad outcome, which is important for smaller owner-operated vessels.
  - with the exception of AK crab, all groups have a dom-shaped relationship between CV of revenues and HHI such that small amounts fo diversification are associated with an increase in the CV of revenues, but additional diversification lowers CV of vessel revenues at an increasing rate.
  - for some groups of vessels, including large vessels and participants in the AK and WC groundfish, CV declines with any significant level of diversification.
  - these results are consistent both across vessels and within vessels.
+ discussion
  - reasons diversity of income is not desireable:
    + technological change for increased efficiency may increase incentives to specialize
    + integration of global seafood markets may reduce asynchronous variation in prices, which weakens incentives to diversify
    + diversification could increase physical risk if it involves fishing in less-familiar areas or further from port
    + diversification may be costly, if different gear is required, licences or quota.
    + for many fishermen the variability in revenues may be less important than the variability in costs, which may not be reduced by fishery diversification
    + also may be that vessels participating in many fisheries are not optimized for any one fishery. So it may be less efficient than more-specialized vessels creating a tradeoff between average profitability and risk reduction. Although the potential tradeoff between risk reduction through diversification and efficiency loss has been noted in the literature (18, 29), it has not been documented and quantified empirically. And doing so would be an important extension.  
+ questions
  - don't understand the AK crab, sablefish, WC groundfish distinctions. Do they pull out all trips that have majority of those species? Or do they only have access to annual revenue, so they examine by species? In which case they don't consider the rest of the revenue which might be brought in?
+ summary
  - look for trends in diversification over time plotting annual diversity averaged aross all vessels each year. Also break this up to different sized length vessels, different revenue classes and vessels which participate in different fisheries.
  - look for a relationship between diversity and year-to-year variation by calculating the mean HHI across all years for a vessel and the CV of annual income across all years. Do this for different sized vessels, different income classes, and vessels which participate in different fisheries.
  - not clear how they split revenue, whether by species or what. no details on that.

@sethialaskanincome

+ motivations
  - re-authorization of Magnuson-Stevens Act requires managers to consider the impacts of regulation on fishing communities. Particular interest is in undersatnding a community's ability to withstand and adapt to a range of stressors (3,4).
  - fishing communities may differ in the opportunities available to diversify their portfolio of fishing revenue flows due to differences in their proximity to commercial fisheries or differences in fleet characteristics which promote or constrain participation in diversified fisheries
  - predict that the larger the number of assets in the portfolio, the greater the benefits of diversification, but the effectiveness of the portfolio diversification depends on the correlation between asset returns. Benefits are enhanced if assets are negatively correlated (17,18).
+ methods
  - use CFEC commercial fishing permits to reconstruct fisheries participation and home address for community designation
  - use community-level gross fishing revenues data, discounted to 2010USD ushing the Anchorage Consumer Price Index.
  - community attributes were
    - population size
    - number of different fisheries in which community members participated and Simpson's diversity index
    - fleet investment: sum of length of vessels registered to a community per active year
    - proxy of fishermen skill by number of years of commercial fishing tenure
    - location (lat/lon)
  - characterize economic risk as revenue variability
  - examine revenue variability across different alaskan communities
  - identify characteristics that correlate with revenue variability
  -
+ results
  - find that catch portfolio size and diversification are related to revenues variability after controlling for community size, fleet investments (length and horsepower) and fishermen experience
  - portfolio size and diversity appear to be related to the number of local fisheries
+ discussion
  - suggests that a community's fishing portfolio, and thus exposure to risk, is predetermined by its location.
+ questions

# My work
Motivation to understand revenue variability becuase of Magnuson-Stevens focus on fishery, possibly increased variability with climate change, and evidence that ability to diversify may be "predetermined" by location.

There is also an open question as to what explains the lack of diversity we may see. It could be that vessels which participated across gears are less efficient at either, so there is a tradeoff between efficiency and exposure to risk. It also may be true that the catches do not vary independently or asynchronously. Such that diversifying across fisheries doesn't reduce varability.

 We examined at the level of ownership to see if this had the expected relationship between diversity and reduced risk.

## Methods
Calculated mean annual diversity across years for each vessel. Compared to coefficient of variation in between year revenue. Split by vessels which trawled (have to be bigger) and vessels that never trawled (have to be smaller). Split also by mean annual revenue. And examined whether these characteristics predicted revenue variability (length proxy, port location).

Then examined whether there was any evidence for specialist premium. That vessels which specialized could compete effectively with generalists. Do this by examining vessels with highest class of income (most income, most fishing days?) to focus on vessels which likely have mostly fishing-income. (For example, 20,000 to 500,000, 500,000 to 1,000,000 and > 1,000,000).

According to McKelvy et al. would expect that specialists make up base of fishery, but when profits are good, generalists would enter. So if we look at mean revenue by week, as mean revenue increases, would expect "generalist" fishermen to enter the fishery. Would want to look at the number of "specialist" fishermen landing each day, and the number of generalists fishing. definitely will probably see this with crab.


------

we look to see whether the expected relationship between diversity and variation in revenues is present.

find the CV for annual revenue across years and mean Simpson's index. 

if it's not, we seek to understand what predicts variability in revenues.

and we consider specialist-generalist implications?
