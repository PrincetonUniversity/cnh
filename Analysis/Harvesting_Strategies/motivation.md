# Harvesting Strategies - Integrating behavior into predator-prey modeling and it's implications

Introduction - what am I working with?

+ Fishermen going after fish
+ I think that the foraging strategies vessels use should be predictable based on the ecology of the prey, type of gear, and type of management the vessel is under.
+ For example: you will be completely unsurprised to hear that tuna fishermen make levy-walk like trips and often fish in small groups. While groundfish fishermen go to the same spots and are intensely private about their patch locations and information relating therein.
+ So here I'm thinking that the system constraints broadly shape foraging strategy.
+ But there doesn't exist a broad conceptual framework for predicting under what system conditions we should get what type of foraging strategy.

Definitions - but what is a foraging strategy?

+ I don't know.
+ Ideas: the three major axes are how you specialize in space, time and how social you are.
+ I'm sort of unsure whether or not to think about prey choice as a foraging strategy and instead more take that as a given to allow choosen prey distribution to affect foraging strategy.
+ What I see in the literature under "foraging strategy"
  + determining  (or evaluating that) a set of rules that maximizes your encounter rate (bacteria: Stocker 2012)
  + what types of communication facilitate effective search (Torney et al. 2009, Berdahl et al. 2013)

Existing modeling framework for foraging strategies

+ producer scroungers (only tells you for social or not, assumes a single patch and random distribution)
+ patch choice (single forager, no social information, small patches)
+ diet choice (single forager, no patches, random encounter with food items)
+ group size (can incorporate patches but only tells you when you should share food, not about particular patch foraging choices)
+ How to weave together?

Possibilities

Allison Shaw’s model that related resource distributions to sources of information. But doesn’t exactly match the consumer-resource system (why?)
Our ABM tells us that we should expect more social behavior when particular distribution of resources.
Colin/Berhdal’s model of signalling
Others?

Introduction

+ Most simple predator prey model is Lotka Volterra, which has no behavior in it
  + approximates interactions by chemical laws.
  + Find theoretical ecology paper about this
+ Turns out that when you add behavior, you can get very different population-level outcomes. For example.
  + fryxell lion paper (makes pred-prey models more likely to be stable) [@Fryxell:2007cz]
  + abrams harvest traps (makes system susceptible to catastrophic collapse) [@Abrams:2012tn]
+ Exists a large body of theoretical literature which figures out implications of adding new behavior and re-deriving functional responses.
+ Problem is no way to constrain parameter space!
+ Solution: use my fishing data to empirically quantify foraging behavior of vessels, re-derive functional responses and see if they have any implications

Details

+ For fishermen, typically the biggest challenge is _finding_ the fish. Thus I will focus on considering the encounter rate first.
+ Lotka Volterra, and almost all other pred-prey models begin with the assumption that predators and prey are _randomly_ distributed. For most predators, we know this is often not the case. And for fishermen, particularly so.
+ Lotka Volterra also assumes that individuals encounter each other randomly, according to ideal-gas laws [@Gurarie:2012fo]
+ Instead fishermen have patches they visit, either based on static characteristics (bottom substrate), or dynamic (temperature, current conditions), or both.
  + Increasing recognition that ecological communities occur in fragmented and spatially heterogeneous environments, and is especially of concern as habitat fragmentation occurs and marine spatial management picks up steam.
  + Fisheries have a  long history of applying lotka-volterra models without space.
  + Even when meta-community models are considered, frequently assume random movement from patch to patch.
  + Interested in fishermen (predators) and fish (prey). Fishermen don't move randomly from patch to patch.
  + Dynamic conditions have two broad scales: seasonal and sub-seasonal. i.e. some broad movements from season to season, but finer scale week-to-week depending on weather (probably)
+ The question is, will there be fish in these patches at a given time?
  + Could consider predator and prey habitat use and examine degree of overlap in space and time.
+ Probably true that a type-II functional response is reasonable within a patch. Although not sure how I would know this from the data.

+ Of course this means moving across scales. The ways people link two different scales is through **functional responses** or **patch use**[^1]
  + Brief review of a functional response (probably not terribly needed with lab-tea crew)[^2]
    + Fryxell lion paper (rederives functional and changes encounter rate and )
  + Brief review of patch visiting
    + Abrams paper that finds harvesting traps
+ Extensive literature on how you might re-derive functional responses, and different outcomes
+ Problem is no way to constrain parameter space
+ Solution: use fishing data to empirically constain parameter space. (Hopefully obvious from introduction about how to do that)
+ Interested in implications of new population-level parameters.

[^1]: Need to work on this first, are these the general two ways that behavior moves into population level modeling?

[^2]: Relationship between prey density and per-predator prey consumption; Solomon 1949; Holling 1959 @Hunsicker:2011cg

Approach for linking scales so far has been to derive functional-responses from individual behavioral patterns. And then estimate these rates from data.

To incorporate new behavior, you have to re-derive the functional response in order to figure out the proper parameters to measure.

My focus is figuring out what types of behavior are important and relevant ot a functional response.

Most functional responses focus on specialist predators (i.e. holling). But more complex behaviors have been incorporated

+ predator-dependent responses (Abrams & Ginzburg 2000; Walters & Kitchell 2001; Essington & Hansson 2004) which occur when the total consumption of prey is decoupled from predator abundance due to processes such as predator interference (DeAngelis 1975), prey refuge use (Abrams 1994), or spatial heterogeneity in predator and prey abundance (Keeling et al. 2000) [@Hunsicker:2011cg].

Maybe unrelated

+ two steps: quanifying foraging strategy (how long to stay at a patch, how to decide when to move, how to decide where to go?). Given these decisions, what are implications for population level predation rates? And what are the implications of those for prey?
+ Leads me to try to figure out, how should you weight different sources of information.

Use Allison's framework [@Shaw:2013ks]

+ First, under what type of ecological conditions can an individual maximize its fitness by searching for new patches (i.e. exploring) rather than keeping to territory (i.e. exploiting).
+ Second, what types of information use to guide their movement in either strategy.

She finds that migration is selected for when resource distributions are dominated more by seasonality than by local patchiness, and residence is selected when the reverse is true.

Could be the same for vessels.. But what are the operational definitions of this?

Moving between patches is both a movement behavior and an information usage strategy: we find that different types of information are important depending on the ecological conditions and availability of information.

Finally present empirical support for main results, drawing from fishing patterns from a variety of fisheries.

Two scales of movement: a seasonal one and a intra-seasonal one. Seasonal one could be considered migration, the intra-seasonal one is choosing between patches. This could be figured out using autocorrelation? Expect to see short term autocorrelation, but also yearly lags.
