# NOAA VMS Data Analysis, Optimal foraging strategies and predator-prey models

## Conceptual overview and next steps

Recently I've been thinking of the social-ecological system of fishermen and fish as a predator-prey model. The mandate for management is to keep a sustainable population of both fish and fishermen 'alive.' In a predator prey model it's always most interesting to figure out whether there's a non-trivial equilibrium in which both predators and prey can survive. Same thing.

Thus I'm interested in fitting the fishermen and fish into a predator prey model and considering its dynamics. It's possible that these systems are far from equilibrium, but do we even expect them to be stable as time goes to infinity? Additionally, the stability of predator-prey problems is often dependent on the exact functional response and demographic parameters chosen. This provides an interesting source of variation to consider how different strategies/gear (predator parameters) or different ecologies of targeted fish (prey parameters) and management (mostly predator parameters again) will shift the dynamics of a system. Ultimately it would be fun (if a long shot) to look at fisheries most prone to collapse and see if there's any connection between the stability analyses of properly parameterized predator-prey models and empirical evidence of collapse. 

To do this there are 4 major steps {approaches in curly brackets}

1. Determine the strategies fishermen use {VMS data, optimal/social foraging theory}
2. Convert strategies into encounter rates/functional responses appropriate for predator prey models {modeling framework in progress with James, Andrew and Alex}
3. Analyze the predator-prey model {waiting}
4. Perturb that model (management changes, abundance shocks, climate change, whatever popular disturbance is 'in') {waiting}

For the two approaches I've begun, my thoughts are summarized below.

## Determine strategies fishermen use

The first step in building a predator prey model is to determine the strategy fishermen will use to catch fish. I want the approach to be game theoretic so that strategies are chosen in response to both the characteristics of fish and other fishermen. 

### Optimal foraging strategy

The simplest assumption in my mind is to assume that the strategy fishermen use is the optimal one given the specific parameters of their system. The $n$-armed bandit approach[^1] seemed very promising as a way to determine the optimal way a fishermen should sample fishing grounds. Because fishing is an exploration-exploitation type problem, I propose that my approach should be to adopt previous solutions to the bandit problem and adjust them for the fishing ground scenario (collaborate with Naomi's group?) to include travel time, and some degree of spatial correlation of payoff. It doesn't seem like Naomi's group's solution takes other players into account, and how difficult it would be to do so. But I'd like to incorporate other players to the degree I can (this is obviously fuzzy).

With an adjusted optimal sampling algorithm I'd like to use the agent-based model (possibly adjusted) to generate fishing data from agents behaving optimally. I'll sample the simulation data at a resolution to match that of our VMS data set. By comparing the simulation-data with known rules to the emprical data (along with simulated data of a null model -- agents moving randomly) I'll be able to investigate whether we see evidence that fishermen behave optimally. 

[^1]: This problem is one first described in regards to a gambler faced with a row of slot machines (slot machines are sometimes referered to as 'one-armed bandits'). Each machine gives a random reward from a distrbution specific to the machine. The problem is how to choose which bandit to play. The gambler's goal is to maximize the sum of winnings earned through a sequence of lever pulls. You can also incorporate traveling time as a cost (transition costs) and spatially correlated payoffs. It's easy to see that if we replace 'machine' with 'fishing ground' this is a good fit to our problem. 

#### Work plan

1. Evaluate existing solutions to n-armed bandit problem, do they
	- include travel costs (Naomi's does)
	- have spatially correlated payoffs (Naomi's does)
	- (are they) game-theoretic? 
 2. Other possible problems are that
	- payoff distributions of each arm remain constant, appropriate for fishing grounds? (should be a function of local abundance?)
	- It seems like the best arm payoff is required in order to minimize regret. Is it realistic to expect that a fishermen could know this? (Maybe best catch that was recorded that day?)
2. Make necessary changes and determine optimal sampling strategy
3. Adjust existing agent-based model (if need be) and run simulation with appropriate system paramters (gear/species ecology) with fishermen using optimal strategy
4. Sample data at same resolution as existing VMS data
5. Compare to VMS data (see workplan for VMS data analysis below)

### VMS data analysis

In order to examine the VMS data for signatures of strategies we first need a model that will infer behavior from movement data. Specifically we need to be able to infer when fishermen are traveling between fishing grounds and when they're fishing. This can be as simple as when the fishermen are moving slowly they're 'fishing' and when they're moving above some speed threshold then they're steaming. This has been done for GPS tracks of foraging animals [@Wakefieldetal2013] and with Bayesian approaches for VMS data [@Vermardetal2010; @Bezetal2011]. This will be required regardless of how we search for signatures of strategies in the data.

#### Work Plan
1. Get data (in process, Jameal has data in hand will send our way next 2-3 weeks?)
2. Port data into R: txt --> netcdf?
3. Draw maps (explore data)
	- vessel tracks
	- heat map for vessel tracks over entire space and time
	- PCA to cluster ships by fishery
		- if works well then can do analysis within a fishery to see if there are natural breakages in movement there
4. Link VMS to landings data (tickets/observer+tickets data)
4. Determine which fisheries to focus on
	- depends on coverage, management histories
5. Back out behavior from VMS data
	- still need to determine modeling approach
6. Compare empirical behavior to simulated data
	- Still need to decide on statistical approach

## Predator-prey model
This is the model we shared with you on which we're still at work. The thorniest problem is how to convert a foraging strategy into a functional response. The existing framework is as follows. 

We have a linked system of prey (fish) and predators (fishermen) that can take one of two possible harvesting strategies (details below). The two submodels are linked via the harvest of fish by fishermen (strategy affects harvest which affects abundance) and the encounter rate (lower abundance reduces encounter rate and increases variance) which affects which strategy fishermen should choose. 

![layout](layout.pdf)

Here the two strategies are producers and opportunists. Producers are fishermen who search for fish independently. Opportunists will also search for fish independently, but if another fishermen (either producer or opportunist) locates fish, will join to catch the found fish. While opportunists get the benefit of searching for their own fish and exploiting other fishermen’s discoveries, they encounter fish at a lower rate. Finders of fish are assumed to get a fixed proportion of the fish before the rest of the opportunists show up to join in the exploitation. Again we take a game-theoretic approach so the question is: when should individuals be opportunists rather than producers?

Here $\lambda$ is the encounter rate (g/day), $\alpha$ is the fraction of fish a finder gets from a clump before an opportunist shows up, $\beta$ is the reduced encounter rate (opportunists are nto as good as producers at searching ($\beta>\alpha$); $q$ is the fraction of opportunists in the population and $G$ is group size. $I_p$ and $I_o$ are the catch rates of producers and opportunists, $\pi_i$ is the payoff for strategy $i$, $p$ is price per gram fish, $w$ is costs per unit effort and $\epsilon$ is an individual's effort. 

#### Work plan

1. Determine encounter rate function, should it be folded into the strategies themselves or its own sub-model?
	- How to be mechanistic about variance in catch rather than addding a noise term? Worth doing so?
2. Evaluate social sub-model, are the strategies interesting ones?
3. When optimal foraging theory determined, needs to be converted into predator-prey form
	- This seems hard
4. Analyze model

### Notes
The existing strategies are placeholders. I'm not sure how to defend my choice of these particular strategies. I'm also not sure how far I should go in analyzing a model like this before I replace the strategies with optimal ones defined above. 

Also I wonder if it's possible to work from the predator-prey model to the simulation with the optimal foraging theory. 

## Major questions

+ It's not clear to me how to break these into seperate papers. I'm really excited about the whole idea, and not sure at what point I'm supposed to start thinking about publishable work. 
+ There's a lot of work here, I'd like advice on how to break these down into manageable chunks and any thoughts on which order seems like the best to tackle these chunks.
+ Suggestions for reading on converting foraging strategies to predator-prey type models?
+ Suggestions for places to look for help on the bandit problem? To evaluate whether it's the right approach?

### References
