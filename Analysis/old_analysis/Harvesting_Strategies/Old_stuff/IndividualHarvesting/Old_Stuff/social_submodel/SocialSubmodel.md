Simon pressed us for a more mechanistic model of how the strategy a fishermen chooses affects other fishermen in the population. So it seems like we need three sub-models: 

1. Interactions between fishermen: social foraging submodel (details below)
2. Interactions between fishermen and fish: searching/fishing submodel (Poisson process - part of what Alex is working on?)
3. Fish population dynamics: Population submodel. (Logistic growth, already in place) 

### Social Foraging
We originally considered two strategies: cooperating and independent searching which were assumed to have some degree of grouping. Ideally it seems like we'd like a system in which there were three strategies: those that participated in a coop (cooperators), those that searched independently (producers) and those that took advantage of other's discoveries (opportunists). Because previous authors have considered when grouping is the optimal strategy (@ClarkMangel:1979a; @MangelClark:1983) I'm dropping the grouping aspect and instead starting with two strategies: producers and opportunists. Producers are fishermen who search for fish independently. Opportunists will also search for fish independently, but if another fishermen (either producer or opportunist) locates fish, will join to catch the found fish. While opportunists get the benefit of searching for their own fish and exploiting other fishermen's discoveries, they encounter fish at a lower rate. Finders of fish are assumed to get a fixed proportion of the fish before the rest of the opportunists show up to join in the exploitation. Again we take a game-theoretic approach the question is: when should individuals be opportunists rather than producers?

#### Producer - Opportunist approach (deterministic)
Parameters are: 

$\lambda$ = encounter rate (g/day)

$\alpha$ = fraction of fish that a finder gets from a clump before opportunists show up

$\beta$ = the reduced encounter rate - opportunists aren't as good as producers at searching ($\beta < 1$)

$q$ = fraction of opportunists

$G$ = number of fishermen

Catch rate of producer is 

$$I_p = \alpha\lambda+\frac{(1-\alpha)\lambda}{1+qG}$$

Catch rate of a opportunist is 

$$I_o = \alpha\beta\lambda + \frac{(1-\alpha)(1-q)G\lambda}{1+qG}$$

Converted to the deterministic framework for fish we have the encounter rate depending on the total abundance of fish _R_, the catchability, and the amount of time spent out on the water. 

$$\lambda = \text{encounter rate } \rightarrow q \epsilon R$$

Because effort is the same for producers and opportunists the total harvesting rate is a sum of the catch rates for each strategy

$$H = n((1-q)I_p + qI_o)$$

#### Assumptions
This setup requires 

+ All opportunists know about all fish discoveries
+ All opportunists can make it to all fish discoveries (travel time is small relative to the amount of time it takes to harvest fish)
+ Fish clumps are found sequentially 
+ Fish clumps are shared evenly between all opportunists + finder (no assymetry beyond initital fraction that goes to finder)

### Moving to a stochastic framework - risk minimizing
This will require re-writing the catch rate function slightly (**yet to be done**), but an overview follows: 

The foundational metric should be the payoff ($\pi$) a fishermen can make (profits -- cost), and switching should hinge on which strategy can offer the greatest differential. Here we assume fishermen attempt to minimize the probability of bankruptcy. 

Above is a deterministic setup with the encounter rate depending only on the abundance of fish. Ideally we'd like to incorporate variability in catches and understand how the variance can affect strategy fitness. Previous work in social foraging have used z-scores to measure the fitness of a given strategy. In this set-up there is an assumed minimum caloric amount that a forager must meet in a time-period $R$ (or minimum amount of catch). The probability of starvation (or bancrupcy) is calculated based on the expected catch and its variance in the following metric

$$ \text{z}_i = \frac{\text{E}(\pi_i)-R}{\text{Var}(\pi_i)}, $$ 

with $i = p, o$ (producers, opportunists). Thus when the z-score is negative then the strategy is not expected to result in the minimum required payoff and individuals should shift away from that strategy. This fits in with the replicator equation (with $q$ representing the fraction of opportunists)

$$ \frac{dq}{dt} = (1-q)(z_o - z_p) $$

Thus we can use a Poisson process to determine the encounter rate (and thus calculate the mean and variance), or whatever variant we've dreamed up. 

Worthwhile to note that this set up doesn't seem well adapted to deal with fishing in which the travel time may be significant and not worth moving if you get a small fraction ($\alpha$) of the catch. 

### Further additions - entry and exit decisions
The _z_-score offers an absolute measure of the success of a fishery: if positive fishermen are making more than they're spending and negative if not. In the above the _z_-score is used to decide how fishermen should switch between strategies, but the absolute value of the _z_-score could determine entry and exit decisions. In particular, if the _z_-score for both stratagies is negative, while it makes sense to switch to the stratagy with the least negative value, it may be actually in a fishermen's best interest to leave the fishery entirely. Similarly, if both strategies are positive, then this may tempt fishermen to join the fishery.
