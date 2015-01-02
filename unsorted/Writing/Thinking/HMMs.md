


HMMs
========================================================
I'm pretty sure some variant of a hidden-Markov model is appropriate for my data, so the questions are 

1. How do they work?
2. Which variant to use?
3. What package/library to use in which language (R, Matlab, Python)?

## How do they work 

### Markov processes
See [here](http://statisticalrecipes.blogspot.com/search/label/Markov%20chains) and [here](http://stats.stackexchange.com/questions/165/how-would-you-explain-markov-chain-monte-carlo-mcmc-to-a-layperson) for a good introduction to a Markov process, and [here](http://statisticalrecipes.blogspot.com/search/label/Markov%20chains#uds-search-results) for a discussion of stationary distributions. Although I still don't understand the math for calculating the stationary distribution. And finally an example using the `RHmm` package [here](https://gist.github.com/keithshep/438217).


### Notes on <a href="http://dx.doi.org/10.1109/5.18626">Rabiner (1989)</a>

+ Processes generate signals, signal model translate back from the signal to the process. 
+ There are two types of signal models: deterministic and stochastic. An HMM is an example of a stochastic signal model

#### Discrete Markov Processes
Reviews the theory of discrete Markov chains and shows how the concept of hidden states, where the observation is a probabilistic function of the state, can be used. Illustrates with two examples (coin-tossing and ball-in-urns system).


#### Three fundamental probelms of HMMs
1. Finding the probability of the observed sequence of events given a particular HMM model
2. Determination of the best sequence of model states
3. Fitting model parameters

#### Examples of HMMs
Discuss the various types but also the model features such as the form of the observation density function, the state duration density, and the optimization criterion for chooosing optimal HMM parameter values. 

#### Issues in implementing HMMs 
Includes topics of scaling, initial parameter estimates, model size, model form, missing data, and multiple observation sequences. 

