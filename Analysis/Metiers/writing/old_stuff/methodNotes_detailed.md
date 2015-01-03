# Detailed methods notes

## Methods

+ Fish tickets are not unique between years, hence trip_ids which fuse ftid and year
+ Filter out rare species
+ Use just the retained species to cluster over
+ First I build a training dataset in which I define metiers. Do this with both 2010 and 2012 data to check for differences depending on base year chosen
    + Partition trips by grgroup and year.
    + For each grgroup/year set of trips, I collate the amount of species each trip returns
    + I calculate the BC index of dissimilarity between each pair of trips in the grgroup, then transform it to a measure of a similarity by taking 1-BC. Now groups that are identical have a similarity of 1, those that are completely dissimilar have a similarity of 0
    + Use those values of similarity to build a undirected, weighted network. Where nodes are trips, and the weight of edges is the similarity score.
    + For each trip network, I run infoMap to find modules. Returned modules are groups of trips which belong to the same metier.
+ Once I have a training set, I use a k-nearest-neighbor algorithm to assign other trips to the most similar metier
    + use only the nearest neighbor because some of these fisheries are very small, don't want the bigger fisheries to swamp. If I used the majority vote of 10 nearest neighbors and the "correct" fishery only had 3 other trips in it. Then I'd start pulling from something far away.
    + To determine if the base year biased my results, classified 2009/2011/2013 trips using 2010 and 2012 and compared results using the adjusted rand index.
    + ARI ranges between 0 and 1, where 1 is identical classification and 0 is totally dissimilar. Almost all years/grgroups returned an ARI of .9 or above. So went with 2010.

## InfoMap

+ Information-theoretic approach
+ Use probability flow of random walks on a network as a proxy for information flows in the real system. Decompose the network into modules by compressing a description of the probability flow.
+ Result is a map that simplifies and highlights the regularities in the structure of relationships
+ Using an information-theoretic approach, can measure how much detail is lost in the process of simplification, which allows quantification of the tradeoff
+ A group of nodes among which information flows quickly and easily can be aggregated and described as a single, well connected module.
+ Use a random walk as a proxy for information flow, because it uses all the information in the network and nothing more.
+ Find efficient code to describe a random walk on a network. And fidnign community structure in networks is equivalent to solving a coding problem.
+ Goal is to partition the network that minimizes the description length in the map equation.
+ It's infeasible to check all possible partitions to find the one tha tminimzies the description length, so use computational search.
    - compute fraction of time each node is visited by a random walker using the power method by using a deterministic greed search algorithm.
        - repeatedly merge the two modules that give the largest decrease in description length until further merging increases description length.
    - refine results with simulated annealing approach or the heat-bath algorithm.
        - starting heat-bath algorithm at several different temperatures, select the run that gives the shortest description of the map.
+ In order to deal with directional network, introduce a small "teleportation probability" in the random walk: with probability tau, the process jumps to a random node anywhere in the network, which converts the random walker into the sort of random surfer that drives Google's PageRank algorithm.
+ Clustering results are robust to the particular choice of fraction tau.
+ In general, the more significant the regularities, the higher tau can be before frequent teleportation swamps the network structure.
+ Choose tau = 0.15 corresponding to the well known damping factor d = 0.85 in the PageRank algorithm.
