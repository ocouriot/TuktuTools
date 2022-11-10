require(TuktuTools)

data(caribou)

# Try the getHerdAssignment on the pool of individuals we have, with 2 clusters
assignment <- getHerdAssignment(caribou, K = 2)

# we see that one individuals being further from the other ones
# just out of curiosity, let's try with K = 3
assignment <- getHerdAssignment(caribou, K = 3)

# the returned dataframe now contains one column with the cluster
head(assignment)
