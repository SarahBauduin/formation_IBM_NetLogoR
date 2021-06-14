## FOREST MODEL ##

library(NetLogoR)
rm(list=ls()) # reset the R environment
set.seed(1234) # same seed so that everybody has the same results

# Create a forest of 25 cells representing 25 plots where trees can grow
# 1 tree per cell (= patch)
forest <- createWorld(minPxcor = 0, 
                      maxPxcor = 4, 
                      minPycor = 0, 
                      maxPycor = 4)
plot(forest)

# Trees are represented by their age (or it can be their size)
# Give a random age (between 1 and 10) to the 25 trees
# NLset() to assign or modify pacthes' value
forest <- NLset(world = forest, 
                agents = patches(forest), 
                val = sample(x = 1:10, 
                             size = 25, 
                             replace = TRUE))
# sample() (not from NetLogoR) does random sample and permutations
# Examples:
sample(x = 1:10, size = 25, replace = TRUE)
sample(x = 1:10, size = 5, replace = FALSE) # cannot take more than what there is available without replace
sample(x = 10, size = 25, replace = TRUE) # carreful ! x = 10 means x = 1:10 !!! Always check x length

plot(forest, 
     breaks = 0:15, col = rainbow(16)) # keep the same color scale for all plots
# What's in forest
forest

# Trees grow, their age increment of 1
# First, retrieve their current age
ageTrees <- of(world = forest, 
               agents = patches(forest))
newAgeTrees <- ageTrees + 1
# Then, assign the trees their new age
forest <- NLset(world = forest, 
                agents = patches(forest), 
                val = newAgeTrees)
plot(forest, 
     breaks = 0:15, col = rainbow(16))

# Cut 5 trees randomly
# First, select 5 random patches
cutTrees <- nOf(agents = patches(forest), 
                n = 5)
# Then, put their age at 0
forest <- NLset(world = forest, 
                agents = cutTrees, 
                val = 0)
plot(forest, 
     breaks = 0:15, col = rainbow(16))

