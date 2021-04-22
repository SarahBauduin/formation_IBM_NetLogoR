## FOREST MODEL ##

library(NetLogoR)

# Create a forest of 25 cells representing 25 plots where trees can grow (1 tree per cell)
w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
plot(w1)

# Trees are represented by their age (or it can be their size)
# Give a random age (between 1 and 10) to the 25 trees
w1 <- NLset(world = w1, agents = patches(w1), val = sample(x = 1:10, size = 25, replace = TRUE))
plot(w1)
# What's in w1
w1

# Trees grow, their age increment of 1
ageTrees <- of(world = w1, agents = patches(w1))
newAgeTrees <- ageTrees + 1
w1 <- NLset(world = w1, agents = patches(w1), val = newAgeTrees)
plot(w1)

# Cut 5 trees randomly (put their age at 0)
cutTrees <- nOf(agents = patches(w1), n = 5)
w1 <- NLset(world = w1, agents = cutTrees, val = 0)
plot(w1)

