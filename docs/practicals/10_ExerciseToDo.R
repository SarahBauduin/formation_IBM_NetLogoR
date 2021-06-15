## FINAL GROUP EXERCISE ##

library(NetLogoR)
library(testthat)
rm(list=ls()) # reset the R environment
set.seed(1234) # same seed so that everybody has the same results

##################
## EXERCISE 1/3 ##

## Create a population of moving individuals where males and females have a different movement pattern

# Create a world of 10 x 10 patches with random values on each patch between 0 and 1

# Create a population with 15 males and 15 females
# Random locations using randomXYcor()
# Color of males = red, color of females = black

# Define 2 movement patterns/functions

# Movement of females: in an random direction, move 2 patches at the time, in a wrapped world

# Movement of males: move to one of the 8 neighboring cells where there is a female on it, otherwise on one of the 8 neighboring cells randomly
# What are the cells around the individuals?
# On which cells there are females?
# If there are no females on the cells around, choose one cell randomly
# If there is one cell with female(s) on it, choose this cell
# If there are several cells with females on it, choose one cell among these ones randomly

# Create a loop of 20 times steps where all females move first then all the males


##################
## EXERCISE 2/3 ##

## Make reproduction happens when a male meets a female, with the production of one offspring

# Define a reproduction function where all females produce one offspring each
# Update the sex, breed and color of the offspring

# Identify when a male and a female are on a patch together
# And keep in memory the ID of these females which will reproduce

# Apply the reproduction function in this case
# Use the movement loop written before
# After the movement, evaluate which females will reproduce (i.e., are on a patch with a male)
# If there are reproducing females, apply reproduction


##################
## EXERCISE 3/3 ##

## Plot and show the evolution of the population

# Plot the world

# Plot the individuals at each time step
# Add plotting functions after all other functions
# Plot males and females with their respective colors
# Use Sys.sleep(1) to slow the function

# Count the number of individuals, of males and of females at each time step
# Increment this vector at each time step

# Plot the number of individuals (all, males and females) over time
# Plot line color according to the male and female colors
# Add a legend

