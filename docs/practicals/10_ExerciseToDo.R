## FINAL GROUP EXERCISE ##

library(NetLogoR)
library(testthat)

##################
## EXERCISE 1/3 ##
## Create a population with males and females 

# Create a world of 10 x 10 patches with random values on each patch 
# between 0 and 1

# Create a population with 15 males and 15 females
# Random locations using randomXYcor()
# Color of males = red, color of females = black

# Plot the world
# Plot males and females with their respective colors

# Count the number of individuals, of males and of females at each time step


##################
## EXERCISE 2/3 ##

# Create 2 movement functions

# Movement of females: in an random direction, move the distance of 2 patches 
# at the time, in a wrapped world

# Movement of males: move to one of the 8 neighboring cells where there is a 
# female on it, otherwise on one of the 8 neighboring cells randomly
# What are the cells around the individuals?
# On which cells there are females?
# If there are no females on the cells around, choose one cell randomly
# If there is one cell with female(s) on it, choose this cell
# If there are several cells with females on it, choose one cell among 
# these ones randomly
# Move the individuals to the chosen patches

# Create a loop of 20 times steps where all females move first 
# then all the males
# Need to put back together the modified males and females for 
# the next step (updated t1)
# Add plotting functions after all other functions
# Plot males and females with their respective colors
# Use Sys.sleep(1) to slow the function


##################
## EXERCISE 3/3 ##

## Make reproduction happens when a male meets a female, with the production 
# of one offspring

# Define a reproduction function where all females produce one offspring each
# Newborn inherit all the data from the parent (female) so we update 
# the sex and color
# Update the sex, breed and color of the offspring
# Change the sex randomly
# Give them the breed "turtle"
# Update the colors according to the sex

# Identify when a male and a female are on a patch together
# Females that are on patches where there are male 
# And keep in memory the ID of these females which will reproduce

# Use the loop created before and add the reproduction before the plot functions
# Count the number of individuals, of males and of females at each time step
# Loop 
# Need to put back together the modified males and females for 
# the next step (updated t1)
# After the movement, evaluate which females will reproduce 
# (i.e., are on a patch with a male)
# If there are reproducing females, apply reproduction
# Add plotting functions after all other functions
# Plot males and females with their respective colors
# Use Sys.sleep(1) to slow the function
# Increment the number of individuals, males and females

# After the loop finished, plot the number of individuals 
# (all, males and females) over time
# Plot line color according to the male and female colors
# Add a legend




##########
## HELP ##

# 1/3
# List of NetLogoR functions used:
# createWorld, createTurtles, turtlesOwn, NLset, of, NLcount, NLwith

# 2/3
# Create a function
# moveFemale <- function(movingInd){
#   
#   return(movingInd)
# }
# and a function
# moveMale <- function(movingInd){
#   
#   return(movingInd)
# }
# List of NetLogoR functions used in moveFemale:
# right, NLcount, fd
# List of NetLogoR functions used in moveMale:
# neighbors, patchHere, NLwith, oneOf, moveTo
# List of NetLogoR functions used in the loop:
# NLwith, turtleSet, of

# 3/3
# Create a function
# reproduction <- function(allInd, whoReproducingFemales){
# 
#   return(allInd)
# }
# and a function
# encounter <- function(allInd){
#   
#   return(whoFemalesWithMales)
# }
# List of NetLogoR functions used in reproduction:
# hatch, NLset, NLwith, NLcount
# List of NetLogoR functions used in encounter:
# NLwith, turtlesOn, of
# List of NetLogoR functions used in the loop:
# NLwith, turtleSet, of, NLcount