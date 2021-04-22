## NETLOGOR IN DETAILS ##

library(NetLogoR)

# World with a single value per patch: worldMatrix
w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = runif(25)) 
plot(w1)
w1@minPxcor 
w1@maxPxcor 
w1@minPycor 
w1@maxPycor 
w1@extent 
w1@res 
w1@pCoords
w1@.Data 

# World with multiple values per patch: worldArray
w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25) 
w2 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 25:1) 
# Stacking multiple worldMatrix into a worldArray
w3 <- stackWorlds(w1, w2)
plot(w3)
w3@.Data 

# The moving individuals: agentMatrix
t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10)) 
t1 # factor variables are shown with labels but individuals coordinates are not shown
t1@.Data # individuals coordinates are visible but labels of factor variables are not shown
# Create a new variable 
t1 <- turtlesOwn(turtles = t1, tVar = "sex", tVal = c("F", "F", "F","F", "F", "F", "M", "M","M", "M"))
t1 
# Create two types of individuals
t2 <- createTurtles(world = w1, n = 10, breed = c(rep("sheep", 5), rep("wolf", 5))) 
sheep <- NLwith(agents = t2, var = "breed", val = "sheep") 
wolves <- NLwith(agents = t2, var = "breed", val = "wolf") 
sheep <- createTurtles(world = w1, n = 5, breed = "sheep") 
wolves <- createTurtles(world = w1, n = 5, breed = "wolf")


