## BUTTERFLY HILLTOPPING ##

library(NetLogoR)

# Create a world with the desired extent
hill <- createWorld(minPxcor = 1, maxPxcor = 100, minPycor = 1, maxPycor = 100)
# Define the patches values
# Elevation decreases linearly with distance from the center of the hill
# Hill center is at (30,30)
# The hill is 100 units high
elevation <- 100 - NLdist(agents = patches(hill), agents2 = cbind(x = 30, y = 30))
# Assign the elevation values to the patches
hill <- NLset(world = hill, agents = patches(hill), val = elevation)
# Visualize the world
plot(hill)

# Create turtles (3 butterflies in this model)
# The butterflies initial location are at [50,50]
b3 <- createTurtles(n = 3, coords = cbind(xcor = 50, ycor = 50))
# Visualize the butterflies
points(b3, pch = 16, col = of(agents = b3, var = "color"))

# What's inside this loop will be iterated 100 times
for (time in 1:100) {
  
  # With a probability of 0.5
  if (runif(n = 1, min = 0, max = 1) < 0.5) {
    
    # Either move all butterflies uphill considering the 8 neighboring patches
    # i.e., move each butterfly to the patch with the highest (elevation) value among the 8 patches around
    b3 <- uphill(world = hill, turtles = b3, nNeighbors = 8)
  } else {
    
    # Or move all butterflies to one of its neighboring patches at random
    # First, identify for each butterfly the 8 patches around each one
    allNeighbors <- neighbors(world = hill, agents = b3, nNeighbors = 8)
    # Second, for each set of "neighboring patches", select one randomly
    oneNeighbor <- oneOf(allNeighbors)
    # Finally, move the butterflies to their randomly selected neighboring patch
    b3 <- moveTo(turtles = b3, agents = oneNeighbor)
  }
  
  # Visualize each new position for t1
  points(b3, pch = 16, col = of(agents = b3, var = "color"))
  
  # Wait 1 second between each loop to see the butterflies moving
  #Sys.sleep(1)
}

