## FINAL GROUP EXERCISE ##

## Create a population of moving individuals where males and females have a different movement pattern
library(NetLogoR)
library(testthat)

# Create a world
w1 <- createWorld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10, data = runif(n = 100))
plot(w1)
# Create a population with males and females
t1 <- createTurtles(n = 30, coords = randomXYcor(world = w1, n = 30))
t1 <- turtlesOwn(turtles = t1, tVar = "sex", tVal = c(rep("male", 15), rep("female", 15)))
t1 <- NLset(turtles = t1, agents = t1, var = "color", val = c(rep("red", 15), rep("black", 15)))
points(t1, pch = 19, col = of(agents = t1, var = "color"))
popInit <- t1 # keep in memory for later

# Define 2 movement patterns/functions
moveFemale <- function(movingInd){
  # Move randomly two steps in any direction
  movingInd <- right(turtles = movingInd, angle = runif(n = NLcount(movingInd), min = 0, max = 360))
  movingInd <- fd(turtles = movingInd, dist = 2, world = w1, torus = TRUE)
  return(movingInd)
}

moveMale <- function(movingInd){
  # Move to one of the 8 neighboring cells where there is a female on it, otherwise randomly
  allNeighbors <- neighbors(world = w1, agents = movingInd, nNeighbors = 8)
  patchFemale <- patchHere(world = w1, turtles = NLwith(agents = t1, var = "sex", val = "female"))
  patchFemale <- cbind(patchFemale, femaleHere = 1)
  allNeighborsFem <- merge(allNeighbors, patchFemale, all.x = TRUE)
  allNeighborsFem[is.na(allNeighborsFem$femaleHere), "femaleHere"] <- 0
  
  selectPatches <- cbind(pxcor = numeric(), pycor = numeric())
  for(each in unique(allNeighborsFem$id)){
    if(sum(allNeighborsFem[allNeighborsFem$id == each, "femaleHere"]) == 0){
      # Move to one cell at random
      selectPatches <- rbind(selectPatches, oneOf(as.matrix(allNeighborsFem[allNeighborsFem$id == each, c("pxcor", "pycor")])))
    } else if(sum(allNeighborsFem[allNeighborsFem$id == each, "femaleHere"]) == 1){
      # Move to the cell with females on it
      selectPatches <- rbind(selectPatches, 
                             allNeighborsFem[allNeighborsFem$id == each & allNeighborsFem$femaleHere == 1, c("pxcor", "pycor"), drop = FALSE])
    } else {
      # Move to one cell with a female on it randomly among those that have females on it
      selectPatches <- rbind(selectPatches, oneOf(as.matrix(allNeighborsFem[allNeighborsFem$id == each & allNeighborsFem$femaleHere == 1, c("pxcor", "pycor")])))
    }
  }
  
  movingInd <- moveTo(turtles = movingInd, agents = as.matrix(selectPatches))
  return(movingInd)
}

# Create a loop where males move with one movement function and female with the other one
for(timeStep in 1:20){
  newFemales <- moveFemale(NLwith(agent = t1, var = "sex", val = "female"))
  newMales <- moveMale(NLwith(agent = t1, var = "sex", val = "male"))
  t1 <- turtleSet(newFemales, newMales)
}


## Make reproduction happens when a male meets a female, with the production of one offspring

# Define a reproduction function which produces an offspring
reproduction <- function(allInd, whoReproducingFemales){
  allInd <- hatch(turtles = allInd, who = whoReproducingFemales, n = 1, breed = "offspring")
  # Newborn inherit all the data from the parent (female) so we update the sex and color
  allInd <- NLset(turtles = allInd, agents = NLwith(agents = allInd, var = "breed", val = "offspring"),
                  var = "sex", val = sample(c("female", "male"), size = NLcount(NLwith(agents = allInd, var = "breed", val = "offspring")), replace = TRUE))
  allInd <- NLset(turtles = allInd, agents = NLwith(agents = allInd, var = "breed", val = "offspring"),
                  var = "breed", val = "turtle")
  allInd <- NLset(turtles = allInd, agents = NLwith(agents = allInd, var = "sex", val = "male"),
                  var = "color", val = "red")
  allInd <- NLset(turtles = allInd, agents = NLwith(agents = allInd, var = "sex", val = "female"),
                  var = "color", val = "black")
  return(allInd)
}

# Identify when a male and a female are on a patch together
encounter <- function(allInd){
  patchMales <- patchHere(world = w1, turtles = NLwith(agents = allInd, var = "sex", val = "male"))
  patchFemales <- patchHere(world = w1, turtles = NLwith(agents = allInd, var = "sex", val = "female"))
  # Patches in both group
  patchFemalesWho <- cbind(patchFemales, whoReproducingFemales = of(agents = NLwith(agents = allInd, var = "sex", val = "female"), var = "who"))
  patchBoth <- merge(as.data.frame(patchMales), as.data.frame(patchFemalesWho))
  whoReproducingFemales <- unique(patchBoth$whoReproducingFemales)
  return(whoReproducingFemales)
}

# Apply the reproduction function in this case
t1 <- popInit 
for(timeStep in 1:20){
  # Movement
  newFemales <- moveFemale(NLwith(agent = t1, var = "sex", val = "female"))
  newMales <- moveMale(NLwith(agent = t1, var = "sex", val = "male"))
  t1 <- turtleSet(newFemales, newMales)
  
  # Reproduction
  whoReproducingFemales <- encounter(t1)
  if(length(whoReproducingFemales) > 0){
    t1 <- reproduction(allInd = t1, whoReproducingFemales = whoReproducingFemales)
  }
}


## Plot and show the evolution of the population

# Plot the world
plot(w1)
# Plot the individuals at each time step
t1 <- popInit 
for(timeStep in 1:20){
  # Movement
  newFemales <- moveFemale(NLwith(agent = t1, var = "sex", val = "female"))
  newMales <- moveMale(NLwith(agent = t1, var = "sex", val = "male"))
  t1 <- turtleSet(newFemales, newMales)
  
  # Reproduction
  whoReproducingFemales <- encounter(t1)
  if(length(whoReproducingFemales) > 0){
    t1 <- reproduction(allInd = t1, whoReproducingFemales = whoReproducingFemales)
  }
  
  # Plot
  plot(w1)
  points(t1, pch = 19, col = of(agents = t1, var = "color"))
  Sys.sleep(1)
  print(timeStep)
}

# Count the number of individuals (males and females or both) at each time step
numInd <- numeric()
numMale <- numeric()
numFemale <- numeric()
t1 <- popInit 
for(timeStep in 1:20){
  # Movement
  newFemales <- moveFemale(NLwith(agent = t1, var = "sex", val = "female"))
  newMales <- moveMale(NLwith(agent = t1, var = "sex", val = "male"))
  t1 <- turtleSet(newFemales, newMales)
  
  # Reproduction
  whoReproducingFemales <- encounter(t1)
  if(length(whoReproducingFemales) > 0){
    popSize <- NLcount(t1)
    t1 <- reproduction(allInd = t1, whoReproducingFemales = whoReproducingFemales)
    #browser()
    expect_identical(NLcount(t1), popSize + length(whoReproducingFemales))
  }
  
  # Plot
  plot(w1)
  points(t1, pch = 19, col = of(agents = t1, var = "color"))
  Sys.sleep(1)
  print(timeStep)
  
  # Outputs
  numInd <- c(numInd, NLcount(t1))
  numMale <- c(numMale, NLcount(NLwith(agents = t1, var = "sex", val = "male")))
  numFemale <- c(numFemale, NLcount(NLwith(agents = t1, var = "sex", val = "female")))
}

# Plot the number of individuals
plot(1:20, numInd, ylim = c(0, max(numInd)), main = "Number of individuals", type = "l", col = "green", lwd = 2)
lines(1:20, numMale, col = "red")
lines(1:20, numFemale, col = "black")
legend("topleft", legend = c("Total ind.", "Males", "Females"), col = c("green", "red", "black"),
       lty = c(1, 1, 1), lwd = c(2, 1, 1))

