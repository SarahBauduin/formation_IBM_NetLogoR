## FINAL GROUP EXERCISE ##

library(NetLogoR)
library(testthat)

##################
## EXERCISE 1/3 ##
## Create a population with males and females

# Create a world of 10 x 10 patches with random values on each patch 
# between 0 and 1
w1 <- createWorld(minPxcor = 1, 
                  maxPxcor = 10, 
                  minPycor = 1, 
                  maxPycor = 10, 
                  data = runif(n = 100))

# Create a population with 15 males and 15 females
# Random locations using randomXYcor()
t1 <- createTurtles(n = 30, 
                    coords = randomXYcor(world = w1, 
                                         n = 30))
t1 <- turtlesOwn(turtles = t1, 
                 tVar = "sex", 
                 tVal = c(rep("male", 15), rep("female", 15)))
# Color of males = red, color of females = black
t1 <- NLset(turtles = t1, 
            agents = t1, 
            var = "color", 
            val = c(rep("red", 15), rep("black", 15)))

# Plot the world
plot(w1)
# Plot males and females with their respective colors
points(t1, 
       pch = 19, 
       col = of(agents = t1, 
                var = "color"))

# Count the number of individuals, of males and of females at each time step
numInd <- NLcount(t1)
numMale <- NLcount(NLwith(agents = t1,
                          var = "sex",
                          val = "male"))
numFemale <- NLcount(NLwith(agents = t1,
                            var = "sex",
                            val = "female"))


##################
## EXERCISE 2/3 ##

# Create 2 movement functions

# Movement of females: in an random direction, move the distance of 2 patches 
# at the time, in a wrapped world
moveFemale <- function(movingInd){
  movingInd <- right(turtles = movingInd, 
                     angle = runif(n = NLcount(movingInd), 
                                   min = 0, 
                                   max = 360))
  movingInd <- fd(turtles = movingInd, 
                  dist = 2, 
                  world = w1, 
                  torus = TRUE)
  return(movingInd)
}

# Movement of males: move to one of the 8 neighboring cells where there is a 
# female on it, otherwise on one of the 8 neighboring cells randomly
moveMale <- function(movingInd){
  
  # What are the cells around the individuals?
  allNeighbors <- neighbors(world = w1, 
                            agents = movingInd, 
                            nNeighbors = 8)
  # On which cells there are females?
  patchFemale <- patchHere(world = w1, 
                           turtles = NLwith(agents = t1, 
                                            var = "sex", 
                                            val = "female"))
  patchFemale <- cbind(patchFemale, 
                       femaleHere = 1)
  allNeighborsFem <- merge(allNeighbors, 
                           patchFemale, 
                           all.x = TRUE)
  allNeighborsFem[is.na(allNeighborsFem$femaleHere), "femaleHere"] <- 0
  
  selectPatches <- cbind(pxcor = numeric(), 
                         pycor = numeric())
  for(each in unique(allNeighborsFem$id)){ # for each moving individual
    
    # If there are no females on the cells around, choose one cell randomly
    if(sum(allNeighborsFem[allNeighborsFem$id == each, "femaleHere"]) == 0){ 

      selectPatches <- rbind(selectPatches, 
                             oneOf(as.matrix(allNeighborsFem[allNeighborsFem$id == each, 
                                                             c("pxcor", "pycor")])))
    
    # If there is one cell with female(s) on it, choose this cell
    } else if(sum(allNeighborsFem[allNeighborsFem$id == each, "femaleHere"]) == 1){ 
      
      selectPatches <- rbind(selectPatches, 
                             allNeighborsFem[allNeighborsFem$id == each & 
                                               allNeighborsFem$femaleHere == 1, 
                                             c("pxcor", "pycor"), drop = FALSE])
    
    } else { # If there are several cells with females on it, choose one cell among 
             # these ones randomly

      selectPatches <- rbind(selectPatches, 
                             oneOf(as.matrix(allNeighborsFem[allNeighborsFem$id == each & 
                                                               allNeighborsFem$femaleHere == 1, 
                                                             c("pxcor", "pycor")])))
    }
  }
  
  # Move the individuals to the chosen patches
  movingInd <- moveTo(turtles = movingInd, 
                      agents = as.matrix(selectPatches))
  return(movingInd)
}

# Create a loop of 20 times steps where all females move first 
# then all the males
for(timeStep in 1:20){
  newFemales <- moveFemale(NLwith(agent = t1, 
                                  var = "sex", 
                                  val = "female"))
  newMales <- moveMale(NLwith(agent = t1, 
                              var = "sex", 
                              val = "male"))
  
  expect_equal(NLcount(newFemales) + NLcount(newMales),
               NLcount(t1))
  
  # Need to put back together the modified males and females for 
  # the next step (updated t1)
  t1 <- turtleSet(newFemales, 
                  newMales)
  
  # Add plotting functions after all other functions
  plot(w1)
  # Plot males and females with their respective colors
  points(t1, 
         pch = 19, 
         col = of(agents = t1, 
                  var = "color"))
  # Use Sys.sleep(1) to slow the function
  Sys.sleep(1)
  print(timeStep)
  
}


##################
## EXERCISE 3/3 ##

## Make reproduction happens when a male meets a female, with the production 
# of one offspring

# Define a reproduction function where all females produce one offspring each
reproduction <- function(allInd, whoReproducingFemales){

  popSize <- NLcount(allInd) # for the test
  # Check that whoReproducingFemales are females'ID
  expect_true(all(of(agents = turtle(turtles = allInd, 
                                     who = whoReproducingFemales), 
                     var = "sex") == "female"))
  
  allInd <- hatch(turtles = allInd, 
                  who = whoReproducingFemales, 
                  n = 1, 
                  breed = "offspring")
  
  # Newborn inherit all the data from the parent (female) so we update 
  # the sex and color
  # Update the sex, breed and color of the offspring
  # Change the sex randomly
  allInd <- NLset(turtles = allInd, 
                  agents = NLwith(agents = allInd, 
                                  var = "breed", 
                                  val = "offspring"),
                  var = "sex", 
                  val = sample(c("female", "male"), 
                               size = NLcount(NLwith(agents = allInd, 
                                                     var = "breed", 
                                                     val = "offspring")), 
                               replace = TRUE))
  # Give them the breed "turtle"
  allInd <- NLset(turtles = allInd, 
                  agents = NLwith(agents = allInd, 
                                  var = "breed", 
                                  val = "offspring"),
                  var = "breed", 
                  val = "turtle")
  # Update the colors according to the sex
  allInd <- NLset(turtles = allInd, 
                  agents = NLwith(agents = allInd, 
                                  var = "sex", 
                                  val = "male"),
                  var = "color", 
                  val = "red")
  allInd <- NLset(turtles = allInd, 
                  agents = NLwith(agents = allInd, 
                                  var = "sex", 
                                  val = "female"),
                  var = "color", 
                  val = "black")
  
  # Check population size
  expect_equal(popSize + length(whoReproducingFemales),
               NLcount(allInd))
  # Check there are no more "offspring"
  expect_equal(NLcount(NLwith(agents = allInd, 
                              var = "breed", 
                              val = "offspring")), 
               0)
  
  return(allInd)
}

# Identify when a male and a female are on a patch together
encounter <- function(allInd){
  males <- NLwith(agents = allInd,
                  var = "sex",
                  val = "male")
  females <- NLwith(agents = allInd,
                    var = "sex",
                    val = "female")
  # Females that are on patches where there are male 
  femalesWithMales <- turtlesOn(world = w1, turtles = females, 
                                agents = males)
  # And keep in memory the ID of these females which will reproduce
  whoFemalesWithMales <- of(agents = femalesWithMales, var = "who")
  return(whoFemalesWithMales)
}

# Use the loop created before and add the reproduction before the plot functions
# Count the number of individuals, of males and of females at each time step
numInd <- rep(NA, 20)
numMale <- rep(NA, 20)
numFemale <- rep(NA, 20)
# Loop 
for(timeStep in 1:20){
  newFemales <- moveFemale(NLwith(agent = t1, 
                                  var = "sex", 
                                  val = "female"))
  newMales <- moveMale(NLwith(agent = t1, 
                              var = "sex", 
                              val = "male"))
  
  expect_equal(NLcount(newFemales) + NLcount(newMales),
               NLcount(t1))
  
  # Need to put back together the modified males and females for 
  # the next step (updated t1)
  t1 <- turtleSet(newFemales, 
                  newMales)
  
  # After the movement, evaluate which females will reproduce 
  # (i.e., are on a patch with a male)
  whoReproducingFemales <- encounter(t1)
  # If there are reproducing females, apply reproduction
  if(length(whoReproducingFemales) > 0){
    t1 <- reproduction(allInd = t1, 
                       whoReproducingFemales = whoReproducingFemales)
  }
  
  # Add plotting functions after all other functions
  plot(w1)
  # Plot males and females with their respective colors
  points(t1, 
         pch = 19, 
         col = of(agents = t1, 
                  var = "color"))
  # Use Sys.sleep(1) to slow the function
  Sys.sleep(1)
  print(timeStep)
  
  # Increment the number of individuals, males and females
  numInd[timeStep] <- NLcount(t1)
  numMale[timeStep] <- NLcount(NLwith(agents = t1,
                                      var = "sex",
                                      val = "male"))
  numFemale[timeStep] <- NLcount(NLwith(agents = t1,
                                        var = "sex",
                                        val = "female"))
}
  
# After the loop finished, plot the number of individuals 
# (all, males and females) over time
plot(1:20, 
     numInd, 
     ylim = c(0, max(numInd)), 
     main = "Number of individuals", 
     type = "l", 
     col = "green", 
     lwd = 2)
# Plot line color according to the male and female colors
lines(1:20, 
      numMale, 
      col = "red")
lines(1:20, 
      numFemale, 
      col = "black")
# Add a legend
legend("topleft", 
       legend = c("Total ind.", "Males", "Females"), 
       col = c("green", "red", "black"),
       lty = c(1, 1, 1), 
       lwd = c(2, 1, 1))

