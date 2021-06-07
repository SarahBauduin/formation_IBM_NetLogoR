## WOLF SHEEP PREDATION ##

library(NetLogoR)
set.seed(1234) # same seed so that everybody has the same results

## Setup
# Create the world
grass <- createWorld(minPxcor = -10, 
                     maxPxcor = 10, 
                     minPycor = -10, 
                     maxPycor = 10, 
                     data = 0)
# Create the sheep
sheep <- createTurtles(n = 50, 
                       coords = randomXYcor(world = grass, 
                                            n = 50),
                       breed = "aSheep", 
                       color = rep("red", 50))
# Create the wolves
wolves <- createTurtles(n = 20, 
                        coords = randomXYcor(world = grass, 
                                             n = 20),
                        breed = "wolf", 
                        color = rep("black", 
                                    20))

# Visualize the world
plot(grass)
points(sheep, 
       pch = 16, 
       col = "red")
points(wolves, 
       pch = 16, 
       col = "black")


## Functions used in the for loop
# Function with arguments "turtles" can be used by sheep and wolves
moveRandomly <- function(turtles, landscape, moveAngle) {
  # Move one step in the direction between (- moveAngle) and (+ moveAngle)
  turtles <- right(turtles = turtles, 
                   angle = runif(n = NLcount(turtles), 
                                 min = -moveAngle, 
                                 max = moveAngle))
  turtles <- fd(world = landscape, 
                turtles = turtles, 
                dist = 1, 
                torus = TRUE)
  return(turtles)
}
# Example of moveRandomly
wolves@.Data
wolvesTest <- moveRandomly(turtles = wolves, landscape = grass, moveAngle = 0)
wolvesTest@.Data # turtles have move one step forward, their heading has not changed 
wolvesTest <- moveRandomly(turtles = wolves, landscape = grass, moveAngle = 180)
wolvesTest@.Data # turtles have move one step in a new direction, their heading has changed 

reproduce <- function(turtles) {
  # 10% of the individuals reproduce
  repro <- runif(n = NLcount(turtles), 
                 min = 0, 
                 max = 100) < 10
  whoTurtles <- of(agents = turtles, 
                   var = "who") # "who" (ID) of all turtles
  reproWho <- whoTurtles[repro] # "who" of turtles which reproduce
  reproInd <- turtle(turtles, 
                     who = reproWho) # turtles which reproduce
  
  # if there is at least one individual reproducing
  if (NLcount(reproInd) != 0) {
    turtles <- hatch(turtles = turtles, 
                     who = reproWho, 
                     n = 1) # hatch one offspring per parent
  }
  return(turtles)
}
# Example of reproduce
sheep
sheepTest <- reproduce(turtles = sheep)
sheepTest # new sheep added

catchSheep <- function(prey, predator, landscape) {
  # "who" numbers of prey that are on the same patches as the predator
  preypredator <- turtlesOn(world = landscape, 
                           turtles = prey, 
                           agents = predator, 
                           simplify = FALSE)
  if(nrow(preypredator) != 0) {
    preyGrabbed <- oneOf(agents = preypredator) # grab one random prey
    prey <- die(turtles = prey, 
                 who = preyGrabbed) # kill the grabbed prey
  }
  return(prey)# return the object updated in this function
}
# Example of catchSheep
sheep
sheepTest <- catchSheep(prey = sheep, predator = wolves, landscape = grass)
sheepTest # some sheep have died


## For loop
time <- 0
maxTime <- 15
while((NLany(sheep) | NLany(wolves)) & time < maxTime) {
  ## as long as there are sheep or wolves in the world but with a maximum time step of 15
  
  # Sheep
  if (NLcount(sheep) != 0) {
    sheep <- moveRandomly(turtles = sheep, landscape = grass, moveAngle = 50)
    sheep <- reproduce(turtles = sheep)
  }
  # Wolves
  wolves <- moveRandomly(turtles = wolves, landscape = grass, moveAngle = 50)
  sheep <- catchSheep(prey = sheep, predator = wolves, landscape = grass) # the result returned is the prey (sheep)
  wolves <- reproduce(turtles = wolves)

  time <- time + 1
  print(time) 
  
  # Slow the model
  plot(grass)
  points(sheep, 
         pch = 16, 
         col = "red")
  points(wolves, 
         pch = 16, 
         col = "black")
  
  Sys.sleep(.5)
}
