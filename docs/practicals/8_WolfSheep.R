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
move <- function(turtles) {
  # Move one step in the direction between -50 and +50
  turtles <- right(turtles = turtles, 
                   angle = runif(n = NLcount(turtles), 
                                 min = -50, 
                                 max = 50))
  turtles <- fd(world = grass, 
                turtles = turtles, 
                dist = 1, 
                torus = TRUE)
  return(turtles)
}

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

catchSheep <- function() {
  # "who" numbers of sheep that are on the same patches as the wolves
  sheepWolves <- turtlesOn(world = grass, 
                           turtles = sheep, 
                           agents = wolves, 
                           simplify = FALSE)
  if(nrow(sheepWolves) != 0) {
    sheepGrabbed <- oneOf(agents = sheepWolves) # grab one random sheep
    sheep <- die(turtles = sheep, 
                 who = sheepGrabbed) # kill the grabbed sheep
  }
  return(sheep)# return the object updated in this function
}


## For loop
time <- 0
maxTime <- 15
while((NLany(sheep) | NLany(wolves)) & time < maxTime) {
  ## as long as there are sheep or wolves in the world but with a maximum time step of 15
  
  # Sheep
  if (NLcount(sheep) != 0) {
    sheep <- move(sheep)
    sheep <- reproduce(sheep)
  }
  # Wolves
  wolves <- move(wolves)
  sheep <- catchSheep() # the result returned is the "sheep"
  wolves <- reproduce(wolves)

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
  
  Sys.sleep(1)
}
