## POPULATION MODEL ##

library(NetLogoR)
set.seed(1234) # same seed so that everybody has the same results

# Create a landscape over which mice evolve
# We use the forest from 1_ForestModel.R
forest <- createWorld(minPxcor = 0, 
                      maxPxcor = 4, 
                      minPycor = 0, 
                      maxPycor = 4)
forest <- NLset(world = forest, 
                agents = patches(forest), 
                val = sample(x = 1:10, 
                             size = 25, 
                             replace = TRUE))
plot(forest)

# Create a population of 6 mice
# Put the mice in the forest we just created
mice <- createTurtles(n = 6, 
                      breed = "mouse", 
                      world = forest)
points(mice, 
       pch = 19, 
       col = of(agents = mice, 
                var = "color")) # all mice are in the center of the forest

# What's in mice
mice
# Access to the mice locations
mice@.Data # but labels or factor variables are no longer visible

# Move the mice randomly
# Their headings (directions) were randomly set when they were created
# Move mice forward of a step length of 0.5 (step =  resolution of the forest)
mice <- fd(turtles = mice, 
           dist = 0.5) 
points(mice, 
       pch = 19, 
       col = of(agents = mice, 
                var = "color"))
# Make them all rotate a 45 degrees angle to the right and move again 0.5 step
mice <- right(turtles = mice, 
              angle = 45)
mice <- fd(turtles = mice, 
           dist = 0.5) 
points(mice, 
       pch = 19, 
       col = of(agents = mice, 
                var = "color"))
# Make them all rotate a 45 degrees angle to the left and move again 0.5 step
mice <- left(turtles = mice, 
             angle = 45)
mice <- fd(turtles = mice, 
           dist = 0.5) 
points(mice, 
       pch = 19, 
       col = of(agents = mice, 
                var = "color"))

# Give mice an age, randomly between 1 and 5
mice <- turtlesOwn(turtles = mice, 
                   tVar = "age", 
                   tVal = sample(x = 1:5, 
                                 size = 6, 
                                 replace = TRUE))
mice
# Increment the mice age of 1
# NLset() to assign or modify turtles' variable(s)
mice <- NLset(turtles = mice, 
              agents = mice, 
              var = "age", 
              val = of(agents = mice, 
                       var = "age") + 1)
mice

# Give mice a sex, 3 "male" and 3 "female"
# turtlesOwn() to create a new turtles' variable and assign it a value
mice <- turtlesOwn(turtles = mice, 
                   tVar = "sex", 
                   tVal = c(rep("male", 3), 
                            rep("female", 3)))
mice
# Make the females produce 1 offspring each
mice <- hatch(turtles = mice, 
              who = of(agents = NLwith(agents = mice, 
                                       var = "sex", 
                                       val = "female"), 
                       var = "who"),
              n = 1, 
              breed = "offspring")
mice
# Offspring inherit all the data from their mother
mice <- NLset(turtles = mice, 
              agents = NLwith(agents = mice, 
                              var = "breed", 
                              val = "offspring"), 
              var = c("age", "sex"),
              val = cbind.data.frame(age = rep(0, 3), 
                                     sex = sample(c("male", "female"),
                                                  size = 3, 
                                                  replace = TRUE)))

# Kill the 3 oldest mice
ageTurtles <- of(agents = mice, 
                 var = c("who", "age"))
library(doBy)
# doBy needed to use which.maxn()
# which.maxn() finds the n largest elements
turtlesToKill <- ageTurtles[, "who"][which.maxn(ageTurtles[, "age"], n = 3)]
mice <- die(turtles = mice, 
            who = turtlesToKill)
mice

