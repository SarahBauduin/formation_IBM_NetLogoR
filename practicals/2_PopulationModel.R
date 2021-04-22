## POPULATION MODEL ##

library(NetLogoR)

# Create a population of 6 turtles
# Put the turtles in the forest we just created
t1 <- createTurtles(n = 6, world = w1)
points(t1, pch = 19, col = of(agents = t1, var = "color")) # all turtles are in the center of the world

# What's in t1
t1
# Access to the turtles locations
t1@.Data # but labels or factor variables are no longer visible

# Move the turtles randomly
# Their headings (directions) were randomly set when they were created
# Move turtles forward of a step length of 0.5 (step =  resolution of the world)
t1 <- fd(turtles = t1, dist = 0.5, world = w1, torus = TRUE) 
points(t1, pch = 19, col = of(agents = t1, var = "color"))
# Make them all rotate a 45 degrees angle to the right and move again 0.5 step
t1 <- right(turtles = t1, angle = 45)
t1 <- fd(turtles = t1, dist = 0.5, world = w1, torus = TRUE) 
points(t1, pch = 19, col = of(agents = t1, var = "color"))
# Make them all rotate a 45 degrees angle to the left and move again 0.5 step
t1 <- left(turtles = t1, angle = 45)
t1 <- fd(turtles = t1, dist = 0.5, world = w1, torus = TRUE) 
points(t1, pch = 19, col = of(agents = t1, var = "color"))

# Give turtles an age, randomly between 1 and 5
t1 <- turtlesOwn(turtles = t1, tVar = "age", tVal = sample(x = 1:5, size = 6, replace = TRUE))
t1
# Increment the turtles age of 1
t1 <- NLset(turtles = t1, agents = t1, var = "age", val = of(agents = t1, var = "age") + 1)
t1

# Give turtles a sex, 3 "male" and 3 "female"
t1 <- turtlesOwn(turtles = t1, tVar = "sex", tVal = c(rep("male", 3), rep("female", 3)))
t1
# Make the females produce 1 offspring each
t1 <- hatch(turtles = t1, who = of(agents = NLwith(agents = t1, var = "sex", val = "female"), var = "who"),
            n = 1, breed = "offspring")
t1
# Offspring inherit all the data from their mother
t1 <- NLset(turtles = t1, agents = NLwith(agents = t1, var = "breed", val = "offspring"), var = c("age", "sex"),
            val = cbind.data.frame(age = rep(0, 3), sex = sample(c("male", "female"), size = 3, replace = TRUE)))

# Kill the 3 oldest turtles
ageTurtles <- of(agents = t1, var = c("who", "age"))
library(doBy)
turtlesToKill <- ageTurtles[, "who"][which.maxn(ageTurtles[, "age"], n = 3)]
t1 <- die(turtles = t1, who = turtlesToKill)
t1

