## AGENTMATRIX EXERCISE ## 

library(NetLogoR)
rm(list=ls()) # reset the R environment

# Create a population of 10 individuals, 
# all at the location [0;0] and 
# with their heading either North or South, randomly.
# Don't hesitate to use help()
help("createTurtles")
t1 <- createTurtles(n = 10, 
                    coords = cbind(pxcor = rep(0, 10), 
                                   pycor = rep(0, 10)), 
                    heading = sample(c(0, 180), 
                                     size = 10, 
                                     replace = TRUE))
t1@.Data

# Give them all an age of 5
help("turtlesOwn")
t1 <- turtlesOwn(turtles = t1, 
                 tVar = "age", 
                 tVal = 5)

# Give them a sex “male” or “female” randomly
t1 <- turtlesOwn(turtles = t1, 
                 tVar = "sex", 
                 tVal = sample(x = c("male", "female"), 
                               size = 10, 
                               replace = TRUE))
