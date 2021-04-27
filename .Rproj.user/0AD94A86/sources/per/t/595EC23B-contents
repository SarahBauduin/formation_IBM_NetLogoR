## DEBUG ##

library(NetLogoR)

# Unit tests with testthat

library(testthat)

set.seed(1234) # same seed so that everybody has the same results
runTests <- TRUE

# Forest model

# Create a forest of 25 cells representing 25 plots where trees can grow
# 1 tree per cell (= patch)
forest <- createWorld(minPxcor = 0, 
                      maxPxcor = 4, 
                      minPycor = 0, 
                      maxPycor = 4)

# Trees are represented by their age (or it can be their size)
# Give a random age (between 1 and 10) to the 25 trees
forest <- NLset(world = forest, 
                agents = patches(forest), 
                val = sample(x = 1:10, 
                             size = 25, 
                             replace = TRUE))

if(runTests){
  expect_true(all(of(agents = patches(forest), 
                     world = forest) <= 11))
}


# Population model 

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

# Create a population of 6 mice
# Put the mice in the forest we just created
mice <- createTurtles(n = 6, 
                      breed = "mouse", 
                      world = forest)


# Give mice a sex, 3 "male" and 3 "female"
mice <- turtlesOwn(turtles = mice, 
                   tVar = "sex", 
                   tVal = c(rep("male", 3), 
                            rep("female", 3)))

# Make the females produce 1 offspring each
mice <- hatch(turtles = mice, 
              who = of(agents = NLwith(agents = mice, 
                                       var = "sex", 
                                       val = "female"), 
                       var = "who"),
              n = 1, 
              breed = "offspring")

if(runTests){
  expect_identical(of(agents = mice, 
                      var = "breed"), 
                   c(rep("mouse", 6), rep("offspring", 3)))
}

