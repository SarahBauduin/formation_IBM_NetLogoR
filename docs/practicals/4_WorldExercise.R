## WORLDMATRIX AND WORLDARRAY EXERCISE ##

library(NetLogoR)
rm(list=ls()) # reset the R environment

# Create a 3-layer world of 15 patches
# Don't hesitate to use the help()
help("createWorld")
# The 1st layer where all patches are equal to 12
w1 <- createWorld(minPxcor = 1, 
                  maxPxcor = 3, 
                  minPycor = 1, 
                  maxPycor = 5, 
                  data = 12)

# The 2nd layer where patches are either 1 or 2, randomly
w2 <- createWorld(minPxcor = 1, 
                  maxPxcor = 3, 
                  minPycor = 1, 
                  maxPycor = 5, 
                  data = sample(x = c(1,2), 
                                size = 15, 
                                replace = TRUE))

# The 3rd layer where patches are equal to their pxcor
w3 <- createWorld(minPxcor = 1, 
                  maxPxcor = 3, 
                  minPycor = 1, 
                  maxPycor = 5, 
                  data = patches(w1)[, "pxcor"])

# Stack the 3 layers
help("stackWorlds")
wAll <- stackWorlds(w1, w2, w3)
# Plot the 3 layers
plot(wAll)
# Plot only the 2nd layer
plot(wAll[[2]])
plot(wAll[["w2"]])
