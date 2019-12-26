library(gganimate)
library(feather)
library(tidyverse)
library(gifski)
#install.packages('gifski')
#install.packages('png')


setwd("~/Projects/abm_IUU_simulation/")

dat = read_feather("~/Projects/abm_IUU_simulation/data/sim_data.feather")

p <- ggplot(dat, aes(x, y)) + geom_point() +
  transition_states(t) +
  # labs(title = 'Date: {frame_time}') +
  NULL

animate(p)




dat = read_feather("~/Projects/abm_IUU_simulation/data/test_boid.feather")
dat

p <- ggplot(dat, aes(boid_x, boid_y)) + geom_point() +
  transition_states(timestep) +
  # labs(title = 'Date: {frame_time}') +
  NULL

animate(p)
