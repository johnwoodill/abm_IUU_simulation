library(gganimate)
library(feather)
library(tidyverse)
library(gifski)
#install.packages('gifski')
#install.packages('png')


setwd("~/Projects/abm_IUU_simulation/")

dat = read_feather("~/Projects/abm_IUU_simulation/data/test_dat.feather")
head(dat)

p <- ggplot(dat, aes(x1, y1)) + 
  geom_point() +
  geom_point(aes(fx1, fy1), color="red") +
  transition_states(time) +
  # labs(title = 'Date: {frame_time}') +
  NULL

animate(p)


for (i in 1:20){
  ggplot(filter(dat, time == i), aes(x1, y1)) + geom_point() 
  Sys.sleep(1)
  print(i)
}


dat = read_feather("~/Projects/abm_IUU_simulation/data/test_boid.feather")
dat

p <- ggplot(dat, aes(boid_x, boid_y)) + geom_point() +
  transition_states(timestep) +
  # labs(title = 'Date: {frame_time}') +
  NULL

animate(p)
