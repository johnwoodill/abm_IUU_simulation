#library(gganimate)
#library(feather)
#library(tidyverse)
#library(gifski)
#install.packages('gifski')
#install.packages('png')


setwd("~/Projects/abm_IUU_simulation/")

dat = read_feather("~/Projects/abm_IUU_simulation/data/test_dat.feather")
head(dat)



p <- ggplot(dat, aes(x1, y1)) + 
  geom_point() +
  # geom_point(aes(fx1, fy1), color="red") +
  transition_states(time) +
  ylim(0, 1) +
  xlim(0, 1) +
  geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='blue') +
  geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='blue') +
  geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='blue') +
  geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='blue') +
  # labs(title = 'Date: {frame_time}') +
  NULL

animate(p)

# dat = filter(dat, vessel == 10)
# 
# ggplot(dat, aes(x1, y1)) + 
#   geom_path() + 
#   geom_point(aes(fx1, fy1), color='red') +
#   geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='blue') +
#   geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='blue') +
#   geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='blue') +
#   geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='blue') +
#   ylim(0, 1) +
#   xlim(0, 1) +
#   NULL
#   