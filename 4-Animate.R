library(gganimate)
library(feather)
library(tidyverse)
library(gifski)
#install.packages('gifski')
#install.packages('png')


setwd("~/Projects/abm_IUU_simulation/")

dat = read_feather("~/Projects/abm_IUU_simulation/data/vessel_dat.feather")
head(dat)

idat = read_feather("~/Projects/abm_IUU_simulation/data/iuu_vessel_dat.feather")
head(idat)
tail(idat)

length(unique(dat$t))

p <- ggplot(dat, aes(x1, y1)) + 
  geom_point() +
  geom_point(data=idat, color="red") +
  transition_manual(frames = t) +
  ylim(0, 1) +
  xlim(0, 1) +
  # geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='blue') +
  # geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='blue') +
  # geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='blue') +
  # geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='blue') +
  labs(title = 'Hour of Month: {current_frame}') +
  NULL


animate(p, nframes = max(dat$t))
# anim_save("abm_iuu_simulation.mp4", a)

# dat = filter(dat, t==185)
# idat = filter(idat, t==185)
# 
# ggplot(dat, aes(x1, y1)) + 
#   geom_point() +
#   geom_point(data=idat, color="red") +
#   ylim(0, 1) +
#   xlim(0, 1) +
#   geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='blue') +
#   geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='blue') +
#   geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='blue') +
#   geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='blue') +
#   NULL


# dat = filter(dat, vessel == 10)
# 
ggplot(idat, aes(x1, y1)) +
  geom_path() +
  geom_label(aes(label=t)) +
  # geom_point(aes(fx1, fy1), color='red') +
  # geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='blue') +
  # geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='blue') +
  # geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='blue') +
  # geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='blue') +
  ylim(0, 1) +
  xlim(0, 1) +
  NULL
