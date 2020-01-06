library(gganimate)
library(feather)
library(tidyverse)
library(gifski)
library(moments)
library(magick)
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
  theme_bw() +
  geom_point() +
  geom_point(data=idat, color="red") +
  annotate("text", x= .7, y= .85, label="Fishing Area", color='blue') +
  geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='blue') +
  geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='blue') +
  geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='blue') +
  geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='blue') +
  transition_manual(frames = t) +
  ylim(min(dat$y1), max(dat$y1)) +
  xlim(min(dat$x1), max(dat$x1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = 'Hour of Month: {current_frame}') +
  NULL

p
ap1 <- animate(p, nframes = max(dat$t))
# anim_save("abm_iuu_simulation.mp4", a)
# gg_animate(p, "output.gif")


# KS-Statistic
ksdat = read_feather("~/Projects/abm_IUU_simulation/data/ks_data.feather")

# Mean results
ksm <- ksdat %>% 
  group_by(t) %>% 
  summarise(ks = mean(ks))
ksm

p2 <- ggplot(ksm, aes(t, ks, group=1)) + 
  geom_point() + 
  geom_line() +
  theme_bw() +
  labs(x="Hours in Month", y="Anomaly Index (Mean)") +
  transition_manual(frames = t) +
  transition_reveal(along = t) +
  enter_fade() +
  NULL


ap2 <- animate(p2, nframes = max(dat$t))


# Animate both plots
a_mgif <- image_read(ap1)
b_mgif <- image_read(ap2)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:max(dat$t)){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

gg_animate(new_gif, "output.gif")



# Kurtosis results
# ksk <- ksdat %>% 
#   group_by(t) %>% 
#   summarise(ks = kurtosis(ks))
# ksk
# 
# ggplot(ksk, aes(t, ks)) + geom_point() + geom_line()


