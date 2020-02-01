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

# sum: 10886.89
# sd : 0.1172957
sum(dat$x1)
sd(dat$x1)

idat = read_feather("~/Projects/abm_IUU_simulation/data/iuu_vessel_dat.feather")
head(idat)
tail(idat)

length(unique(dat$t))

# dat = filter(dat, t <= 90)
# idat = filter(idat, t <= 90)

print("Creating Plot 1")

p1 <- ggplot(dat, aes(x1, y1)) +
  labs(y="Latitude", x="Longitude") +
  theme_bw() +
  geom_point() +
  geom_point(data=idat, color="red") +
  # geom_point(aes(x1, y1, color=factor(alert_status))) +
  geom_point(aes(x1, y1, color=factor(fishing_status))) +
  annotate("text", x= .7, y= .85, label="Fishing Area", color='blue') +
  geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='blue') +
  geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='blue') +
  geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='blue') +
  geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='blue') +
  transition_manual(frames = t) +
  # ylim(-5, 5) +
  # xlim(-5, 5) +
  ylim(min(dat$y1) - 0.10, max(dat$y1) + 0.10) +
  xlim(min(dat$x1) - 0.10, max(dat$x1) + 0.10) +
  theme(#axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(), 
        legend.position = c(.5, .025),
        legend.direction = "horizontal", 
        legend.margin=margin(t=0, unit='cm')) +
  labs(title = 'Hour of Month: {current_frame} \nHour of IUU Event: 313') +
  NULL
p1
# p1

# animate(p, nframes = 100)
ap1 <- animate(p1, nframes = max(dat$t))


# anim_save("abm_iuu_simulation.mp4", a)
# gg_animate(p1, "output.gif")


# KS-Statistic
ksdat = read_feather("~/Projects/abm_IUU_simulation/data/ks_data.feather")


# Mean results
ksm <- ksdat %>% 
  group_by(t) %>% 
    summarise(ks = mean(ks)) 

# Get 99th percentile
qt95 = quantile(ksm$ks, c(.95))


ksm$signal = ifelse(ksm$ks >= qt95, 1, 0)

ksm

print("Creating Plot 2")

p2 <- ggplot(ksm, aes(t, ks, group=1, color = factor(signal))) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x="Hours in Month", y="Anomaly Index (Mean)") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", "red")) +
  # transition_manual(frames = t) +
  # transition_reveal(along = t) +
  # enter_fade() +
  NULL
p2

# Save object
ap2 <- animate(p2, nframes = max(dat$t))



# Kurtosis results
ksk <- ksdat %>%
  group_by(t) %>%
  summarise(kurt = kurtosis(ks))
ksk

qt95 = quantile(ksk$kurt, c(.95))
ksk$signal = ifelse(ksk$kurt >= qt95, 1, 0)

ksk

print("Creating Plot 3")

p3 <- ggplot(ksk, aes(t, kurt, group=1, color = factor(signal))) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x="Hours in Month", y="Anomaly Index (Kurtosis)") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", "red")) +
  # transition_manual(frames = t) +
  # transition_reveal(along = t) +
  # enter_fade() +
  NULL
p3


ap3 <- animate(p3, nframes = max(dat$t))

print("Combing Plot 1, 2, 3")

# Animate both plots
a_mgif <- image_read(ap1)
b_mgif <- image_read(ap2)
c_mgif <- image_read(ap3)

new_gif <- image_append(c(a_mgif[1], b_mgif[1], c_mgif[1]))
for(i in 2:max(dat$t)){
  combined <- image_append(c(a_mgif[i], b_mgif[i], c_mgif[i]))
  new_gif <- c(new_gif, combined)
}

# new_gif

print("Saving Plot")

# Save animation
anim_save("~/Projects/abm_IUU_simulation/figures/abm_iuu_simulation_2.gif", new_gif)





