library(gganimate)
library(feather)
library(tidyverse)
library(gifski)
library(moments)
library(magick)
#install.packages('gifski')
#install.packages('png')


setwd("~/Projects/abm_IUU_simulation/")

dat = read_feather("~/Projects/abm_IUU_simulation/data/v0.50/vessel_dat.feather")
head(dat)

nframes = 200

# sum: 10886.89
# sd : 0.1172957
sum(dat$x1)
sd(dat$x1)

idat = read_feather("~/Projects/abm_IUU_simulation/data/v0.50/iuu_vessel_dat.feather")
head(idat)
tail(idat)

length(unique(dat$t))

# dat = filter(dat, t <= 90)
# idat = filter(idat, t <= 90)

print("Creating Plot 1")

p1 <- 
  ggplot(dat, aes(x1, y1)) +
  # ggplot(filter(dat, t == 350), aes(x1, y1)) +
  labs(y="Latitude", x="Longitude") +
  theme_bw(base_size = 15) +
  geom_point() +
  geom_point(data=idat, color="red", size = 3) +
  geom_point(data=idat, color="red", shape = 1, size=5.5) +
  # geom_point(aes(x1, y1, color=factor(alert_status))) +
  geom_point(aes(x1, y1, color=factor(fishing_status)), size=4) +
  annotate("text", x= .7, y= .85, label="Fishing Area", color='blue', size = 6) +
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

animate(p1, nframes = nframes)

ap1 <- animate(p1, nframes = nframes)


# anim_save("abm_iuu_simulation.mp4", a)
# gg_animate(p1, "output.gif")


# KS-Statistic
ksdat = read_feather("~/Projects/abm_IUU_simulation/data/v0.50/ks_data.feather")


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
  theme_bw(base_size = 15) +
  labs(x="Hours in Month", y="Anomaly Index (Mean)") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", "red")) +
  transition_manual(frames = t) +
  transition_reveal(along = t) +
  enter_fade() +
  NULL
# p2

# Save object
ap2 <- animate(p2, nframes = nframes)



# Kurtosis results
ksk <- ksdat %>%
  group_by(t) %>%
  summarise(kurt = kurtosis(ks))
ksk

qt95 = quantile(ksk$kurt, c(.95), na.rm = TRUE)
ksk$signal = ifelse(ksk$kurt >= qt95, 1, 0)

ksk

print("Creating Plot 3")

p3 <- ggplot(ksk, aes(t, kurt, group=1, color = factor(signal))) + 
  geom_point() +
  geom_line() +
  theme_bw(base_size = 15) +
  labs(x="Hours in Month", y="Anomaly Index (Kurtosis)") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", "red")) +
  transition_manual(frames = t) +
  transition_reveal(along = t) +
  enter_fade() +
  NULL
# p3


ap3 <- animate(p3, nframes = nframes)

print("Combing Plot 1, 2, 3")

# Animate both plots
a_mgif <- image_read(ap1)
b_mgif <- image_read(ap2)
c_mgif <- image_read(ap3)

new_gif <- image_append(c(a_mgif[1], b_mgif[1], c_mgif[1]))
for(i in 2:nframes){
  combined <- image_append(c(a_mgif[i], b_mgif[i], c_mgif[i]))
  new_gif <- c(new_gif, combined)
}

# new_gif

print("Saving Plot")

# Save animation
anim_save("~/Projects/abm_IUU_simulation/figures/abm_iuu_simulation_v0.50.gif", new_gif)




# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# Misc stills

nframe = 100
p1 <- ggplot(filter(dat, t == nframe), aes(x1, y1)) +
  labs(y="Latitude", x="Longitude") +
  theme_bw(base_size = 15) +
  geom_point() +
  geom_point(data=filter(idat, t == nframe), color="red", size = 3) +
  geom_point(data=filter(idat, t == nframe), color="red", shape=1, size=3.5) +
  # geom_point(aes(x1, y1, color=factor(alert_status))) +
  geom_point(aes(x1, y1, color=factor(fishing_status))) +
  annotate("text", x= .7, y= .85, label="Fishing Area", color='blue', size = 6) +
  geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='blue') +
  geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='blue') +
  geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='blue') +
  geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='blue') +
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
    legend.position = "none",
    # legend.direction = "none", 
    legend.margin=margin(t=0, unit='cm')) +
  labs(title = 'Hour of Month: 100') +
  NULL

p1

p2 <- ggplot(filter(ksm, t <= nframe), aes(t, ks, group=1, color = factor(signal))) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 15) +
  labs(x="Hours in Month", y="Anomaly Index (Mean)") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", "red")) +
  xlim(0, 720) +
  geom_vline(xintercept = 313, color = 'red') +
  annotate("text", x= 210, y= 0.55, label="IUU Event", color='red', size = 6) +
  ylim(0, 0.55) +
  NULL
p2


p3 <- ggplot(filter(ksk, t <= nframe), aes(t, kurt, group=1, color = factor(signal))) + 
  geom_point() +
  geom_line() +
  theme_bw(base_size = 15) +
  labs(x="Hours in Month", y="Anomaly Index (Kurtosis)") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", "red")) +
  xlim(0, 720) +
  geom_vline(xintercept = 313, color = 'red') +
  ylim(0, 16) +
  NULL
p3


plot_grid(p1, p2, p3, ncol=3)

ggsave("~/Projects/abm_IUU_simulation/figures/IUU_Still_1.png", width = 18, height = 4)







nframe = 350
p1 <- ggplot(filter(dat, t == nframe), aes(x1, y1)) +
  labs(y="Latitude", x="Longitude") +
  theme_bw(base_size = 15) +
  geom_point() +
  geom_point(data=filter(idat, t == nframe), color="red", size = 3) +
  geom_point(data=filter(idat, t == nframe), color="red", shape=1, size=5) +
  # geom_point(aes(x1, y1, color=factor(alert_status))) +
  geom_point(aes(x1, y1, color=factor(fishing_status))) +
  annotate("text", x= .7, y= .85, label="Fishing Area", color='blue', size = 6) +
  geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='blue') +
  geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='blue') +
  geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='blue') +
  geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='blue') +
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
    legend.position = "none",
    # legend.direction = "none", 
    legend.margin=margin(t=0, unit='cm')) +
  labs(title = 'Hour of Month: 350') +
  NULL

p1

p2 <- ggplot(filter(ksm, t <= nframe), aes(t, ks, group=1, color = factor(signal))) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 15) +
  labs(x="Hours in Month", y="Anomaly Index (Mean)") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", "red")) +
  xlim(0, 720) +
  geom_vline(xintercept = 313, color = 'red') +
  annotate("text", x= 210, y= 0.55, label="IUU Event", color='red', size = 6) +
  ylim(0, 0.55) +
  NULL
p2

p3 <- ggplot(filter(ksk, t <= nframe), aes(t, kurt, group=1, color = factor(signal))) + 
  geom_point() +
  geom_line() +
  theme_bw(base_size = 15) +
  labs(x="Hours in Month", y="Anomaly Index (Kurtosis)") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", "red")) +
  xlim(0, 720) +
  geom_vline(xintercept = 313, color = 'red') +
  ylim(0, 16) +
  NULL
p3


plot_grid(p1, p2, p3, ncol=3)

ggsave("~/Projects/abm_IUU_simulation/figures/IUU_Still_2.png", width = 18, height = 4)





nframe = 500
p1 <- ggplot(filter(dat, t == nframe), aes(x1, y1)) +
  labs(y="Latitude", x="Longitude") +
  theme_bw(base_size = 15) +
  geom_point() +
  geom_point(data=filter(idat, t == nframe), color="red", size = 3) +
  geom_point(data=filter(idat, t == nframe), color="red", shape=1, size=3.5) +
  # geom_point(aes(x1, y1, color=factor(alert_status))) +
  geom_point(aes(x1, y1, color=factor(fishing_status))) +
  annotate("text", x= .7, y= .85, label="Fishing Area", color='blue', size = 6) +
  geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='blue') +
  geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='blue') +
  geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='blue') +
  geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='blue') +
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
    legend.position = "none",
    # legend.direction = "none", 
    legend.margin=margin(t=0, unit='cm')) +
  labs(title = 'Hour of Month: 500') +
  NULL

p1

p2 <- ggplot(filter(ksm, t <= nframe), aes(t, ks, group=1, color = factor(signal))) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 15) +
  labs(x="Hours in Month", y="Anomaly Index (Mean)") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", "red")) +
  xlim(0, 720) +
  geom_vline(xintercept = 313, color = 'red') +
  annotate("text", x= 210, y= 0.55, label="IUU Event", color='red', size = 6) +
  ylim(0, 0.55) +
  NULL
p2


p3 <- ggplot(filter(ksk, t <= nframe), aes(t, kurt, group=1, color = factor(signal))) + 
  geom_point() +
  geom_line() +
  theme_bw(base_size = 15) +
  labs(x="Hours in Month", y="Anomaly Index (Kurtosis)") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", "red")) +
  xlim(0, 720) +
  geom_vline(xintercept = 313, color = 'red') +
  ylim(0, 16) +
  NULL
p3


plot_grid(p1, p2, p3, ncol=3)

ggsave("~/Projects/abm_IUU_simulation/figures/IUU_Still_3.png", width = 18, height = 4)
