library(feather)
library(tidyverse)
library(gifski)
library(moments)
library(magick)
library(cowplot)
library(ggthemes)
#install.packages('gifski')
#install.packages('png')


setwd("~/Projects/abm_IUU_simulation/")

dat = read_feather("~/Projects/abm_IUU_simulation/data/v0.50/vessel_dat.feather")
head(dat)

# sum: 10886.89
# sd : 0.1172957
sum(dat$x1)
sd(dat$x1)

idat = read_feather("~/Projects/abm_IUU_simulation/data/v0.50/iuu_vessel_dat.feather")
head(idat)
tail(idat)

length(unique(dat$t))

tt = 100



pdat1 = filter(dat, t == tt)
pdat1s = filter(dat, t == tt | t == tt + 1)
ipdat1 = filter(idat, t == tt)


pdat1_nn <- pdat1 %>% mutate(k = 1) 

pdat1_nn <- pdat1_nn %>% 
 full_join(pdat1_nn, by = "k") %>% 
 mutate(dist = sqrt((x1.x - x1.y)^2 + (y1.x - y1.y)^2)) %>%
 select(-k) %>% 
 group_by(vessel.x) %>% 
 arrange(dist) %>% 
 mutate(nn = 1:10) %>% 
 filter(row_number() %in% c(2, 3, 4, 5, 6))  %>% 
 mutate(nn = 1:5) %>% 
 arrange(vessel.x)

pdat2 <- pdat1 %>% 
  arrange(vessel, t) %>% 
  mutate(x_2 = x1 + 0.0001 * (lead(x1) - x1),
         y_2 = y1 + 0.0001 * (lead(y1) - y1)) %>%
  filter(!is.na(x_2))


  
print("Creating Plot 1")

p1 <- ggplot(pdat1, aes(x1, y1)) +
  labs(y="Latitude", x="Longitude") +
  # theme_bw() +
  theme_tufte(12) +
  # geom_point(shape=2) +
  geom_point(data=ipdat1, color="red") +
  # geom_point(aes(x1, y1, color=factor(alert_status))) +
  # geom_point(aes(x1, y1, color=factor(fishing_status)), shape=2) +
  geom_segment(data=pdat1_nn, aes(x=x1.x, y=y1.x, xend=x1.y, yend=y1.y), color='grey') +
  # geom_segment(data = pdat2, aes(x=x1, y=y1, xend = x_2, yend = y_2),
  #              arrow = arrow(length = unit(.3,"cm"))) +
  # geom_point(aes(x1, y1, color=factor(fishing_status)), color='red', size=2) +
  geom_segment(data = pdat2, aes(x=x1, y=y1, xend = x_2, yend = y_2),
               arrow = arrow(length = unit(.3,"cm")), color='red') +
  annotate("text", x= .7, y= .25, label="Fishing Area", color='black') +
  geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='grey', linetype="dashed") +
  geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='grey', linetype="dashed") +
  geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='grey', linetype="dashed") +
  geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='grey', linetype="dashed") +
  annotate("text", x= 0.5551, y= 0.90, label="Normal", color='black') +
  scale_color_manual(values=c("black", "red")) +
  # transition_manual(frames = t) +
  ylim(0.1, 0.9) +
  xlim(0.55, 0.85) +
  # ylim(min(dat$y1) - 0.10, max(dat$y1) + 0.10) +
  # xlim(min(dat$x1) - 0.10, max(dat$x1) + 0.10) +
  theme(
    #axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.title = element_blank(), 
    legend.position = "none",
    panel.border = element_rect(colour = "grey", fill=NA, size=1),
    # legend.position = c(.5, .025),
    legend.direction = "horizontal", 
    legend.margin=margin(t=0, unit='cm')) +
  # labs(title = 'Hour of Month: {current_frame} \nHour of IUU Event: 313') +
  NULL
p1




# 335

tt = 335


pdat2 = filter(dat, t == tt)

ipdat2 = filter(idat, t == tt)

pdat2s = filter(dat, t == tt | t == tt + 1)

#Adjust to show exaggerated directional change
# pdat2s[pdat2s['vessel'] == 0, 'y1'][[1]][2] = pdat2s[pdat2s['vessel'] == 0, 'y1'][[1]][1] + 0.5
pdat2s[pdat2s['vessel'] == 1, 'y1'][[1]][2] = pdat2s[pdat2s['vessel'] == 1, 'y1'][[1]][1] + 0.5
pdat2s[pdat2s['vessel'] == 2, 'y1'][[1]][2] = pdat2s[pdat2s['vessel'] == 2, 'y1'][[1]][1] + 0.5


pdat2_nn <- pdat2 %>% mutate(k = 1) 

pdat2_nn <- pdat2_nn %>% 
 full_join(pdat2_nn, by = "k") %>% 
 mutate(dist = sqrt((x1.x - x1.y)^2 + (y1.x - y1.y)^2)) %>%
 select(-k) %>% 
 group_by(vessel.x) %>% 
 arrange(dist) %>% 
 mutate(nn = 1:10) %>% 
 filter(row_number() %in% c(2, 3, 4, 5, 6))  %>% 
 mutate(nn = 1:5) %>% 
 arrange(vessel.x)

pdat3 <- pdat2s %>% 
  arrange(vessel, t) %>% 
  mutate(x_2 = x1 + 0.0001 * (lead(x1) - x1),
         y_2 = y1 + 0.0001 * (lead(y1) - y1)) %>%
  filter(!is.na(x_2)) %>% 
  filter(t == tt)




print("Creating Plot 1")

p2 <- ggplot(pdat2, aes(x1, y1)) +
  labs(y="Latitude", x="Longitude") +
  theme_tufte(12) +
  # geom_point(shape=2) +
  # geom_point(data=ipdat2, color="red2", shape=16, size=20, alpha=0.25) + # Shaded region IUU 
  geom_segment(data=pdat2_nn, aes(x=x1.x, y=y1.x, xend=x1.y, yend=y1.y), color='grey') +
  # geom_point(aes(x1, y1, color=factor(fishing_status)), color='red', size=2) +
  geom_segment(data = pdat3, aes(x=x1, y=y1, xend = x_2, yend = y_2),
               arrow = arrow(length = unit(.3,"cm")), color='red') +
  annotate("text", x= .7, y= .25, label="Fishing Area", color='black') +
  geom_segment(aes(x=0.60, xend=0.8, y=0.2, yend=0.20), color='grey', linetype="dashed") +
  geom_segment(aes(x=0.60, xend=0.8, y=0.8, yend=0.80), color='grey', linetype="dashed") +
  geom_segment(aes(x=0.60, xend=0.6, y=0.2, yend=0.80), color='grey', linetype="dashed") +
  geom_segment(aes(x=0.80, xend=0.8, y=0.2, yend=0.80), color='grey', linetype="dashed") +
  annotate("text", x= 0.562, y= 0.9, label="IUU Event", color='black') +
  scale_color_manual(values=c("black", "red")) +
  geom_point(data=ipdat2, color="darkred", alpha = 0.15, size=100) +
  geom_point(data=ipdat2, color="red", shape=1, size=3.5) +
  geom_point(data=ipdat2, color="red") +
  geom_rect(data = NULL, aes(xmin=0.8, xmax=1, ymin=0.1, ymax=.9), fill="white", inherit.aes = FALSE) +
  geom_rect(data = NULL, aes(xmin=0.6, xmax=1, ymin=0, ymax=0.2), fill="white", inherit.aes = FALSE) +
  # geom_segment(aes(x = 0.7715, y = 0.6, xend = 0.7715, yend = 0.67),
              # color='red', size=.75, arrow = arrow(length = unit(0.15, "cm"))) +
  # geom_segment(aes(x = 0.7625, y = 0.58, xend = 0.745, yend = 0.635),
              # color='red', size=.75, arrow = arrow(length = unit(0.15, "cm"))) +
  # geom_segment(aes(x = 0.76, y = 0.5575, xend = 0.735, yend = 0.5575),
              # color='red', size=.75, arrow = arrow(length = unit(0.15, "cm"))) +
  # geom_segment(aes(x = 0.50, y = 0.35, xend = 0.5, yend = 0.45),
              # color='red', size=.75, arrow = arrow(length = unit(0.15, "cm"))) +
  # transition_manual(frames = t) +
  # ylim(-5, 5) +
  # xlim(-5, 5) +
  # ylim(min(dat$y1) - 0.10, max(dat$y1) + 0.10) +
  # xlim(min(dat$x1) - 0.10, max(dat$x1) + 0.10) +
  # ylim(0.08, 0.9) +
  coord_cartesian(ylim=c(0.08, 0.9), xlim = c(0.55, 0.85)) +
  # xlim(0.55, 0.85) +
  theme(
    #axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.title = element_blank(), 
    legend.position = "none",
    # legend.position = c(.5, .025),
    panel.border = element_rect(colour = "grey", fill=NA, size=1),
    legend.direction = "horizontal", 
    legend.margin=margin(t=0, unit='cm')) +
  # labs(title = 'Hour of Month: {current_frame} \nHour of IUU Event: 313') +
  NULL
p2


# plot_grid(p1, p2, p3, p4, ncol=2, labels = c("A", "B", "C", "D"))

# ggsave('figures/abm_sim.png', width=12, height = 8)


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

p3 <- ggplot(ksm, aes(t, ks, group=1, color = factor(signal))) +
  geom_point(data=filter(ksm, signal == 1), aes(t, ks), color='#d62728', size=3) +
  geom_line(color='#1f77b4') +
  theme_tufte(12) +
  labs(x="Hour in Month", y="Anomaly Index (Mean)") +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  scale_color_manual(values=c("black", "red")) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700)) +
  # geom_vline(xintercept = 14*24, linetype = "dashed") +
  geom_vline(xintercept = 15*24, linetype = "dashed", color='#d62728', size=1) +
  # annotate("text", x=20*24, y=.55, label="IUU Event Window") +
  NULL
p3


# Kurtosis results
ksk <- ksdat %>%
  group_by(t) %>%
  summarise(kurt = kurtosis(ks))
ksk

qt95 = quantile(ksk$kurt, c(.95), na.rm = TRUE)
ksk$signal = ifelse(ksk$kurt >= qt95, 1, 0)

ksk

print("Creating Plot 3")

p4 <- ggplot(ksk, aes(t, kurt, group=1, color = factor(signal))) + 
  geom_point(data=filter(ksk, signal == 1), aes(t, kurt), color='#d62728', size=3) +
  geom_line(color='#1f77b4') +
  theme_tufte(12) +
  labs(x="Hour in Month", y="Anomaly Index (Kurtosis)") +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  scale_color_manual(values=c("black", "red")) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700)) +
  # geom_vline(xintercept = 14*24, linetype = "dashed") +
  geom_vline(xintercept = 15*24, linetype = "dashed", color='#d62728', size=1) +
  # annotate("text", x=20*24, y=16, label="IUU Event Window") +
  NULL
p4


plot_grid(p1, p2, p3, p4, ncol=2, labels = c("A", "B", "C", "D"))

ggsave('figures/abm_sim.png', width=12, height = 8)

