library(tidyverse)
library(data.table)
library(moments)
library(stringr)

# Calculate standard error for mean and kurtosis
ster <- function(x, stat){
  n <- length(x)
  if (n < 4){
    return(NA)
  }
  if ( (stat == "kurtosis") & (n >= 4) ){
    ses = sqrt( (6*n*(n - 1) ) / ( (n - 2)*(n + 1)*(n + 3) ) )
    sek = 2 * ses * sqrt( (n^2 - 1) / ( (n - 3) * (n + 5) ) )
    return(sek)
  }
  if (stat == "mean"){
    mu <- ( (sd(x)) / (sqrt(n)) )
    return(mu)
  }
}

# rblindlist all csv files
files <- list.files("~/Projects/abm_IUU_simulation/data/sens", full.names = TRUE)
bdat <- rbindlist(lapply(files, read_csv))
bdat <- as.data.frame(bdat)

bdat <- filter(bdat, sep_ie <= 0.50)
bdat$sep_ie <- bdat$sep_ie*100

# bdat$ks_mean_delta <- bdat$ks_mean/(mean(bdat$ks_mean))
# bdat$ks_kurt_delta <- bdat$ks_kurt/(mean(bdat$ks_kurt))

# bdat$ks_mean <- bdat$ks_mean_delta
# bdat$ks_kurt <- bdat$ks_kurt_delta


pdat1 <- bdat %>% 
  group_by(nagents) %>% 
  summarise(mean_se = ster(ks_mean, "mean"),
            kurt_se = ster(ks_kurt, "kurtosis"),
            mean_mu = mean(ks_mean),
            kurt_mu = mean(ks_kurt))

pdat2 <- bdat %>% 
  group_by(sep_ie) %>% 
  summarise(mean_se = ster(ks_mean, "mean"),
            kurt_se = ster(ks_kurt, "kurtosis"),
            mean_mu = mean(ks_mean),
            kurt_mu = mean(ks_kurt))



p1 <- ggplot(pdat1, aes(x=nagents, y=mean_mu)) + 
  theme_tufte(12) +
  labs(x="Number of Agents", y="Anomaly Index (Mean) Sensitivity") +
  geom_line() + 
  ylim(0, 0.45) +
  scale_color_gradient(low="blue", high="red") +
  geom_ribbon(aes(ymin = (mean_mu - mean_se*1.96),
                  ymax = (mean_mu + mean_se*1.96)), alpha = 0.2) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  # theme(legend.position = "none") +
  NULL

# ggsave("figures/S1-ABM_Sensitivity_Mean.png", width=6, height=5)

p2 <- ggplot(pdat1, aes(x=nagents, y=kurt_mu)) + 
  theme_tufte(12) +
  labs(x="Number of Agents", y="Anomaly Index (Kurtosis) Sensitivity") +
  geom_line() + 
  scale_color_gradient(low="blue", high="red") +
  geom_ribbon(aes(ymin = (kurt_mu - kurt_se*1.96),
                  ymax = (kurt_mu + kurt_se*1.96)), alpha = 0.2) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  # theme(legend.position = "none") +
  NULL

p3 <- ggplot(pdat2, aes(x=sep_ie, y=mean_mu)) + 
  theme_tufte(12) +
  labs(x="Exclusion Margin to IUU Vessel (km)", y="Anomaly Index (Mean) Sensitivity") +
  geom_line() + 
  scale_color_gradient(low="blue", high="red") +
  geom_ribbon(aes(ymin = (mean_mu - mean_se*1.96),
                  ymax = (mean_mu + mean_se*1.96)), alpha = 0.2) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  # theme(legend.position = "none") +
  ylim(0, max(pdat2$mean_mu) + 0.05) +
  NULL

# ggsave("figures/S1-ABM_Sensitivity_Mean.png", width=6, height=5)

p4 <- ggplot(pdat2, aes(x=sep_ie, y=kurt_mu)) + 
  theme_tufte(12) +
  labs(x="Exclusion Margin to IUU Vessel (km)", y="Anomaly Index (Kurtosis) Sensitivity") +
  geom_line() + 
  scale_color_gradient(low="blue", high="red") +
  geom_ribbon(aes(ymin = (kurt_mu - kurt_se*1.96),
                  ymax = (kurt_mu + kurt_se*1.96)), alpha = 0.2) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  # theme(legend.position = "none") +
  ylim(0, max(pdat2$kurt_mu) + 1) +
  NULL


plot_grid(p1, p2, p3, p4, ncol=2, labels = c("A", "B", "C", "D"))

ggsave("figures/S1-ABM_Sensitivity_Mean_Kurt.png", width=12, height=10)


# ---------------------------------------------------------
# Figure S2 Sensitivity around IUU event

# rblindlist all csv files
files <- list.files("~/Projects/abm_IUU_simulation/data/full_sens/", full.names = TRUE)
ifiles <- list.files("~/Projects/abm_IUU_simulation/data/full_sens", full.names = FALSE)


for (i in 1:length(files)){
  dat <- read_csv(files[i], progress = FALSE)
  filename <- ifiles[i]
  agents <- str_extract(filename, "[^-]+")
  margin <- sub(".*ie", "", filename)
  margin <- substr(margin, 1, nchar(margin) - 4)

  # Get mean, kurtosis, find 95% and filter out
  dat2 <- dat %>% 
    group_by(t) %>% 
    summarise(mean_ks = mean(ks),
              kurt_ks = kurtosis(ks),
              mean_pvalue = mean(pvalue))
  
  mean_95 <- quantile(filter(dat2, t <= 200)$mean_ks, c(0.95), na.rm=TRUE)
  kurt_95 <- quantile(filter(dat2, t <= 200)$kurt_ks, c(0.95), na.rm=TRUE)
  
  kurt_95 <- ifelse(is.na(kurt_95), 999, kurt_95)
  
  mean_mdat <- filter(dat2, mean_ks >= mean_95)
  kurt_mdat <- filter(dat2, kurt_ks >= kurt_95)
  
  mean_mdat$metric <- "mean"
  mean_mdat$agents <- agents
  mean_mdat$margin <- margin
  
  if (nrow(kurt_mdat) > 0){
    kurt_mdat$metric <- "kurt"
    kurt_mdat$agents <- agents
    kurt_mdat$margin <- margin
    mdat <- rbind(mean_mdat, kurt_mdat)
  } else {
    mdat <- mean_mdat
  }
  
  # mdat <- filter(mdat, mean_pvalue <= 0.05)
  
  write_csv(mdat, paste0("~/Projects/abm_IUU_simulation/data/figureS2_data/", agents, "-", margin, ".csv"))
  print(i)
}

# Get files and build figure
# rblindlist all csv files
files <- list.files("~/Projects/abm_IUU_simulation/data/figureS2_data/", full.names = TRUE)
bdat <- rbindlist(lapply(files, read_csv))
bdat <- as.data.frame(bdat)

head(bdat)

ppdat1 <- filter(bdat, metric == "mean" & mean_pvalue <= 0.025)

p5 <- ggplot(ppdat1, aes(x=t, y=agents, color=mean_pvalue)) +
  theme_tufte(12) +
  geom_point(shape=15) +
  geom_vline(xintercept = 24*14.5, color='red', size=1.5) +
  labs(x=NULL, y="# of Agents \n (Mean Anomaly Index)") +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1), legend.position = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700), limits = c(0, 720)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), limits = c(0, 100)) +
  NULL

  
p6 <- ggplot(ppdat1, aes(x=t, y=margin*100, color=mean_pvalue)) +
  theme_tufte(12) +
  geom_point(shape=15) +
  lims(color = c(0, 0.025)) +
  geom_vline(xintercept = 24*14.5, color='red', size=1.5) +
  labs(x=NULL, y="Exclusion Margin (km) \n (Mean Anomaly Index)", color="p-value") +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700), limits = c(0, 720)) +
  guides(fill = FALSE,
   color = guide_colorbar(title.hjust = unit(1.1, 'cm'),
                          title.position = "top",
                          frame.colour = "black",
                          barwidth = .5,
                          barheight = 20,
                          label.position = 'right')) +
  NULL


ppdat2 <- filter(bdat, metric == "kurt" & mean_pvalue <= 0.025)

p7 <- ggplot(ppdat2, aes(x=t, y=agents, color=mean_pvalue)) + 
  theme_tufte(12) +
  geom_point(shape=15) + 
  geom_vline(xintercept = 24*14.5, color='red', size=1.5) +
  labs(x="Day of Month", y="# of Agents \n (Kurtosis Anomaly Index)") +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1), legend.position = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700), limits = c(0, 720)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), limits = c(0, 100)) +
  NULL
p7

p8 <- ggplot(ppdat2, aes(x=t, y=margin*100, color=mean_pvalue)) + 
  theme_tufte(12) +
  geom_point(shape=15) +
  lims(color = c(0, 0.025)) +
  geom_vline(xintercept = 24*14.5, color='red', size=1.5) +
  labs(x="Day of Month", y="Exclusion Margin (km)\n (Kurtosis Anomaly Index)", color="p-value") +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700), limits = c(0, 720)) +
  guides(fill = FALSE,
     color = guide_colorbar(title.hjust = unit(1.1, 'cm'),
                            title.position = "top",
                            frame.colour = "black",
                            barwidth = .5,
                            barheight = 20,
                            label.position = 'right')) +
  NULL


plot_grid(p5, p6, p7, p8, ncol=2, labels = c("A", "B", "C", "D"), rel_widths = c(0.875, 1, 0.875, 1))

ggsave("figures/S2-ABM_Sensitivity_Mean_Kurt_Day.png", width=12, height=10)



# ---------------------------------------------------------
# Figure S3 Sensitivity around IUU event - fraction of days alert in event window


files <- list.files("~/Projects/abm_IUU_simulation/data/figureS2_data/", full.names = TRUE)
bdat <- rbindlist(lapply(files, read_csv))
bdat <- as.data.frame(bdat)

# Get proportion of event window alert
bdat$agents_margin <- paste0(bdat$agents, "_", bdat$margin)

head(bdat)

# Mean
bdat2 <- bdat %>% 
  filter(metric == "mean" & margin <= 0.40) %>% 
  filter(t > 13*24 & t < 16*24) %>% 
  group_by(agents_margin) %>% 
  summarise(frac = n()/72,
            agents = mean(agents),
            margin = mean(margin))

dp1 <- ggplot(bdat2, aes(x=margin*100, y=agents)) + 
  geom_tile(aes(fill=frac)) +
  theme_tufte(12) +
  ggtitle("Proportion of Alerted Days in Event Window \n (Mean Anomaly Index)") +
  labs(x=NULL, y="Number of Agents", fill="P(Days)") +
  lims(fill = c(0, 1)) +
  scale_fill_gradientn(colours=rev(brewer.pal(11, "Spectral")), na.value = 'salmon', 
                       limits = c(0, 1), 
                       breaks = c(0.0, 0.25, 0.50, 0.75, 1)) +
  scale_y_continuous(expand=c(0,0), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_x_continuous(expand=c(0,0), breaks = seq(0, 50, 5)) +
  theme(legend.position = 'right',
        
        plot.title = element_text(hjust = 0.5),
        legend.margin=margin(l = 0, unit='cm'),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        panel.grid = element_blank(),
        panel.background=element_rect(fill="#5E4FA2", colour="#5E4FA2")) +
  guides(fill = guide_colorbar(label.hjust = unit(0, 'cm'),
                               frame.colour = "black",
                               barwidth = .5,
                               barheight = 13,
                               draw.ulim = TRUE)) +
  NULL


# Kurtosis
bdat3 <- bdat %>% 
  filter(metric == "kurt" & margin <= 0.40) %>% 
  filter(t > 14*24 & t < 15*24) %>% 
  group_by(agents_margin) %>% 
  summarise(frac = n()/24,
            agents = mean(agents),
            margin = mean(margin))

dp2 <- ggplot(bdat3, aes(x=margin*100, y=agents)) + 
  geom_tile(aes(fill=frac)) +
  theme_tufte(12) +
  ggtitle("Proportion of Alert on 15th Day \n (Kurtosis Anomaly Index)") +
  labs(x="Exclusion Margin (km)", y="Number of Agents", fill="P(Days)") +
  lims(fill = c(0, 1)) +
  scale_fill_gradientn(colours=rev(brewer.pal(11, "Spectral")), na.value = 'salmon', 
                       limits = c(0, 1), 
                       breaks = c(0.0, 0.25, 0.50, 0.75, 1)) +
  scale_y_continuous(expand=c(0,0), 
                     breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     labels = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_x_continuous(expand=c(0,0), breaks = seq(0, 50, 5)) +
  theme(legend.position = 'right',
        plot.title = element_text(hjust = 0.5),
        legend.margin=margin(l = 0, unit='cm'),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        panel.grid = element_blank(),
        panel.background=element_rect(fill="#5E4FA2", colour="#5E4FA2")) +
  guides(fill = guide_colorbar(label.hjust = unit(0, 'cm'),
                               frame.colour = "black",
                               barwidth = .5,
                               barheight = 13,
                               draw.ulim = TRUE)) +
  NULL

dp2
# dp1
# dp2

plot_grid(dp1, dp2, ncol = 1,labels = c("A", "B"))

ggsave("figures/S4-ABM_Sensitivity_Mean_Prop_Days.pdf", width = 5, height = 8)
  
  
  
  
