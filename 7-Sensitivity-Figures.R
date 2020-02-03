library(tidyverse)
library(data.table)


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
  labs(x="Repulsion Margin to IUU Vessel", y="Anomaly Index (Mean) Sensitivity") +
  geom_line() + 
  scale_color_gradient(low="blue", high="red") +
  geom_ribbon(aes(ymin = (mean_mu - mean_se*1.96),
                  ymax = (mean_mu + mean_se*1.96)), alpha = 0.2) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  # theme(legend.position = "none") +
  NULL

# ggsave("figures/S1-ABM_Sensitivity_Mean.png", width=6, height=5)

p4 <- ggplot(pdat2, aes(x=sep_ie, y=kurt_mu)) + 
  theme_tufte(12) +
  labs(x="Repulsion Margin to IUU Vessel", y="Anomaly Index (Kurtosis) Sensitivity") +
  geom_line() + 
  scale_color_gradient(low="blue", high="red") +
  geom_ribbon(aes(ymin = (kurt_mu - kurt_se*1.96),
                  ymax = (kurt_mu + kurt_se*1.96)), alpha = 0.2) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  # theme(legend.position = "none") +
  NULL


plot_grid(p1, p2, p3, p4, ncol=2, labels = c("A", "B", "C", "D"))

ggsave("figures/S1-ABM_Sensitivity_Mean_Kurt.png", width=12, height=10)
