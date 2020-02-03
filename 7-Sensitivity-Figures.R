library(tidyverse)
library(data.table)


# rblindlist all csv files
files <- list.files("~/Projects/abm_IUU_simulation/data/sens", full.names = TRUE)
bdat <- rbindlist(lapply(files, read_csv))

pdat1 <- bdat %>% 
  group_by(nagents) %>% 
  summarise(mean_mu = mean(ks_mean),
            kurt_mu = mean(ks_kurt))

ggplot(pdat1, aes(nagents, mean_mu)) + geom_line()

ggplot(pdat1, aes(nagents, kurt_mu)) + geom_line()

ggplot(bdat, aes(nagents, mean, color=sep_ie)) + geom_point()
