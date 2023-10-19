rm(list = ls())

library(dplyr)
library(ggplot2)

# source("/home/femeunier/Documents/projects/YGB/scripts/relate_climate_and_AGB.R")
# source("/home/femeunier/Documents/projects/YGB/scripts/relate_climate_and_AGB_model.R")

Climate_AGB_model <- readRDS("./outputs/Climate_AGB_model.RDS") %>% mutate(AGB = 20*AGB) %>% filter(lat <= -8)
Climate_AGB_data <- readRDS("./outputs/Climate_AGB_data.RDS")

Climate_AGB <- bind_rows(list(Climate_AGB_model %>% mutate(type = "model"),
                              Climate_AGB_data %>% mutate(type = "data")))

delta = 0.25

Climate_AGB_sum <- Climate_AGB %>%
  group_by(type,var,value) %>%
  summarise(AGB.med = median(AGB,na.rm = TRUE),
            AGB.mean = mean(AGB,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(value = case_when(type == "data" ~ (value - delta),
                           type == "model" ~ (value + delta)))

ggplot(data = Climate_AGB) +

  geom_line(data = Climate_AGB_sum,
            aes(x = value,y = AGB.med,
                color = type),
            alpha = 1) +

  geom_boxplot(aes(x = (value),y = AGB,
                   fill = type,
                   group = interaction(type,value)),
               outlier.shape = NA,
               alpha = 0.25) +

  facet_grid(~var, scales = "free_x") +

  scale_x_continuous(breaks = seq(1,10)) +

  theme_bw()
