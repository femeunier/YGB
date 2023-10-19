rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

# Data
data.file <- file.path(getwd(),"data","CO2_Filou.csv")
data <- read.csv(data.file)

data.f <- data %>% filter(qc_co2_flux < 2)
data.sum <- data.f %>% mutate(h = round(48*(DOY - floor(DOY))))  %>% mutate(h = case_when(h > 48 ~ (h - 48),
                                                                                          h < 0 ~ h + 48,
                                                                                          TRUE ~h))  %>% group_by(h) %>%
  summarise(CO2 = mean(co2_flux,na.rm = TRUE),
            H2O = mean(h2o_flux, na.rm = TRUE))

data.f2 <- data %>% filter(qc_h2o_flux < 2)

data.sum2 <-
  data.f2 %>% mutate(h = round(48 * (DOY - floor(DOY))))  %>% mutate(h = case_when(h > 48 ~ (h - 48),
                                                                                   h < 0 ~ h + 48,
                                                                                   TRUE ~
                                                                                     h))  %>% group_by(h) %>%
  summarise(H2O = mean(h2o_flux, na.rm = TRUE))

# Model outputs

# system2("rsync",c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df_OP_ensemble_YGB.RDS",
#                   file.path(".","outputs","df_OP_ensemble_YGB.RDS")))
#
# system2("rsync",c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df_param_ensemble_YGB.RDS",
#                   file.path(".","outputs","df_param_ensemble_YGB.RDS")))

df_OP_ensemble_YGB <- readRDS(file.path(".","outputs","df_OP_ensemble_YGB.RDS"))
df_param_ensemble_YGB <- readRDS(file.path(".","outputs","df_param_ensemble_YGB.RDS"))

ggplot(data = df_OP_ensemble_YGB %>% mutate(h = t,
                                            h = h + 4,
                                            h = case_when(h > 48 ~ (h - 48),
                                                          TRUE ~ h))) +
  geom_line(aes(x = h,y = NEP, color = run,group = run)) +
  geom_point(data = data.sum,
             aes(x = h + 1,y = -CO2)) +
  theme_bw()

ggplot(data = df_OP_ensemble_YGB %>% mutate(h = t ,
                                            h = h + 4,
                                            h = case_when(h > 48 ~ (h - 48),
                                                          TRUE ~ h))) +
  geom_line(aes(x = h,y = ET, color = run,group = run)) +
  geom_point(data = data.sum2,
             aes(x = h + 1,y = H2O)) +
  theme_bw()

ggplot(data = df_OP_ensemble_YGB %>% filter(t > 18 & t < 28) %>% mutate(h = t,
                                                                        h = h + 4,
                                                                        h = case_when(h > 48 ~ (h - 48),
                                                                                      TRUE ~ h))) +
  geom_line(aes(x = h,y = NEP/ET, color = run,group = run)) +
  geom_point(data = data.sum %>% filter(h > 21 & h < 31),
             aes(x = h + 1,y = -CO2/H2O)) +
  theme_bw()
