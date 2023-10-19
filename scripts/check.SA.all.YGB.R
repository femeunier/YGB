rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

files2transfer <- c("df.diurnal.Water.SA.RDS",
                    "df.seasonal.Water.SA.RDS",
                    "df.diurnal.C.SA.RDS",
                    "df.seasonal.C.SA.RDS",
                    "df_param_SA_YGB.RDS")
# files2transfer <- c("df.diurnal.Water.SA.RDS","df.seasonal.Water.SA.RDS","df.diurnal.C.SA.RDS","df.seasonal.C.SA.RDS","df_param_SA_YGB.RDS")

OP.list <- list()

for (ifile in seq(1,length(files2transfer))){
  system2("rsync",c("-avz",paste0("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",files2transfer[ifile]),
                    file.path(".","outputs",files2transfer[ifile])))
  OP.list[[tools::file_path_sans_ext(files2transfer[ifile])]] <- readRDS(file.path(".","outputs",files2transfer[ifile]))
}

###########################################################################################################################
# Data
###########################################################################################################################

data <- read.csv(file.path(getwd(),"data","CO2_Filou.csv")) %>% filter(qc_co2_flux < 2,qc_h2o_flux<2) %>%
  dplyr::select(date,time,DOY,co2_flux,h2o_flux) %>% mutate(t = as.POSIXct(paste(date,time),tz="CET"),
                                                            month = month(t),
                                                            year = year(t)) %>% rename(hour = time) %>%
  mutate(co2_flux = co2_flux/1000*1e-6*12*86400*365,
         ET = h2o_flux/1000*18/1000*86400) %>%
  filter(co2_flux <= 15) # Weird values! To be double-checked with Marijn/Lodewijk

###########################################################################################################################
## Seasonal
###########################################################################################################################

data.seasonal <- data %>% mutate(NEP = -(co2_flux),
                                 ET = (ET))
levels(data.seasonal$hour) <- seq(0,23.5,0.5)
data.seasonal$hour <- seq(0,23.5,0.5)[as.numeric(data.seasonal$hour)]

data.seasonal.C <- data.seasonal %>% dplyr::select(-c(ET))
data.seasonal.C.long <- data.seasonal.C %>% mutate(var = "NEP") %>% rename(value = NEP)
data.seasonal.C.long.sum <- data.seasonal.C.long %>% group_by(month,hour,var) %>% summarise(value.m = mean(value),
                                                                                            value.sd = sd(value),
                                                                                            N = length(value)) %>%
  group_by(month,var) %>% summarise(value.sd = NA,
                                    value.m = mean(value.m))


data.seasonal.Water <- data.seasonal %>% dplyr::select(-c(NEP))
data.seasonal.Water.long <- data.seasonal.Water %>% mutate(var = "ET") %>% rename(value = ET)
data.seasonal.Water.long.sum <- data.seasonal.Water.long %>% group_by(month,hour,var) %>% summarise(value.m = mean(value),
                                                                                                    value.sd = sd(value),
                                                                                                    N = length(value)) %>%
  group_by(month,var) %>% summarise(value.sd = NA,
                                    value.m = mean(value.m))

# data.seasonal <- data %>% group_by(year,month) %>% summarise(NEP = mean(-co2_flux),
#                                                              ET = mean(ET))
#
# data.seasonal.C <- data.seasonal %>% dplyr::select(-c(ET))
# data.seasonal.C.long <- data.seasonal.C %>% mutate(var = "NEP") %>% rename(value = NEP)
# data.seasonal.C.long.sum <- data.seasonal.C.long %>% group_by(month,var) %>% summarise(value.m = mean(value),
#                                                                                        value.sd = sd(value))
#
# data.seasonal.Water <- data.seasonal %>% dplyr::select(-c(NEP))
# data.seasonal.Water.long <- data.seasonal.Water %>% mutate(var = "ET") %>% rename(value = ET)
# data.seasonal.Water.long.sum <- data.seasonal.Water.long %>% group_by(month,var) %>% summarise(value.m = mean(value),
#                                                                                                value.sd = sd(value))


###########################################################################################################################
## Diurnal
###########################################################################################################################

data.diurnal <- data %>% mutate(NEP = -(co2_flux),
                                ET = (ET))
levels(data.diurnal$hour) <- seq(0,23.5,0.5)
data.diurnal$hour <- seq(0,23.5,0.5)[as.numeric(data.diurnal$hour)]

data.diurnal.C <- data.diurnal %>% dplyr::select(-c(ET))
data.diurnal.C.long <- data.diurnal.C %>% mutate(var = "NEP") %>% rename(value = NEP)
data.diurnal.C.long.sum <- data.diurnal.C.long %>% group_by(hour,var) %>% summarise(value.m = mean(value),
                                                                                    value.sd = sd(value))

data.diurnal.Water <- data.diurnal %>% dplyr::select(-c(NEP))
data.diurnal.Water.long <- data.diurnal.Water %>% mutate(var = "ET") %>% rename(value = ET)
data.diurnal.Water.long.sum <- data.diurnal.Water.long %>% group_by(hour,var) %>% summarise(value.m = mean(value),
                                                                                            value.sd = sd(value))

C.diurnal.ensemble <- OP.list[[3]] %>% group_by(param,quantile) %>% mutate(run = cur_group_id()) %>%
  filter(year > min(year)) %>% ungroup() %>% dplyr::select(hour,NEP,run)
Water.diurnal.ensemble <- OP.list[[1]] %>% group_by(param,quantile) %>% mutate(run = cur_group_id()) %>%
  filter(year > min(year)) %>%  ungroup() %>% dplyr::select(hour,ET,run)
C.seasonal.ensemble <- OP.list[[4]] %>% group_by(param,quantile) %>% mutate(run = cur_group_id()) %>%
  filter(year > min(year)) %>%  ungroup() %>% dplyr::select(month,NEP,run)
Water.seasonal.ensemble <- OP.list[[2]] %>% group_by(param,quantile) %>% mutate(run = cur_group_id()) %>%
  filter(year > min(year)) %>%  ungroup() %>% dplyr::select(month,ET,run)

runs = C.seasonal.ensemble %>% filter(month %in% c(10,11,12)) %>%  group_by(run,month) %>% summarise(NEP = mean(NEP)) %>% group_by(run) %>%
  mutate(diff = NEP - data.seasonal.C.long.sum$value.m,
         diff.2 = diff^2) %>% summarise(RMSE = sum(diff.2)) %>% arrange(RMSE) %>% pull(run)
best.run <- runs[1]

Water.seasonal.ensemble.sum <- Water.seasonal.ensemble %>% group_by(month) %>% summarise(ET.m = mean(ET[run == best.run]),
                                                                                         ET.min = min(ET),
                                                                                         ET.max = max(ET))

C.seasonal.ensemble.sum <- C.seasonal.ensemble %>% group_by(month) %>% summarise(NEP.m = mean(NEP[run == best.run]),
                                                                                 NEP.min = min(NEP),
                                                                                 NEP.max = max(NEP))

Water.diurnal.ensemble.sum <- Water.diurnal.ensemble %>% group_by(hour) %>% summarise(ET.m = mean(ET[run == best.run]),
                                                                                      ET.min = min(ET),
                                                                                      ET.max = max(ET))

C.diurnal.ensemble.sum <- C.diurnal.ensemble %>% group_by(hour) %>% summarise(NEP.m = mean(NEP[run == best.run]),
                                                                              NEP.min = min(NEP),
                                                                              NEP.max = max(NEP))

ggplot(data = data.diurnal.C.long.sum) +
  geom_ribbon(data = C.diurnal.ensemble.sum,
              aes(x = hour,ymin = NEP.min,ymax = NEP.max),fill = "lightgrey",color = NA,alpha = 0.3) +
  geom_line(data = C.diurnal.ensemble.sum,
            aes(x = hour,y = NEP.m),color = "black") +
  geom_point(aes(x = hour,y = value.m,group = interaction(var))) +
  geom_errorbar(aes(x = hour,y = value.m,ymin = value.m + value.sd,ymax = value.m - value.sd,
                    group = interaction(var))) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(0,24,6)) +
  facet_wrap(~ var) +
  theme_bw()

ggsave(filename = "./Figures/best.diurnal.C.png", plot = last_plot(),
       width = 20,height = 12,dpi = 300,units = "cm")

ggplot(data = data.diurnal.Water.long.sum) +
  geom_ribbon(data = Water.diurnal.ensemble.sum,
              aes(x = hour,ymin = ET.min,ymax = ET.max),fill = "lightgrey",color = NA,alpha = 0.3) +
  geom_line(data = Water.diurnal.ensemble.sum,
            aes(x = hour,y = ET.m),color = "black") +
  geom_point(aes(x = hour,y = value.m,group = interaction(var))) +
  geom_errorbar(aes(x = hour,y = value.m,ymin = value.m + value.sd,ymax = value.m - value.sd,
                    group = interaction(var))) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(0,24,6)) +
  facet_wrap(~ var) +
  theme_bw()

ggsave(filename = "./Figures/best.diurnal.Water.png", plot = last_plot(),
       width = 20,height = 12,dpi = 300,units = "cm")

ggplot(data = data.seasonal.C.long.sum) +
  geom_ribbon(data = C.seasonal.ensemble.sum,
              aes(x = month,ymin = NEP.min,ymax = NEP.max),fill = "lightgrey",color = NA,alpha = 0.3) +
  geom_line(data = C.seasonal.ensemble.sum,
            aes(x = month,y = NEP.m),color = "black") +
  geom_point(aes(x = month,y = value.m,group = interaction(var))) +
  geom_errorbar(aes(x = month,y = value.m,ymin = value.m + value.sd,ymax = value.m - value.sd,
                    group = interaction(var))) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(0,24,6)) +
  facet_wrap(~ var) +
  theme_bw()

ggsave(filename = "./Figures/best.seasonal.C.png", plot = last_plot(),
       width = 20,height = 12,dpi = 300,units = "cm")

ggplot(data = data.seasonal.Water.long.sum) +
  geom_ribbon(data = Water.seasonal.ensemble.sum,
              aes(x = month,ymin = ET.min,ymax = ET.max),fill = "lightgrey",color = NA,alpha = 0.3) +
  geom_line(data = Water.seasonal.ensemble.sum,
            aes(x = month,y = ET.m),color = "black") +
  geom_point(aes(x = month,y = value.m,group = interaction(var))) +
  geom_errorbar(aes(x = month,y = value.m,ymin = value.m + value.sd,ymax = value.m - value.sd,
                    group = interaction(var))) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(0,24,6)) +
  facet_wrap(~ var) +
  theme_bw()

ggsave(filename = "./Figures/best.seasonal.Water.png", plot = last_plot(),
       width = 20,height = 12,dpi = 300,units = "cm")

