rm(list = ls())

library(rhdf5)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(stringr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)

source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")

system2("rsync",c("-avz",
                  "hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/analy/YGB_tower_default-T-2017-00-00-000000-g01.h5",
                  "/home/femeunier/Documents/projects/YGB/outputs/"))

##################################################################################################################
# ED2

# h5file <- "/home/femeunier/Documents/projects/YGB/outputs/YGB_tower-T-2017-00-00-000000-g01.h5"
h5file <- "/home/femeunier/Documents/projects/YGB/outputs/YGB_tower_default-T-2017-00-00-000000-g01.h5"
# h5file <- "/home/femeunier/Downloads/YGB_tower_default-T-2015-00-00-000000-g01.h5"
mymont    = lapply(h5read_opt(h5file),FUN=aperm)

delta_h = 8.5 # 5.5 (t-file + 2h utc --> + 2)
df <- data.frame(t = with_tz(seq(as.POSIXct("2017-01-01 00:00:00", ts = "UTC"),
                         as.POSIXct("2017-12-31 23:30:00", ts = "UTC"),
                         1800) + delta_h*3600,
                         tzone = "UTC"),
                 GPP = mymont$FMEAN_GPP_PY,
                 Rauto = mymont$FMEAN_PLRESP_PY,
                 NPP = mymont$FMEAN_NPP_PY,
                 Rhetero = mymont$FMEAN_RH_PY,
                 NEP = mymont$FMEAN_NEP_PY,
                 PAR = mymont$FMEAN_ATM_PAR_PY) %>%
  mutate(Reco = GPP - NEP) %>%
  mutate(yr = year(t),
         m = month(t),
         d = day(t),
         h = hour(t) + minute(t)/60) %>%
  filter(yr == 2017)

data.frame(seq(1,11),
           seq(0,5.,0.5))

df.long <- df %>%
  pivot_longer(cols = -c(t,yr,m,d,h),
               names_to = "variable",
               values_to = "value")

df.seasonal <- df.long %>% group_by(variable,m) %>%
  summarise(value.m = mean(value),
            value.sd = sd(value),
            .groups = "keep")


##############################################################################################################
# Data

data <- read.csv(file.path(getwd(),"data","CO2_Filou.csv")) %>% filter(qc_co2_flux < 2,qc_h2o_flux < 2) %>%
  dplyr::select(date,time,DOY,co2_flux,h2o_flux) %>% mutate(t = as.POSIXct(paste(date,time),tz="CET"),
                                                            month = month(t),
                                                            year = year(t),
                                                            DOY = floor(yday(t))) %>% rename(h = time) %>%
  mutate(co2_flux = co2_flux/1000*1e-6*12*86400*365,
         ET = h2o_flux/1000*18/1000*86400)

data.diurnal <- data %>% mutate(NEP = -(co2_flux),
                                ET = (ET))
levels(data.diurnal$h) <- seq(0,23.5,0.5)
data.diurnal$h <- seq(0,23.5,0.5)[as.numeric(data.diurnal$h)]

data.diurnal <- data.diurnal %>% mutate(h = as.numeric(h)) %>%
  complete(h,nesting(date)) %>% arrange(date,h) %>%
  mutate(t = as.POSIXct(paste(date),tz="CET")) %>%
  mutate(m = month(t),
         year = year(t),
         DOY = yday(t)) %>% dplyr::select(-c(t)) %>%
  dplyr::select(m,h,ET,NEP) %>%
  pivot_longer(cols = c(ET,NEP),
               names_to = "variable",
               values_to = "value")

data.diurnal.sum <- data.diurnal %>% group_by(variable,m,h) %>% summarise(value.m = mean(value,na.rm = TRUE),
                                                                          value.sd = sd(value,na.rm = TRUE),
                                                                          .groups = "keep")

data.seasonal.sum <- data.diurnal %>% group_by(variable,m) %>% summarise(value.m = mean(value,na.rm = TRUE),
                                                                         value.sd = sd(value,na.rm = TRUE),
                                                                         .groups = "keep")


ggplot(df.seasonal %>% dplyr::filter(variable %in% c("GPP","NEP",
                                                     "Reco","Rauto","Rhetero"))) +
  geom_line(aes(x = m, y = value.m, color = variable)) +
  geom_point(data = data.seasonal.sum %>% filter(variable %in% c("NEP")),
             aes(x = m, y = value.m, color = variable)) +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "", y = "Carbon flux (kg C/m²/yr)") +
  theme_bw() +
  theme(text = element_text(size = 22))

df.diurnal <- df.long %>% ungroup() %>% group_by(variable,m,h) %>%
  summarise(value.m = mean(value),
            value.sd = sd(value),
            .groups = "keep")

ggplot(df.diurnal %>%
         dplyr::filter(variable %in% c("GPP","NEP",
                                       "Reco","Rauto","Rhetero")) %>%
         dplyr::filter(m %in% c(seq(10,12)))) +
  geom_line(aes(x = h, y = value.m, color = variable)) +

  geom_point(data = data.diurnal.sum %>% dplyr::filter(variable %in% c("NEP")),
             aes(x = h, y = value.m, color = variable)) +

  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  # geom_vline(xintercept = 12, color = "black", linetype = 2) +

  scale_x_continuous(breaks = seq(0,24,6)) +

  facet_wrap(~ m, nrow = 1) +
  theme_bw() +
  labs(x = "", y = "Carbon flux (kg C/m²/yr)", color = "") +
  theme(legend.position = c(0.9,0.2),
        text = element_text(size = 22))

