rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Fluxtower
data <- read.csv(file.path(getwd(),"data","CO2_Filou.csv")) %>% filter(qc_co2_flux < 2,qc_h2o_flux < 2) %>%
  dplyr::select(date,time,DOY,co2_flux,h2o_flux) %>% mutate(t = as.POSIXct(paste(date,time),tz="CET"),
                                                            month = month(t),
                                                            year = year(t),
                                                            DOY = floor(yday(t))) %>% rename(hour = time) %>%
  mutate(co2_flux = co2_flux/1000*1e-6*12*86400*365,
         ET = h2o_flux/1000*18/1000*86400) %>%
  filter(co2_flux <= 15)


data.diurnal <- data %>% mutate(NEP = -(co2_flux),
                                ET = (ET))
levels(data.diurnal$hour) <- seq(0,23.5,0.5)
data.diurnal$hour <- seq(0,23.5,0.5)[as.numeric(data.diurnal$hour)]

data.diurnal <- data.diurnal %>% mutate(hour = as.numeric(hour)) %>%
  complete(hour,nesting(date)) %>% arrange(date,hour) %>%
  mutate(t = as.POSIXct(paste(date),tz="CET")) %>%
  mutate(month = month(t),
         year = year(t),
         DOY = yday(t)) %>% dplyr::select(-c(t))

data.diurnal.sum <- data.diurnal %>% group_by(month,hour) %>% summarise(NEP.m = mean(NEP,na.rm = TRUE),
                                                                        NEP.sd = sd(NEP,na.rm = TRUE),
                                                                        ET.m = mean(ET,na.rm = TRUE),
                                                                        ET.sd = sd(ET,na.rm = TRUE))

ggplot(data = data.diurnal) +
  geom_line(aes(x = hour, y = NEP, group = DOY),color = "darkgrey",alpha = 0.8) +
  geom_line(data = data.diurnal.sum,
              aes(x = hour,y = NEP.m),color = "black") +
  facet_wrap(~ month) +
  scale_x_continuous(breaks = seq(0,24,6)) +
  labs(x = "",y = "NEP (kgC/m²/yr)") +
  theme_bw()

ggplot(data = data.diurnal) +
  geom_line(aes(x = hour, y = ET, group = DOY),color = "darkgrey",alpha = 0.8) +
  geom_line(data = data.diurnal.sum,
            aes(x = hour,y = ET.m),color = "black") +
  facet_wrap(~ month) +
  scale_x_continuous(breaks = seq(0,24,6)) +
  labs(x = "",y = "ET (kgW/m²/d)") +
  theme_bw()


# Radiation
data.rad <- read.csv(file.path(getwd(),"data","Radiation_filou.csv")) %>% mutate(t = strptime(Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  dplyr::select(SW_IN_1_1_1,LW_IN_1_1_1,PPFD_IN_1_1_1,t) %>% rename(SW = SW_IN_1_1_1,
                                                                       LW = LW_IN_1_1_1,
                                                                       PPFD = PPFD_IN_1_1_1) %>%
  mutate(month = month(t),
         year = year(t),
         DOY = yday(t),
         h = hour(t),
         min = minute(t)) %>% mutate(hour = (2*h + min/30)/2) %>%
  complete(hour,nesting(DOY)) %>% arrange(DOY,hour) %>% filter(!is.na(month))

data.rad.sum <- data.rad %>% group_by(month,hour) %>% summarise(SW.m = mean(SW,na.rm = TRUE),
                                                                SW.sd = sd(SW,na.rm = TRUE),
                                                                LW.m = mean(LW,na.rm = TRUE),
                                                                LW.sd = sd(LW,na.rm = TRUE),
                                                                PPFD.m = mean(PPFD,na.rm = TRUE),
                                                                PPFD.sd = sd(PPFD,na.rm = TRUE))

ggplot(data = data.rad) +
  geom_line(aes(x = hour, y = SW, group = DOY),color = "darkgrey",alpha = 0.8) +
  geom_line(data = data.rad.sum,
            aes(x = hour,y = SW.m),color = "black") +
  facet_wrap(~ month) +
  scale_x_continuous(breaks = seq(0,24,6)) +
  labs(x = "",y = "Shortwave radiation (W/m²)") +
  theme_bw()

########################################################################################################
# All together

data.all <- bind_rows(list(data.diurnal %>% dplyr::select(hour,DOY,month,year,ET,NEP) %>% pivot_longer(cols = c(ET,NEP),
                                                                                           names_to = "var",
                                                                                           values_to = "value"),
               data.rad %>% dplyr::select(hour,DOY,month,year,SW) %>% pivot_longer(cols = c(SW),
                                                                                   names_to = "var",
                                                                                   values_to = "value")))

data.all.sum <- bind_rows(list(data.diurnal.sum %>% dplyr::select(hour,month,ET.m,NEP.m) %>% rename(ET = ET.m,
                                                                                                         NEP = NEP.m) %>%
                                 pivot_longer(cols = c(ET,NEP),
                                              names_to = "var",
                                              values_to = "value"),
                           data.rad.sum %>% dplyr::select(hour,month,SW.m) %>% rename(SW = SW.m) %>%
                             pivot_longer(cols = c(SW),
                                          names_to = "var",
                                          values_to = "value")))

########################################################################################################
# Radiation and  GPP

data.merged <- data.diurnal %>% dplyr::select(year,month,DOY,hour,ET,NEP) %>%
  left_join(data.rad %>% dplyr::select(year,month,DOY,hour,SW,LW,PPFD), by = c("year","month","DOY","hour"))

data.merged.sum <- data.merged  %>% group_by(year,month,hour) %>% summarise(NEP.sd = sd(NEP,na.rm = TRUE),
                                                                       NEP = mean(NEP,na.rm = TRUE),
                                                                       ET.sd = sd(ET,na.rm = TRUE),
                                                                       ET = mean(ET,na.rm = TRUE),
                                                                       SW.sd = sd(SW,na.rm = TRUE),
                                                                       SW = mean(SW,na.rm = TRUE),
                                                                       LW.sd = sd(LW,na.rm = TRUE),
                                                                       LW = mean(LW,na.rm = TRUE),
                                                                       PPFD.sd = sd(PPFD,na.rm = TRUE),
                                                                       PPFD = mean(PPFD,na.rm = TRUE))

ggplot() +
  geom_point(data = data.merged,
             aes(x = SW,y = NEP, color = as.factor(month))) +
  theme_bw()

########################################################################################################
# Compare with ED2

file <- "/home/femeunier/Documents/projects/YGB/outputs/analysis.RData"
load(file)

delta_time = 4

ED2.diurnal.C <- data.frame(year = rep(datum$year,48),
                            month = rep(datum$month,48),
                            hour = sort(rep(seq(0,23.9,0.5),length(datum$year))),
                            GPP = as.vector(datum$qmean$gpp),
                            ET = as.vector(datum$qmean$transp) + as.vector(datum$qmean$evap),
                            NEP = as.vector(-datum$qmean$nee)/1000*1e-6*12*86400*365,
                            SW = as.vector(datum$qmean$rshort),
                            PPFD = as.vector(datum$qmean$par.tot)) %>% mutate(hour = hour + delta_time/2) %>%
  mutate(hour = case_when(hour > 23.5 ~ (hour - 24),
                          TRUE ~ hour))

ggplot(data = ED2.diurnal.C) +
  geom_point(aes(x = SW,y = NEP, color = as.factor(month))) +
  theme_bw()

data.model <- bind_rows(list(data.merged.sum %>% dplyr::select(SW,NEP,month,hour) %>% mutate(type = "data"),
                             ED2.diurnal.C %>% dplyr::select(SW,NEP,year,month,hour) %>% mutate(type = "ED2")))

ggplot(data = data.model %>% filter(month %in% seq(10,12))) +
  geom_point(aes(x = SW,y = NEP, color = as.factor(hour))) +
  facet_wrap(~ type) +
  theme_bw()


model.all <- ED2.diurnal.C %>% dplyr::select(year,month,hour,ET,NEP,SW) %>% pivot_longer(cols = c(ET,NEP,SW),
                                                                                         names_to = "var",
                                                                                         values_to = "value")


data.all$month <- factor(data.all$month,levels = c(10,11,12))
levels(data.all$month) <- c("October","November","December")

data.all.sum$month <- factor(data.all.sum$month,levels = c(10,11,12))
levels(data.all.sum$month) <- c("October","November","December")

model.all$month <- factor(model.all$month,levels = c(10,11,12))
levels(model.all$month) <- c("October","November","December")

ggplot(data = data.all) +
  geom_rect(aes(xmin = 6,xmax = 18,ymin = -Inf,ymax = Inf),fill = "lightgrey",alpha = 0.02) +
  geom_line(aes(x = hour, y = value, group = DOY),color = "darkgrey",alpha = 0.8) +
  geom_line(data = data.all.sum,
            aes(x = hour,y = value),color = "black") +
  geom_line(data = model.all %>% filter(year == min(year),month %in% c("October","November","December")),
            aes(x = hour,y = value),color = "red") +
  facet_grid(var ~ month,scales = "free") +
  scale_x_continuous(breaks = seq(0,24,6)) +
  labs(x = "",y = "") +
  geom_vline(xintercept = 12,linetype = 2, color = "black") +
  geom_hline(yintercept = 0,linetype = 1, color = "black") +
  theme_bw() +
  theme(text = element_text(size = 20))

ggsave(filename = "./Figures/Tower.measurements.model.png", plot = last_plot(),
       width = 30,height = 25,dpi = 300,units = "cm")


