rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(rhdf5)
library(lubridate)

save.figure <- FALSE

###########################################################################################################################
# Ecosystem Demography Model, version 2.2
###########################################################################################################################

system2("rsync",
        c("-avz","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/analy/YGB_tower_default.RData",
          "./outputs/"))

# system2("rsync",
#         c("-avz","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/analy/YGB_tower_default-Q-2016-01-00-000000-g01.h5",
#           "./outputs/"))

file <- "/home/femeunier/Documents/projects/YGB/outputs/YGB_tower_default.RData"
load(file)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 6
cols = gg_color_hue(n)

###########################################################################################################################
# Seasonal cycle
## Carbon
###########################################################################################################################
# Units kgC/m²/yr
ED2.seasonal.C <- data.frame(year = datum$year,
                             month = datum$month,
                             GPP = datum$emean$gpp,
                             Reco = datum$emean$reco,
                             Rauto = datum$emean$plant.resp,
                             Rhetero = datum$emean$het.resp,
                             NEP = -datum$emean$nee/1000*1e-6*12*86400*365,
                             NPP = datum$emean$npp) %>% filter(year > min(year))

ED2.seasonal.C.long <- ED2.seasonal.C %>% pivot_longer(cols = -c(year,month),
                                                       names_to = "var",
                                                       values_to = "value")

ED2.seasonal.C.long.sum <- ED2.seasonal.C.long %>% group_by(month,var) %>% summarise(value.m = mean(value),
                                                                                     value.sd = sd(value))

# ggplot(data = ED2.seasonal.C.long.sum) +
#   geom_line(aes(x = month,y = value.m, color = var)) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   theme_bw()

###########################################################################################################################
## Water
###########################################################################################################################

# Units kgW/m²/day
ED2.seasonal.Water <- data.frame(year = datum$year,
                                 month = datum$month,
                                 ET = datum$emean$et,
                                 evap = datum$emean$evap,
                                 transp = datum$emean$transp) %>% filter(year > min(year))

ED2.seasonal.Water.long <- ED2.seasonal.Water %>% pivot_longer(cols = -c(year,month),
                                                               names_to = "var",
                                                               values_to = "value")

ED2.seasonal.Water.long.sum <- ED2.seasonal.Water.long %>% group_by(month,var) %>% summarise(value.m = mean(value),
                                                                                             value.sd = sd(value))

# ggplot(data = ED2.seasonal.Water.long.sum) +
#   geom_line(aes(x = month,y = value.m, color = var)) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   theme_bw()

###########################################################################################################################
# Diurnal
## Carbon
###########################################################################################################################

delta_time = 4

ED2.diurnal.C <- data.frame(year = rep(datum$year,48),
                          month = rep(datum$month,48),
                          hour = sort(rep(seq(0,23.9,0.5),length(datum$year))),
                          GPP = as.vector(datum$qmean$gpp),
                          Reco = as.vector(datum$qmean$reco),
                          Rauto = as.vector(datum$qmean$reco - datum$qmean$het.resp),
                          Rhetero = as.vector(datum$qmean$het.resp),
                          NEP = as.vector(-datum$qmean$nee)/1000*1e-6*12*86400*365,
                          NPP = as.vector(datum$qmean$npp)) %>% mutate(hour = hour + delta_time/2) %>%
  mutate(hour = case_when(hour > 23.5 ~ (hour - 24),
                          TRUE ~ hour))

ED2.diurnal.C.long <- ED2.diurnal.C %>% pivot_longer(cols = -c(year,month,hour),
                                                     names_to = "var",
                                                     values_to = "value")

ED2.diurnal.C.long.sum <- ED2.diurnal.C.long %>% group_by(month,var,hour) %>% summarise(value.m = mean(value),
                                                                                          value.sd = sd(value))

# ggplot(data = ED2.diurnal.C.long.sum) +
#   geom_line(aes(x = hour,y = value.m, color = as.factor(month),group = interaction(month,var))) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   geom_vline(xintercept = 12,linetype = 1, color = "black") +
#   scale_x_continuous(breaks = seq(0,24,6)) +
#   facet_wrap(~ var,scales = "free") +
#   theme_bw()

###########################################################################################################################
## Water
###########################################################################################################################

ED2.diurnal.Water <- data.frame(year = rep(datum$year,48),
                                month = rep(datum$month,48),
                                hour = sort(rep(seq(0,23.9,0.5),length(datum$year))),
                                ET = as.vector(datum$qmean$transp) + as.vector(datum$qmean$evap),
                                evap = as.vector(datum$qmean$evap),
                                transp = as.vector(datum$qmean$transp)) %>% mutate(hour = hour + delta_time/2) %>%
  mutate(hour = case_when(hour > 23.5 ~ (hour - 24),
                          TRUE ~ hour))

ED2.diurnal.Water.long <- ED2.diurnal.Water %>% pivot_longer(cols = -c(year,month,hour),
                                                             names_to = "var",
                                                             values_to = "value")

ED2.diurnal.Water.long.sum <- ED2.diurnal.Water.long %>% group_by(month,var,hour) %>% summarise(value.m = mean(value),
                                                                                                value.sd = sd(value))

# ggplot(data = ED2.diurnal.Water.long.sum) +
#   geom_line(aes(x = hour,y = value.m, color = as.factor(month),group = interaction(month,var))) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   geom_vline(xintercept = 12,linetype = 1, color = "black") +
#   scale_x_continuous(breaks = seq(0,24,6)) +
#   facet_wrap(~ var) +
#   theme_bw()

###########################################################################################################################
# Data
###########################################################################################################################

data <- read.csv(file.path("/home/femeunier/Documents/projects/YGB","data","CO2_Filou.csv")) %>% filter(qc_co2_flux < 2,qc_h2o_flux<2) %>%
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


###########################################################################################################################
## Diurnal
###########################################################################################################################

data.diurnal <- data %>% mutate(NEP = -(co2_flux),
                                ET = (ET))
levels(data.diurnal$hour) <- seq(0,23.5,0.5)
data.diurnal$hour <- seq(0,23.5,0.5)[as.numeric(data.diurnal$hour)]

data.diurnal.C <- data.diurnal %>% dplyr::select(-c(ET))
data.diurnal.C.long <- data.diurnal.C %>% mutate(var = "NEP") %>% rename(value = NEP)
data.diurnal.C.long.sum <- data.diurnal.C.long %>% group_by(month,hour,var) %>% summarise(value.m = mean(value),
                                                                                          value.sd = sd(value))

data.diurnal.Water <- data.diurnal %>% dplyr::select(-c(NEP))
data.diurnal.Water.long <- data.diurnal.Water %>% mutate(var = "ET") %>% rename(value = ET)
data.diurnal.Water.long.sum <- data.diurnal.Water.long %>% group_by(month,hour,var) %>% summarise(value.m = mean(value),
                                                                                          value.sd = sd(value))

# ggplot(data = data.diurnal.Water.long.sum) +
#   geom_line(aes(x = hour,y = value.m, color = as.factor(month),group = interaction(month,var))) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   geom_vline(xintercept = 12,linetype = 1, color = "black") +
#   scale_x_continuous(breaks = seq(0,24,6)) +
#   facet_wrap(~ var) +
#   theme_bw()

##################################################################################################################
# ORCHIDEE

h5file <- file.path('/home/femeunier/Documents/projects/YGB/data/ORCHIDEE/','YGB_hourly_2015.nc')
h5ls(h5file)
h5readAttributes(h5file,"/growth_resp")

tinit <- (0*24)+1
tend <- 365*24
gpp <- h5read(h5file,"/gpp")[1,1,,tinit:tend]/1000*86400*365
nep <- -h5read(h5file,"/nee")[1,1,,tinit:tend]/1000*86400*365
ET <- h5read(h5file,"/fluxlat")[1,1,tinit:tend]/1000/2256*86400
transpir <- h5read(h5file,"/transpir")[1,1,,tinit:tend]
hetero_resp <- h5read(h5file,"/hetero_resp")[1,1,,tinit:tend]/1000*86400*365
auto_resp <- (h5read(h5file,"/maint_resp")[1,1,,tinit:tend] + h5read(h5file,"/growth_resp")[1,1,,tinit:tend])/1000*86400*365

# Day of the year
hour <- h5read(h5file,"/time_centered")[tinit:tend]/3600/24

maxvegetfrac <-  h5read(h5file,"/maxvegetfrac")[1,1,,tinit:tend]

nep.w <- sapply(1:dim(nep)[2],function(i){
  return(weighted.mean(nep[,i],maxvegetfrac[,i]))})
gpp.w <- sapply(1:dim(nep)[2],function(i){
  return(weighted.mean(gpp[,i],maxvegetfrac[,i]))})
transpir.w <- sapply(1:dim(nep)[2],function(i){
  return(weighted.mean(transpir[,i],maxvegetfrac[,i]))})
hetero_resp.w <- sapply(1:dim(nep)[2],function(i){
  return(weighted.mean(hetero_resp[,i],maxvegetfrac[,i]))})
auto_resp.w <- sapply(1:dim(nep)[2],function(i){
  return(weighted.mean(auto_resp[,i],maxvegetfrac[,i]))})


ORCHIDEE.df <- data.frame(time = hour,
                          GPP = gpp.w,
                          NEP = nep.w,
                          transp = transpir.w,
                          ET = ET,
                          evap = ET - transpir.w,
                          Rhetero = hetero_resp.w,
                          Rauto = auto_resp.w,
                          Reco = hetero_resp.w + auto_resp.w,
                          NPP = gpp.w - auto_resp.w) %>% mutate(date = as.POSIXlt(x = time*86400 - 3600 + delta_time/2*3600,tz = "CET",origin = "2015-01-01")) %>%
  mutate(year = year(date),
         month = month(date)) %>% filter(year == 2015)

###########################################################################################################################
# Seasonal
## Carbon
###########################################################################################################################

ORCHIDEE.seasonal.C <- ORCHIDEE.df %>% dplyr::select(-c(ET,transp,evap,date)) %>% group_by(year,month) %>%
  summarise(GPP = mean(GPP),
            NPP = mean(NPP),
            Reco = mean(Reco),
            NEP = mean(NEP),
            Rhetero = mean(Rhetero),
            Rauto = mean(Rauto))

ORCHIDEE.seasonal.C.long <- ORCHIDEE.seasonal.C %>% pivot_longer(cols = -c(year,month),
                                                                 names_to = "var",
                                                                 values_to = "value")

ORCHIDEE.seasonal.C.long.sum <- ORCHIDEE.seasonal.C.long %>% group_by(month,var) %>% summarise(value.m = mean(value),
                                                                                               value.sd = sd(value))

# ggplot(data = ORCHIDEE.seasonal.C.long.sum) +
#   geom_line(aes(x = month,y = value.m, color = var)) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   theme_bw()

###########################################################################################################################
## Water
###########################################################################################################################

ORCHIDEE.seasonal.Water <- ORCHIDEE.df %>% dplyr::select(c(ET,transp,year,month)) %>% group_by(year,month) %>%
  summarise(ET = mean(ET),
            evap = mean(ET) - mean(transp),
            transp = mean(transp))

ORCHIDEE.seasonal.Water.long <- ORCHIDEE.seasonal.Water %>% pivot_longer(cols = -c(year,month),
                                                                         names_to = "var",
                                                                         values_to = "value")

ORCHIDEE.seasonal.Water.long.sum <- ORCHIDEE.seasonal.Water.long %>% group_by(month,var) %>% summarise(value.m = mean(value),
                                                                                                       value.sd = sd(value))

# ggplot(data = ORCHIDEE.seasonal.Water.long.sum) +
#   geom_line(aes(x = month,y = value.m, color = var)) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   theme_bw()

###########################################################################################################################
# Diurnal
## Carbon
###########################################################################################################################

ORCHIDEE.diurnal.C <- ORCHIDEE.df %>% dplyr::select(-c(ET,evap,transp)) %>% mutate(h = hour(date),
                                                                              min = minute(date)) %>% mutate(hour = 0.5*(2*h + min/30)) %>%
  dplyr::select(-c(time,date,h,min))


ORCHIDEE.diurnal.C.long <- ORCHIDEE.diurnal.C %>% pivot_longer(cols = -c(year,month,hour),
                                                                 names_to = "var",
                                                                 values_to = "value")

ORCHIDEE.diurnal.C.long.sum <- ORCHIDEE.diurnal.C.long %>% group_by(month,var,hour) %>% summarise(value.m = mean(value),
                                                                                               value.sd = sd(value))


# ggplot(data = ORCHIDEE.diurnal.C.long.sum) +
#   geom_line(aes(x = hour,y = value.m, color = as.factor(month),group = interaction(month,var))) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   geom_vline(xintercept = 12,linetype = 1, color = "black") +
#   scale_x_continuous(breaks = seq(0,24,6)) +
#   facet_wrap(~ var) +
#   theme_bw()

###########################################################################################################################
## Water
###########################################################################################################################

ORCHIDEE.diurnal.Water <- ORCHIDEE.df %>% dplyr::select(c(date,ET,transp,evap,year,month)) %>% mutate(h = hour(date),
                                                                                                 min = minute(date)) %>% mutate(hour = 0.5*(2*h + min/30)) %>%
  dplyr::select(-c(date,h,min))


ORCHIDEE.diurnal.Water.long <- ORCHIDEE.diurnal.Water %>% pivot_longer(cols = -c(year,month,hour),
                                                                       names_to = "var",
                                                                       values_to = "value")

ORCHIDEE.diurnal.Water.long.sum <- ORCHIDEE.diurnal.Water.long %>% group_by(month,var,hour) %>% summarise(value.m = mean(value),
                                                                                                          value.sd = sd(value))


# ggplot(data = ORCHIDEE.diurnal.Water.long.sum) +
#   geom_line(aes(x = hour,y = value.m, color = as.factor(month),group = interaction(month,var))) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   geom_vline(xintercept = 12,linetype = 1, color = "black") +
#   scale_x_continuous(breaks = seq(0,24,6)) +
#   facet_wrap(~ var) +
#   theme_bw()

###########################################################################################################################
# All together
###########################################################################################################################

all.diurnal.C <- bind_rows(list(ED2.diurnal.C.long.sum %>% mutate(source = "ED2"),
                                 data.diurnal.C.long.sum %>% mutate(source = "data"),
                                 ORCHIDEE.diurnal.C.long.sum %>% mutate(source = "ORCHIDEE")))

# ggplot(data = all.diurnal.C) +
#   geom_line(aes(x = hour,y = value.m, color = as.factor(month))) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   facet_grid(source ~ var) +
#   theme_bw()

all.seasonal.C <- bind_rows(list(ED2.seasonal.C.long.sum %>% mutate(source = "ED2"),
                                 data.seasonal.C.long.sum %>% mutate(source = "data"),
                                 ORCHIDEE.seasonal.C.long.sum %>% mutate(source = "ORCHIDEE")))

# ggplot(data = all.seasonal.C) +
#   geom_line(aes(x = month,y = value.m, color = as.factor(var))) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   facet_grid(~ source) +
#   theme_bw()


all.seasonal.Water <- bind_rows(list(ED2.seasonal.Water.long.sum %>% mutate(source = "ED2"),
                                     data.seasonal.Water.long.sum %>% mutate(source = "data"),
                                     ORCHIDEE.seasonal.Water.long.sum %>% mutate(source = "ORCHIDEE")))

# ggplot(data = all.seasonal.Water) +
#   geom_line(aes(x = month,y = value.m, color = as.factor(var))) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   facet_grid(~ source) +
#   theme_bw()

all.diurnal.Water <- bind_rows(list(ED2.diurnal.Water.long.sum %>% mutate(source = "ED2"),
                                    data.diurnal.Water.long.sum %>% mutate(source = "data"),
                                    ORCHIDEE.diurnal.Water.long.sum %>% mutate(source = "ORCHIDEE")))

# ggplot(data = all.diurnal.Water) +
#   geom_line(aes(x = hour,y = value.m, color = as.factor(month))) +
#   geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   facet_grid(source ~ var) +
#   theme_bw()

###########################################################################################################################
# Nice Figures
###########################################################################################################################
# Seasonal cycle
# NEP only

data2plot <- bind_rows(list(all.seasonal.C %>% filter(var == "NEP",source == "data") %>% mutate(type = "ED2"),
                            all.seasonal.C %>% filter(var == "NEP",source == "data") %>% mutate(type = "ORCHIDEE"))) %>%
  mutate(value.sd = case_when(is.na(value.sd) ~ 0.,
                              TRUE ~ value.sd))

model2plot <- all.seasonal.C %>% filter(var == "NEP",source != "data") %>% mutate(type = source)

ggplot() +
  geom_line(data = model2plot,
            aes(x = month,y = value.m), color = "black") +
  geom_ribbon(data = model2plot,
              aes(x = month,ymin = value.m - value.sd,ymax = value.m + value.sd), color = "lightgrey", alpha = 0.2) +
  geom_point(data = data2plot,
                aes(x = month,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd)) +
  geom_errorbar(data = data2plot,
                aes(x = month,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd)) +
  facet_wrap(~ type) +
  scale_x_continuous(breaks = seq(1,12),labels = c("J","F","M","A","M","J",
                                                   "J","A","S","O","N","D")) +
  labs(x = "",y = "NEP (kgC/m²/yr)") +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  theme_bw()

if (save.figure){
  ggsave(plot = last_plot(),
         filename = file.path("/home/femeunier/Documents/projects/YGB","Figures","seasonal.NEP.png"),
         width = 20,height = 10, units = "cm",dpi = 300)
}


# Carbon cycle

data2plot <- bind_rows(list(all.seasonal.C %>% filter(source == "data") %>% mutate(type = "ED2"),
                            all.seasonal.C %>% filter(source == "data") %>% mutate(type = "ORCHIDEE"))) %>%
  mutate(value.sd = case_when(is.na(value.sd) ~ 0.,
                              TRUE ~ value.sd))

model2plot <- all.seasonal.C %>% filter(source != "data") %>% mutate(type = source)

ggplot() +
  geom_line(data = model2plot,
            aes(x = month,y = value.m,color = var)) +
  geom_ribbon(data = model2plot,
              aes(x = month,ymin = value.m - value.sd,ymax = value.m + value.sd,color = var, fill = var),color = NA,alpha = 0.2) +
  geom_point(data = data2plot,
             aes(x = month,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd,color = var)) +
  geom_errorbar(data = data2plot,
                aes(x = month,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd,color = var)) +
  facet_wrap(~ type) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(1,12),labels = c("J","F","M","A","M","J",
                                                   "J","A","S","O","N","D")) +
  labs(x = "",y = "Carbon flux (kgC/m²/yr)") +
  theme_bw()

if (save.figure){
  ggsave(plot = last_plot(),
         filename = file.path("/home/femeunier/Documents/projects/YGB","Figures","seasonal.CC.png"),
         width = 20,height = 10, units = "cm",dpi = 300)
}

# LE only

data2plot <- bind_rows(list(all.seasonal.Water %>% filter(var == "ET",source == "data") %>% mutate(type = "ED2"),
                            all.seasonal.Water %>% filter(var == "ET",source == "data") %>% mutate(type = "ORCHIDEE"))) %>%
  mutate(value.sd = case_when(is.na(value.sd) ~ 0.,
                              TRUE ~ value.sd))

model2plot <- all.seasonal.Water %>% filter(var == "ET",source != "data") %>% mutate(type = source)

ggplot() +
  geom_ribbon(data = model2plot,
              aes(x = month,ymin = value.m - value.sd,ymax = value.m + value.sd), color = "lightgrey", alpha = 0.2) +
  geom_line(data = model2plot,
            aes(x = month,y = value.m), color = "black") +
  geom_point(data = data2plot,
             aes(x = month,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd)) +
  geom_errorbar(data = data2plot,
                aes(x = month,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd)) +
  facet_wrap(~ type) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(1,12),labels = c("J","F","M","A","M","J",
                                                   "J","A","S","O","N","D")) +
  labs(x = "",y = "Evapotranspiration (kgW/m²/d)") +
  theme_bw()

if (save.figure){
  ggsave(plot = last_plot(),
         filename = file.path("/home/femeunier/Documents/projects/YGB","Figures","seasonal.ET.png"),
         width = 20,height = 10, units = "cm",dpi = 300)
}

# Water cycle
data2plot <- bind_rows(list(all.seasonal.Water %>% filter(source == "data") %>% mutate(type = "ED2"),
                            all.seasonal.Water %>% filter(source == "data") %>% mutate(type = "ORCHIDEE"))) %>%
  mutate(value.sd = case_when(is.na(value.sd) ~ 0.,
                              TRUE ~ value.sd))

model2plot <- all.seasonal.Water %>% filter(source != "data") %>% mutate(type = source)

ggplot() +
  geom_line(data = model2plot,
            aes(x = month,y = value.m,color = var)) +
  geom_ribbon(data = model2plot,
              aes(x = month,ymin = value.m - value.sd,ymax = value.m + value.sd,color = var, fill = var),color = NA,alpha = 0.2) +
  geom_point(data = data2plot,
             aes(x = month,y = value.m,color = var)) +
  geom_errorbar(data = data2plot,
                aes(x = month,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd,color = var)) +
  facet_wrap(~ type) +
  scale_fill_manual(values = cols[2:4]) +
  scale_color_manual(values = cols[2:4]) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(1,12),labels = c("J","F","M","A","M","J",
                                                   "J","A","S","O","N","D")) +
  labs(x = "",y = "Water flux (kgW/m²/d)") +
  theme_bw()

if (save.figure){
  ggsave(plot = last_plot(),
         filename = file.path("/home/femeunier/Documents/projects/YGB","Figures","seasonal.WaterCycle.png"),
         width = 20,height = 10, units = "cm",dpi = 300)
}

###########################################################################################################################
# Diurnal cycle
# NEP only

data2plot <- bind_rows(list(all.diurnal.C %>% filter(var == "NEP",source == "data") %>% mutate(type = "ED2"),
                            all.diurnal.C %>% filter(var == "NEP",source == "data") %>% mutate(type = "ORCHIDEE"))) %>% group_by(hour,source,type) %>%
  summarise(value.sd = sd(value.m),
            value.m = mean(value.m))

model2plot <- all.diurnal.C %>% filter(var == "NEP",source != "data") %>% mutate(type = source) %>% group_by(hour,source,type) %>%
  summarise(value.sd = sd(value.m),
            value.m = mean(value.m))

ggplot(data = model2plot) +
  geom_rect(aes(xmin = 6,xmax = 18,ymin = -Inf,ymax = Inf),fill = "lightgrey",alpha = 0.02) +
  geom_line(data = model2plot,
            aes(x = hour,y = value.m), color = "black") +
  geom_ribbon(data = model2plot,
              aes(x = hour,ymin = value.m - value.sd,ymax = value.m + value.sd), color = "lightgrey", alpha = 0.4) +
  geom_point(data = data2plot,
             aes(x = hour,y = value.m)) +
  geom_errorbar(data = data2plot,
                aes(x = hour,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd)) +
  facet_wrap(~ type) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(0,24,6)) +
  labs(x = "",y = "NEP (kgC/m²/yr)") +
  theme_bw()

if (save.figure){
  ggsave(plot = last_plot(),
         filename = file.path("/home/femeunier/Documents/projects/YGB","Figures","diurnal.NEP.png"),
         width = 20,height = 10, units = "cm",dpi = 300)
}

# Carbon cycle

data2plot <- bind_rows(list(all.diurnal.C %>% filter(source == "data") %>% mutate(type = "ED2"),
                            all.diurnal.C %>% filter(source == "data") %>% mutate(type = "ORCHIDEE"))) %>% group_by(hour,source,type,var) %>%
  summarise(value.sd = sd(value.m),
            value.m = mean(value.m))

model2plot <- all.diurnal.C %>% filter(source != "data") %>% mutate(type = source) %>% group_by(hour,source,type,var) %>%
  summarise(value.sd = sd(value.m),
            value.m = mean(value.m))

ggplot(data = model2plot) +
  geom_rect(aes(xmin = 6,xmax = 18,ymin = -Inf,ymax = Inf),fill = "lightgrey",alpha = 0.02) +
  geom_line(data = model2plot,
            aes(x = hour,y = value.m,color = var)) +
  geom_ribbon(data = model2plot,
              aes(x = hour,ymin = value.m - value.sd,ymax = value.m + value.sd,color = var, fill = var),color = NA,alpha = 0.4) +
  geom_point(data = data2plot,
             aes(x = hour,y = value.m,color = var)) +
  geom_errorbar(data = data2plot,
                aes(x = hour,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd,color = var)) +
  facet_wrap(~ type) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(0,24,6)) +
  labs(x = "",y = "Carbon flux (kgC/m²/yr)") +
  theme_bw()

if (save.figure){
  ggsave(plot = last_plot(),
         filename = file.path("/home/femeunier/Documents/projects/YGB","Figures","diurnal.CC.png"),
         width = 20,height = 10, units = "cm",dpi = 300)
}

ggplot() +
  geom_line(data = model2plot,
            aes(x = hour,y = value.m), color = "black") +
  geom_ribbon(data = model2plot,
              aes(x = hour,ymin = value.m - value.sd,ymax = value.m + value.sd),color = NA,fill = "lightgrey",alpha = 0.4) +
  geom_point(data = data2plot,
             aes(x = hour,y = value.m), color = "black") +
  geom_errorbar(data = data2plot,
                aes(x = hour,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd),color = "black") +
  facet_grid(var ~ type) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(0,24,6)) +
  labs(x = "",y = "Carbon flux (kgC/m²/yr)") +
  theme_bw()

# LE only

data2plot <- bind_rows(list(all.diurnal.Water %>% filter(var == "ET",source == "data") %>% mutate(type = "ED2"),
                            all.diurnal.Water %>% filter(var == "ET",source == "data") %>% mutate(type = "ORCHIDEE"))) %>% group_by(hour,source,type) %>%
  summarise(value.sd = sd(value.m),
            value.m = mean(value.m))

model2plot <- all.diurnal.Water %>% filter(var == "ET",source != "data") %>% mutate(type = source) %>% group_by(hour,source,type) %>%
  summarise(value.sd = sd(value.m),
            value.m = mean(value.m))

ggplot() +
  geom_line(data = model2plot,
            aes(x = hour,y = value.m), color = "black") +
  geom_ribbon(data = model2plot,
              aes(x = hour,ymin = value.m - value.sd,ymax = value.m + value.sd), color = "lightgrey", alpha = 0.3) +
  geom_point(data = data2plot,
             aes(x = hour,y = value.m)) +
  geom_errorbar(data = data2plot,
                aes(x = hour,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd)) +
  facet_wrap(~ type) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(0,24,6)) +
  labs(x = "",y = "Evapotranspiration (kgW/m²/d)") +
  theme_bw()

if (save.figure){
  ggsave(plot = last_plot(),
         filename = file.path("/home/femeunier/Documents/projects/YGB","Figures","diurnal.ET.png"),
         width = 20,height = 10, units = "cm",dpi = 300)
}

# Water cycle

data2plot <- bind_rows(list(all.diurnal.Water %>% filter(source == "data") %>% mutate(type = "ED2"),
                            all.diurnal.Water %>% filter(source == "data") %>% mutate(type = "ORCHIDEE"))) %>% group_by(hour,source,type,var) %>%
  summarise(value.sd = sd(value.m),
            value.m = mean(value.m))

model2plot <- all.diurnal.Water %>% filter(source != "data") %>% mutate(type = source) %>% group_by(hour,source,type,var) %>%
  summarise(value.sd = sd(value.m),
            value.m = mean(value.m))

ggplot(data = model2plot) +
  geom_rect(aes(xmin = 6,xmax = 18,ymin = -Inf,ymax = Inf),fill = "lightgrey",alpha = 0.02) +
  geom_line(data = model2plot,
            aes(x = hour,y = value.m,color = var)) +
  geom_ribbon(data = model2plot,
              aes(x = hour,ymin = value.m - value.sd,ymax = value.m + value.sd,color = var, fill = var),color = NA,alpha = 0.4) +
  geom_point(data = data2plot,
             aes(x = hour,y = value.m,color = var)) +
  geom_errorbar(data = data2plot,
                aes(x = hour,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd,color = var)) +
  facet_wrap(~ type) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_fill_manual(values = cols[2:4]) +
  scale_color_manual(values = cols[2:4]) +
  scale_x_continuous(breaks = seq(0,24,6)) +
  labs(x = "",y = "Water flux (kgW/m²/d)") +
  theme_bw()

if (save.figure){
  ggsave(plot = last_plot(),
         filename = file.path("/home/femeunier/Documents/projects/YGB","Figures","diurnal.WaterCycle.png"),
         width = 20,height = 10, units = "cm",dpi = 300)
}

ggplot() +
  geom_line(data = model2plot,
            aes(x = hour,y = value.m), color = "black") +
  geom_ribbon(data = model2plot,
              aes(x = hour,ymin = value.m - value.sd,ymax = value.m + value.sd),color = NA,fill = "lightgrey",alpha = 0.4) +
  geom_point(data = data2plot,
             aes(x = hour,y = value.m), color = "black") +
  geom_errorbar(data = data2plot,
                aes(x = hour,y = value.m,ymin = value.m - value.sd,ymax = value.m + value.sd),color = "black") +
  facet_grid(var ~ type) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(breaks = seq(0,24,6)) +
  labs(x = "",y = "Evapotranspiration (kgW/m²/d)") +
  theme_bw()

