rm(list = ls())

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(hdf5r)
library(reshape2)
library(YGB)
library(zoo)
library(tidyr)
library(imputeTS)

#########################################################################################################
# YGB data

data.file <- file.path("/home/femeunier/Documents/projects/YGB/data/MET/METdata","TA00410.csv")

deltaT = 5*60
data <- read.csv(data.file) %>% mutate(time = strptime(timestamp..UTC., format = '%Y-%m-%d %H:%M')) %>%
  rename(pres = atmosphericpressure..kPa.,
         prate = precipitation..mm.,
         SW_IN = radiation..W.m2.,
         RH = relativehumidity....,
         temp = temperature..degrees.Celsius.,
         WD = winddirection..degrees.,
         WS = windspeed..m.s.) %>% dplyr::select(-c(timestamp..UTC.,windgusts..m.s.)) %>%
  mutate(year = year(time),
         month = month(time),
         hour = hour(time),
         min = minute(time),
         temp = temp + 273.15,
         pres = pres*1000) %>% mutate(sh = rh2qair(rh = RH,T = temp,press = pres),
                                      ugrd = WS*cos((270-WD)*pi/180),
                                      vgrd = WS*sin((270-WD)*pi/180),
                                      prate = prate/deltaT,
                                      nbdsf = SW_IN * (1 - 0.474) *0.55,
                                      nddsf = SW_IN * (1 - 0.474) *0.45,
                                      vbdsf = SW_IN * (0.474) * 0.46,
                                      vddsf = SW_IN * (0.474) * 0.54,
                                      dlwrf = 395 + 15*sin(((hour + min/60) - 11)*2*pi/24)) %>% dplyr::select(c(time,year,month,hour,min,
                                                                                                               temp,pres,sh,ugrd,vgrd,prate,
                                                                                                               nbdsf,nddsf,vbdsf,vddsf,dlwrf))

data.long <- data %>% pivot_longer(cols = -c(time,year,month,hour,min),
                                   names_to = "var",
                                   values_to = "value")

data.long.seasonal <- bind_rows(list(data.long %>% filter(var == "prate") %>% group_by(year,month,var) %>% summarise(value = sum(value,na.rm = TRUE)) %>% group_by(month,var) %>% summarise(value = mean(value)),
                                     data.long %>% filter(var != "prate") %>% group_by(var,month) %>% summarise(value = mean(value,na.rm = TRUE))))

ggplot(data = data.long.seasonal) +
  geom_line(aes(x = month,y = value)) +
  scale_x_continuous(breaks = c(1,3,6,9,12),
                     labels = c("J","M","J","S","D")) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()

data.long.diel <- bind_rows(list(data.long %>% filter(var == "prate") %>% group_by(year,month,hour,var) %>% summarise(value = sum(value,na.rm = TRUE)) %>% group_by(month,hour,var) %>% summarise(value = mean(value)),
                                 data.long %>% filter(var != "prate") %>% group_by(var,month,hour) %>% summarise(value = mean(value,na.rm = TRUE))))

ggplot(data = data.long.diel) +
  geom_line(aes(x = hour,y = value,color = as.factor(month))) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()

saveRDS(object = data,file = file.path("./outputs/","metYGB.RDS"))

# fill gap

df <- readRDS('./outputs/metYGB.RDS') %>%
  mutate(timestamp = as.POSIXct(time, tz = "GMT"))

df.prate <- df %>% group_by(year) %>% summarise(MAP = sum(prate*300,na.rm = TRUE),
                                                N = length(prate[!is.na(prate)]),
                                                Ndays = N/12/24)
plot(df.prate$Ndays,df.prate$MAP)

dfc <- df %>% dplyr::select(timestamp,temp,pres,sh,ugrd,vgrd,prate,nbdsf,nddsf,vbdsf,vddsf,dlwrf) %>%
  complete(timestamp = seq.POSIXt(min(df$timestamp),
                                  max(df$timestamp), by = "5 min"),
           fill = list(temp = NA,pres = NA,sh = NA,ugrd = NA,vgrd = NA,prate = NA,nbdsf = NA,nddsf = NA,vbdsf = NA,vddsf = NA,dlwrf = NA))

inputs <- dfc %>% arrange(timestamp) %>%
  mutate(doy = yday(timestamp),
         year = year(timestamp),
         mo = month(timestamp),
         day = day(timestamp),
         hour = hour(timestamp),
         min = minute(timestamp),
         tot.s = (doy - 1)*86400 + hour*3600 + min*60)  %>%
  mutate(deltat = c(300,diff(tot.s))) %>% tibble::rownames_to_column()

inputs.gf <- inputs %>% group_by(doy,hour) %>% mutate(temp = case_when(is.na(temp) ~ mean(temp,na.rm = TRUE),
                                                                       TRUE ~ temp),
                                                      pres = case_when(is.na(pres) ~ mean(pres,na.rm = TRUE),
                                                                       TRUE ~ pres),
                                                      sh = case_when(is.na(sh) ~ mean(sh,na.rm = TRUE),
                                                                       TRUE ~ sh),
                                                      ugrd = case_when(is.na(ugrd) ~ mean(ugrd,na.rm = TRUE),
                                                                       TRUE ~ ugrd),
                                                      vgrd = case_when(is.na(vgrd) ~ mean(vgrd,na.rm = TRUE),
                                                                       TRUE ~ vgrd),
                                                      prate = case_when(is.na(prate) ~ mean(prate,na.rm = TRUE),
                                                                       TRUE ~ prate),
                                                      nbdsf = case_when(is.na(nbdsf) ~ mean(nbdsf,na.rm = TRUE),
                                                                       TRUE ~ nbdsf),
                                                      nddsf = case_when(is.na(nddsf) ~ mean(nddsf,na.rm = TRUE),
                                                                       TRUE ~ nddsf),
                                                      vbdsf = case_when(is.na(vbdsf) ~ mean(vbdsf,na.rm = TRUE),
                                                                       TRUE ~ vbdsf),
                                                      vddsf = case_when(is.na(vddsf) ~ mean(vddsf,na.rm = TRUE),
                                                                       TRUE ~ vddsf),
                                                      dlwrf = case_when(is.na(dlwrf) ~ mean(dlwrf,na.rm = TRUE),
                                                                       TRUE ~ dlwrf))


# var = "nbdsf"
# inputs.gf %>% filter(is.na(!!var))
# plot(inputs.gf$timestamp,inputs.gf[[var]],type = "l")
# lines(inputs$timestamp,inputs[[var]],type = "l",col = 'red',lty = 2)


inputs.gf_long <- inputs.gf %>% dplyr::select(-c(rowname)) %>% pivot_longer(cols = c(temp,pres,sh,ugrd,vgrd,prate,nbdsf,nddsf,vbdsf,vddsf,dlwrf),
                                                                            names_to = "var",
                                                                            values_to = "value")

inputs.gf_long.seasonal <- bind_rows(list(inputs.gf_long %>% filter(var == "prate") %>% group_by(year,mo,var) %>% summarise(value = sum(value,na.rm = TRUE)) %>% group_by(mo,var) %>% summarise(value = mean(value)),
                                          inputs.gf_long %>% filter(var != "prate") %>% group_by(var,mo) %>% summarise(value = mean(value,na.rm = TRUE))))

inputs.gf_long.diel <- inputs.gf_long %>% group_by(var,mo,hour) %>% summarise(value = mean(value,na.rm = TRUE))


ggplot(data = inputs.gf_long.seasonal) +
  geom_line(aes(x = mo,y = value)) +
  scale_x_continuous(breaks = c(1,3,6,9,12),
                     labels = c("J","M","J","S","D")) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()

ggplot(data = inputs.gf_long.diel) +
  geom_line(aes(x = hour,y = value, color = as.factor(mo))) +
  scale_x_continuous(breaks = seq(0,23,6)) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()

outfolder <- file.path("./outputs")
start_date <- "2019/01/01"
end_date <-  "2019/12/31"
lst = 0
lat = 0.78
lon = 24.44
overwrite = TRUE
verbose = TRUE
leap_year = TRUE
CO2constant = 410

correctPrate= 1750/2300

IP <- inputs.gf %>% rename(y = year) %>% filter(y == 2019) %>%
  group_by(y,doy,hour) %>% summarise(temp = mean(temp,na.rm = TRUE),
                                   pres = mean(pres,na.rm = TRUE),
                                   sh = mean(sh,na.rm = TRUE),
                                   ugrd = mean(ugrd,na.rm = TRUE),
                                   vgrd = mean(vgrd,na.rm = TRUE),
                                   prate = mean(prate,na.rm = TRUE)*correctPrate,
                                   nbdsf = mean(nbdsf,na.rm = TRUE),
                                   nddsf = mean(nddsf,na.rm = TRUE),
                                   vbdsf = mean(vbdsf,na.rm = TRUE),
                                   vddsf = mean(vddsf,na.rm = TRUE),
                                   dlwrf = mean(dlwrf,na.rm = TRUE),
                                   tot.s = min(tot.s,na.rm = TRUE),
                                   month = mean(mo,na.rm = TRUE))

plot(cumsum(IP$prate*3600))

IP$ugrd <- na_seadec(IP$ugrd, algorithm = "interpolation")
IP$vgrd <- na_seadec(IP$vgrd, algorithm = "interpolation")

IP.csv <- IP %>% mutate(t = as.POSIXct(tot.s,origin = "2019-01-01",tz = "GMT")) %>% ungroup() %>%
  dplyr::select(t,temp,pres,sh,ugrd,vgrd,prate,nbdsf,nddsf,vbdsf,vddsf,dlwrf)

colnames(IP.csv) <- c("Time","Air temperature (K)","Pressure (Pa)","Specific humidity (-)",
                      "Zonal wind speed (m/s)","Meridional wind speed (m/s)","Precipitation rate (kg/m²/s)",
                      "Downward NIR direct irradiance (W/m²)","Downward NIR diffuse irradiance (W/m²)",
                      "Downward visible direct irradiance (W/m²)","Downward visible diffuse irradiance (W/m²)",
                      "Downward thermal infrared irradiance")
write.csv(x = IP.csv,
          file = "./outputs/MET_Gapfilled_YGB_2019.csv")

# met_folder <- outfolder
# met_header_file <- file.path(met_folder, "ED_MET_DRIVER_HEADER")
# metvar <- c("nbdsf", "nddsf", "vbdsf", "vddsf", "prate", "dlwrf",
#             "pres", "hgt", "ugrd", "vgrd", "sh", "tmp", "co2")
# metvar_table <- data.frame(
#   variable = metvar,
#   update_frequency = 3600,
#   flag = 1
# )
#
# ed_metheader <- list(list(
#   path_prefix = met_folder,
#   nlon = 1,
#   nlat = 1,
#   dx = 1,
#   dy = 1,
#   xmin = lon,
#   ymin = lat,
#   variables = metvar_table
# ))
#
# check_ed_metheader

# met2model.ED2(IP, outfolder, start_date, end_date, lst, lat,
#               lon, overwrite = TRUE, verbose = TRUE, leap_year = TRUE, CO2constant = 410)
# system2("rsync",paste("-avz",file.path("./outputs","2019*"),paste0("hpc:","/data/gent/vo/000/gvo00074/ED_common_data/met/Yangambi/localMET/")))
# system2("rsync",paste("-avz",file.path("./outputs","ED_MET*"),paste0("hpc:","/data/gent/vo/000/gvo00074/ED_common_data/met/Yangambi/localMET/")))
