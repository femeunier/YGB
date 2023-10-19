rm(list = ls())

library(ggplot2)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(corrplot)
library(dplyr)
library(tidyr)
library(YGB)

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.ERA5.RDS",
#                       "./data/Precip/"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.CRU.RDS",
#                       "./data/Precip/"))
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.JRA.RDS",
#                       "./data/Precip/"))
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.GPCC1.RDS",
#                       "./data/Precip/"))
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.GPCC2.RDS",
#                       "./data/Precip/"))
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.MERRA2.RDS",
#                       "./data/Precip/"))

df.CRU <- readRDS("./data/Precip/df.CRU.RDS") %>%
  rename(Pmm = MAP)

df.ERA5 <- readRDS("./data/Precip/df.ERA5.RDS") %>%
  rename(Pmm = MAP)

df.GPCC <- bind_rows(list(readRDS("./data/Precip/df.GPCC1.RDS"),
                          readRDS("./data/Precip/df.GPCC2.RDS")))  %>%
  rename(Pmm = MAP)

df.3B43 <- readRDS("./data/Precip/df.3B43.RDS") %>%
  dplyr::select(year,month,lon,lat,Pmm)

df.MERRA2 <- readRDS("./data/Precip/df.MERRA2.RDS") %>%
  rename(Pmm = MAP) %>%
  mutate(Pmm = Pmm*86400*30)

df.JRA <- readRDS("./data/Precip/df.JRA.RDS") %>%
  rename(Pmm = MAP) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))


df.all <- bind_rows(list(df.CRU %>% mutate(data = "CRU"),
                         df.JRA %>% mutate(data = "JRA"),
                         df.MERRA2 %>% mutate(data = "MERRA2"),
                         df.GPCC %>% mutate(data = "GPCC"),
                         df.ERA5 %>% mutate(data = "ERA5"),
                         df.3B43 %>% mutate(data = "3B43")))

df.all.sum <- df.all %>%
  group_by(data,lat,lon,month) %>%
  summarise(Pmm = mean(Pmm),
            .groups = "keep") %>%
  group_by(data,month) %>%
  summarise(P.m = mean(Pmm,na.rm = TRUE),
            P.sd = sd(Pmm,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.all) +
  # geom_line(aes(x = month, y = Pmm, group = as.factor(year)), color = "black", size = 0.1) +
  geom_line(data = df.all.sum,
            aes(x = month, y = P.m), color = "blue", size = 2) +
  geom_ribbon(data = df.all.sum,
              aes(x = month, y = P.m, ymin = P.m - P.sd, ymax = P.m + P.sd), color = NA, alpha = 0.4) +
  facet_wrap(~ data) +
  theme_bw()

world <- ne_countries(scale = "medium", returnclass = "sf")

df.year <- df.all %>%
  group_by(data,lat,lon,year) %>%
  summarise(MAP = sum(Pmm,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(data,lat,lon) %>%
  summarise(MAP.m = mean(MAP,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(MAP.m.bnd = case_when(MAP.m >= 3000 ~ 3000,
                               TRUE ~ MAP.m))

ggplot(data = world) +
  geom_raster(data = df.year,
            aes(x = lon, y = lat,fill = MAP.m.bnd),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "blue",na.value = "transparent")+
  labs(x = "",y = "") +
  facet_wrap(~ data) +
  theme_bw()

df.all.pval <- df.all %>%
  filter(!is.na(Pmm)) %>%
  group_by(data,year,month) %>%
  summarise(Pmm = mean(Pmm),
            .groups = "keep") %>%
  group_by(data,month) %>%
  mutate(p.value = coef(summary(lm(Pmm ~ year, data = cur_data())))[2,4])

ggplot(data = df.all %>%
         filter(!is.na(Pmm)) %>%
         group_by(data,year,month) %>%
         summarise(Pmm = mean(Pmm),
                   .groups = "keep"),
       aes(x = year, y = Pmm, color = as.factor(month))) +
  geom_point(size = 0.2) +
  # stat_smooth(method = "lm",se = FALSE,
  #             linetype = 3) +
  stat_smooth(data = df.all.pval,
              method = "lm",se = FALSE, alpha = 0.5, size = 0.5, linetype = 2) +
  stat_smooth(data = df.all.pval %>% filter(p.value < 0.05),
              method = "lm",se = FALSE) +
  facet_wrap(~ data, scales = "free_x") +
  theme_bw()


#################################################################################
# Calculate MCWD
df.MCWD <- df.all %>%
  filter(!is.na(Pmm)) %>%
  dplyr::select(data,lon,lat,Pmm,year,month) %>%
  group_by(data,month,lon,lat) %>%
  summarise(Pmm = mean(Pmm),
            .groups = "keep") %>%
  group_by(data,lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = 3.33,
         Etot = E*Ndays) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD))

ggplot(data = world) +
  geom_raster(data = df.MCWD,
            aes(x = lon, y = lat,fill = MCWD),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "red",high = "white",na.value = "transparent")+
  labs(x = "",y = "") +
  facet_wrap(~data) +
  theme_bw()

MCWD.ERA5 <- rasterFromXYZ(df.MCWD %>% filter(data == "ERA5") %>%
                             ungroup() %>%
                             dplyr::select(lon,lat,MCWD) )

MCWD.GPCC <- rasterFromXYZ(df.MCWD %>% filter(data == "GPCC") %>%
                             ungroup() %>%
                             dplyr::select(lon,lat,MCWD) )

MCWD.MERRA2 <- rasterFromXYZ(df.MCWD %>% filter(data == "MERRA2") %>%
                             ungroup() %>%
                             dplyr::select(lon,lat,MCWD) )

MCWD.CRU <- rasterFromXYZ(df.MCWD %>% filter(data == "CRU") %>%
                             ungroup() %>%
                             dplyr::select(lon,lat,MCWD) )

MCWD.JRA <- rasterFromXYZ(df.MCWD %>% filter(data == "JRA") %>%
                            ungroup() %>%
                            dplyr::select(lon,lat,MCWD) )

MCWD.3B43 <- rasterFromXYZ(df.MCWD %>% filter(data == "3B43") %>%
                             ungroup() %>%
                             dplyr::select(lon,lat,MCWD) )

MCWD.ERA5.rspld <- resample(MCWD.ERA5,MCWD.CRU)
MCWD.ERA5.rspld[is.na(MCWD.CRU)] <- NA
MCWD.3B43.rspld <- resample(MCWD.3B43,MCWD.CRU)
MCWD.3B43.rspld[is.na(MCWD.CRU)] <- NA
MCWD.MERRA2.rspld <- resample(MCWD.MERRA2,MCWD.CRU)
MCWD.MERRA2.rspld[is.na(MCWD.CRU)] <- NA
MCWD.GPCC.rspld <- resample(MCWD.GPCC,MCWD.CRU)
MCWD.GPCC.rspld[is.na(MCWD.CRU)] <- NA
MCWD.JRA.rspld <- resample(MCWD.JRA,MCWD.CRU)
MCWD.JRA.rspld[is.na(MCWD.CRU)] <- NA

df.MCWD.mask <- bind_rows(list(as.data.frame(MCWD.CRU,xy = TRUE) %>% mutate(data = "CRU"),
                               as.data.frame(MCWD.GPCC.rspld,xy = TRUE) %>% mutate(data = "GPCC"),
                               as.data.frame(MCWD.MERRA2.rspld,xy = TRUE) %>% mutate(data = "MERRA2"),
                               as.data.frame(MCWD.ERA5.rspld,xy = TRUE) %>% mutate(data = "ERA5"),
                               as.data.frame(MCWD.JRA.rspld,xy = TRUE) %>% mutate(data = "JRA"),
                               as.data.frame(MCWD.3B43.rspld,xy = TRUE) %>% mutate(data = "3B43"))) %>%
  rename(lon = x, lat = y) %>%
  filter(!is.na(MCWD))

ggplot(data = df.MCWD.mask) +
  geom_density(aes(x = MCWD, fill = data),alpha = 0.5) +
  theme_bw()

saveRDS(object = df.MCWD.mask,file = "./data/MCWD/df.MCWD.RDS")

ggplot(data = world) +
  geom_raster(data = df.MCWD.mask,
              aes(x = lon, y = lat,fill = MCWD),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "red",high = "white",na.value = "transparent")+
  labs(x = "",y = "") +
  facet_wrap(~data) +
  theme_bw()


df.data.MCWD <- df.MCWD.mask %>%
  pivot_wider(names_from = "data",
              values_from = "MCWD") %>%
  dplyr::select(CRU,ERA5,`3B43`,JRA,GPCC,MERRA2) %>%
  filter(!is.na(CRU),!is.na(ERA5),!is.na(`3B43`),!is.na(JRA),!is.na(GPCC),!is.na(MERRA2))

M.MCWD <- cor(df.data.MCWD)
corrplot(M.MCWD, method = 'ellipse', order = 'AOE', type = 'upper')


###########################################################################################################
# Resample precip as well

precip.ERA5 <- rasterFromXYZ(df.all %>% filter(data == "ERA5") %>%
                               ungroup() %>%
                               group_by(lat,lon,year) %>%
                               summarise(MAP = sum(Pmm,na.rm = TRUE),
                                         .groups = "keep") %>%
                               group_by(lat,lon) %>%
                               summarise(MAP.m = mean(MAP),
                                         .groups = "keep") %>%
                               dplyr::select(lon,lat,MAP.m))


precip.CRU <- rasterFromXYZ(df.all %>% filter(data == "CRU") %>%
                              ungroup() %>%
                              group_by(lat,lon,year) %>%
                              summarise(MAP = sum(Pmm),
                                        .groups = "keep") %>%
                              group_by(lat,lon) %>%
                              summarise(MAP.m = mean(MAP),
                                        .groups = "keep") %>%
                              dplyr::select(lon,lat,MAP.m))

precip.MERRA2 <- rasterFromXYZ(df.all %>% filter(data == "MERRA2") %>%
                              ungroup() %>%
                              group_by(lat,lon,year) %>%
                              summarise(MAP = sum(Pmm),
                                        .groups = "keep") %>%
                              group_by(lat,lon) %>%
                              summarise(MAP.m = mean(MAP),
                                        .groups = "keep") %>%
                              dplyr::select(lon,lat,MAP.m))

precip.GPCC <- rasterFromXYZ(df.all %>% filter(data == "GPCC") %>%
                              ungroup() %>%
                              group_by(lat,lon,year) %>%
                              summarise(MAP = sum(Pmm),
                                        .groups = "keep") %>%
                              group_by(lat,lon) %>%
                              summarise(MAP.m = mean(MAP),
                                        .groups = "keep") %>%
                              dplyr::select(lon,lat,MAP.m))

precip.JRA <- rasterFromXYZ(df.all %>% filter(data == "JRA") %>%
                              ungroup() %>%
                              group_by(lat,lon,year) %>%
                              summarise(MAP = sum(Pmm),
                                        .groups = "keep") %>%
                              group_by(lat,lon) %>%
                              summarise(MAP.m = mean(MAP),
                                        .groups = "keep") %>%
                              dplyr::select(lon,lat,MAP.m))

precip.3B43 <- rasterFromXYZ(df.all %>% filter(data == "3B43") %>%
                               ungroup() %>%
                               group_by(lat,lon,year) %>%
                               summarise(MAP = sum(Pmm,na.rm = TRUE),
                                         .groups = "keep") %>%
                               group_by(lat,lon) %>%
                               summarise(MAP.m = mean(MAP),
                                         .groups = "keep") %>%
                               dplyr::select(lon,lat,MAP.m))

precip.ERA5.rspld <- resample(precip.ERA5,precip.CRU)
precip.ERA5.rspld[is.na(precip.CRU)] <- NA

precip.JRA.rspld <- resample(precip.JRA,precip.CRU)
precip.JRA.rspld[is.na(precip.CRU)] <- NA

precip.MERRA2.rspld <- resample(precip.MERRA2,precip.CRU)
precip.MERRA2.rspld[is.na(precip.CRU)] <- NA

precip.GPCC.rspld <- resample(precip.GPCC,precip.CRU)
precip.GPCC.rspld[is.na(precip.CRU)] <- NA

precip.3B43.rspld <- resample(precip.3B43,precip.CRU)
precip.3B43.rspld[is.na(precip.CRU)] <- NA

df.precip.mask <- bind_rows(list(as.data.frame(precip.CRU,xy = TRUE) %>% mutate(data = "CRU"),
                                 as.data.frame(precip.ERA5.rspld,xy = TRUE) %>% mutate(data = "ERA5"),
                                 as.data.frame(precip.MERRA2.rspld,xy = TRUE) %>% mutate(data = "MERRA2"),
                                 as.data.frame(precip.GPCC.rspld,xy = TRUE) %>% mutate(data = "GPCC"),
                                 as.data.frame(precip.JRA.rspld,xy = TRUE) %>% mutate(data = "JRA"),
                                 as.data.frame(precip.3B43.rspld,xy = TRUE) %>% mutate(data = "3B43"))) %>%
  rename(lon = x, lat = y) %>%
  filter(!is.na(MAP.m)) %>%
  mutate(MAP.m.bnd = case_when(MAP.m >= 3000 ~ 3000,
                               TRUE ~ MAP.m))

df.precip.mask %>%
  group_by(data) %>%
  summarise(MAP.min = min(MAP.m,na.rm = TRUE),
            MAP.mean = mean(MAP.m,na.rm = TRUE),
            MAP.med = median(MAP.m,na.rm = TRUE),
            MAP.max = max(MAP.m,na.rm = TRUE))


ggplot(data = df.precip.mask) +
  geom_density(aes(x = MAP.m, fill = data),alpha = 0.5) +
  theme_bw()

saveRDS(object = df.precip.mask,file = "./data/Precip/df.precip.RDS")

ggplot(data = world) +
  geom_raster(data = df.precip.mask ,
              aes(x = lon, y = lat,fill = MAP.m.bnd),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "blue",na.value = "transparent")+
  labs(x = "",y = "") +
  facet_wrap(~data) +
  theme_bw()

df.data.precip <- df.precip.mask %>%
  dplyr::select(-MAP.m.bnd) %>%
  pivot_wider(names_from = "data",
              values_from = "MAP.m") %>%
  dplyr::select(CRU,ERA5,`3B43`,JRA,GPCC,MERRA2) %>%
  filter(!is.na(CRU),!is.na(ERA5),!is.na(`3B43`),!is.na(JRA),!is.na(GPCC),!is.na(MERRA2))

M.precip <- cor(df.data.precip)
corrplot(M.precip, method = 'ellipse', order = 'AOE', type = 'upper')
