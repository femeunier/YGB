rm(list = ls())

library(ggplot2)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(corrplot)
library(dplyr)
library(tidyr)
library(matlab)

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.SW.CRU.RDS",
#                       "./data/SW/CRU/"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.SW.ERA5.RDS",
#                       "./data/SW/ERA5/"))
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.SW.JRA.RDS",
#                       "./data/SW/JRA/"))
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.SW.MERRA2.RDS",
#                       "./data/SW/MERRA2/"))


df.CERES <- readRDS("./data/SW/CERES/df.CERES.RDS") %>% dplyr::select(lon,lat,year,month,value) %>% rename(SW = value)
df.CRU <- readRDS("./data/SW/CRU/df.SW.CRU.RDS")
df.MERRA2 <- readRDS("./data/SW/MERRA2/df.SW.MERRA2.RDS") %>% rename(SW = MAP)
df.JRA <- readRDS("./data/SW/JRA/df.SW.JRA.RDS") %>% mutate(SW = SW/3600/6)
lats <- unique(df.JRA$lat)
lats.flipped <- fliplr(lats)
df.JRA <- df.JRA %>%
  mutate(lat = as.factor(lat))
levels(df.JRA$lat) <- as.character(lats.flipped)
df.JRA$lat <- as.numeric(as.vector(df.JRA$lat))

df.ERA5 <- readRDS("./data/SW/ERA5/df.SW.ERA5.RDS") %>% rename(SW = sw)

# ggplot(data = world) +
#   geom_tile(data = df.JRA %>% filter(year == 1901, month == 1),
#             aes(x = lon, y = lat,fill = SW),na.rm = TRUE, alpha = 1) +
#   geom_sf(fill = NA) +
#   coord_sf(xlim = c(-10, 50),
#            ylim = c(-20, 15),
#            expand = FALSE) +
#   scale_fill_gradient(low = "white",high = "red",na.value = "transparent")+
#   labs(x = "",y = "") +
#   theme_bw()


df.all <- bind_rows(list(df.CERES %>% mutate(data = "CERES"),
                         df.CRU %>% mutate(data = "CRU"),
                         df.MERRA2 %>% mutate(data = "MERRA2"),
                         df.JRA %>% mutate(data = "JRA"),
                         df.ERA5 %>% mutate(data = "ERA5")))

df.all.sum <- df.all %>%
  group_by(data,lat,lon,month) %>%
  summarise(SWm = mean(SW),
            .groups = "keep") %>%
  group_by(data,month) %>%
  summarise(SW.m = mean(SWm,na.rm = TRUE),
            SW.sd = sd(SWm,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.all) +
  geom_line(data = df.all.sum,
            aes(x = month, y = SW.m), color = "blue", size = 2) +
  geom_ribbon(data = df.all.sum,
              aes(x = month, y = SW.m, ymin = SW.m - SW.sd, ymax = SW.m + SW.sd), color = NA, alpha = 0.4) +
  facet_wrap(~ data) +
  theme_bw()

world <- ne_countries(scale = "medium", returnclass = "sf")

df.year <- df.all %>%
  group_by(data,lat,lon) %>%
  summarise(SWm = mean(SW,na.rm = TRUE),
            .groups = "keep")

# ggplot(data = world) +
#   geom_raster(data = df.year,
#               aes(x = lon, y = lat,fill = SWm),na.rm = TRUE, alpha = 1) +
#   geom_sf(fill = NA) +
#   coord_sf(xlim = c(-10, 50),
#            ylim = c(-20, 15),
#            expand = FALSE) +
#   scale_fill_gradient(low = "white",high = "red",na.value = "transparent")+
#   labs(x = "",y = "") +
#   facet_wrap(~ data) +
#   theme_bw()

df.all.pval <- df.all %>%
  filter(!is.na(SW)) %>%
  group_by(data,year,month) %>%
  summarise(SWm = mean(SW),
            .groups = "keep") %>%
  group_by(data,month) %>%
  mutate(p.value = coef(summary(lm(SWm ~ year, data = cur_data())))[2,4])

ggplot(data = df.all %>%
         filter(!is.na(SW)) %>%
         group_by(data,year,month) %>%
         summarise(SWm = mean(SW),
                   .groups = "keep"),
       aes(x = year, y = SWm, color = as.factor(month))) +
  geom_point(size = 0.2) +
  stat_smooth(data = df.all.pval,
              method = "lm",se = FALSE, alpha = 0.5, size = 0.5, linetype = 2) +
  stat_smooth(data = df.all.pval %>% filter(p.value < 0.05),
              method = "lm",se = FALSE) +
  facet_wrap(~ data, scales = "free_x") +
  theme_bw()


SW.ERA5 <- rasterFromXYZ(df.year %>% filter(data == "ERA5") %>%
                             ungroup() %>%
                             dplyr::select(lon,lat,SWm) )
SW.CRU <- rasterFromXYZ(df.year %>% filter(data == "CRU") %>%
                            ungroup() %>%
                            dplyr::select(lon,lat,SWm) )
SW.MERRA2 <- rasterFromXYZ(df.year %>% filter(data == "MERRA2") %>%
                          ungroup() %>%
                          dplyr::select(lon,lat,SWm) )
SW.JRA <- rasterFromXYZ(df.year %>% filter(data == "JRA") %>%
                          ungroup() %>%
                          dplyr::select(lon,lat,SWm) )
SW.CERES <- rasterFromXYZ(df.year %>% filter(data == "CERES") %>%
                             ungroup() %>%
                             dplyr::select(lon,lat,SWm))

SW.ERA5.rspld <- resample(SW.ERA5,SW.CRU)
SW.ERA5.rspld[is.na(SW.CRU)] <- NA

SW.JRA.rspld <- resample(SW.JRA,SW.CRU)
SW.JRA.rspld[is.na(SW.CRU)] <- NA

SW.MERRA2.rspld <- resample(SW.MERRA2,SW.CRU)
SW.MERRA2.rspld[is.na(SW.CRU)] <- NA

SW.CERES.rspld <- resample(SW.CERES,SW.CRU)
SW.CERES.rspld[is.na(SW.CRU)] <- NA

df.SW.mask <- bind_rows(list(as.data.frame(SW.CRU,xy = TRUE) %>% mutate(data = "CRU"),
                             as.data.frame(SW.ERA5.rspld,xy = TRUE) %>% mutate(data = "ERA5"),
                             as.data.frame(SW.MERRA2.rspld,xy = TRUE) %>% mutate(data = "MERRA2"),
                             as.data.frame(SW.JRA.rspld,xy = TRUE) %>% mutate(data = "JRA"),
                             as.data.frame(SW.CERES.rspld,xy = TRUE) %>% mutate(data = "CERES"))) %>%
  rename(lon = x, lat = y) %>%
  filter(!is.na(SWm))

ggplot(data = df.SW.mask) +
  geom_density(aes(x = SWm, fill = data),alpha = 0.5) +
  theme_bw()

ggplot(data = world) +
  geom_tile(data = df.SW.mask,
              aes(x = lon, y = lat,fill = SWm),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "red",na.value = "transparent")+
  labs(x = "",y = "") +
  facet_wrap(~data) +
  theme_bw()


df.data <- df.SW.mask %>%
  pivot_wider(names_from = "data",
              values_from = "SWm") %>%
  dplyr::select(CRU,ERA5,JRA,CERES,MERRA2) %>%
  filter(!is.na(CRU),!is.na(ERA5),!is.na(CERES),!is.na(JRA),!is.na(MERRA2))

M <- cor(df.data)
corrplot(M, method = 'ellipse', order = 'AOE', type = 'upper')
