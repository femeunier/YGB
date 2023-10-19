rm(list = ls())

library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(YGB)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(YGB)
library(ggrepel)
library(wesanderson)

Avi.AFR <- stack("/home/femeunier/Documents/projects/SoilSensitivity/data/AGBavit_AFR.gri")

e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
Avi.AFR.crop <- crop(Avi.AFR, e)

plot(Avi.AFR.crop/20)

ESA.AFR <- brick("/home/femeunier/Documents/projects/YGB/outputs/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0.aggr.tif",
                 varname="agb")
plot(ESA.AFR)

ESA.AFR.rspld <- resample(ESA.AFR,Avi.AFR.crop)


df.all <- as.data.frame(Avi.AFR.crop,xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         AGB.Avi = Avitabile_AGB_Map) %>%
  mutate(AGB.ESA = as.vector(ESA.AFR.rspld))

df.all.long <- df.all %>%
  pivot_longer(cols = c(AGB.Avi,AGB.ESA),
               names_to = "source",
               values_to = "value")

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_raster(data = df.all.long,
              aes(x = lon, y = lat,fill = value),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent")+
  labs(x = "",y = "") +
  facet_wrap(~ source) +
  theme_bw()

ggplot(data = df.all %>% filter(AGB.ESA > 0.1,
                                AGB.Avi > 0.1),
       aes(x = AGB.Avi, y = AGB.ESA)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  coord_fixed() +
  stat_smooth(method = "lm") +
  theme_bw()

ggplot(data = df.all %>% filter(AGB.ESA > 0.1,
                                AGB.Avi > 0.1),
       aes(x = AGB.Avi, y = AGB.ESA)) +
  geom_bin2d(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = "lm", color = "red", se = FALSE, size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 1) +
  theme_bw()

AGB.cor <- cor(df.all %>% filter(!is.na(AGB.ESA),!is.na(AGB.Avi)) %>%
                    dplyr::select(AGB.ESA,AGB.Avi))
corrplot(AGB.cor, method = 'ellipse', order = 'AOE', type = 'upper')

