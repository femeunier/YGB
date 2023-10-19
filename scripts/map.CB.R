rm(list = ls())

library(ncdf4)
library(ggplot2)
library(dplyr)
library(reshape2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(tidyr)
library(plotbiomes)

world <- ne_countries(scale = "medium", returnclass = "sf")
mask <- readRDS("/home/femeunier/Documents/projects/YGB/data/LandSeaMask.RDS")

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_CB.RDS",
                      "./outputs/"))

df_CB <- readRDS(file.path("/home/femeunier/Documents/projects/YGB/outputs/","df_CB.RDS"))

df <- df_CB %>% left_join(mask,
                       by = c("lat","lon")) %>%
  filter(mask == 1)

df.final <- df %>%
  group_by(lat,lon) %>%
  filter(yr == 2000) %>%
  mutate(AGB.bnd = case_when(AGB >= 30 ~ 30,
                             TRUE ~ AGB))

df.final %>% nrow

summary(unique(df.final$AGB))
hist(df.final$yr)

ggplot(data = world) +
  geom_tile(data = df.final,
            aes(x = lon, y = lat,fill = AGB),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = df.final) +
  geom_density(aes(x = AGB)) +
  theme_bw()

ggplot(data = df %>% filter(lat < Inf)) +
  geom_line(aes(x = yr, y = AGB, group = interaction(lat,lon))) +
  theme_bw()

ggplot(data = df %>% filter(lat < -8)) +
  geom_line(aes(x = yr, y = LAI, group = interaction(lat,lon))) +
  theme_bw()

