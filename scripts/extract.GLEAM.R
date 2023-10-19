rm(list = ls())

library(reshape2)
library(lubridate)
library(dplyr)
library(ncdf4)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

######################################################################
# GLEAM

WD <- "/home/femeunier/Documents/projects/YGB/data/GLEAM/"

Efile.GLEAM <- file.path(WD,"E_1980-2021_GLEAM_v3.6a_MO.nc")

nc <- nc_open(Efile.GLEAM)

times <- ncvar_get(nc,"time")
lats <- ncvar_get(nc,"lat")
pos.lat <- which(lats >= -15 & lats <= 10)
lons <- ncvar_get(nc,"lon")
pos.lon <- which(lons >= -10 & lons <= 45)

E <- ncvar_get(nc,"E",
               start = c(min(pos.lon),min(pos.lat),1),
               count = c(length(pos.lon),length(pos.lat),length(times)))

nc_close(nc)

E.df <- melt(E) %>%
  mutate(Var1 = (lons[pos.lon])[Var1],
         Var2 = (lats[pos.lat])[Var2],
         Var3 = times[Var3]) %>%
  rename(lon = Var1,
         lat = Var2,
         time = Var3)

E.df <- E.df %>%
  filter(lat <= 10,lat >= -15, lon <= 45,lon >= -10) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.Date(time, origin = "1980-01-31")) %>%
  mutate(year = year(date),
         month = month(date))

E.df.sum <- E.df %>%
  group_by(lat,lon,year) %>%
  summarise(Etot = sum(value),
            .groups = "keep") %>%
  group_by(lat,lon) %>%
  summarise(Etot.m = mean(Etot),
            .groups = "keep") %>%
  mutate(Edaily = Etot.m/365.25)

world <- ne_countries(scale = "medium", returnclass = "sf")


ggplot(data = world) +
  geom_tile(data = E.df.sum,
            aes(x = lon, y = lat,fill = Edaily),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "blue",na.value = "transparent")+
  labs(x = "",y = "") +
  theme_bw()

saveRDS(object = E.df,file = "./data/GLEAM/df.GLEAM.RDS")


