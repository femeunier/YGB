rm(list = ls())

library(reshape2)
library(lubridate)
library(dplyr)
library(ncdf4)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

WD <- "/home/femeunier/Documents/projects/YGB/data/CERES/"

CERES.file <- file.path(WD,"CERES_EBAF_Ed4.1_Subset_200003-202202.nc")

nc <- nc_open(CERES.file)

times <- ncvar_get(nc,"time")
lats <- ncvar_get(nc,"lat")
lons <- ncvar_get(nc,"lon")

sw <- ncvar_get(nc,"sfc_sw_down_all_mon")

nc_close(nc)

sw.df <- melt(sw) %>%
  mutate(Var1 = (lons)[Var1],
         Var2 = (lats)[Var2],
         Var3 = times[Var3]) %>%
  rename(lon = Var1,
         lat = Var2,
         time = Var3) %>%
  mutate(lon = case_when(lon > 180 ~ lon - 360,
                         TRUE ~ lon)) %>%
  filter(lat <= 10,lat >= -15, lon <= 45,lon >= -10) %>%
  mutate(date = as.Date(time, origin = "2000-03-01")) %>%
  mutate(year = year(date),
         month = month(date))

sw.df.sum <- sw.df %>%
  group_by(lat,lon,year) %>%
  summarise(sw.m = mean(value),
            .groups = "keep") %>%
  group_by(lat,lon) %>%
  summarise(sw.m = mean(sw.m),
            .groups = "keep")

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_tile(data = sw.df.sum,
            aes(x = lon, y = lat,fill = sw.m),
            na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "blue",na.value = "transparent")+
  labs(x = "",y = "") +
  theme_bw()

hist(sw.df.sum$sw.m)

saveRDS(object = sw.df,file = "./data/CERES/df.CERES.RDS")
