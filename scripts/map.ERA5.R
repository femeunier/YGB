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

# ######################################################################################################################
# # Mask
# #
# nc.file <- "./data/lsm_1279l4_0.1x0.1.grb_v4_unpack.nc"
# nc <- nc_open(nc.file)
#
# lats <- ncvar_get(nc,"latitude")
# lons <- ncvar_get(nc,"longitude")
# times <- ncvar_get(nc,"time")
# mask <- ncvar_get(nc,"lsm")
#
# nc_close(nc)
#
# df <- melt(mask) %>%
#   mutate(lon = (lons)[Var1],
#          lat = (lats)[Var2],
#          mask = value) %>%
#   dplyr::select(lat,lon,mask) %>%
#   mutate(lon = case_when(lon > 180 ~ (lon -360),
#                          TRUE ~ lon)) %>%
#   mutate(clon = round(lon*2)/2,
#          clat = round(lat*2)/2) %>%
#   group_by(clon,clat) %>%
#   summarise(mask = ceiling(mean(mask)),
#             .groups = "keep") %>%
#   rename(lon = clon,
#          lat = clat)
#
# ggplot(data = world) +
#   geom_tile(data = df,
#             aes(x = lon, y = lat,fill = as.factor(mask)),na.rm = TRUE) +
#   geom_sf(fill = NA) +
#   coord_sf(expand = FALSE) +
#   labs(x = "",y = "") +
#   theme_bw()
#
# saveRDS(df,"./data/LandSeaMask.RDS")
#
#
# ##############################################################################################

mask <- readRDS("./data/LandSeaMask.RDS")

nc.file <- "./data/ERA5_2019.nc"

nc <- nc_open(nc.file)

lats <- ncvar_get(nc,"latitude")
lons <- ncvar_get(nc,"longitude")
times <- ncvar_get(nc,"time")

var.names <- c("tp","t2m")

for (ivar.name in seq(1,length(var.names))){

  cvar.name <- var.names[ivar.name]
  cVar <- ncvar_get(nc,cvar.name)

  if (ivar.name == 1){
    df <- melt(cVar) %>%
      mutate(lon = (lons)[Var1],
             lat = (lats)[Var2],
             time = times[Var3]) %>%
      dplyr::select(lat,lon,time,value) %>%
      rename(!!cvar.name := value)
  } else{
    df <- df %>% mutate(!!cvar.name := as.vector(cVar))

  }
}

nc_close(nc)

df <- df %>% left_join(mask,
                       by = c("lat","lon")) %>%
  filter(mask == 1) %>%
  mutate(time0 = time - min(time),
         yr = floor(time0/24/365),
         lon = case_when(lon > 180 ~ (lon -360),
                         TRUE ~ lon))

df.yr <- df %>% group_by(lat,lon) %>%
  summarise(MAP = sum(tp)*1000*3,  # because hourly but only one third downloaded
            MAT = mean(t2m - 273.15),
            .groups = "keep") %>%
  mutate(MAP.bnd = case_when(MAP >= 3000 ~ 3000,
                             TRUE ~ MAP))

summary(unique(df.yr$MAP))
hist(df.yr$MAP)

ggplot(data = world) +
  geom_tile(data = df.yr,
            aes(x = lon, y = lat,fill = MAP.bnd),na.rm = TRUE) +
  geom_sf(fill = NA) +
  # coord_sf(xlim = c(23, 25),
  #          ylim = c(0, 2),
  #          expand = FALSE) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "blue",na.value = "transparent") +
  labs(x = "",y = "") +
  theme_bw()


ggplot() +
  geom_polygon(data = Whittaker_biomes,
               aes(x    = temp_c,
                   y    = precp_cm*10,
                   fill = biome),
               colour = "gray98", # colour of polygon border
               size   = 0.5,
               alpha = 0.7) +    # thickness of polygon border
  geom_point(data = df.yr,
             aes(x = MAT, y = MAP), size = 0.1) +
  scale_fill_manual(name   = "Whittaker biomes",
                    breaks = names(Ricklefs_colors),
                    labels = names(Ricklefs_colors),
                    values = Ricklefs_colors) +
  theme_bw() +
  theme(legend.position = c(0.15,0.8))
