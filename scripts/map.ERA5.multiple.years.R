rm(list = ls())

library(plotbiomes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/climate.CB.RDS",
                    "./outputs/climate.CB.RDS"))
system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/climate.CB.month.RDS",
                    "./outputs/climate.CB.month.RDS"))

climate.CB <- readRDS("./outputs/climate.CB.RDS")
climate.CB.month <- readRDS("./outputs/climate.CB.month.RDS")

climate.CB.m <- climate.CB %>% group_by(lat,lon) %>%
  summarise(MAP.m = mean(MAP),
            MAT.m = mean(MAT),
            SW.m = mean(SW),
            .groups = "keep") %>%
  mutate(MAP.bnd = case_when(MAP.m >= 3000 ~ 3000,
                             TRUE ~ MAP.m))

ggplot(data = world) +
  geom_tile(data = climate.CB.m,
            aes(x = lon, y = lat,fill = MAP.bnd),na.rm = TRUE) +
  geom_sf(fill = NA) +
  geom_point(data = data.frame(lat = 0.77, lon = 24.78),
             aes(x = lon, y = lat), color = "red", size = 2) +
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
  geom_point(data = climate.CB %>% filter(year == 2019),
             aes(x = MAT, y = MAP), size = 0.1) +
  scale_fill_manual(name   = "Whittaker biomes",
                    breaks = names(Ricklefs_colors),
                    labels = names(Ricklefs_colors),
                    values = Ricklefs_colors) +
  theme_bw()


CB.anomaly <- climate.CB %>% group_by(lat,lon) %>%
  mutate(MAT.m = mean(MAT[year < 1980]),
         MAT.sd = sd(MAT[year < 1980]),

         MAP.m = mean(MAP[year < 1980]),
         MAP.sd = sd(MAP[year < 1980]),

         SW.m = mean(SW[year < 1980]),
         SW.sd = sd(SW[year < 1980])) %>%
  mutate(anomaly.MAT = (MAT - MAT.m),
         anomaly.MAT.norm = (MAT - MAT.m)/MAT.sd,

         anomaly.MAP = MAP - MAP.m,
         anomaly.MAP.norm = (MAP - MAP.m)/MAP.sd,

         anomaly.SW = (SW - SW.m),
         anomaly.SW.norm = (SW - SW.m)/SW.sd) %>%

  dplyr::select(lat,lon,year,
                anomaly.MAT.norm,anomaly.MAP.norm,anomaly.SW.norm) %>%
  pivot_longer(cols = c(anomaly.MAT.norm,anomaly.MAP.norm,anomaly.SW.norm),
               names_to = "var",
               values_to = "value")


CB.anomaly.m <- CB.anomaly %>% group_by(year,var) %>%
  summarise(value.m = mean(value),
            value.sd = sd(value),.groups = "keep")

ggplot() +
  geom_rect(data = data.frame(xmin = 2015, xmax = 2016, ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.2, color = NA) +
  geom_ribbon(data = CB.anomaly.m,
            aes(x = year, y = value.m, ymin = value.m - value.sd, ymax = value.m + value.sd), fill = "grey",alpha = 0.4, color = NA) +
  geom_line(data = CB.anomaly %>% filter(lat == 0.5,
                                         lon == 24.5),
            aes(x = year, y = value, group = interaction(lat,lon)),size = 0.2, color = "blue") +
  geom_line(data = CB.anomaly.m,
            aes(x = year, y = value.m)) +
  geom_hline(yintercept = 0, color = "black",linetype = 2) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()


ggplot(data = world) +
  geom_tile(data = CB.anomaly %>% filter(year >= 2010) %>%
              group_by(lat,lon,var) %>%
              summarise(value.m = mean(value),
                        .groups = "keep"),
            aes(x = lon, y = lat,fill = value.m),na.rm = TRUE) +
  geom_sf(fill = NA) +
  geom_point(data = data.frame(lat = 0.77, lon = 24.78),
             aes(x = lon, y = lat), color = "red", size = 1) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",midpoint = 0,na.value = "transparent") +
  facet_wrap(~ var) +
  labs(x = "",y = "") +
  theme_bw()


#################################################################################################################
# Seasonality

climate.CB.month.m <- climate.CB.month %>% group_by(m,lat,lon) %>%
  summarise(MAP.m = mean(MAP),
            MAT.m = mean(MAT),
            SW.m = mean(SW),
            .groups = "keep")


climate.CB.month.sum.m <- climate.CB.month.m %>%
  group_by(m) %>%
  summarise()


ggplot() +
  geom_rect(data = data.frame(xmin = 2015, xmax = 2016, ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.2, color = NA) +
  geom_ribbon(data = CB.anomaly.m,
              aes(x = year, y = value.m, ymin = value.m - value.sd, ymax = value.m + value.sd), fill = "grey",alpha = 0.4, color = NA) +
  geom_line(data = CB.anomaly %>% filter(lat == 0.5,
                                         lon == 24.5),
            aes(x = year, y = value, group = interaction(lat,lon)),size = 0.2, color = "blue") +
  geom_line(data = CB.anomaly.m,
            aes(x = year, y = value.m)) +
  geom_hline(yintercept = 0, color = "black",linetype = 2) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()




