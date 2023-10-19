rm(list = ls())

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(matlab)
library(dplyr)
library(ggplot2)
library(raster)
library(YGB)
library(tidyr)

# system2("rsync",paste("-avz",
#                       "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/TrENDY.cVeg.RDS",
#                       "./outputs/"))

TrENDY.cVeg <- readRDS("./outputs/TrENDY.cVeg.RDS") %>%
  mutate(cVeg = case_when(cVeg < 0 ~ 0,
                          TRUE ~ cVeg))

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_raster(data = TrENDY.cVeg ,
            aes(x = lon, y = lat,fill = cVeg)) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  facet_wrap(~ model) +
  theme_bw()


ggplot() +
  geom_raster(data = TrENDY.cVeg ,
            aes(x = lon, y = lat,
                fill = cVeg),
            alpha = 1) +
  geom_sf(data = world,fill = NA,color = "black") +
  theme_bw() +
  scale_x_continuous(limits = c(-10,45),expand = c(0,0)) +
  scale_y_continuous(limits = c(-16,10),expand = c(0,0)) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "", y = "", fill = "AGB \r\n (kgC/m²)") +
  guides(size = "none") +
  facet_wrap(~ model,nrow = 3) +
  theme(text = element_text(size = 14),
        legend.position="bottom",
        legend.box = "horizontal")


models <- unique(TrENDY.cVeg$model)

#####################################################################################

df.MAP <- readRDS(file = "./data/Precip/df.precip.RDS") %>%
  dplyr::filter(data == "JRA")  %>%
  group_by(lat,lon) %>%
  summarise(MAP.m = sum(MAP.m,na.rm = TRUE),
            .groups = "keep") %>%
  ungroup() %>%
  # dplyr::select(-data) %>%
  dplyr::select(lon,lat,MAP.m)
dfr.MAP <- rasterFromXYZ(df.MAP %>% dplyr::select(lon,lat,MAP.m))

df.MCWD <- readRDS("./data/MCWD/df.MCWD.RDS") %>%
  filter(!is.na(MCWD)) %>%
  filter(data == "JRA") %>%
  group_by(lon,lat) %>%
  summarise(MCWD = unique(MCWD),
            .groups = "keep")
dfr.MCWD <- rasterFromXYZ(df.MCWD %>% dplyr::select(lon,lat,MCWD))

df.sw <- readRDS("./data/SW/JRA/df.SW.JRA.RDS")
lats <- unique(df.sw$lat)
lats.flipped <- matlab::fliplr(lats)
df.sw <- df.sw %>%
  mutate(lat = as.factor(lat))
levels(df.sw$lat) <- as.character(lats.flipped)
df.sw$lat <- as.numeric(as.vector(df.sw$lat))
df.sw <- df.sw %>%
  mutate(SW = SW/3600/6) %>%
  group_by(lon,lat,year) %>%
  summarise(SW = mean(SW),
            .groups = "keep") %>%
  group_by(lon,lat) %>%
  summarise(SW.m = mean(SW),
            .groups = "keep")
dfr.SW <- rasterFromXYZ(df.sw %>% dplyr::select(lon,lat,SW.m))

###########################################################################

delta_quant = 0.1

MAP.quant <- as.numeric(quantile(df.MAP %>% filter(!is.na(MAP.m)) %>% pull(MAP.m),seq(delta_quant,1 - delta_quant,delta_quant)))
SW.quant <- as.numeric(quantile(df.sw %>% filter(!is.na(SW.m)) %>% pull(SW.m),seq(delta_quant,1 - delta_quant,delta_quant)))
MCWD.quant <- as.numeric(quantile(df.MCWD %>% filter(!is.na(MCWD)) %>% pull(MCWD),seq(delta_quant,1 - delta_quant,delta_quant)))

###########################################################################

df.all <- data.frame()

for (imodel in seq(1,length(models))){
  cdf <- TrENDY.cVeg %>% filter(model == models[imodel])

  craster <- cdf %>% dplyr::select(lon,lat,cVeg)
  dfr.cVeg <- rasterFromXYZ(craster)

  dfr.sw.rspld <- resample(dfr.SW,dfr.cVeg)
  dfr.MAP.rspld <- resample(dfr.MAP,dfr.cVeg)
  dfr.MCWD.rspld <- resample(dfr.MCWD,dfr.cVeg)

  cdf.rspld <- as.data.frame(dfr.cVeg,xy = TRUE) %>%
    rename(lon = x,
           lat = y) %>%
    mutate(MAP = as.vector(dfr.MAP.rspld),
           SW = as.vector(dfr.sw.rspld),
           MCWD = as.vector(dfr.MCWD.rspld))

  df <- cdf.rspld %>%
    mutate(MAP.cat = classify.quant(MAP,MAP.quant),
           SW.cat = classify.quant(SW,SW.quant),
           MCWD.cat = classify.quant(MCWD,MCWD.quant))

  df.all <- bind_rows(list(df.all,
                           df %>% mutate(model = models[imodel])))

}


df.all.sum <- df.all %>%
  dplyr::select(model,cVeg,MAP.cat,SW.cat,MCWD.cat) %>%
  pivot_longer(cols = c(MAP.cat,SW.cat,MCWD.cat),
               names_to = "type",
               values_to = "value") %>%
  group_by(model,type,value) %>%
  summarise(cVeg.med = median(cVeg,na.rm = TRUE),
            .groups = "keep")

data <- readRDS("./outputs/data.quant.RDS") %>%
  rename(type = var)


df.all.sum.sum <- df.all.sum %>%
  group_by(type,value) %>%
  summarise(cVeg.med.m = median(cVeg.med),
            cVeg.med.min = quantile(cVeg.med,0.025,na.rm = TRUE),
            cVeg.med.max = quantile(cVeg.med,0.975,na.rm = TRUE),
            cVeg.med.sd = sd(cVeg.med),
            .groups = "keep")

data.sum <- data %>%
  group_by(type,value) %>%
  summarise(AGB.med = median(AGB,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.all.sum) +
  geom_boxplot(data = data,
               aes(x = (value), y = AGB/20,group = value),
               outlier.shape = NA) +
  # geom_line(aes(x = value, y = cVeg.med, group = model),
  #           color = "black",
  #           size = 0.2,
  #           show.legend = FALSE) +
  geom_line(data = data.sum,
            aes(x = value, y = AGB.med/20), color = "red",linetype = 1) +
  geom_line(data = df.all.sum.sum,
            aes(x = value, y = cVeg.med.m), color = "red",linetype = 2) +
  # geom_ribbon(data = df.all.sum.sum,
  #           aes(x = value, y = cVeg.med.m,
  #               ymin = cVeg.med.min, ymax = cVeg.med.max), fill = "red", alpha = 0.4, color = NA) +
  facet_wrap(~ type) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "", y = "AGB (kgC/m²)") +
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank())


data2plot.model <- df.all %>%
  dplyr::select(cVeg,MAP.cat,SW.cat,MCWD.cat) %>%
  pivot_longer(cols = c(MAP.cat,SW.cat,MCWD.cat),
               names_to = "type",
               values_to = "value") %>%
  rename(AGB = cVeg) %>%
  mutate(var = "model")

data2plot <- bind_rows(list(
  data %>% dplyr::select(type,value,AGB) %>%
    mutate(var = "data") %>%
    mutate(AGB = AGB/20),
  data2plot.model
))

delta = 0.25
data2plot.sum <- data2plot %>%
  group_by(type,var,value) %>%
  summarise(AGB.med = median(AGB,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(value = case_when(var == "data" ~ value - delta,
                           var == "model" ~ value + delta))

ggplot(data = data2plot) +
  # geom_line(data = df.all.sum,
  #           aes(x = value, y = cVeg.med,
  #               group = model),
  #           color = "darkgrey",
  #           size = 0.2) +
  geom_boxplot(aes(x = (value),y = AGB,
                   fill = var,
                   group = interaction(var,value)),
               outlier.shape = NA,
               alpha = 0.25) +
  geom_line(data = data2plot.sum,
            aes(x = value,y = AGB.med,
                color = var),
            show.legend = FALSE,
            alpha = 1) +
  facet_grid(~type, scales = "free_x") +
  scale_x_continuous(breaks = seq(1,10)) +
  labs(x = "", y = "AGB \r\n (kgC/m²)", fill = "") +
  theme_bw() +
  theme(legend.position = c(0.08,0.85),
        text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank())
