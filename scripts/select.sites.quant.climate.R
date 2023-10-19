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

precip.source <- "ERA5"

#########################################################
# MAP

df.MAP <- readRDS(file = "./data/Precip/df.precip.RDS")
if (precip.source == "all"){
  df.MAP <- df.MAP %>% group_by(data,lat,lon) %>%
    group_by(lat,lon) %>%
    summarise(MAP.m = mean(MAP.m,na.rm = TRUE),
              .groups = "keep")%>%
    ungroup() %>%
    dplyr::select(lon,lat,MAP.m)
} else {

  df.MAP <- df.MAP %>%
    filter(data == precip.source)  %>%
    ungroup() %>%
    dplyr::select(-data) %>%
    dplyr::select(lon,lat,MAP.m)
}

dfr.MAP <- rasterFromXYZ(df.MAP)
dfr.MAP.rspld <- resample(dfr.MAP,Avi.AFR.crop)

#########################################################
# SW
df.sw <- readRDS("./data/SW/CERES/df.CERES.RDS") %>%
  group_by(lon,lat,year) %>%
  summarise(SW = mean(value),
            .groups = "keep") %>%
  group_by(lon,lat) %>%
  summarise(SW.m = mean(SW),
            .groups = "keep")

dfr.sw <- rasterFromXYZ(df.sw)
dfr.sw.rspld <- resample(dfr.sw,Avi.AFR.crop)

#########################################################
# MCWD

if (precip.source == "all"){
  df.MCWD <- readRDS("./data/MCWD/df.MCWD.RDS") %>%
    filter(!is.na(MCWD)) %>%
    group_by(data,lon,lat) %>%
    summarise(MCWD = unique(MCWD),
              .groups = "keep") %>%
    group_by(lon,lat) %>%
    summarise(MCWD = mean(MCWD),
              .groups = "keep")
} else{
  df.MCWD <- readRDS("./data/MCWD/df.MCWD.RDS") %>%
    filter(!is.na(MCWD)) %>%
    filter(data == precip.source) %>%
    group_by(lon,lat) %>%
    summarise(MCWD = unique(MCWD),
              .groups = "keep")
}

dfr.MCWD <- rasterFromXYZ(df.MCWD)
dfr.MCWD.rspld <- resample(dfr.MCWD,Avi.AFR.crop)

df <- as.data.frame(Avi.AFR.crop,xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         AGB = Avitabile_AGB_Map) %>%
  mutate(MAP = as.vector(dfr.MAP.rspld),
         SW = as.vector(dfr.sw.rspld),
         MCWD = as.vector(dfr.MCWD.rspld)) %>%
  filter(!is.na(AGB))

delta_quant = 0.1

MAP.quant <- as.numeric(quantile(df %>% filter(!is.na(MAP)) %>% pull(MAP),seq(delta_quant,1 - delta_quant,delta_quant)))
SW.quant <- as.numeric(quantile(df %>% filter(!is.na(SW)) %>% pull(SW),seq(delta_quant,1 - delta_quant,delta_quant)))
MCWD.quant <- as.numeric(quantile(df %>% filter(!is.na(MCWD)) %>% pull(MCWD),seq(delta_quant,1 - delta_quant,delta_quant)))

df <- df %>%
  mutate(MAP.cat = classify.quant(MAP,MAP.quant),
         SW.cat = classify.quant(SW,SW.quant),
         MCWD.cat = classify.quant(MCWD,MCWD.quant))


df.long <- df %>%
  dplyr::select(lon,lat,AGB,
                MAP.cat,SW.cat,MCWD.cat) %>%
  pivot_longer(cols = c(MAP.cat,SW.cat,MCWD.cat),
               names_to = "var",
               values_to = "value")

N = 5
df.long.med <- df.long %>%
  filter(!is.na(value)) %>%
  group_by(var,value) %>%
  mutate(AGB.med = median(AGB),
         AGB.diff = AGB - AGB.med,
         SSQ = AGB.diff**2) %>%
  slice_min(order_by = SSQ,n = N,with_ties = FALSE) %>%
  dplyr::select(lon,lat,AGB,var,value)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_tile(data = df.long ,
            aes(x = lon, y = lat,
                fill = AGB),
            alpha = 1) +
  geom_sf(data = world,fill = NA,color = "black") +
  geom_point(data = df.long.med,
             aes(x = lon, y = lat, color = var, shape = var),
             show.legend = FALSE) +
  theme_bw() +
  scale_x_continuous(limits = c(-10,45),expand = c(0,0)) +
  scale_y_continuous(limits = c(-16,10),expand = c(0,0)) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "", y = "", fill = "AGB \r\n (kgC/m²)") +
  guides(size = "none") +
  theme(text = element_text(size = 20))

df.long.sum <- df.long.med %>%
  group_by(var,value) %>%
  summarise(AGB.med = median(AGB,na.rm = TRUE),
            AGB.mean = mean(AGB,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.long.med) +
  geom_boxplot(aes(x = (value), y = AGB,group = value),
               outlier.shape = NA) +
  geom_line(data = df.long.sum,
            aes(x = value,y = AGB.med), color = "red") +
  facet_wrap(~var,scales = "free") +
  labs(x = "", y = "AGB (kgC/m²)") +
  theme_bw() +
  theme(text = element_text(size = 20))

select.sites <- df.long.med %>%
  ungroup() %>%
  dplyr::select(lon,lat) %>%
  distinct()

Nruns <- nrow(select.sites)

saveRDS(object = select.sites,
        file = "./outputs/select.sites.RDS")

system2("scp",paste("./outputs/select.sites.RDS",
                    "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

