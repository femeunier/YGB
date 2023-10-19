rm(list = ls())

library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(tidyr)
library(raster)
library(YGB)


##############################################################################

Avi.AFR <- stack("/home/femeunier/Documents/projects/SoilSensitivity/data/AGBavit_AFR.gri")

e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
Avi.AFR.crop <- crop(Avi.AFR, e)
Avi.AFR.crop.df <- as.data.frame(Avi.AFR.crop,
                                 xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         cVeg.m.m = Avitabile_AGB_Map) %>%
  mutate(cVeg.m.m = cVeg.m.m/20)

##############################################################################

# system2("rsync",paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/cVeg.CMIP6.scenarhistorical_yr.min_2000_yr.max_2020.RDS",
#                       "/home/femeunier/Documents/projects/YGB/outputs/"))

cVeg <- readRDS("/home/femeunier/Documents/projects/YGB/outputs/cVeg.CMIP6.scenarhistorical_yr.min_2000_yr.max_2020.RDS") %>%
  group_by(model,lat,lon) %>%
  summarise(cVeg.m = mean(cVeg),
            .groups = "keep") %>%
  group_by(model) %>%
  mutate(cVeg.m = case_when(model == "E3SM-1-1-ECA" ~ cVeg.m/1000,
                            TRUE ~ cVeg.m))


world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_raster(data = cVeg ,
              aes(x = lon, y = lat,fill = cVeg.m)) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  facet_wrap(~ model) +
  theme_bw()

###################################################################################################

models <- cVeg %>% pull(model) %>% unique()
df.rspld.all <- data.frame()
for (imodel in seq(1,length(models))){


  cdf <- cVeg %>% dplyr::filter(model == models[imodel])

  dfr.cdf <- tryCatch(rasterFromXYZ(cdf %>% ungroup() %>% dplyr::select(lon,lat,cVeg.m)),
                      error = function(e) NULL)
  if (is.null(dfr.cdf)) next()

  dfr.cdf.rspld <- resample(dfr.cdf,Avi.AFR.crop)
  dfr.cdf.rspld[is.na(Avi.AFR.crop)] <- NA

  cdf.rspld <- as.data.frame(dfr.cdf.rspld,xy = TRUE) %>%
    rename(lon = x,
           lat = y) %>% filter(!is.na(cVeg.m))

  df.rspld.all <- bind_rows(list(df.rspld.all,
                                 cdf.rspld %>% mutate(model = models[imodel])))


}

df.rspld.all.sum <- df.rspld.all %>%
  group_by(lat,lon) %>%
  summarise(cVeg.m.m = mean(cVeg.m,na.rm = TRUE),
            .groups = "keep")

data.vs.ESM <- bind_rows(list(Avi.AFR.crop.df %>% mutate(source = "Avitabile"),
                              df.rspld.all.sum %>% mutate(source = "ESM")))

ggplot(data = world) +
  geom_raster(data = data.vs.ESM ,
              aes(x = lon, y = lat,fill = cVeg.m.m)) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  facet_wrap(~ source) +
  theme_bw()

data.vs.ESM.wide <- data.vs.ESM %>%
  pivot_wider(names_from = source,
              values_from = cVeg.m.m)

ggplot(data = data.vs.ESM) +
  # geom_density(data = df.rspld.all,
  #              aes(x = cVeg.m, group = model), fill = NA, color = "grey", size = 0.4) +
  geom_density(aes(x = cVeg.m.m, color = source), alpha = 0.4, fill = NA) +
  # scale_y_log10() +
  theme_bw() +
  theme(legend.position = c(0.8,0.8))

ggplot(data = data.vs.ESM.wide,
       aes(x = ESM , y = Avitabile)) +
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  scale_x_log10() +
  scale_y_log10() +
  coord_fixed() +
  stat_smooth(method = "lm",color = "red",linetype = 2, size = 1,se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 1) +
  theme_bw() +
  theme(
    legend.position='none'
  )

summary(lm(data = data.vs.ESM.wide, formula = Avitabile ~ ESM))

###################################################################################################


climate <- readRDS("./outputs/CMIP.climate.historical.RDS")

delta_quant = 0.1

MAP.quant <- as.numeric(quantile(climate %>% filter(!is.na(MAP)) %>% pull(MAP),seq(delta_quant,1 - delta_quant,delta_quant)))
MCWD.quant <- as.numeric(quantile(climate %>% filter(!is.na(MCWD)) %>% pull(MCWD),seq(delta_quant,1 - delta_quant,delta_quant)))

cVeg.climate <- cVeg %>% left_join(climate,
                                   by = c("model","lon","lat")) %>%
  mutate(MAP.cat = classify.quant(MAP,MAP.quant),
         MCWD.cat = classify.quant(MCWD,MCWD.quant))

cVeg.climate.sum <- cVeg.climate %>%
  dplyr::select(model,cVeg.m,MAP.cat,MCWD.cat) %>%
  pivot_longer(cols = c(MAP.cat,MCWD.cat),
               names_to = "type",
               values_to = "value") %>%
  group_by(model,type,value) %>%
  summarise(cVeg.med = median(cVeg.m,na.rm = TRUE),
            .groups = "keep")

cVeg.climate.sum.Emean <- cVeg.climate.sum %>%
  group_by(type,value) %>%
  summarise(cVeg.med.m = mean(cVeg.med,na.rm = TRUE),
            .groups = "keep")


data <- readRDS("./outputs/data.quant.RDS") %>%
  rename(type = var)

data.sum <- data %>%
  group_by(type,value) %>%
  summarise(AGB.med = median(AGB,na.rm = TRUE),
            .groups = "keep")


ggplot() +
  geom_boxplot(data = data %>% filter(type != "SW.cat"),
               aes(x = (value), y = AGB/20,group = value),
               outlier.shape = NA) +
  # geom_line(aes(x = value, y = cVeg.med, group = model),
  #           color = "black",
  #           size = 0.2,
  #           show.legend = FALSE) +
  geom_line(data = cVeg.climate.sum,
            aes(x = value, y = cVeg.med, group = model),linetype = 1,
            color = "grey", size = 0.4,
            show.legend = FALSE) +
  geom_line(data = cVeg.climate.sum.Emean,
            aes(x = value, y = cVeg.med.m), color = "black",linetype = 2, size = 1) +
  facet_wrap(~ type) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "", y = "AGB (kgC/m²)") +
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank())


cVeg.climate.cat <- cVeg.climate %>%
  mutate(cat = factor(case_when(MCWD >= -300 ~ "Rainforest",
                         MAP >= 1000 ~ "Seasonal forest",
                         TRUE ~ "Savanna"),
                       levels = c("Savanna","Rainforest","Seasonal forest")))


cVeg.climate.cat.filtered <-
  cVeg.climate.cat %>%
  group_by(model) %>%
  mutate(N.cat = length(unique(cat))) %>%
  filter(N.cat == 3)

ggplot(data = cVeg.climate.cat.filtered) +
  geom_boxplot(aes(x = model, group = interaction(as.factor(cat),model),
                   fill = as.factor(cat),
                   y = cVeg.m),outlier.shape = NA) +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  # scale_y_log10() +
  # facet_wrap(~ model) +
  theme_bw() +
  theme(legend.position = c(0.8,0.8)) +
  labs(x = "",y = "AGB (kgC/m²)", fill = "")

