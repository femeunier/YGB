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
library(YGB)

world <- ne_countries(scale = "medium", returnclass = "sf")
mask <- readRDS("./data/LandSeaMask.RDS")

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_CB_Efiles.RDS",
                      "./outputs/"))

df_CB <- readRDS(file.path("./outputs/","df_CB_Efiles.RDS"))

df <- df_CB %>% left_join(mask,
                          by = c("lat","lon")) %>%
  filter(mask == 1)

df.final <- df %>%
  group_by(lat,lon) %>%
  filter(yr == max(yr)) %>%
  mutate(AGB.bnd = case_when(AGB >= 30 ~ 30,
                             TRUE ~ AGB))

df.final %>% nrow()

summary(unique(df.final$AGB))
hist(df.final$AGB)

ggplot(data = world) +
  geom_tile(data = df.final %>% filter(month == 1),
            aes(x = lon, y = lat,fill = AGB),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent")+
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = world) +
  geom_tile(data = df.final %>% filter(month == 1),
            aes(x = lon, y = lat,fill = LAI),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent")+
  labs(x = "",y = "") +
  theme_bw()


ggplot(data = df %>% filter(lat == -10)) +
  geom_line(aes(x = month, y = gpp, group = interaction(lat,lon))) +
  theme_bw()


df.sum <- df %>% filter(yr == 2000) %>%
  mutate(ET = 3.3/86400,
         diff = (precip - ET)*86400*30) %>%
  group_by(lat,lon) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD)) %>%
  summarise(AGB = mean(AGB),
            sw = mean(sw),
            MAP = sum(precip*86400*30),
            MCWD = unique(MCWD),
            .groups = "keep")

ggplot(data = world) +
  geom_tile(data = df.sum,
            aes(x = lon, y = lat,fill = sw),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent")+
  labs(x = "",y = "") +
  theme_bw()

delta.quant = 0.1
MAP.quant <- as.numeric(quantile(df.sum$MAP,seq(delta.quant,1-delta.quant,delta.quant)))
MCWD.quant <- as.numeric(quantile(df.sum$MCWD,seq(delta.quant,1-delta.quant,delta.quant)))
SW.quant <- as.numeric(quantile(df.sum$sw,seq(delta.quant,1-delta.quant,delta.quant)))


MAP.quant.all <- c(df.sum %>% filter(!is.na(MAP)) %>% pull(MAP) %>% min(),
                   MAP.quant) +
  diff(c(df.sum %>% filter(!is.na(MAP)) %>% pull(MAP) %>% min(),
         MAP.quant,
         df.sum %>% filter(!is.na(MAP)) %>% pull(MAP) %>% max()))/2

SW.quant.all <- c(df.sum %>% filter(!is.na(sw)) %>% pull(sw) %>% min(),
                  SW.quant) +
  diff(c(df.sum %>% filter(!is.na(sw)) %>% pull(sw) %>% min(),
         SW.quant,
         df.sum %>% filter(!is.na(sw)) %>% pull(sw) %>% max()))/2

MCWD.quant.all <- c(df.sum %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% min(),
                    MCWD.quant) +
  diff(c(df.sum %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% min(),
         MCWD.quant,
         df.sum %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% max()))/2

df.sum <- df.sum %>%
  mutate(MAP.cat = classify.quant(MAP,MAP.quant),
         MCWD.cat = classify.quant(MCWD,MCWD.quant),
         SW.cat = classify.quant(sw,SW.quant)) %>%
  mutate(MAP.cat.abs = MAP.quant.all[MAP.cat],
         SW.cat.abs = SW.quant.all[SW.cat],
         MCWD.cat.abs = MCWD.quant.all[MCWD.cat])

df.long <- df.sum %>%
  dplyr::select(lon,lat,AGB,
                MAP.cat,SW.cat,MCWD.cat) %>%
  pivot_longer(cols = c(MAP.cat,SW.cat,MCWD.cat),
               names_to = "var",
               values_to = "value")

ggplot(data = df.long) +
  geom_boxplot(aes(x = value,y = AGB*20, group = value),outlier.shape = NA) +
  facet_wrap(~var, scales = "free") +
  theme_bw()

# lon   lat   AGB var          value
# <dbl> <dbl> <dbl> <chr>        <dbl>
#   1 -9.75  9.75 14.8  MAP.cat.abs  1606.

saveRDS(df.long,file = "./outputs/Climate_AGB_model.RDS")
