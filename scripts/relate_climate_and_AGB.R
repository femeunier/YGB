rm(list = ls())

library(raster)
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)
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

precip.source <- "JRA"
months <- 1:12

plot(Avi.AFR/20)

#########################################################
# MAP

df.MAP.month <- data.frame()

for (imonth in seq(1,12)){
  cdf.MAP <- readRDS(file = paste0("./data/Precip/df.precip",imonth,".RDS"))
  df.MAP.month <- bind_rows(list(df.MAP.month,
                                 cdf.MAP %>% mutate(month = imonth)))
}

df.MAP.month <- df.MAP.month %>% filter(month %in% months)

# df.MAP
# df.MAP <- readRDS(file = "./data/Precip/df.precip.RDS")
if (precip.source == "all"){
  df.MAP <- df.MAP.month %>%
    group_by(data,lat,lon) %>%
    summarise(MAP.m = sum(MAP.m,na.rm = TRUE),
              .groups = "keep") %>%
    group_by(lat,lon) %>%
    summarise(MAP.m = mean(MAP.m,na.rm = TRUE),
              .groups = "keep")%>%
    ungroup() %>%
    dplyr::select(lon,lat,MAP.m)
} else {

  df.MAP <- df.MAP.month %>%
    filter(data == precip.source)  %>%
    group_by(lat,lon) %>%
    summarise(MAP.m = sum(MAP.m,na.rm = TRUE),
              .groups = "keep") %>%
    ungroup() %>%
    # dplyr::select(-data) %>%
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

MAP.quant.all <- c(df %>% filter(!is.na(MAP)) %>% pull(MAP) %>% min(),
                   MAP.quant) +
  diff(c(df %>% filter(!is.na(MAP)) %>% pull(MAP) %>% min(),
         MAP.quant,
         df %>% filter(!is.na(MAP)) %>% pull(MAP) %>% max()))/2


SW.quant.all <- c(df %>% filter(!is.na(SW)) %>% pull(SW) %>% min(),
                  SW.quant) +
  diff(c(df %>% filter(!is.na(SW)) %>% pull(SW) %>% min(),
         SW.quant,
         df %>% filter(!is.na(SW)) %>% pull(SW) %>% max()))/2

MCWD.quant.all <- c(df %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% min(),
                    MCWD.quant) +
  diff(c(df %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% min(),
         MCWD.quant,
         df %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% max()))/2
# ggsave(last_plot(),filename = "./Figures/AGB.climate.png",dpi = 300, width = 30, height = 8,unit = "cm")

world <- ne_countries(scale = "medium", returnclass = "sf")

#########################################################################
# Link climate vs LC

df.LC <- readRDS("./data/LC.RDS") %>%
  dplyr::filter(!is.na(LC)) %>%
  dplyr::select(lon,lat,LC) %>%
  mutate(LC = as.integer(LC))

dfr.LC <- rasterFromXYZ(df.LC)
dfr.LC.rspld <- resample(dfr.LC,Avi.AFR.crop)

df.all <- as.data.frame(Avi.AFR.crop,xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         AGB = Avitabile_AGB_Map) %>%
  mutate(MAP = as.vector(dfr.MAP.rspld),
         SW = as.vector(dfr.sw.rspld),
         MCWD = as.vector(dfr.MCWD.rspld),
         LC = round(as.vector(dfr.LC.rspld))) %>%
  filter(!is.na(LC),
         LC < 4)

df.all <- df.all %>%
  mutate(MAP.cat = classify.quant(MAP,MAP.quant),
         SW.cat = classify.quant(SW,SW.quant),
         MCWD.cat = classify.quant(MCWD,MCWD.quant)) %>%
  mutate(MAP.cat.abs = MAP.quant.all[MAP.cat],
         SW.cat.abs = SW.quant.all[SW.cat],
         MCWD.cat.abs = MCWD.quant.all[MCWD.cat])

df.all.long <- df.all %>%
  dplyr::select(lon,lat,AGB,LC,
                MAP.cat,SW.cat,MCWD.cat) %>%
  pivot_longer(cols = c(MAP.cat,SW.cat,MCWD.cat),
               names_to = "var",
               values_to = "value")

df.all.long.sum <- df.all.long %>%
  group_by(var,LC,value) %>%
  summarise(AGB.med = median(AGB,na.rm = TRUE),
            AGB.mean = mean(AGB,na.rm = TRUE),
            .groups = "keep")

df.long.sum <- df.all.long %>%
  group_by(var,value) %>%
  summarise(AGB.med = median(AGB,na.rm = TRUE),
            AGB.mean = mean(AGB,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.all.long) +
  geom_boxplot(aes(x = (value), y = AGB/20,group = interaction(value,LC), fill = as.factor(LC)),
               position=position_dodge(1),
               outlier.shape = NA) +
  geom_line(data = df.all.long.sum,
            aes(x = value,y = AGB.med/20, color = as.factor(LC))) +
  facet_wrap(~var,scales = "free") +
  labs(x = "", y = "AGB (kgC/m²)") +
  scale_x_continuous(breaks = 1:10) +
  theme_bw() +
  theme(text = element_text(size = 20))


ggplot(data = df.all.long) +
  geom_boxplot(aes(x = (value), y = AGB/20,group = interaction(value)),
               position=position_dodge(1),
               outlier.shape = NA) +
  geom_line(data = df.long.sum,
            aes(x = value,y = AGB.med/20),
            color = "red") +
  facet_wrap(~var,scales = "free") +
  labs(x = "", y = "AGB (kgC/m²)") +
  scale_x_continuous(breaks = 1:10) +
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank())


df.all.long.prop <- df.all.long %>% group_by(var,LC,value) %>%
  summarise(N = length(value),
            .groups = "keep") %>%
  group_by(var,LC) %>%
  mutate(prop = 100*N/sum(N))

ggplot(data = df.all.long.prop) +
  geom_line(aes(x = value, y = prop, color = as.factor(LC))) +
  facet_wrap(~var) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "", y = "Proportion (%)") +
  scale_color_manual(values = c("#c49402","#005401","#448704")) +
  theme_bw() +
  guides(color = "none")

########################################################################################################


trans.matrix <- function(X, prob=T){
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}


param1 = -300; param1bis = param1; param2 = 1000

df.all.class <- df.all %>% mutate(LC.predicted = case_when(MCWD > param1 ~ 2,
                                                           MCWD <= param1 & MAP >= param2 ~ 3,
                                                           MAP < param2 & MCWD < param1bis ~ 1))

saveRDS(df.all.class,
        "./outputs/class.cat.current.RDS")

CMIP.model <- readRDS("/home/femeunier/Documents/projects/YGB/outputs/CMIP.climate.historical.RDS") %>%
  group_by(model) %>%
  summarise(MAP.m = mean(MAP,na.rm = TRUE),
            MCWD.m = mean(MCWD,na.rm = TRUE),
            .groups = "keep")
CMIP.model.sum <- CMIP.model %>%
  ungroup() %>%
  summarise(MAP.m = mean(MAP.m),
            MCWD.m = mean(MCWD.m))


####################################################################################################################
# CRU

CRU.MAP <- readRDS(file = "./data/Precip/df.precip.RDS") %>%
  filter(data == "CRU")  %>%
  ungroup() %>%
  dplyr::select(-data) %>%
  dplyr::select(lon,lat,MAP.m)

dfr.MAP.CRU <- rasterFromXYZ(CRU.MAP)
dfr.MAP.CRU.rspld <- resample(dfr.MAP.CRU,Avi.AFR.crop)

df.CRU.MCWD <- readRDS("./data/MCWD/df.MCWD.RDS") %>%
  filter(!is.na(MCWD)) %>%
  filter(data == precip.source) %>%
  group_by(lon,lat) %>%
  summarise(MCWD = unique(MCWD),
            .groups = "keep")

dfr.CRU.MCWD <- rasterFromXYZ(df.CRU.MCWD)
dfr.CRU.MCWD.rspld <- resample(dfr.CRU.MCWD,Avi.AFR.crop)

df.CRU <- as.data.frame(Avi.AFR.crop,xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         AGB = Avitabile_AGB_Map) %>%
  mutate(MAP = as.vector(dfr.MAP.CRU.rspld),
         MCWD = as.vector(dfr.CRU.MCWD.rspld))

df.CRU.sum <- df.CRU %>% ungroup() %>%
  summarise(MAP.m = mean(MAP, na.rm = TRUE),
            MCWD.m = mean(MCWD, na.rm = TRUE),
            .groups = "keep")

####################################################################################################################


df.all2plot <- df.all %>%
  mutate(LC.new = case_when(LC == 1 ~ "Savanna",
                            LC == 2 ~ "Rainforest",
                            LC == 3 ~ "Seasonal forest")) %>%
  mutate(LC.new = factor(LC.new,levels = c("Rainforest","Seasonal forest","Savanna")))


saveRDS(df.all2plot,
        "./outputs/Climate.vs.cVeg_data.RDS")

ggplotLandVSClim <- ggplot(data = df.all2plot %>% filter(!is.na(LC))) +
  geom_point(aes(x = MCWD,y = MAP,
                 color = as.factor(LC.new)), show.legend = TRUE, alpha = 0.3) +
  scale_shape_manual(values = seq(1,nrow(CMIP.model))) +
  geom_hline(yintercept = param2, color = "black",linetype = 2) +
  geom_vline(xintercept = c(param1,param1bis), color = "black",linetype = 2) +
  geom_point(data = CMIP.model,
             aes(x = MCWD.m, y = MAP.m), size = 1, shape = 4, show.legend = FALSE) +
  # geom_point(data = CMIP.model.sum,
  #            aes(x = MCWD.m, y = MAP.m), size = 5, shape = 4, show.legend = FALSE, color = "black") +
  geom_point(data = df.CRU.sum,
             aes(x = MCWD.m, y = MAP.m), size = 5, shape = 20, show.legend = FALSE, color = "black") +
  scale_color_manual(values = c("#005401","#448704","#c49402")) +
  labs(x = "MCWD (mm)", y = "MAP (mm)", color = "") +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  theme_bw() +
  theme(text = element_text(size = 22),
        legend.position = c(0.2,0.8))

ggplotMAP <- ggplot(data = df.all2plot %>% filter(!is.na(LC))) +
  geom_boxplot(aes(x = as.factor(LC.new), y = MAP, fill = as.factor(LC.new)),outlier.shape = NA) +
  scale_fill_manual(values = c("#005401","#448704","#c49402")) +
  labs(x = "") +
  geom_hline(yintercept = param2, color = "black",linetype = 2) +
  theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  guides(fill = "none")

ggplotMCWD <- ggplot(data = df.all2plot %>% filter(!is.na(LC))) +
  geom_boxplot(aes(x = as.factor(LC.new), y = MCWD, fill = as.factor(LC.new)),outlier.shape = NA) +
  scale_fill_manual(values = c("#005401","#448704","#c49402")) +
  labs(x = "") +
  theme_bw() +
  geom_hline(yintercept = c(param1,param1bis), color = "black",linetype = 2) +
  guides(fill = "none") +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  rotate()

yplot <- ggplotMAP + clean_theme() + rremove("legend")
xplot <- ggplotMCWD + clean_theme() + rremove("legend")

plot_grid(xplot, NULL, ggplotLandVSClim, yplot, ncol = 2, align = "hv",
          rel_widths = c(2, 1), rel_heights = c(1, 2))

# ggsave(last_plot(),filename = "./Figures/AGB.climate_class.png",dpi = 300, width = 15, height = 12,unit = "cm")

ggplot(data = df.all %>% filter(!is.na(LC))) +
  geom_point(aes(x = MCWD,y = MAP,
                 shape = as.factor(LC),
                 color = as.factor(LC))) +
  scale_shape_manual(values = c(0,1,2)) +
  scale_color_manual(values = c("#c49402","#005401","#448704")) +
  geom_hline(yintercept = param2, color = "black") +
  geom_vline(xintercept = c(param1,param1bis), color = "black",linetype = 2) +
  facet_wrap(~LC) +
  theme_bw()

df.all.class2plot <- df.all.class %>%
  filter(!is.na(LC)) %>%
  mutate(Land.Cover = case_when(LC == 1 ~ "Savanna",
                                LC == 2 ~ "Rainforest",
                                LC == 3 ~ "Seasonal forest")) %>%
  mutate(Land.Cover = factor(Land.Cover,
                             levels = c("Rainforest",
                                        "Seasonal forest",
                                        "Savanna")))


df.all.class2plot %>% group_by(Land.Cover) %>%
  summarise(m = median((AGB/20)))

ggplot(data = df.all.class2plot) +
  geom_boxplot(aes(x = Land.Cover,y = (AGB/20),
                   fill = as.factor(Land.Cover)),
               outlier.shape = NA) +
  scale_fill_manual(values = c("#005401","#448704","#c49402")) +
  scale_y_log10() +
  labs(x = "", y = "AGB (kgC/m²)") +
  theme_bw() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# ggsave(last_plot(),filename = "./Figures/Boxplot.AGB.png",
#        dpi = 300, width = 10, height = 8,unit = "cm")

ggplot(data = df.all.class %>% filter(!is.na(LC))) +
  geom_density(aes(x = AGB/20)) +
  theme_bw()

ggplot() +
  geom_tile(data = df.all.class ,
            aes(x = lon, y = lat,
                fill = AGB/20),
            alpha = 1) +
  geom_sf(data = world,fill = NA,color = "black") +
  theme_bw() +
  scale_x_continuous(limits = c(-10,45),expand = c(0,0)) +
  scale_y_continuous(limits = c(-16,10),expand = c(0,0)) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "", y = "", fill = "AGB \r\n (kgC/m²)") +
  guides(size = "none") +
  theme(text = element_text(size = 20))

# ggsave(last_plot(),filename = "./Figures/AGB.map.Avitable.png",dpi = 300, width = 20, height = 10,unit = "cm")


###############################################################################

world <- ne_countries(scale = "medium", returnclass = "sf")

df.all.class.long <- df.all.class %>%
  filter(!is.na(LC)) %>%
  dplyr::select(lon,lat,LC,LC.predicted) %>%
  pivot_longer(cols = c(LC,LC.predicted),
               names_to = "class",
               values_to = "LC")

ggplot(data = world) +
  geom_tile(data = df.all.class.long %>% filter(!is.na(LC)),
            aes(x = lon, y = lat,fill = as.factor(LC)),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  labs(x = "",y = "") +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  facet_wrap(~ class) +
  theme_bw() +
  guides(fill = "none")

# df.all.class.long %>% filter(class == "LC.predicted",
#                              is.na(LC))

df.all.classif.OP <- df.all.class %>%
  mutate(classification = case_when(LC == LC.predicted ~ TRUE,
                                    TRUE ~ FALSE))

ggplot(data = world) +
  geom_tile(data = df.all.classif.OP,
            aes(x = lon, y = lat,fill = classification),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  labs(x = "",y = "") +
  scale_fill_manual(values = c("red","darkgreen")) +
  theme_bw()

###################################################################################

pred.mat <- trans.matrix(as.matrix(df.all.class %>% dplyr::select(LC,LC.predicted)),prob = FALSE)
sum(diag(pred.mat))/nrow(df.all %>% filter(!is.na(LC)))

summary(aov(data = df.all.class %>% filter(!is.na(LC)),
            formula = log10(AGB) ~ LC))

###################################################################################

pc.df <- df.all %>% dplyr::select(AGB,MAP,SW,MCWD) %>%
  filter(!is.na(AGB),!is.na(MAP),!is.na(SW),!is.na(MCWD))

summary(lm(data = pc.df,
           formula = AGB ~ MAP + SW + MCWD))

# saveRDS(df.long,file = "./outputs/Climate_AGB_data.RDS")

# summary(aov(data = df.all.class %>% filter(!is.na(LC)),
#             formula = SW ~ MCWD))
#
# ggplot(data = df.all %>% filter(!is.na(LC)),
#        aes(x = MCWD,y = SW)) +
#   geom_point() +
#   # geom_hline(yintercept = param2, color = "black") +
#   # geom_vline(xintercept = c(param1,param1bis), color = "black",linetype = 2) +
#   stat_smooth(method = "lm",se = TRUE) +
#   theme_bw()
#
#
# param1 = -250; param1bis = param1; param2 = 250
#
# df.all.class <- df.all %>% mutate(LC.predicted = case_when(MCWD > param1 ~ 2,
#                                                            MCWD <= param1 & SW <= param2 ~ 3,
#                                                            SW > param2 & MCWD < param1bis ~ 1))
#
# ggplot(data = df.all %>% filter(!is.na(LC))) +
#   geom_point(aes(x = MCWD,y = SW,
#                  shape = as.factor(LC),
#                  color = as.factor(LC))) +
#   scale_shape_manual(values = c(0,1,2)) +
#   geom_hline(yintercept = param2, color = "black") +
#   geom_vline(xintercept = c(param1,param1bis), color = "black",linetype = 2) +
#   theme_bw()
#
# pred.mat <- trans.matrix(as.matrix(df.all.class %>% dplyr::select(LC,LC.predicted)),prob = FALSE)
# sum(diag(pred.mat))/nrow(df.all %>% filter(!is.na(LC)))



