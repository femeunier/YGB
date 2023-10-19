rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(YGB)
library(raster)

##############################################################################

Avi.AFR <- stack("/home/femeunier/Documents/projects/SoilSensitivity/data/AGBavit_AFR.gri")

e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
Avi.AFR.crop <- crop(Avi.AFR, e)

##############################################################################

file <- "pr.CMIP6.scenarssp585_yr.min_2090_yr.max_2100.RDS"

# system2("rsync",paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/pr.CMIP6.scenarhistorical_yr.min_2000_yr.max_2020.RDS",
#                       "/home/femeunier/Documents/projects/CongoAS/outputs/"))
#
# system2("rsync",paste(paste0("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/",file),
#                       "/home/femeunier/Documents/projects/CongoAS/outputs/"))

CMIP.model_init <- readRDS(paste0("/home/femeunier/Documents/projects/YGB/outputs/pr.CMIP6.scenarhistorical_yr.min_2000_yr.max_2020.RDS"))  %>%
  dplyr::filter(lat <= 10, lat >= -15, lon <= 45, lon >= -10) %>%
  group_by(model,yr,lat,lon) %>%
  mutate(MAP = mean(pr)*86400*365,
         N = length(pr)) %>%
  filter(N == 12) %>%
  mutate(month = 1:12)

CMIP.model_init.MAP <- CMIP.model_init %>%
  mutate(Pmm=pr*86400*30) %>%
  filter(!is.na(Pmm)) %>%
  group_by(model,yr,lat,lon,month) %>%
  dplyr::select(model,lon,lat,Pmm,yr,month) %>%
  group_by(model,month,lon,lat) %>%
  summarise(Pmm = mean(Pmm),
            .groups = "keep") %>%
  group_by(model,lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = 3.33,
         Etot = E*Ndays) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD))

CMIP.model_init.MAP.MCWD <- CMIP.model_init.MAP %>%
  group_by(model,lon,lat) %>%
  summarise(MAP = sum(Pmm),
            MCWD = unique(MCWD),
            .groups = "keep")

saveRDS(CMIP.model_init.MAP.MCWD,"./outputs/CMIP.climate.historical.RDS")

CMIP.model_init.MAP.MCWD.model <- CMIP.model_init.MAP.MCWD %>%
  group_by(model) %>%
  summarise(MAP.m = mean(MAP),
            MCWD.m = mean(MCWD),
            MCWD.se = sd(MCWD)/(1.96*sqrt(length(MCWD))),
            MAP.se = sd(MAP)/(1.96*sqrt(length(MAP))),
            .groups = "keep")


###############################################################################################################
CMIP.model_scenar <- readRDS(paste0("/home/femeunier/Documents/projects/YGB/outputs/",
                                    file)) %>%
  dplyr::filter(lat <= 10, lat >= -15, lon <= 45, lon >= -10) %>%
  group_by(model,yr,lat,lon) %>%
  mutate(MAP = mean(pr)*86400*365,
         N = length(pr)) %>%
  filter(N == 12) %>%
  mutate(month = 1:12)

CMIP.model_scenar.MAP <- CMIP.model_scenar %>%
  mutate(Pmm=pr*86400*30) %>%
  filter(!is.na(Pmm)) %>%
  group_by(model,yr,lat,lon,month) %>%
  dplyr::select(model,lon,lat,Pmm,yr,month) %>%
  group_by(model,month,lon,lat) %>%
  summarise(Pmm = mean(Pmm),
            .groups = "keep") %>%
  group_by(model,lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = 3.33,
         Etot = E*Ndays) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD))

CMIP.model_scenar.MAP.MCWD <- CMIP.model_scenar.MAP %>%
  group_by(model,lon,lat) %>%
  summarise(MAP = sum(Pmm),
            MCWD = unique(MCWD),
            .groups = "keep")

CMIP.model_scenar.MAP.MCWD.model <- CMIP.model_scenar.MAP.MCWD %>%
  group_by(model) %>%
  summarise(MAP.m = mean(MAP),
            MCWD.m = mean(MCWD),
            MCWD.se = sd(MCWD)/(1.96*sqrt(length(MCWD))),
            MAP.se = sd(MAP)/(1.96*sqrt(length(MAP))),
            .groups = "keep")

############################################################################################################

transition <- bind_rows(list(CMIP.model_init.MAP.MCWD.model %>% mutate(scenario = "historical"),
                             CMIP.model_scenar.MAP.MCWD.model %>% mutate(scenario = "scenar"))) %>%
  pivot_wider(names_from = scenario,
              values_from = c(MCWD.m,MAP.m,MCWD.se,MAP.se))

transition %>% filter(!is.na(MCWD.m_historical),
                      !is.na(MAP.m_historical),
                      !is.na(MCWD.m_scenar),
                      !is.na(MAP.m_scenar))


#############################################################################################################

df.CRU.MCWD <- readRDS("./data/MCWD/df.MCWD.RDS") %>%
  dplyr::filter(!is.na(MCWD)) %>%
  dplyr::filter(data == "CRU") %>%
  group_by(lon,lat) %>%
  summarise(MCWD = unique(MCWD),
            .groups = "keep")

dfr.CRU.MCWD <- rasterFromXYZ(df.CRU.MCWD)
dfr.CRU.MCWD.rspld <- resample(dfr.CRU.MCWD,Avi.AFR.crop)


CRU.MAP <- readRDS(file = "./data/Precip/df.precip.RDS") %>%
  filter(data == "CRU")  %>%
  ungroup() %>%
  dplyr::select(-data) %>%
  dplyr::select(lon,lat,MAP.m)

dfr.MAP.CRU <- rasterFromXYZ(CRU.MAP)
dfr.MAP.CRU.rspld <- resample(dfr.MAP.CRU,Avi.AFR.crop)

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

#############################################################################################################

data.df <- readRDS("./outputs/Climate.vs.cVeg_data.RDS")

ggplot(data = transition,
       aes(x = MCWD.m_historical, y = MAP.m_historical)) +

  geom_point(data = data.df %>% filter(!is.na(LC)),
             aes(x = MCWD,y = MAP,
                 color = as.factor(LC.new)), show.legend = FALSE, alpha = 0.3) +
  geom_point() +
  geom_segment(aes(xend = MCWD.m_scenar, yend = MAP.m_scenar),
               arrow = arrow(length = unit(0.2, "cm"))) +
  scale_x_continuous(limits = c(-1000,0)) +
  scale_y_continuous(limits = c(0,3000)) +
  scale_color_manual(values = c("#005401","#448704","#c49402")) +
  geom_hline(yintercept = 1000, color = "black",linetype = 2) +
  geom_vline(xintercept = -300, color = "black",linetype = 2) +
  labs(x = "MCWD (mm)", y = "MAP (mm)", color = "") +
  geom_point(data = df.CRU.sum,
             aes(x = MCWD.m, y = MAP.m), size = 5, shape = 20, show.legend = FALSE, color = "black") +
  theme_bw() +
  theme(text = element_text(size = 22),
        legend.position = c(0.2,0.8))


#############################################################################################################
# Forest types changes

CMIP.model_init.MAP.MCWD.cat <- CMIP.model_init.MAP.MCWD %>%
  mutate(cat = case_when(MCWD >= -300 ~ "Rainforest",
                         MAP >= 1000 ~ "Seasonal forest",
                         TRUE ~ "Savanna"),
         cat.num = case_when(MCWD >= -300 ~ 2,
                             MAP >= 1000 ~ 3,
                             TRUE ~ 1))

CMIP.model_init.MAP.MCWD.cat.sum <- CMIP.model_init.MAP.MCWD.cat %>%
  group_by(model,cat) %>%
  summarise(N = length(MAP),
            .groups = "keep")

CMIP.model_scenar.MAP.MCWD.cat <- CMIP.model_scenar.MAP.MCWD %>%
  mutate(cat = case_when(MCWD >= -300 ~ "Rainforest",
                         MAP >= 1000 ~ "Seasonal forest",
                         TRUE ~ "Savanna"),
         cat.num = case_when(MCWD >= -300 ~ 2,
                         MAP >= 1000 ~ 3,
                         TRUE ~ 1))

CMIP.model_scenar.MAP.MCWD.cat.sum <- CMIP.model_scenar.MAP.MCWD.cat %>%
  group_by(model,cat) %>%
  summarise(N = length(MAP),
            .groups = "keep")

# Map

Map_CMIP_change_cat <- bind_rows(list(CMIP.model_init.MAP.MCWD.cat %>% mutate(type = "Historical"),
                                      CMIP.model_scenar.MAP.MCWD.cat %>% mutate(type = "Scenario")))

# Resample

models <- unique(Map_CMIP_change_cat$model)
types <- unique(Map_CMIP_change_cat$type)

df.rspld.all <- data.frame()
for (imodel in seq(1,length(models))){

  ctypes <- Map_CMIP_change_cat %>% dplyr::filter(model == models[imodel]) %>% pull(type) %>% unique()

  if (length(ctypes) < 2) next()

  for (itype in seq(1,length(types))){

    cdf <- Map_CMIP_change_cat %>% dplyr::filter(model == models[imodel],
                                                 type == types[itype])


    dfr.cdf <- tryCatch(rasterFromXYZ(cdf %>% ungroup() %>% dplyr::select(lon,lat,cat.num)),
                        error = function(e) NULL)
    if (is.null(dfr.cdf)) next()
    dfr.cdf.rspld <- resample(dfr.cdf,Avi.AFR.crop,
                              method = "ngb")
    dfr.cdf.rspld[is.na(Avi.AFR.crop)] <- NA

    cdf.rspld <- as.data.frame(dfr.cdf.rspld,xy = TRUE) %>%
      rename(lon = x,
             lat = y) %>% filter(!is.na(cat.num))

    df.rspld.all <- bind_rows(list(df.rspld.all,
                                   cdf.rspld %>% mutate(model = models[imodel],
                                                        type = types[itype])))

  }
}

world <- ne_countries(scale = "medium", returnclass = "sf")

models2keep <- unique(df.rspld.all$model)

ggplot() +
  geom_raster(data = df.rspld.all %>% dplyr::filter(model %in% models2keep[sample(1:length(models2keep),10,replace = FALSE)]),
            aes(x = lon, y = lat,
                fill = as.factor(cat.num)),
            alpha = 1) +
  # geom_sf(data = world,fill = NA,color = "black") +
  theme_bw() +
  scale_x_continuous(limits = c(-10,45),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(-16,10),
                     expand = c(0,0)) +
  labs(x = "", y = "", fill = "Vegetation type") +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  facet_grid(type ~ model) +
  guides(size = "none") +
  theme(text = element_text(size = 20))


transition2 <- df.rspld.all %>%
  group_by(model,type,cat.num) %>%
  summarise(N = length(cat.num),
            .groups = "keep") %>%
  group_by(model) %>%
  mutate(N.rel = N/N[type == "Historical"],
         nobs = length(N[!is.na(N)])) %>%
  filter(nobs == 6)

transition2.final <- bind_rows(list(transition2 %>% mutate(var = "Individual model"),
                                    transition2 %>%
                                      group_by(type,cat.num) %>%
                                      summarise(N.rel = mean(N.rel,na.rm = TRUE),
                                                N = mean(N,na.rm = TRUE),
                                                .groups = "keep") %>%
                                      mutate(var = "Model ensemble"))) %>%
  mutate(timing = case_when(type == "Historical" ~ 1,
                            TRUE ~ 2),
         cat = case_when(cat.num == 1 ~ "Savanna",
                         cat.num == 2 ~ "Rainforest",
                         cat.num == 3 ~ "Seasonal forest")) %>%
  mutate(cat = factor(cat,levels = c("Savanna","Rainforest","Seasonal forest")))

df.data <- readRDS("./outputs/class.cat.current.RDS")
df.data.sum <- df.data %>% group_by(LC.predicted) %>%
  filter(!is.na(LC.predicted)) %>%
  summarise(N = length(LC.predicted),
            .groups = "keep") %>%
  mutate(timing = 1) %>%
  mutate(cat = case_when(LC.predicted == 1 ~ "Savanna",
                         LC.predicted == 2 ~ "Rainforest",
                         LC.predicted == 3 ~ "Seasonal forest"))


ggplot(data = transition2.final,
       aes(x = timing, y = N.rel)) +
  geom_line(aes(group = model, color = var, size = var)) +
  scale_color_manual(values = c("darkgrey","black")) +
  scale_size_manual(values = c(0.2,2)) +
  facet_wrap(~ as.factor(cat)) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "", y = "Relative area", color = "", size = "") +
  theme_bw()


ggplot(data = transition2.final,
       aes(x = type, y = N)) +
  geom_line(aes(group = model, color = var, size = var)) +
  scale_color_manual(values = c("darkgrey","black")) +
  geom_point(data = df.data.sum %>% mutate(type = "Historical"),
             size = 2, color = "black") +
  scale_size_manual(values = c(0.2,2)) +
  facet_wrap(~ as.factor(cat)) +
  labs(x = "", y = "Area", color = "", size = "") +
  theme_bw()


df.rspld.all.wide <- df.rspld.all %>%
  pivot_wider(names_from = type,
              values_from = cat.num) %>%
  mutate(change.type = case_when(Historical == Scenario ~ "No change",
                                 ((Scenario > Historical) & (Historical == 1)) | (Scenario==2 & Historical == 3) ~ "Forest gain",
                                 TRUE ~ "Forest loss")) %>%
  mutate(change.type = factor(change.type,
                              levels = c("Forest loss","No change","Forest gain")))

# ggplot() +
#   geom_raster(data = df.rspld.all.wide,
#               aes(x = lon, y = lat,
#                   fill = as.factor(change.type)),
#               alpha = 1) +
#   geom_sf(data = world,fill = NA,color = "black") +
#   theme_bw() +
#   scale_x_continuous(limits = c(-10,45),
#                      expand = c(0,0)) +
#   scale_y_continuous(limits = c(-16,10),
#                      expand = c(0,0)) +
#   labs(x = "", y = "", fill = "Vegetation change") +
#   scale_fill_manual(values = c("red","white","darkgreen")) +
#   facet_wrap(~ model) +
#   guides(size = "none") +
#   theme(text = element_text(size = 20))


df.rspld.all.wide.sum <- df.rspld.all.wide %>%
  filter(change.type != "No change") %>%
  group_by(model,change.type) %>%
  summarise(N = length(Scenario),
            .groups = "keep")

ggplot() +
  geom_boxplot(data = df.rspld.all.wide.sum,
               aes(x = change.type, y = N)) +
  theme_bw()


##########################################################

df.rspld.all.sum <- df.rspld.all %>%
  group_by(type,lat,lon) %>%
  summarise(cat.num2 = modal(cat.num),
            cat.num.most = case_when( (max(table(cat.num)) > (length(cat.num)*0.75)) ~ modal(cat.num),
                                      TRUE ~ NA_real_),
            cat.num = case_when( (max(table(cat.num)) > (length(cat.num)/2)) ~ modal(cat.num),
                                TRUE ~ NA_real_),
            .groups = "keep") %>%
  rename(timing = type)

df.rspld.all.sum.with.data <- bind_rows(list(df.rspld.all.sum,
                                             df.data %>% dplyr::select(lat,lon,LC.predicted) %>%
                                               mutate(timing = "data") %>%
                                               rename(cat.num2 = LC.predicted)))



ggplot() +
  geom_raster(data = df.rspld.all.sum %>% filter(!is.na(cat.num.most)),
              aes(x = lon, y = lat,
                  fill = as.factor(cat.num.most)),
              alpha = 1) +
  geom_sf(data = world,fill = NA,color = "black") +
  theme_bw() +
  scale_x_continuous(limits = c(-10,45),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(-16,10),
                     expand = c(0,0)) +
  labs(x = "", y = "", fill = "Vegetation type") +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  facet_wrap(~ as.factor(timing)) +
  guides(size = "none") +
  theme(text = element_text(size = 20))


df.rspld.all.sum.with.data.mod <-
  df.rspld.all.sum.with.data %>%
  mutate(cat = case_when(cat.num2 == 1 ~ "Savanna",
                         cat.num2 == 2 ~ "Rainforest",
                         cat.num2 == 3 ~ "Seasonal forest")) %>%
  ungroup() %>%
  mutate(cat = factor(cat,levels = c("Savanna","Rainforest","Seasonal forest")))

ggplot() +
  geom_raster(data = df.rspld.all.sum.with.data.mod %>% filter(!is.na(cat.num2)),
              aes(x = lon, y = lat,
                  fill = as.factor(cat)),
              alpha = 1) +
  geom_sf(data = world,fill = NA,color = "black") +
  geom_point(data = df.rspld.all.sum.with.data %>% filter(!is.na(cat.num.most)),
             aes(x = lon, y = lat), size = 0.1) +
  theme_bw() +
  scale_x_continuous(limits = c(-10,45),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(-16,10),
                     expand = c(0,0)) +
  labs(x = "", y = "", fill = "Vegetation type") +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  facet_wrap(~ as.factor(timing)) +
  guides(size = "none") +
  theme(text = element_text(size = 20))

df.rspld.all.sum.change <- df.rspld.all.sum %>%
  dplyr::select(timing,lat,lon,cat.num2,cat.num.most) %>%
  pivot_wider(names_from = timing,
              values_from = c(cat.num2,cat.num.most)) %>%
  filter(! (is.na(cat.num.most_Historical) | !is.na(cat.num.most_Scenario))) %>%
  dplyr::select(-c(cat.num.most_Historical,cat.num.most_Scenario)) %>%
  pivot_longer(cols = c(cat.num2_Historical,cat.num2_Scenario),
               names_to = "timing",
               values_to = "cat.num2") %>%
  mutate(timing = sub(".*\\_", "", timing)) %>%
  group_by(timing,cat.num2) %>%
  summarise(N = length(cat.num2),
            .groups = "keep") %>%
  group_by(cat.num2) %>%
  mutate(N.rel = N/N[timing == "Historical"],
         timing.num = case_when(timing == "Historical" ~ 1,
                                TRUE ~ 2))

ggplot(data = df.rspld.all.sum.change,
       aes(x = timing.num, y = N.rel, color = as.factor(cat.num2))) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "", y = "Relative area", color = "", size = "") +
  scale_color_manual(values = c("#c49402","#005401","#448704")) +
  theme_bw()

ggplot(data = df.rspld.all.sum.change,
       aes(x = timing.num, y = N, color = as.factor(cat.num2))) +
  geom_line() +
  # geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "", y = "Relative area", color = "", size = "") +
  scale_color_manual(values = c("#c49402","#005401","#448704")) +
  theme_bw()

