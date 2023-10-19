rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)

data.file <- "/home/femeunier/Documents/projects/YGB/data/Baumgartner_et_al_2020/Datasets/Soil_CO2_fluxes_Baumgartner_et_al.csv"
header <- read.csv(data.file,header = FALSE,sep = ';',nrows = 1, as.is = TRUE)
data <- read.csv(data.file,skip = 1,header = TRUE,sep = ';')
colnames(data) <- header

# CO2 fluxes

data.mod <- data %>% dplyr::select(DSAMP,TIME,LOCATION,`F-CO2`,SITE) %>%
  rename(fco2 = `F-CO2`) %>%
  mutate(time = as.Date(DSAMP,format = "%d.%m.%Y")) %>%
  mutate(week = week(time))


# Figure 2 https://bg.copernicus.org/articles/17/6207/2020/bg-17-6207-2020.pdf
data.mod.sum <- data.mod %>% group_by(SITE,week) %>%
  summarise(fco2.m = mean(fco2,na.rm = TRUE),
            fco2.sd = sd(fco2,na.rm = TRUE),.groups = "keep")

ggplot(data = data.mod.sum) +
  geom_point(aes(x = week,y = fco2.m, color = as.factor(SITE))) +
  geom_errorbar(aes(x = week,y = fco2.m,
                    ymin = fco2.m - fco2.sd,ymax = fco2.m + fco2.sd,
                    color = as.factor(SITE))) +
  scale_y_continuous(limits = c(0,10)) +
  theme_bw()

ggplot(data = data.mod) +
  geom_boxplot(aes(x = as.factor(SITE),y = fco2)) +
  scale_y_continuous(limits = c(0,10)) +
  theme_bw()


data.mod.sum2 <- data.mod %>% group_by(LOCATION,week) %>%
  summarise(fco2.m = mean(fco2,na.rm = TRUE),
            fco2.sd = sd(fco2,na.rm = TRUE),.groups = "keep")

ggplot(data = data.mod.sum2 %>% filter(LOCATION %in% c("YGB","YGB-GIL","YO","YO-GIL"))) +
  geom_point(aes(x = week,y = fco2.m, color = as.factor(LOCATION))) +
  geom_errorbar(aes(x = week,y = fco2.m,
                    ymin = fco2.m - fco2.sd,ymax = fco2.m + fco2.sd,
                    color = as.factor(LOCATION))) +
  scale_y_continuous(limits = c(0,10)) +
  theme_bw()


# Soil moisture
soil.moisture.file <- "/home/femeunier/Documents/projects/YGB/data/Baumgartner_et_al_2020/Datasets/soil_moisture_YO_Baumgartner_et_al.csv"
header.SM <- read.csv(soil.moisture.file,skip = 6,header = FALSE,sep = ';',nrows = 1,stringsAsFactors = FALSE)
data.SM <- read.csv(soil.moisture.file,skip = 6,header = TRUE,sep = ';')
colnames(data.SM) <- as.vector(header.SM,"character")

data.SM <- data.SM %>% dplyr::select(LOCATION,DateTime,`Average VWC`,`WFPS [%]`) %>% mutate(time = as.Date(DateTime,format = "%d.%m.%Y")) %>%
  mutate(week = week(time)) %>%  rename(VWC = `Average VWC`,
                                        WFPS = `WFPS [%]`)

data.SM.sum <- data.SM %>% group_by(LOCATION,week) %>%
  summarise(VWC.m = mean(VWC,na.rm = TRUE),
            VWC.sd = sd(VWC,na.rm = TRUE),
            WFPS.m = mean(WFPS,na.rm = TRUE),
            WFPS.sd = sd(WFPS,na.rm = TRUE),.groups = "keep")

ggplot(data = data.SM.sum) +
  geom_point(aes(x = week,y = WFPS.m, color = as.factor(LOCATION))) +
  geom_errorbar(aes(x = week,y = WFPS.m,
                    ymin = WFPS.m - WFPS.sd,ymax = WFPS.m + WFPS.sd,
                    color = as.factor(LOCATION))) +
  theme_bw()


# Soil Temperature
soil.moisture.file <- "/home/femeunier/Documents/projects/YGB/data/Baumgartner_et_al_2020/Datasets/soil_Temperature_Baumgartner_et_al.csv"
header.ST <- read.csv(soil.moisture.file,skip = 5,header = FALSE,sep = ';',nrows = 1,stringsAsFactors = FALSE)
data.ST <- read.csv(soil.moisture.file,skip = 5,header = TRUE,sep = ';')
data.ST <- data.ST[-nrow(data.ST),]
colnames(data.ST) <- as.vector(header.ST,"character")

data.ST <- data.ST %>% dplyr::select(DATE,TIME,soilT,LOCATION) %>% mutate(time = as.Date(DATE,format = "%d.%m.%Y")) %>%
  mutate(week = week(time))

data.ST.sum <- data.ST %>% group_by(LOCATION,week) %>%
  summarise(soilT.m = mean(soilT,na.rm = TRUE),
            soilT.sd = sd(soilT,na.rm = TRUE),
            .groups = "keep")

ggplot(data = data.ST.sum) +
  geom_point(aes(x = week,y = soilT.m, color = as.factor(LOCATION))) +
  geom_errorbar(aes(x = week,y = soilT.m,
                    ymin = soilT.m - soilT.sd,ymax = soilT.m + soilT.sd,
                    color = as.factor(LOCATION))) +
  theme_bw()


# Monthly data of interest

monthly.data <- data.mod %>% filter(SITE == "Montane") %>% mutate(month = month(time)) %>% group_by(month) %>%
  summarise(fco2.m = mean(fco2,na.rm = TRUE),
            fco2.sd = sd(fco2,na.rm = TRUE)) %>%
  left_join(data.SM %>% mutate(month = month(time)) %>% group_by(month) %>%
              summarise(VWC.m = mean(VWC,na.rm = TRUE),
                        VWC.sd = sd(VWC,na.rm = TRUE)),
            by = c("month")) %>%
  left_join(data.ST %>% filter(LOCATION %in% c("YGB","YO")) %>% mutate(month = month(time)) %>% group_by(month) %>%
              summarise(soilT.m = mean(soilT,na.rm = TRUE),
                        soilT.sd = sd(soilT,na.rm = TRUE)),
            by = c("month"))


saveRDS(object = monthly.data,file = "./data/Baumgartner_et_al_2020/Baumgartner.RDS")
