rm(list = ls())

library(rhdf5)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(stringr)
library(ggplot2)
library(dplyr)
library(lubridate)

source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")

##################################################################################################################
# ED2

system2("rsync",
        c("-avz","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/SA/SA_reference/histo/history-S-2015-01-01-000000-g01.h5",
          "/home/femeunier/Documents/projects/YGB/outputs/"))

h5file <- "/home/femeunier/Documents/projects/YGB/outputs/history-S-2015-01-01-000000-g01.h5"

mymont1 = lapply(h5read_opt(h5file),FUN=aperm)
sum(mymont1$AGB_PY)

df1 <- data.frame(dbh = mymont1$DBH,
                 AGB = mymont1$AGB_CO,
                 pa = rep(1:mymont1$NPATCHES_GLOBAL,mymont1$PACO_N),
                 area = rep(mymont1$AREA,mymont1$PACO_N),
                 Bs = mymont1$BDEADA,
                 h = mymont1$HITE,
                 n = mymont1$NPLANT,
                 pft = mymont1$PFT)

system2("rsync",
        c("-avz","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/SA/SA_reference/analy/analysis-Q-2015-01-00-000000-g01.h5",
          "/home/femeunier/Documents/projects/YGB/outputs/"))
h5file <- "/home/femeunier/Documents/projects/YGB/outputs/analysis-Q-2015-01-00-000000-g01.h5"

mymont = lapply(h5read_opt(h5file),FUN=aperm)

sum(mymont$AGB_PY[,1:11,c(2,3,4)])
# colSums(mymont$AGB_PY[,1:11,c(2,3,4)])
# rowSums(mymont$AGB_PY[,1:11,c(2,3,4)])


df <- data.frame(dbh = mymont$DBH,
                 AGB = mymont$AGB_CO,
                 pa = rep(1:mymont$NPATCHES_GLOBAL,mymont$PACO_N),
                 area = rep(mymont$AREA,mymont$PACO_N),
                 Bs = mymont$BDEADA,
                 h = mymont$HITE,
                 n = mymont$NPLANT,
                 pft = mymont$PFT)

summary(df1 %>% group_by(pa) %>% filter(dbh >= 10) %>% summarise(dbh.m = weighted.mean(dbh,n)) %>% pull(dbh.m) - df %>% group_by(pa) %>% filter(dbh >= 10) %>% summarise(dbh.m = weighted.mean(dbh,n)) %>% pull(dbh.m))

df.sum <- df %>% group_by(pa) %>% filter(dbh >= 10) %>% summarise(AGB = sum(Bs*n),
                                                                  area = unique(area)) %>% ungroup() %>% summarise(agb = weighted.mean(AGB,area))

df1 %>% group_by(pa) %>% summarise(AGB = sum(AGB*n),
                                             area = unique(area)) %>% ungroup() %>% summarise(agb = weighted.mean(AGB,area))

census.file   = '/home/femeunier/Documents/data/Yangambi/data/inventories/Yangambi_census_MIX05.csv';
a = 36.3576 ; b = 31.6591 ; c= 0.0221

census.data <- read.csv(census.file) %>% mutate(n = 1/(20*20),
                                                h = a - b*exp(-c*dbh),
                                                BA = pi/4*(dbh**2),
                                                AGB = (0.0673*(wood.dens*(dbh**2)*h)**0.976),
                                                AGC = 0.5*AGB,
                                                AGB2 = (0.0509*(wood.dens*(dbh**2)*h)),
                                                AGC2 = 0.5*AGB2) %>% mutate(pft = case_when(wood.dens <= 0.53 ~ 2,
                                                                                            wood.dens >= 0.71 ~ 4,
                                                                                            TRUE ~ 3))

ggplot() +
  geom_line(data = df,
             aes(x = dbh,y = Bs, color = as.factor(pft))) +
  geom_point(data = census.data,
             aes(x = dbh,y = AGC2, color = as.factor(pft))) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

ggplot() +
  geom_line(data = df,
             aes(x = dbh,y = h, color = as.factor(pft))) +
  geom_point(data = census.data,
             aes(x = dbh,y = h, color = as.factor(pft))) +
  theme_bw()

mymont$FAST_SOIL_C_PY
mymont$FAST_GRND_C_PY

mymont$STRUCTURAL_SOIL_C
mymont$STRUCTURAL_GRND_C

mymont$SLOW_SOIL_C_PY

mymont$SLOW_SOIL_C_PY + mymont$FAST_SOIL_C_PY + mymont$FAST_GRND_C_PY + mean(mymont$STRUCTURAL_SOIL_C) + mean(mymont$STRUCTURAL_GRND_C)


