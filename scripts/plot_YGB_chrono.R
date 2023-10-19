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

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_Yoko_chronosequence.RDS",
                      "./outputs/"))

df_Yoko <- readRDS(file.path("./outputs/","df_Yoko_chronosequence.RDS")) %>%
  mutate(t = yr + (month - 1)/12/2)

ggplot(data = df_Yoko) +
  geom_line(aes(x = t, y = AGB)) +
  theme_bw()

