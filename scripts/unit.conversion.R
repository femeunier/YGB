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

file <- "/home/femeunier/Documents/projects/YGB/outputs/analysis.RData"
load(file)

plot(datum$emean$nee,type = 'l')
lines(datum$emean$cflxca, col = 'red')


plot(datum$emean$cflxca/datum$emean$nee)

plot(datum$emean$gpp - datum$emean$reco,datum$emean$nee*(1/1000*1e-6*12*86400*365))
