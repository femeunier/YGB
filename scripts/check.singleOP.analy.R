rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(BayesianTools)
library(rhdf5)

# Import outputs and parameter data.frame
system2("rsync",paste("hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/SA/SA_reference/analy/analysis.RData",
                      "./outputs/"))

load("./outputs/analysis.RData")

emean <- datum$emean
emean$agb[1]

plot(emean$agb,type = 'l')

plot(emean$lai,type = 'l')

plot(emean$nep[13:36],type = 'l')



source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")

system2("rsync",
        c("-avz","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/analy/YGB_new-Q-2015-01-00-000000-g01.h5",
          "./outputs/"))

h5file <- "/home/femeunier/Documents/projects/YGB/outputs/YGB_new-Q-2015-01-00-000000-g01.h5"
mymont <- lapply(h5read_opt(h5file),FUN=aperm)

sum(mymont$AGB_PY)
