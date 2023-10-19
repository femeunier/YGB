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
        c("-avz","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/histo/YGB_new-Q-2032-11-00-000000-g01.h55",
          "./outputs/"))

h5file <- "/home/femeunier/Documents/projects/YGB/outputs/YGB_new-Q-2032-11-00-000000-g01.h5"
mymont    = lapply(h5read_opt(h5file),FUN=aperm)

delta = 0
evap = ( mymont$QMEAN_VAPOR_GC_PY[1,] + mymont$QMEAN_VAPOR_WC_PY[1,] + mymont$QMEAN_VAPOR_LC_PY[1,] ) * 86400
transp = mymont$QMEAN_TRANSP_PY[1,] * 86400
ET = evap + transp
ET <- c(ET[(length(ET) - delta):length(ET)],ET[1:(length(ET) - delta -1)])

nep = - (mymont$QMEAN_CARBON_ST_PY[1,] - mymont$QMEAN_CARBON_AC_PY[1,])
nep <- c(nep[(length(nep) - delta):length(nep)],nep[1:(length(nep) - delta -1)])

par.t = mymont$QMEAN_RSHORT_L_PY[1,]
par.t <- c(par.t[(length(par.t) - delta):length(par.t)],par.t[1:(length(par.t) - delta -1)])

par(mfrow = c(3,1))
plot(((0:47))/2,par.t/max(par.t),type = 'l',xlim = c(0,24),xlab = "",ylab = "Modelled relative flux", xaxt="n",main = "ED2")
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
lines(((0:47))/2,(nep - min(nep))/(max(nep) - min(nep)),type = 'l',col = 'red')
lines(((0:47))/2,(ET - min(ET))/(max(ET) - min(ET)),col = 'green')
