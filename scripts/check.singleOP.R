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
        c("-avz","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/analy/YGB_tower_default-Q-2016-01-00-000000-g01.h5",
          "./outputs/"))

h5file <- "/home/femeunier/Documents/projects/YGB/outputs/YGB_tower_default-Q-2016-01-00-000000-g01.h5"
mymont    = lapply(h5read_opt(h5file),FUN=aperm)

sum(mymont$AGB_PY)

delta = 4
evap = ( mymont$QMEAN_VAPOR_GC_PY[1,] + mymont$QMEAN_VAPOR_WC_PY[1,] + mymont$QMEAN_VAPOR_LC_PY[1,] ) * 86400
transp = mymont$QMEAN_TRANSP_PY[1,] * 86400
ET = evap + transp
ET <- c(ET[(length(ET) - delta):length(ET)],ET[1:(length(ET) - delta -1)])

nep = - (mymont$QMEAN_CARBON_ST_PY[1,] - mymont$QMEAN_CARBON_AC_PY[1,])
nep <- c(nep[(length(nep) - delta):length(nep)],nep[1:(length(nep) - delta -1)])

par.t = mymont$QMEAN_RSHORT_L_PY[1,]
par.t <- c(par.t[(length(par.t) - delta):length(par.t)],par.t[1:(length(par.t) - delta -1)])

pos1 = ((0:47)/2)[which.max(par.t)]; pos2 = ((0:47)/2)[which.max(nep)]; pos3 = ((0:47)/2)[which.max(ET)]

par(mfrow = c(3,1))
plot(((0:47))/2,par.t,type = 'l',xlim = c(0,24),xlab = "",ylab = "Modelled relative flux", xaxt="n",main = "ED2")
abline(v = pos1,col = "black",lty = 3)
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)

plot(((0:47))/2,(nep),type = 'l',col = 'red', xaxt="n")
abline(v = pos2,col = "red",lty = 3)
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)

plot(((0:47))/2,(ET),col = 'green',type = 'l', xaxt="n")
abline(v = pos3,col = "green",lty = 3)
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)

par(mfrow = c(3,1))
plot(((0:47))/2,par.t/max(par.t),type = 'l',xlim = c(0,24),xlab = "",ylab = "Modelled relative flux", xaxt="n",main = "ED2")
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
lines(((0:47))/2,(nep - min(nep))/(max(nep) - min(nep)),type = 'l',col = 'red')
lines(((0:47))/2,(ET - min(ET))/(max(ET) - min(ET)),col = 'green')
pos1 = ((0:47)/2)[which.max(par.t)]; pos2 = ((0:47)/2)[which.max(nep)]; pos3 = ((0:47)/2)[which.max(ET)]
abline(v = pos1,col = "black",lty = 3)
abline(v = pos2,col = "red",lty = 3)
abline(v = pos3,col = "green",lty = 3)


##################################################################################################################
# ORCHIDEE

h5file <- file.path('/home/femeunier/Documents/projects/YGB/data/ORCHIDEE/','YGB_hourly_2015.nc')
h5fileSW <- file.path('/home/femeunier/Documents/projects/YGB/data/ORCHIDEE/','YGB_hourly_2015_swdown.nc')
# h5ls(h5file)
# system2("ncdump",paste("-h",h5file))

tinit <- (274*24)+1
tend <- 365*24
nep <- -h5read(h5file,"/nee")[1,1,,tinit:tend]
LE <- h5read(h5file,"/fluxlat")[1,1,tinit:tend]
SW <- h5read(h5fileSW,"/swdown")[1,1,tinit:tend]
maxvegetfrac <-  h5read(h5file,"/maxvegetfrac")[1,1,,tinit:tend]

nep.w <- sapply(1:dim(nep)[2],function(i){
  return(weighted.mean(nep[,i],maxvegetfrac[,i]))})

df <- data.frame(h = rep(0:23,length(nep.w)/24),nep = nep.w, LE = LE,SW) %>% group_by(h) %>% summarise(nep = mean(nep),
                                                                                                       LE = mean(LE),
                                                                                                       SW = mean(SW))

plot(0:23,df$SW/max(df$SW),type = 'l',xlim = c(0,24),xlab = "",ylab = "Modelled relative flux", xaxt="n",main = "ORCHIDEE")
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
lines(0:23,(df$nep - min(df$nep))/(max(df$nep) - min(df$nep)),type = 'l',xaxt="n", col = 'red')
lines(0:23,df$LE/max(df$LE),type = 'l',col = 'green')

pos1 = ((0:23))[which.max(df$SW)]; pos2 = ((0:23))[which.max(df$nep)]; pos3 = ((0:23))[which.max(df$LE)]
abline(v = pos1,col = "black",lty = 3)
abline(v = pos2,col = "red",lty = 3)
abline(v = pos3,col = "green",lty = 3)


##################################################################################################################
# Data

delta = 0
data <- read.csv(file.path(getwd(),"data","CO2_Filou.csv")) %>% filter(qc_co2_flux < 2,qc_h2o_flux<2)
data.sum <- data %>% mutate(h = round(48*(DOY - floor(DOY))) - delta) %>% mutate(h = case_when(h > 48 ~ (h - 48),
                                                                                               h < 0 ~ h + 48,
                                                                                               TRUE ~h))  %>% group_by(h) %>%
  summarise(CO2 = -mean(co2_flux,na.rm = TRUE),
            ET = mean(ET,na.rm = TRUE))

data.rad <- read.csv(file.path(getwd(),"data","Radiation_filou.csv")) %>% mutate(time = strptime(Time, format = '%Y-%m-%d %H:%M:%S'))
data.rad.sum <- data.rad %>% mutate(h = hour(Time)) %>% filter(!is.na(h)) %>% group_by(h) %>%
  summarise(SW = mean(SW_TOTAL_1_1_1,na.rm = TRUE))

plot(data.rad.sum$h,(data.rad.sum$SW-min(data.rad.sum$SW))/(max(data.rad.sum$SW) - min(data.rad.sum$SW)),type = "l",xlab = "",ylab = "Modelled relative flux", xaxt="n",main = "Data")
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
lines((1:48)/2,(data.sum$CO2 - mean(data.sum$CO2[1:10]))/(max(data.sum$CO2) - mean(data.sum$CO2[1:10])),col = "red")
lines((1:48)/2,(data.sum$ET - min(data.sum$ET))/(max(data.sum$ET) - min(data.sum$ET)),col = "green")

pos1 <- which.max(data.rad.sum$SW)
pos2 <- which.max(data.sum$CO2)
pos3 <- which.max(data.sum$ET)

abline(v = pos1 - 1,col = "black",lty = 3)
abline(v = (data.sum$h/2)[pos2],col = "red",lty = 3)
abline(v = (data.sum$h/2)[pos3],col = "green",lty = 3)


par(mfrow = c(3,1))
plot(data.rad.sum$h,data.rad.sum$SW,type = "l",xlab = "",ylab = "Modelled relative flux", xaxt="n",main = "Data")
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
abline(v = pos1 - 1,col = "black",lty = 3)

plot((1:48)/2,data.sum$CO2,col = "red",xaxt="n",type = 'l')
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
abline(v = (data.sum$h/2)[pos2],col = "red",lty = 3)

plot((1:48)/2,data.sum$ET,col = "green",xaxt="n",type = 'l')
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
abline(v = (data.sum$h/2)[pos3],col = "green",lty = 3)
