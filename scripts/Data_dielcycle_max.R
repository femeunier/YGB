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

data <- read.csv(file.path(getwd(),"data","CO2_Filou.csv")) %>% filter(qc_co2_flux < 2,qc_h2o_flux<2)
data.sum <- data %>% mutate(h = round(48*(DOY - floor(DOY)))) %>% group_by(h) %>%
  summarise(CO2 = -mean(co2_flux,na.rm = TRUE),
            ET = mean(ET,na.rm = TRUE))

data.rad <- read.csv(file.path(getwd(),"data","Radiation_filou.csv")) %>% mutate(time = strptime(Time, format = '%Y-%m-%d %H:%M:%S'))
data.rad.sum <- data.rad %>% mutate(h = hour(Time)) %>% filter(!is.na(h)) %>% group_by(h) %>%
  summarise(SW = mean(SW_TOTAL_1_1_1,na.rm = TRUE))


# par(mfrow = c(3,1))
# plot(data.rad.sum$h,data.rad.sum$SW,type = "p",xlab = "",ylab = "", xaxt="n",main = "data")
# axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
pos1 <- which.max(data.rad.sum$SW)
# abline(v = pos1 - 1,col = "red",lty = 3)
#
#
# plot(data.sum$h/2,data.sum$CO2,type = "p",xlab = "",ylab = "", xaxt="n",main = "data")
# axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
pos2 <- which.max(data.sum$CO2)
# abline(v = (data.sum$h/2)[pos2],col = "red",lty = 3)
#
#
# plot(data.sum$h/2,data.sum$ET,type = "p",xlab = "",ylab = "", xaxt="n",main = "data")
# axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
pos3 <- which.max(data.sum$ET)
# abline(v = (data.sum$h/2)[pos3],col = "red",lty = 3)


par(mfrow = c(1,1))
plot(data.rad.sum$h,(data.rad.sum$SW-min(data.rad.sum$SW))/(max(data.rad.sum$SW) - min(data.rad.sum$SW)),type = "p",xlab = "",ylab = "Modelled relative flux", xaxt="n",main = "data")
lines(data.rad.sum$h,(data.rad.sum$SW-min(data.rad.sum$SW))/(max(data.rad.sum$SW) - min(data.rad.sum$SW)),type = "l",col = "black")
axis(side=1, at=c(0,6,12,18,24),labels = TRUE)
lines(data.sum$h/2,(data.sum$CO2 - min(data.sum$CO2))/(max(data.sum$CO2) - min(data.sum$CO2)),col = "red",type = 'p')
lines(data.sum$h/2,(data.sum$CO2 - min(data.sum$CO2))/(max(data.sum$CO2) - min(data.sum$CO2)),col = "red",type = 'l')
lines(data.sum$h/2,(data.sum$ET - min(data.sum$ET))/(max(data.sum$ET) - min(data.sum$ET)),col = "green",type = 'p')
lines(data.sum$h/2,(data.sum$ET - min(data.sum$ET))/(max(data.sum$ET) - min(data.sum$ET)),col = "green",type = 'l')
abline(v = pos1 - 1,col = "black",lty = 3)
abline(v = (data.sum$h/2)[pos2],col = "red",lty = 3)
abline(v = (data.sum$h/2)[pos3],col = "green",lty = 3)
