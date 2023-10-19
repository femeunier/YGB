rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)

data.file <- file.path(getwd(),"data","CO2_Filou.csv")
data <- read.csv(data.file)

data.f <- data %>% filter(qc_co2_flux < 2)

# ggplot(data = data.f) +
#   geom_line(aes(x = DOY,y = co2_flux),color = "grey") +
#   geom_point(aes(x = DOY,y = co2_flux)) +
#   geom_hline(yintercept = 0) +
#   theme_bw()


data.sum <- data.f %>% mutate(h = round(48*(DOY - floor(DOY))))  %>% mutate(h = case_when(h > 48 ~ (h - 48),
                                                                                          h < 0 ~ h + 48,
                                                                                          TRUE ~h))  %>% group_by(h) %>%
  summarise(CO2 = mean(co2_flux,na.rm = TRUE),
            ET = mean(ET,na.rm = TRUE))

data.seas <- data.f %>% mutate(timestamp = as.POSIXct(date)) %>% mutate(mo = month(timestamp),
                                                                        yr = year(timestamp),
                                                                        d = day(timestamp)) %>% group_by(mo) %>%
  summarise(CO2 = mean(co2_flux,na.rm = TRUE),
            timestamp = mean(timestamp,na.rm = TRUE))

data.f2 <- data %>% filter(qc_h2o_flux < 2)

data.sum2 <-
  data.f2 %>% mutate(h = round(48 * (DOY - floor(DOY))))  %>% mutate(h = case_when(h > 48 ~ (h - 48),
                                                                                   h < 0 ~ h + 48,
                                                                                   TRUE ~
                                                                                     h))  %>% group_by(h) %>%
  summarise(H2O = mean(h2o_flux, na.rm = TRUE))

data.seas2 <- data.f2 %>% mutate(timestamp = as.POSIXct(date)) %>% mutate(mo = month(timestamp),
                                                                          yr = year(timestamp),
                                                                          d = day(timestamp)) %>% group_by(mo) %>%
  summarise(H20 = mean(h2o_flux,na.rm = TRUE),
            timestamp = mean(timestamp,na.rm = TRUE))


# ggplot(data = data.sum2) +
#   geom_point(aes(x = h/2,y = H2O)) +
#   geom_hline(yintercept = 0) +
#   theme_bw()

# Simulations
system2("rsync",
        c("-avz","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/SA/SA_reference/analy/analysis.RData",
          "./outputs/"))

par(mfrow = c(1,1))

load(file.path(getwd(),"outputs","analysis.RData"))
month.t <- c(10:12)
pos.month <- which(datum$month %in% month.t)
NEP <- apply(-t((datum$qmean$nee)[pos.month,]),1,mean)
NEPsd <- apply(-t((datum$qmean$nee)[pos.month,]),1,sd)
trans <- apply(t((datum$qmean$transp + pmax(-Inf,datum$qmean$evap))[pos.month,]),1,mean) *1000/18/86400*1000
transsd <- apply(t((datum$qmean$transp + datum$qmean$evap)[pos.month,]),1,sd) *1000/18/86400*1000

NEP.seas <- -datum$emean$nee
trans.seas <- (datum$emean$transp + datum$emean$evap)*1000/18/86400*1000

df.seas <- data.frame(month = datum$month,
                      year = datum$year,
                      NEP = NEP.seas,
                      transp = trans.seas) %>% filter(year > (min(year) +2)) %>% group_by(month) %>% summarise(NEP = mean(NEP),
                                                                                                               ET = mean(transp))

# matplot(t(datum$qmean$wflxlc[pos.month,]),type = 'l') # leaf evaporation
# matlines(t(datum$qmean$wflxwc[pos.month,]),type = 'l') # wood evaporation
# matlines(t(datum$qmean$wflxgc[pos.month,]),type = 'l') # soil evaporation


# trans <- apply(t((datum$qmean$wflxca)[pos.month,]),1,mean) *1000/18/86400*1000
# transsd <- apply(t((datum$qmean$wflxca)[pos.month,]),1,sd) *1000/18/86400*1000

simu <- data.frame(
  h = seq(1, 48) + 4,
  nep = NEP,
  nep.sd = NEPsd,
  ET = trans,
  ETsd = transsd) %>% mutate(h = case_when(h > 48 ~ (h - 48),
                                           TRUE ~ h))

# par(mfrow = c(1,1))
# plot((1:48)/2,NEP,type = 'l')
# lines((simu$h)/2,simu$nep,type = 'l',col = 'red')

# matplot(seq(0,23.5,length.out = 48) + 4.5,t(datum$qmean$par.tot),type = 'l')
# matplot(t(datum$qmean$transp + datum$qmean$evap)*1000/18/86400*1000,type = 'l')
# plot(apply(t(datum$qmean$transp + datum$qmean$evap),1,mean)*1000/18/86400*1000,type = 'l')


# Diurnal
ggplot(data = data.sum) +
  geom_point(aes(x = h/2,y = -CO2)) +
  geom_hline(yintercept = 0) +
  geom_line(data = simu,
            aes(x = h/2,y = NEP)) +
  labs(x = "",y = "CO2 flux (µmolC/m²/s)") +
  geom_ribbon(data = simu,
              aes(x = h/2,ymin = (nep - nep.sd),ymax = (nep + nep.sd)),alpha = 0.2,color = "darkgrey") +
  theme_bw()

ggplot(data = data.sum2) +
  geom_point(aes(x = h/2,y = H2O)) +
  geom_hline(yintercept = 0) +
  geom_line(data = simu,aes(x = h/2,y = ET)) +
  labs(x = "",y = "H20 flux (mmol/s/m²)") +
  geom_ribbon(data = simu,
              aes(x = h/2,ymin = (ET - ETsd),ymax = (ET + ETsd)),alpha = 0.2,color = "darkgrey") +
  theme_bw()


# plot(data.sum2$H2O,simu %>% arrange(h) %>% pull(ET))
# plot(-data.sum$CO2,simu %>% arrange(h) %>% pull(nep))

# Seasonal
ggplot(data = df.seas) +
  geom_line(aes(x = month, y = NEP)) +
  geom_point(data = data.seas,
             aes(x = mo, y  = - CO2)) +
  scale_x_continuous(breaks = seq(1,12)) +
  theme_bw()

ggplot(data = df.seas) +
  geom_line(aes(x = month, y = ET)) +
  geom_point(data = data.seas2,
             aes(x = mo, y  = H20)) +
  scale_x_continuous(breaks = seq(1,12)) +
  theme_bw()
