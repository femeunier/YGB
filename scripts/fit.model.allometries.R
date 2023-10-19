rm(list = ls())

library(dplyr)
library(ggplot2)

census.file   = '/home/femeunier/Documents/data/Yangambi/data/inventories/Yangambi_census_MIX05.csv'

a = 36.3576 ; b = 31.6591 ; c= 0.0221
a1 = 39.964 - 2.252 ; b1 =  35.224 -2.357 ; c = 0.023
b1Ht = 1.2366; b2Ht = 0.5195; dbh_crit = 78.6525

census.data <- read.csv(census.file) %>% mutate(n = 1/(20*20),
                                                h = a - b*exp(-c*dbh),
                                                PFT = case_when(wood.dens <= 0.53 ~ 2,
                                                                wood.dens >= 0.71 ~ 4,
                                                                TRUE ~ 3),

                                                H = exp(b1Ht + b2Ht * log(pmin(dbh_crit,dbh))),
                                                WD = case_when(PFT == 2 ~ 0.4,
                                                               PFT == 3 ~ 0.6,
                                                               PFT == 4 ~ 0.8),
                                                BA = pi/4*(dbh**2),
                                                AGB = (0.0673*(wood.dens*(dbh**2)*h)**0.976),
                                                AGC = 0.5*AGB,
                                                AGB2 = (0.0509*(wood.dens*(dbh**2)*h)),
                                                AGC2 = 0.5*AGB2) %>%
  mutate(AGB.ED2 = (0.0673*(WD*(dbh**2)*H)**0.976),
         AGC.ED2 = 0.5*AGB.ED2)

census.data %>% group_by(PFT) %>% summarise(mean(wood.dens))


census.data.bis <- census.data %>% mutate(size = dbh*dbh*h,
                                          PFT = case_when(wood.dens <= 0.53 ~ 2,
                                                          wood.dens >= 0.71 ~ 4,
                                                          TRUE ~ 3),
                                          WD = case_when(PFT == 2 ~ 0.4,
                                                         PFT == 3 ~ 0.6,
                                                         PFT == 4 ~ 0.8))


census.data.patch <- census.data %>% group_by(plots) %>% summarise(AGB = sum(AGB*n),
                                                                   AGC = sum(AGC*n),
                                                                   AGB2 = sum(AGB2*n),
                                                                   AGC2 = sum(AGC2*n),
                                                                   AGB.ED2 = sum(AGB.ED2*n),
                                                                   AGC.ED2 = sum(AGC.ED2*n))

census.data.site <- census.data.patch %>% ungroup() %>% summarise(AGB.m = mean(AGB),
                                                                  AGB.sd = sd(AGB),
                                                                  AGC.m = mean(AGC),
                                                                  AGC.sd = sd(AGC),
                                                                  AGB2.m = mean(AGB2),
                                                                  AGB2.sd = sd(AGB2),
                                                                  AGC2.m = mean(AGC2),
                                                                  AGC2.sd = sd(AGC2),
                                                                  AGC.ED2.m = mean(AGC.ED2),
                                                                  AGC.ED2.sd = sd(AGC.ED2))

census.data %>% group_by(PFT) %>% summarise(AGC = sum(AGC.ED2),
                                            N = length(AGC.ED2))

census.data.site

census.data.site2 <- census.data %>% mutate(n = 1/(20*20*25),
                                            BA = pi/4*(dbh**2)) %>% summarise(N = sum(n)*10000,
                                                                              dbh = mean(dbh),
                                                                              BA.m = sum(BA*n),
                                                                              WD.m = weighted.mean(wood.dens,BA),
                                                                              AGB = sum(AGB*n),
                                                                              AGC = sum(AGC*n),
                                                                              AGB2 = sum(AGB2*n),
                                                                              AGC2 = sum(AGC2*n),
                                                                              AGB.ED2 = sum(AGB.ED2*n),
                                                                              AGC.ED2 = sum(AGC.ED2*n))

census.data.site2

# Height

b1Ht = 1.1399630308; b2Ht = 0.5648990273 ; dbh_crit = 166.0200805664

# NLM
href <- 61.7
b1Ht <- 0.035
b2Ht <- 0.69
hmax <-35

modelH3 <- nls(data = census.data, h~ pmin(Hmax,exp(b1Ht + b2Ht * log(dbh))),
               start=list(b1Ht=b1Ht, b2Ht=b2Ht,Hmax = 30))

modelH2 <- nls(data = census.data, h~ exp(b1Ht + b2Ht * log(pmin(dbh_crit,dbh))),
               start=list(b1Ht=b1Ht, b2Ht=b2Ht))

modelH <- nls(data = census.data, h~ exp(b1Ht + b2Ht * log(pmin(dbh_crit,dbh))),
           start=list(b1Ht=coef(modelH2)[1], b2Ht=coef(modelH2)[2], dbh_crit = 100))

dbhs <- seq(0,150)
plot(dbhs,a - b*exp(-c*dbhs),ylim = c(0,50),xlim = c(0,150),type = 'l')
lines(dbhs,exp (b1Ht + b2Ht * log(pmin(dbh_crit,dbhs)) ),type = 'l', col = "red")
lines(dbhs,exp(coef(modelH)[1] + coef(modelH)[2] * log(pmin(coef(modelH)[3],dbhs))),type = 'l', col = "blue",lty = 2)

H = exp(coef(modelH2)[1] + coef(modelH2)[2] * log(pmin(dbh_crit,dbhs)))
lines(dbhs,H,type = 'l', col = "black",lty = 2)


# AGB

b1Bs <- c(0.0321342722,0.0431042016,0.0546971895)
b2Bs <- rep(1.0044784546,3)
# b2Bs <- rep(0.976,3)

plot(census.data$dbh,census.data$AGC.ED2,type = 'p',log = 'xy',xlim = c(10,100),ylim = c(10,10000))
lines(census.data$dbh,census.data$AGC2,type = 'p',col = "red")

size = dbhs*dbhs*H
C <- c("black","darkgrey","grey")
for (i in seq(1,length(b1Bs))){
   lines(dbhs,0.7*b1Bs[i]/2*(size**b2Bs[i]),col = C[i])
}

LM.AGB <- lm(data = census.data.bis, formula = log(AGC) ~ log(size))
b1Bs.best <- exp(coef(LM.AGB)[1])*2/0.7
b2Bs.best <- coef(LM.AGB)[2]

b1ED2 <- 0.88*(0.0673*c(0.448,0.612,0.782)**0.976)/0.7
b1ED2 <- 0.88*(0.0673*c(0.4,0.6,0.8)**0.976)/0.7 # ~12% of biomass is leaf in ED2
b2ED2 <- 0.976

lines(dbhs,0.7*b1Bs.best/2*((size)**b2Bs.best),col = 'red')
lines(dbhs,0.7*b1Bs.best/2*((size)**b2Bs.best),col = 'red')
lines(dbhs,0.7*b1Bs.best/2*((size)**b2Bs.best),col = 'red')

lines(dbhs,0.7*b1ED2[1]/2*((size)**b2ED2),col = 'blue')
lines(dbhs,0.7*b1ED2[2]/2*((size)**b2ED2),col = 'blue')
lines(dbhs,0.7*b1ED2[3]/2*((size)**b2ED2),col = 'blue')

