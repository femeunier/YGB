rm(list = ls())

library(hdf5r)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

#########################################################################################################
# CRUNCEP forcing
dirpath <- '/home/femeunier/Documents/projects/YGB/data/MET/'

month <- c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC')
year <- seq(2001,2010)
vars <- c("nbdsf","nddsf","vbdsf","vddsf","dlwrf","pres","ugrd","vgrd","sh","tmp","prate")
Ntimesteps <- 4

# Reading Prate
A <- sapply(X = seq(month),function(i){
  sapply(X = seq_along(year), FUN = function(j) {
    sapply(X = seq_along(vars), FUN = function(k) {
      sapply(X = seq(Ntimesteps), FUN = function(it) {
        fname <- paste0(year[j],month[i],".h5")
        met_driver <- file.path(dirpath,fname)

        metfile    <- h5file(met_driver, mode = "r")
        cvar <- metfile[[vars[k]]] [,,]
        cvarmat <- t(matrix(cvar,nrow = 4))

        return(ifelse(vars[k] == "prate",sum(cvarmat[,it],na.rm = TRUE),mean(cvarmat[,it],na.rm = TRUE)))
      })
    })
  })
})

A.formatted <- array(data = A,dim = c(4,length(vars),length(month),length(year)))

df.CRUNCEP <- melt(A.formatted) %>% mutate(m = month[Var3],
                                           year = year[Var4],
                                           var = vars[Var2],
                                           t = Var1) %>% dplyr::select(-c("Var1","Var2","Var4")) %>% rename(month = Var3)


df.CRUNCEP.seasonal <- bind_rows(list(df.CRUNCEP %>% filter(var == "prate") %>% group_by(year,month,var) %>% summarise(value = sum(value,na.rm = TRUE)) %>% group_by(month,var) %>% summarise(value = mean(value)),
                                      df.CRUNCEP %>% filter(var != "prate") %>% group_by(var,month) %>% summarise(value = mean(value,na.rm = TRUE))))

ggplot(data = df.CRUNCEP.seasonal) +
  geom_line(aes(x = month,y = value)) +
  scale_x_continuous(breaks = c(1,3,6,9,12),
                     labels = c("J","M","J","S","D")) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()

df.CRUNCEP %>% filter(var == "prate") %>% group_by(year) %>% summarise(sum(value)*3600*6)


df.CRUNCEP.diurnal <- bind_rows(list(df.CRUNCEP %>% filter(var == "prate") %>% group_by(year,t,var) %>% summarise(value = sum(value,na.rm = TRUE)) %>% group_by(t,var) %>% summarise(value = mean(value)),
                                     df.CRUNCEP %>% filter(var != "prate") %>% group_by(var,t) %>% summarise(value = mean(value,na.rm = TRUE))))


ggplot(data = df.CRUNCEP.diurnal) +
  geom_line(aes(x = t,y = value)) +
  scale_x_continuous(breaks = c(1,2,3,4)) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()


df.CRUNCEP.diurnal.month <- bind_rows(list(df.CRUNCEP %>% filter(var == "prate") %>% group_by(year,t,month,var) %>% summarise(value = sum(value,na.rm = TRUE)) %>% group_by(t,month,var) %>% summarise(value = mean(value)),
                                           df.CRUNCEP %>% filter(var != "prate") %>% group_by(var,month,t) %>% summarise(value = mean(value,na.rm = TRUE))))


ggplot(data = df.CRUNCEP.diurnal.month) +
  geom_line(aes(x = t,y = value,color = as.factor(month))) +
  scale_x_continuous(breaks = c(1,2,3,4)) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()


df.rad <- df.CRUNCEP %>% pivot_wider(names_from = var,
                                     values_from = value) %>% mutate(visible.rad = vbdsf + vddsf,
                                                                     nir.rad = nbdsf + nddsf,
                                                                     vis2rad = visible.rad/(nir.rad + visible.rad),
                                                                     frac.b.vis = vbdsf/visible.rad,
                                                                     frac.b.nir = nbdsf/nir.rad,
                                                                     SW = visible.rad + nir.rad,
                                                                     LW = dlwrf,
                                                                     LW2tot = dlwrf/(dlwrf + SW)) %>% dplyr::select(c(t,month,year,
                                                                                                                      visible.rad,nir.rad,
                                                                                                                      vis2rad,
                                                                                                                      frac.b.vis,frac.b.nir,
                                                                                                                      SW,LW,LW2tot)) %>% pivot_longer(cols = -c(month,year,t),
                                                                                                                                                      names_to = "var",
                                                                                                                                                      values_to = "value")

ggplot(data = df.rad) +
  geom_boxplot(aes(x = var,y = value)) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()


df.rad.seasonal <- df.rad %>% group_by(var,month) %>% summarise(value.m = mean(value,na.rm = TRUE),
                                                                value.sd = sd(value,na.rm = TRUE))

ggplot(data = df.rad.seasonal) +
  geom_line(aes(x = month,y = value.m)) +
  geom_ribbon(aes(x = month,ymin = value.m - value.sd,ymax = value.m + value.sd),fill = "lightgrey",col = NA,alpha = 0.4) +
  scale_x_continuous(breaks = c(1,3,6,9,12),
                     labels = c("J","M","J","S","D")) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()


df.rad.seasonal.month <- bind_rows(list(df.rad %>% filter(var == "prate") %>% group_by(year,t,month,var) %>% summarise(value = sum(value,na.rm = TRUE)) %>% group_by(t,month,var) %>% summarise(value = mean(value)),
                                        df.rad %>% filter(var != "prate") %>% group_by(var,month,t) %>% summarise(value = mean(value,na.rm = TRUE))))


ggplot(data = df.rad.seasonal.month) +
  geom_line(aes(x = t,y = value,color = as.factor(month))) +
  scale_x_continuous(breaks = c(1,2,3,4)) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()

