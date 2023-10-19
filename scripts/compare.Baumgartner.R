rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

data.Baumgartner <- readRDS(file = "/home/femeunier/Documents/projects/YGB/data/Baumgartner_et_al_2020/Baumgartner.RDS")

data.Baumgartner.long <- data.Baumgartner %>% pivot_longer(cols = -c(month),
                                                           names_to = "var",
                                                           values_to = "value") %>%
  mutate(var.name = sub("\\..*", "",var),
         var.type = sub(".*\\.", "",var)) %>% dplyr::select(-var) %>%
  pivot_wider(names_from = var.type,
              values_from = value)

ggplot(data = data.Baumgartner.long,
       aes(x = month,y = m,ymin = m - sd, ymax = m + sd)) +
  geom_point() +
  geom_errorbar() +
  facet_wrap(~ var.name, scales = "free") +
  theme_bw()

ggplot(data = data.Baumgartner,
       aes(x = soilT.m,xmin = soilT.m - soilT.sd, xmax = soilT.m + soilT.sd,
           y = fco2.m,ymin = fco2.m - fco2.sd, ymax = fco2.m + fco2.sd,
           color = as.factor(month))) +
  geom_point() +
  geom_errorbar() +
  geom_errorbarh() +
  stat_smooth(method = "lm", col = "grey",alpha = 0.2) +
  theme_bw()

summary(lm(data = data.Baumgartner, formula = fco2.m ~ soilT.m))

ggplot(data = data.Baumgartner,
       aes(x = VWC.m, xmin = VWC.m - VWC.sd, xmax = VWC.m + VWC.sd,
           y = fco2.m,ymin = fco2.m - fco2.sd, ymax = fco2.m + fco2.sd,
           color = as.factor(month))) +
  geom_point() +
  geom_errorbar() +
  geom_errorbarh() +
  stat_smooth(method = "lm", col = "grey",alpha = 0.2) +
  theme_bw()

summary(lm(data = data.Baumgartner, formula = fco2.m ~ soilT.m + VWC.m))


system2("rsync",
        c("-avz","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/analy/YGB_new.RData",
          "/home/femeunier/Documents/projects/YGB/outputs/"))


file <- "/home/femeunier/Documents/projects/YGB/outputs/YGB_new.RData"
load(file)

z = 16
mod <- data.frame(year = datum$year,
                  month = datum$month,
                  VWC = datum$emean$soil.water[,z],
                  soilT = datum$emean$soil.temp[,z],
                  fco2 = datum$emean$soil.resp*1/(1e-6*12/1000*86400*365)) %>% filter(year > min(year)) %>% group_by(month) %>%
  summarise(VWC.m = mean(VWC),
            VWC.sd = sd(VWC),
            soilT.m = mean(soilT),
            soilT.sd = sd(soilT),
            fco2.m = mean(fco2),
            fco2.sd = sd(fco2)) %>% pivot_longer(cols = -c(month),
                                                 names_to = "var",
                                                 values_to = "value") %>%
  mutate(var.name = sub("\\..*", "",var),
         var.type = sub(".*\\.", "",var)) %>% dplyr::select(-var) %>%
  pivot_wider(names_from = var.type,
              values_from = value)

ggplot(data = data.Baumgartner.long,
       aes(x = (month),y = m,ymin = m - sd, ymax = m + sd)) +
  geom_point() +
  geom_errorbar() +
  geom_ribbon(data = mod, fill = "darkgrey",alpha = 0.5,color = "darkgrey") +
  facet_wrap(~ var.name, scales = "free") +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                              "J","A","S","O","N","D")) +
  labs(x = '', y = '') +
  theme_bw()

ggsave(filename = "./Figures/variables.cycle.png",
       plot = last_plot(),
       width = 30, height = 15, dpi = 300, unit = "cm")



SoilC.pools <- data.frame(year = datum$year,
                          month = datum$month,
                          SSC = datum$emean$slow.soil.c,
                          StuctSC = datum$emean$struct.soil.c,
                          StuctSC2 = datum$emean$struct.grnd.c,
                          FSC = datum$emean$fast.soil.c,
                          FSC2 = datum$emean$fast.grnd.c,
                          totalC = datum$emean$slow.soil.c + datum$emean$struct.soil.c +
                            datum$emean$struct.grnd.c + datum$emean$fast.grnd.c) %>% filter(year >= min(year)) %>% pivot_longer(cols = -c(year, month),
                                                                                                                                      names_to = "var",
                                                                                                                                      values_to = "value")

ggplot(data = SoilC.pools) +
  geom_line(aes(x = year + month/12,y = value, color = var)) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

plot(datum$emean$soil.temp[,16],type = 'l')
