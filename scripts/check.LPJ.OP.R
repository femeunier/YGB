rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

data.wide <- readRDS("./data/Yoko_AGB.tot_dyn.RDS") %>%
  mutate(agb.av= agb.av/10,
         agb.sd = agb.sd/10) %>%
  rename(mean = agb.av,
         t = age.num,
         sd = agb.sd) %>%
  mutate(low = mean - sd,
         up = mean + sd)

cmass <- read.table("./outputs/MIP/LPJ-GUESS/cmass.out",header = TRUE) %>%
  dplyr::select(-Lon,-Lat) %>%
  pivot_longer(cols = -Year,
               names_to = "pft",
               values_to = "cmass")

pft2keep <- cmass %>% group_by(pft) %>% summarise(tot = sum(cmass),
                                                  .groups = "keep") %>% filter(tot > 0) %>% pull(pft)

cmass.f <- cmass %>%
  ungroup() %>%
  dplyr::filter(pft %in% pft2keep)

ggplot(data = cmass.f) +
  geom_line(aes(x = Year - 1850, y = cmass, color = pft)) +
  geom_point(data = data.wide,
             aes(x = t,
                 y = mean),
             color = "black",alpha = 0.7,
             size = 2) +
  geom_errorbar(data = data.wide,
                aes(x = t,y = mean,ymin = low,ymax = up),
                width = 1,
                color = "black",alpha = 0.7) +
  theme_bw()


LAI <- read.table("./outputs/MIP/LPJ-GUESS/lai.out",header = TRUE) %>%
  dplyr::select(-Lon,-Lat) %>%
  pivot_longer(cols = -Year,
               names_to = "pft",
               values_to = "LAI")

LAI.f <- LAI %>%
  ungroup() %>%
  dplyr::filter(pft %in% pft2keep)

ggplot(data = LAI.f) +
  geom_line(aes(x = Year, y = LAI, color = pft)) +
  theme_bw()

cpool <- read.table("./outputs/MIP/LPJ-GUESS/cpool.out",header = TRUE) %>%
  dplyr::select(-Lon,-Lat) %>%
  pivot_longer(cols = -Year,
               names_to = "pool",
               values_to = "cpool")

ggplot(data = cpool) +
  geom_line(aes(x = Year, y = cpool, color = pool)) +
  theme_bw()


cflux <- read.table("./outputs/MIP/LPJ-GUESS/cflux.out",header = TRUE) %>%
  dplyr::select(-Lon,-Lat) %>%
  pivot_longer(cols = -Year,
               names_to = "flux",
               values_to = "cflux")

ggplot(data = cflux) +
  geom_line(aes(x = Year, y = cflux, color = flux)) +
  theme_bw()
