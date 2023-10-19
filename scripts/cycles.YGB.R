rm(list = ls())

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/TS_YGB.RDS","./data/TS_YGB.RDS"))

TS_YGB <- readRDS("./data/TS_YGB.RDS")[[1]]

df.YGB <- as.data.frame(TS_YGB) %>%
  tibble::rownames_to_column(var = "time") %>%
  mutate(time = as.numeric(time)) %>%
  mutate(t0 = as.POSIXct( (time - 3600) , origin = '1970-01-01 00:00:00'))%>%
  mutate(year = year(t0),
         month = month(t0),
         day = day(t0),
         h = hour(t0))

df.YGB.year <- df.YGB %>%
  group_by(year) %>%
  summarise(t2m = mean(t2m) -273.15,
          sp = mean(sp),
          d2m = mean(d2m) -273.15,
          tp = sum(tp)* 1000*3,
          u10 = mean(u10),
          v10 = mean(v10),
          ssrd = mean(ssrd)/ (1 * 3600),
          strd = mean(strd) / (1 * 3600)) %>%
  mutate(RH = ((112 - (0.1 * t2m) + d2m) / (112 + (0.9 * t2m)))**8) %>%
  dplyr::select(-d2m)

df.YGB.year.long <- df.YGB.year %>%
  pivot_longer(cols = -year,
               names_to = "var",
               values_to = "value")

ggplot(data = df.YGB.year.long) +
  geom_line(aes(x = year, y = value)) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()

df.YGB.month <- df.YGB %>%
  group_by(year,month) %>%
  summarise(t2m = mean(t2m) -273.15,
            sp = mean(sp),
            d2m = mean(d2m) -273.15,
            tp = sum(tp)* 1000*3,
            u10 = mean(u10),
            v10 = mean(v10),
            ssrd = mean(ssrd)/ (1 * 3600),
            strd = mean(strd) / (1 * 3600),
            .groups = "keep") %>%
  mutate(RH = ((112 - (0.1 * t2m) + d2m) / (112 + (0.9 * t2m)))**8) %>%
  dplyr::select(-d2m) %>%
  group_by(month) %>%
  summarise(t2m = mean(t2m),
            sp = mean(sp),
            RH = mean(RH),
            tp = mean(tp),
            u10 = mean(u10),
            v10 = mean(v10),
            ssrd = mean(ssrd),
            strd = mean(strd),
            .groups = "keep") %>%
  pivot_longer(cols = -month,
               values_to = "value",
               names_to = "var")

ggplot(data = df.YGB.month) +
  geom_line(aes(x = month, y = value)) +
  facet_wrap(~ var,scales = "free") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  theme_bw()
