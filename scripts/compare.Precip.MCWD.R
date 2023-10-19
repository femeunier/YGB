rm(list = ls())

library(terra)
library(lubridate)
library(ncdf4)

##########################################################################################"
# 3B43

years <- 1998:2019
months <- 1:12
WD <- "/home/femeunier/Documents/projects/YGB/data/Precip/3B43"

df.3B43 <- data.frame()
for (iyear in seq(1,length(years))){

  print(iyear/length(years))

  for (imonth in seq(1,length(months))){

    cfile <- file.path(WD,paste0("3B43.",years[iyear],sprintf("%02d",months[imonth]),"01.7A.HDF"))
    if (!file.exists(cfile)){
      cfile <- file.path(WD,paste0("3B43.",years[iyear],sprintf("%02d",months[imonth]),"01.7.HDF"))
    }

    if (file.exists(cfile)){
      x <- rast(cfile)
      y <- t(x)
      z <- flip(y, "v")
      ext(z) <- c(-180, 180, -50, 50)

      # My understanding is that the first layer is "precipitation" (mm/hr),
      # the second the "relative error", and the third the "gaugeRelativeWeighting".

    }

    df.3B43 <- bind_rows(list(df.3B43,
                              as.data.frame(z[[1]],xy = TRUE) %>%
                                rename(lon = x, lat = y,p = `0`) %>%
                                filter(lat <= 10,lat>= -15,lon <= 45,lon>= -10) %>%
                                mutate(year = years[iyear],
                                       month = months[imonth])))

  }
}



df.3B43 <- df.3B43 %>% mutate(total.h = 24*days_in_month(as.Date(paste(year,month,"01",sep = "-"),
                                                                 "%Y-%m-%d"))) %>%
  mutate(Pmm = p*total.h)


df.3B43.sum <- df.3B43 %>%
  group_by(lat,lon,year) %>%
  summarise(MAP = sum(Pmm),
            .groups = "keep") %>%
  group_by(lat,lon) %>%
  summarise(MAP.m = mean(MAP),
            .groups = "keep")

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_tile(data = df.3B43.sum,
            aes(x = lon, y = lat,fill = MAP.m),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "blue",na.value = "transparent")+
  labs(x = "",y = "") +
  theme_bw()


YGB <- df.3B43 %>% mutate(dist = sqrt((lat - 0.3)**2 + (lon - 24.8)**2)) %>%
  filter(dist == min(dist))

YGB.sum <- YGB %>% group_by(month) %>%
  summarise(P.m = mean(Pmm),
            P.sd = sd(Pmm),
            .groups = "keep")


ggplot(data = YGB) +
  geom_line(aes(x = month, y = Pmm, group = as.factor(year)), color = "black", size = 0.2) +
  geom_line(data = YGB.sum,
              aes(x = month, y = P.m), color = "blue", size = 2) +
  geom_ribbon(data = YGB.sum,
              aes(x = month, y = P.m, ymin = P.m - P.sd, ymax = P.m + P.sd), color = NA, alpha = 0.4) +
  theme_bw()

YGB %>% group_by(year) %>% summarise(MAP = sum(Pmm)) %>% pull(MAP) %>% summary()


##########################################################################################
# ERA5

years <- 1960:2019
months <- c("JAN","FEB","MAR","APR","MAY","JUN",
            "JUL","AUG","SEP","OCT","NOV","DEC")

WD <- "/home/femeunier/Documents/projects/YGB/data/Precip/ERA5/"

df.ERA5 <- data.frame()
for (iyear in seq(1,length(years))){

  print(iyear/length(years))

  for (imonth in seq(1,length(months))){

    cfile <- file.path(WD,paste0(years[iyear],months[imonth],".h5"))

    if (file.exists(cfile)){

      nc <- nc_open(cfile)
      prate <- ncvar_get(nc,"prate")

      nc_close(nc)
      cdf <- data.frame(Pmm = sum(prate*3600*3),
                        year = years[iyear],
                        month = imonth)
    }

    df.ERA5 <- bind_rows(list(df.ERA5,
                              cdf))

  }
}

df.ERA5.sum <- df.ERA5 %>% group_by(month) %>%
  summarise(P.m = mean(Pmm),
            P.sd = sd(Pmm),
            .groups = "keep")

ggplot(data = df.ERA5) +
  geom_line(aes(x = month, y = Pmm, group = as.factor(year)), color = "black", size = 0.2) +
  geom_line(data = df.ERA5.sum,
            aes(x = month, y = P.m), color = "blue", size = 2) +
  geom_ribbon(data = df.ERA5.sum,
              aes(x = month, y = P.m, ymin = P.m - P.sd, ymax = P.m + P.sd), color = NA, alpha = 0.4) +
  theme_bw()


##########################################################################################
# CRU

years <- 1931:2009
months <- c("JAN","FEB","MAR","APR","MAY","JUN",
            "JUL","AUG","SEP","OCT","NOV","DEC")

WD <- "/home/femeunier/Documents/projects/YGB/data/Precip/CRU/"

df.CRU <- data.frame()
for (iyear in seq(1,length(years))){

  print(iyear/length(years))

  for (imonth in seq(1,length(months))){

    cfile <- file.path(WD,paste0(years[iyear],months[imonth],".h5"))

    if (file.exists(cfile)){

      nc <- nc_open(cfile)
      prate <- ncvar_get(nc,"prate")

      nc_close(nc)
      cdf <- data.frame(Pmm = sum(prate*3600*5),
                        year = years[iyear],
                        month = imonth)
    }

    df.CRU <- bind_rows(list(df.CRU,
                              cdf))

  }
}

df.CRU.sum <- df.CRU %>% group_by(month) %>%
  summarise(P.m = mean(Pmm),
            P.sd = sd(Pmm),
            .groups = "keep")

ggplot(data = df.CRU) +
  geom_line(aes(x = month, y = Pmm, group = as.factor(year)), color = "black", size = 0.2) +
  geom_line(data = df.CRU.sum,
            aes(x = month, y = P.m), color = "blue", size = 2) +
  geom_ribbon(data = df.CRU.sum,
              aes(x = month, y = P.m, ymin = P.m - P.sd, ymax = P.m + P.sd), color = NA, alpha = 0.4) +
  theme_bw()

########################################################################################################

df.all <- bind_rows(list(df.CRU %>% mutate(data = "CRU"),
                         df.ERA5 %>% mutate(data = "ERA5"),
                         YGB %>% dplyr::select(year,month,Pmm) %>%
                           mutate(data = "3B43")))

df.all.sum <- bind_rows(list(df.CRU.sum %>% mutate(data = "CRU"),
                             df.ERA5.sum %>% mutate(data = "ERA5"),
                             YGB.sum %>% mutate(data = "3B43")))

ggplot(data = df.all) +
  geom_line(aes(x = month, y = Pmm, group = as.factor(year)), color = "black", size = 0.1) +
  geom_line(data = df.all.sum,
            aes(x = month, y = P.m), color = "blue", size = 2) +
  geom_ribbon(data = df.all.sum,
              aes(x = month, y = P.m, ymin = P.m - P.sd, ymax = P.m + P.sd), color = NA, alpha = 0.4) +
  facet_wrap(~ data) +
  theme_bw()

df.all %>% group_by(data,year) %>%
  summarise(MAP = sum(Pmm),
            .groups = "keep") %>%
  group_by(data) %>%
  summarise(MAP.m = mean(MAP))

df.all.pval <- df.all %>%
  group_by(data,month) %>%
  mutate(p.value = coef(summary(lm(Pmm ~ year, data = cur_data())))[2,4],
            .groups = "keep")

ggplot(data = df.all,
       aes(x = year, y = Pmm, color = as.factor(month))) +
  geom_point(size = 0.2) +
  # stat_smooth(method = "lm",se = FALSE,
  #             linetype = 3) +
  stat_smooth(data = df.all.pval %>% filter(p.value < 0.05),
              method = "lm",se = FALSE) +
  facet_wrap(~ data, scales = "free_x") +
  theme_bw()


#################################################################################
# Calculate MCWD

# Single site

tmp <- df.all %>% filter(data == "CRU") %>%
  group_by(month) %>%
  summarise(Pmm = mean(Pmm)) %>%
  ungroup() %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = 3.33,
         Etot = E*Ndays) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD))


df.MCWD <- df.3B43 %>%
  dplyr::select(lon,lat,Pmm,year,month) %>%
  group_by(month,lon,lat) %>%
  summarise(Pmm = mean(Pmm),
            .groups = "keep") %>%
  group_by(lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = 3.33,
         Etot = E*Ndays) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD))

ggplot(data = world) +
  geom_tile(data = df.MCWD,
            aes(x = lon, y = lat,fill = MCWD),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "red",high = "white",na.value = "transparent")+
  labs(x = "",y = "") +
  theme_bw()

hist(df.MCWD$MCWD)

saveRDS(object = df.3B43,file = "./data/Precip/df.3B43.RDS")
saveRDS(object = df.MCWD,file = "./data/MCWD//df.MCWD.RDS")
