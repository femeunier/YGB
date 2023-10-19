rm(list = ls())

library(R.utils)
library(ncdf4)
library(dplyr)
library(lubridate)
library(reshape2)

init.years <- seq(1891,1941,10)
final.years <- init.years + 9

dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/GPCC/"

df.GPCC <- data.frame()

for (ifile in seq(1,length(init.years))){

  print(ifile/length(init.years))

  cfile <- file.path(dir,paste0("full_data_monthly_v2022_",init.years[ifile],"_",final.years[ifile],"_025.nc"))
  gz.file <- paste0(cfile,".gz")

  if (file.exists(gz.file)) gunzip(paste0(cfile,".gz"))

  if (file.exists(cfile)){

    nc <- nc_open(cfile)

    lats <- ncvar_get(nc,"lat")
    lons <- ncvar_get(nc,"lon")
    times <- ncvar_get(nc,"time")

    s.lats <- which(lats <= 10 & lats >= -15)
    s.lons <- which(lons <= 45 & lons >= -10)

    e.lats <- lats[s.lats]
    e.lons <- lons[s.lons]

    prate <- ncvar_get(nc,"precip",
                       start = c(min(s.lons),min(s.lats),1),
                       count = c(length(s.lons),length(s.lats),length(times)))

    nc_close(nc)

    prate.df <- melt(prate) %>%
      mutate(lon = (e.lons)[Var1],
             lat = (e.lats)[Var2],
             time = times[Var3]) %>%
      dplyr::select(lat,lon,time,value) %>%
      rename(prate = value)

    prate.df.time <- prate.df %>%
      mutate(date = as.Date(time/24,origin = paste0(init.years[ifile],"-01-01"))) %>%
      mutate(month = month(date),
             year = year(date))

    cdf <- prate.df.time %>%
      group_by(year,month,lat,lon) %>%
      summarise(MAP = sum(prate),
                .groups = "keep")
  }

  df.GPCC <- bind_rows(list(df.GPCC,
                            cdf %>% filter(!is.na(MAP))))
}

saveRDS(df.GPCC,"./outputs/df.GPCC1.RDS")


rm(list = ls())

library(R.utils)
library(ncdf4)
library(dplyr)
library(lubridate)
library(reshape2)

init.years <- seq(1951,2011,10)
final.years <- init.years + 9

dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/GPCC/"

df.GPCC <- data.frame()

for (ifile in seq(1,length(init.years))){

  print(ifile/length(init.years))

  cfile <- file.path(dir,paste0("full_data_monthly_v2022_",init.years[ifile],"_",final.years[ifile],"_025.nc"))
  gz.file <- paste0(cfile,".gz")

  if (file.exists(gz.file)) gunzip(paste0(cfile,".gz"))

  if (file.exists(cfile)){

    nc <- nc_open(cfile)

    lats <- ncvar_get(nc,"lat")
    lons <- ncvar_get(nc,"lon")
    times <- ncvar_get(nc,"time")

    s.lats <- which(lats <= 10 & lats >= -15)
    s.lons <- which(lons <= 45 & lons >= -10)

    e.lats <- lats[s.lats]
    e.lons <- lons[s.lons]

    prate <- ncvar_get(nc,"precip",
                       start = c(min(s.lons),min(s.lats),1),
                       count = c(length(s.lons),length(s.lats),length(times)))

    nc_close(nc)

    prate.df <- melt(prate) %>%
      mutate(lon = (e.lons)[Var1],
             lat = (e.lats)[Var2],
             time = times[Var3]) %>%
      dplyr::select(lat,lon,time,value) %>%
      rename(prate = value)

    prate.df.time <- prate.df %>%
      mutate(date = as.Date(time/24,origin = paste0(init.years[ifile],"-01-01"))) %>%
      mutate(month = month(date),
             year = year(date))

    cdf <- prate.df.time %>%
      group_by(year,month,lat,lon) %>%
      summarise(MAP = sum(prate),
                .groups = "keep")
  }

  df.GPCC <- bind_rows(list(df.GPCC,
                            cdf %>% filter(!is.na(MAP))))
}

saveRDS(df.GPCC,"./outputs/df.GPCC2.RDS")

# scp /home/femeunier/Documents/projects/YGB/scripts/download.GPCC.R hpc:/data/gent/vo/000/gvo00074/felicien/R
