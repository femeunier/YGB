rm(list = ls())

library(ncdf4)
library(dplyr)
library(stringr)
library(reshape2)
library(lubridate)

dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/MERRA2"
files.precip <- list.files(dir,"MERRA2_....tavgM_2d_lnd_Nx.*")

df.MERRA2 <- data.frame()

for (ifile in seq(1,length(files.precip))){
  print(ifile/length(files.precip))

  cfile <- file.path(dir,files.precip[ifile])

  cyear <- as.numeric(substr(files.precip[ifile],28,31))
  cmonth <- as.numeric(substr(files.precip[ifile],32,33))

  if (file.exists(cfile)){

    nc <- nc_open(cfile)

    stop()

    lats <- ncvar_get(nc,"lat")
    lons <- ncvar_get(nc,"lon")
    times <- ncvar_get(nc,"time")

    s.lats <- which(lats <= 10 & lats >= -15)
    s.lons <- which(lons <= 45 & lons >= -10)

    e.lats <- lats[s.lats]
    e.lons <- lons[s.lons]

    prate <- ncvar_get(nc,"PRECTOTLAND",
                       start = c(min(s.lons),min(s.lats),1),
                       count = c(length(s.lons),length(s.lats),length(times)))

    nc_close(nc)

    prate.df <- melt(prate) %>%
      mutate(lon = (e.lons)[Var1],
             lat = (e.lats)[Var2]) %>%
      dplyr::select(lat,lon,value) %>%
      rename(prate = value)

    prate.df.time <- prate.df %>%
      mutate(date = paste0(cyear,"-",cmonth,"-01")) %>%
      mutate(month = month(date),
             year = year(date))

    cdf <- prate.df.time %>%
      group_by(year,month,lat,lon) %>%
      summarise(MAP = sum(prate),
                .groups = "keep")
  }

  df.MERRA2 <- bind_rows(list(df.MERRA2,
                            cdf %>% filter(!is.na(MAP))))
}

saveRDS(df.MERRA2,"./outputs/df.MERRA2.RDS")

################################################################################################################

rm(list = ls())

library(ncdf4)
library(dplyr)
library(stringr)
library(reshape2)
library(lubridate)

dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/MERRA2"

df.SW.MERRA2 <- data.frame()
files.rad <- list.files(dir,"MERRA2_....tavgU_2d_lfo_Nx.")

for (ifile in seq(1,length(files.rad))){
  print(ifile/length(files.rad))

  cfile <- file.path(dir,files.rad[ifile])

  cyear <- as.numeric(substr(files.rad[ifile],28,31))
  cmonth <- as.numeric(substr(files.rad[ifile],32,33))

  if (file.exists(cfile)){

    nc <- nc_open(cfile)

    lats <- ncvar_get(nc,"lat")
    lons <- ncvar_get(nc,"lon")
    times <- ncvar_get(nc,"time")

    s.lats <- which(lats <= 10 & lats >= -15)
    s.lons <- which(lons <= 45 & lons >= -10)

    e.lats <- lats[s.lats]
    e.lons <- lons[s.lons]

    prate <- ncvar_get(nc,"SWGDN",
                       start = c(min(s.lons),min(s.lats),1),
                       count = c(length(s.lons),length(s.lats),length(times)))

    nc_close(nc)

    prate.df <- melt(prate) %>%
      mutate(lon = (e.lons)[Var1],
             lat = (e.lats)[Var2]) %>%
      dplyr::select(lat,lon,value) %>%
      rename(prate = value)

    prate.df.time <- prate.df %>%
      mutate(date = paste0(cyear,"-",cmonth,"-01")) %>%
      mutate(month = month(date),
             year = year(date))

    cdf <- prate.df.time %>%
      group_by(year,month,lat,lon) %>%
      summarise(MAP = mean(prate),
                .groups = "keep")
  }

  df.SW.MERRA2 <- bind_rows(list(df.SW.MERRA2,
                              cdf %>% filter(!is.na(MAP))))
}

saveRDS(df.SW.MERRA2,"./outputs/df.SW.MERRA2.RDS")



# scp /home/femeunier/Documents/projects/YGB/scripts/summarise.MERRA2.R hpc:/data/gent/vo/000/gvo00074/felicien/R
