rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(lubridate)
library(matlab)

init.years <- seq(1921,2010,1)

WD <- "/data/gent/vo/000/gvo00074/ED_common_data/met/CRUNCEP/CB"

df.CRU <- data.frame()
for (iyear in seq(1,length(init.years))){

  print(iyear/length(init.years))

    cfile <- file.path(WD,paste0("CRUNCEP.",init.years[iyear],".nc"))

    if (file.exists(cfile)){

      nc <- nc_open(cfile)
      prate <- ncvar_get(nc,"precipitation_flux")
      lats <- fliplr(as.vector(ncvar_get(nc,"latitude")))
      lons <- ncvar_get(nc,"longitude")
      times <- ncvar_get(nc,"time")

      nc_close(nc)

      prate.df <- melt(prate) %>%
        mutate(lon = (lons)[Var1],
               lat = (lats)[Var2],
               time = times[Var3]) %>%
        dplyr::select(lat,lon,time,value) %>%
        rename(prate = value)

      prate.df.time <- prate.df %>%
        mutate(date = as.Date(time,origin = paste0(init.years[iyear],"-01-01"))) %>%
        mutate(month = month(date),
               year = year(date))

      cdf <- prate.df.time %>%
        group_by(year,month,lat,lon) %>%
        summarise(MAP = sum(prate),
                  .groups = "keep")

    }

    df.CRU <- bind_rows(list(df.CRU,
                              cdf))

}

saveRDS(df.CRU,"./outputs/df.CRU.RDS")

# scp /home/femeunier/Documents/projects/YGB/scripts/summarise.precip.CRU.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

