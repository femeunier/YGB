rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(lubridate)

init.years <- seq(1960,2019,1)

WD <- "/data/gent/vo/000/gvo00074/ED_common_data/met/CB"

df.ERA5 <- data.frame()
for (iyear in seq(1,length(init.years))){

  print(iyear/length(init.years))


  cfile <- file.path(WD,paste0("ERA5_",init.years[iyear],".nc"))
  if (file.exists(cfile)){

    nc <- nc_open(cfile)
    prate <- ncvar_get(nc,"ssrd")
    lats <- ncvar_get(nc,"latitude")
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
      mutate(date = as.Date(time/24,origin = "1900-01-01")) %>%
      mutate(month = month(date),
             year = year(date))

    cdf <- prate.df.time %>%
      group_by(year,month,lat,lon) %>%
      summarise(MAP = mean(prate)/3600,
                .groups = "keep")
  }

  df.ERA5 <- bind_rows(list(df.ERA5,
                            cdf %>% rename(sw = MAP)))
}

saveRDS(df.ERA5,"./outputs/df.SW.ERA5.RDS")

# scp /home/femeunier/Documents/projects/YGB/scripts/summarise.SW.ERA5.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

