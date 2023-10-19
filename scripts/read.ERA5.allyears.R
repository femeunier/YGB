rm(list = ls())

library(ncdf4)
library(ggplot2)
library(dplyr)
library(reshape2)
library(lubridate)

mask <- readRDS("./data/LandSeaMask.RDS")
# system2("scp",paste("./data/LandSeaMask.RDS","hpc:/data/gent/vo/000/gvo00074/felicien/R/data/"))

years <- 1960:2019

df.climate <- data.frame()
df.climate.month <- data.frame()

for (iyear in seq(1,length(years))){

  print("====================================")
  print(years[iyear])

  nc.file <- file.path("/data/gent/vo/000/gvo00074/ED_common_data/met/CB/",paste0("ERA5_",years[iyear],".nc"))

  nc <- nc_open(nc.file)

  lats <- ncvar_get(nc,"latitude")
  lons <- ncvar_get(nc,"longitude")
  times <- ncvar_get(nc,"time")

  var.names <- c("tp","t2m","ssrd")

  for (ivar.name in seq(1,length(var.names))){

    cvar.name <- var.names[ivar.name]
    cVar <- ncvar_get(nc,cvar.name)

    if (ivar.name == 1){
      df <- melt(cVar) %>%
        mutate(lon = (lons)[Var1],
               lat = (lats)[Var2],
               time = times[Var3]) %>%
        dplyr::select(lat,lon,time,value) %>%
        rename(!!cvar.name := value)
    } else{
      df <- df %>% mutate(!!cvar.name := as.vector(cVar))
    }
  }

  # stop()
  nc_close(nc)

  df.date <- df %>% left_join(mask,
                         by = c("lat","lon")) %>%
    filter(mask == 1) %>%
    mutate(date = as.POSIXlt(time*3600,origin = "1900-01-01 00:00:00")) %>%
    mutate(yr = year(date),
           m = month(date),
           lon = case_when(lon > 180 ~ (lon -360),
                           TRUE ~ lon))

  df.yr <- df.date %>% group_by(lat,lon) %>%
    summarise(MAP = sum(tp)*1000*3,  # because hourly but only one third downloaded
              MAT = mean(t2m - 273.15),
              SW = mean(ssrd)/3600,
              .groups = "keep") %>%
    mutate(MAP.bnd = case_when(MAP >= 3000 ~ 3000,
                               TRUE ~ MAP))


  df.yr.month <- df.date %>% group_by(lat,lon,m) %>%
    summarise(MAP = sum(tp)*1000*3,  # because hourly but only one third downloaded
              MAT = mean(t2m - 273.15),
              SW = mean(ssrd)/3600,
              .groups = "keep") %>%
    mutate(MAP.bnd = case_when(MAP >= 3000 ~ 3000,
                               TRUE ~ MAP))

  df.climate <- bind_rows(list(df.climate,
                               df.yr %>% mutate(year = years[iyear])))

  df.climate.month <- bind_rows(list(df.climate.month,
                                     df.yr.month %>% mutate(year = years[iyear])))

}

saveRDS(df.climate,"./outputs/climate.CB.RDS")
saveRDS(df.climate.month,"./outputs/climate.CB.month.RDS")

# scp /home/femeunier/Documents/projects/YGB/scripts/read.ERA5.allyears.R hpc:/data/gent/vo/000/gvo00074/felicien/R
