rm(list = ls())

library(ncdf4)
library(dplyr)
library(reshape2)
library(stringr)

dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDY"
all.files <- list.files(dir,
                        "*cVeg*")

f.split <- str_split(all.files,
                     pattern = "_")

df.files <- data.frame(fname = all.files,
                       model = sapply(f.split,`[`,1),
                       scenario = sapply(f.split,`[`,2),
                       var = tools::file_path_sans_ext(sapply(f.split,`[`,3)))

df.files2analize <- df.files %>% dplyr::filter(scenario == "S3",
                                               var == "cVeg")

cVeg.df.all <- data.frame()

lat.names <- c("latitude","lat","lat_FULL")
lon.names <- c("longitude","lon","lon_FULL")

time.name <- c("time","time_counter")

for (ifile in seq(1,nrow(df.files2analize))){

  cfile <- file.path(dir,df.files2analize[["fname"]][ifile])

  nc <- nc_open(cfile)

  lats <- NULL ; i = 1
  while(is.null(lats)){
    lats <- tryCatch(ncvar_get(nc,lat.names[i]),
                     error = function(e) NULL)
    i = i +1
  }

  lons <- NULL ; i = 1
  while(is.null(lons)){
    lons <- tryCatch(ncvar_get(nc,lon.names[i]),
                     error = function(e) NULL)
    i = i +1
  }

  times <- NULL ; i = 1
  while(is.null(times)){
    times <- tryCatch(ncvar_get(nc,time.name[i]),
                     error = function(e) NULL)
    i = i +1
  }

  s.lats <- which(lats <= 10 & lats >= -15)
  s.lons <- which(lons <= 45 & lons >= -10)

  e.lats <- lats[s.lats]
  e.lons <- lons[s.lons]

  cVeg <- ncvar_get(nc,"cVeg",
                    start = c(min(s.lons),min(s.lats),1),
                    count = c(length(s.lons),length(s.lats),length(times)))

  nc_close(nc)

  cVeg.df <- melt(cVeg) %>%
    mutate(lon = (e.lons)[Var1],
           lat = (e.lats)[Var2],
           time = times[Var3]) %>%
    dplyr::select(time,lat,lon,value) %>%
    rename(cVeg = value) %>%
    mutate(yr = 1700 + time/365) %>%
    dplyr::filter(time == max(time))

  cVeg.df.all <- bind_rows(list(cVeg.df.all,
                                cVeg.df %>% mutate(model = df.files2analize[["model"]][ifile],
                                                   scenario = df.files2analize[["scenario"]][ifile])))

}

saveRDS(cVeg.df.all,"./outputs/TrENDY.cVeg.RDS")

# scp /home/femeunier/Documents/projects/YGB/scripts/summarise.Trendy.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

