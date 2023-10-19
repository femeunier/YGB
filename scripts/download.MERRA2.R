rm(list = ls())

library(ncdf4)

# nc <- nc_open(ncfile <- "/home/femeunier/Downloads/MERRA2_100.tavg1_2d_lnd_Nx.19800101.nc4")
# nc <- nc_open(ncfile <- "/home/femeunier/Downloads/MERRA2_100.tavgU_2d_lfo_Nx.198001.nc4")

args <- "-load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition"
dir <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2_MONTHLY/M2TMNXLND.5.12.4/"
dir2<- "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2_DIURNAL/M2TUNXLFO.5.12.4/"

years <- 2011:2020
months <- 1:12

setwd("/data/gent/vo/000/gvo00074/ED_common_data/met/MERRA2")

for (iyear in seq(1,length(years))){
  print(iyear/length(years))
  for (imonth in seq(1,length(months))){

    file.name <- paste0("MERRA2_400.tavgM_2d_lnd_Nx.",years[iyear],sprintf("%02d",months[imonth]),".nc4")
    file2.name <- paste0("MERRA2_400.tavgU_2d_lfo_Nx.",years[iyear],sprintf("%02d",months[imonth]),".nc4")

    cfile <- file.path(dir,years[iyear],file.name)
    cfile2 <- file.path(dir2,years[iyear],file2.name)

    system2("wget",paste(args,cfile))
    system2("wget",paste(args,cfile2))
  }
}

# scp /home/femeunier/Documents/projects/YGB/scripts/download.MERRA2.R hpc:/data/gent/vo/000/gvo00074/felicien/R






# prate <- ncvar_get(nc,"PRECTOTLAND")
# SW <- ncvar_get(nc,"SWGDN")
#
# lats <- ncvar_get(nc,"lat")
# lons <- ncvar_get(nc,"lon")
# times <- ncvar_get(nc,"time")
#
# nc_close(nc)
