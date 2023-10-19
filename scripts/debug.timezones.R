rm(list = ls())

library(ncdf4)
library(dplyr)
library(reshape2)
library(lubridate)
library(xts)
library(PEcAn.ED2)

load("/home/femeunier/Documents/Figures/analysis.RData")

plot(datum$emean$rshort[1:12], type = "l",ylim = c(0,350))
lines(datum$emean$rshort[13:24], type = "l",col = "red")
###############################################################


WD <- "/home/femeunier/Documents/projects/YGB/data/SW/ERA5/"

years <- 1961:1961
months <- c("JAN","FEB","MAR","APR","MAY","JUN",
            "JUL","AUG","SEP","OCT","NOV","DEC")

df.ERA5 <- data.frame()
for (iyear in seq(1,length(years))){

  print(iyear/length(years))

  for (imonth in seq(1,length(months))){

    cfile <- file.path(WD,paste0(years[iyear],months[imonth],".h5"))

    if (file.exists(cfile)){

      nc <- nc_open(cfile)

      nbdsf <- ncvar_get(nc,"nbdsf")
      nddsf <- ncvar_get(nc,"nddsf")
      vbdsf <- ncvar_get(nc,"vbdsf")
      vddsf <- ncvar_get(nc,"vddsf")

      nc_close(nc)

      cdf <- data.frame(sw = (nbdsf+nddsf+vbdsf+vddsf),
                        year = years[iyear],
                        month = imonth)
    }

    df.ERA5 <- bind_rows(list(df.ERA5,
                              cdf))

  }
}

df.ERA5 %>% filter(month == 1)

# 1   -3.852720e-15 1961     1
# 2   -3.852720e-15 1961     1
# 3    1.176492e+02 1961     1
# 4    4.786426e+02 1961     1
# 5    6.544619e+02 1961     1
# 6    0.000000e+00 1961     1
# 7   -3.852720e-15 1961     1
# 8   -3.852720e-15 1961     1



df.ERA5.sum <- df.ERA5 %>% group_by(year,month) %>%
  summarise(sw = mean(sw))
lines(df.ERA5.sum$month,df.ERA5.sum$sw,type = "l",col = "red")

# plot(df.ERA5$sw/datum$emean$rshort[1:12])

cfile <- "./data/SW/ERA5/ERA5_1961.nc"
nc <- nc_open(cfile)

lat <- ncvar_get(nc,"latitude")
pos.lat <- which.min((lat-(0))**2)
lon <- ncvar_get(nc,"longitude")
pos.lon <- which.min((lon-25)**2)
time <- as.Date(ncvar_get(nc,"time")/24,origin = "1900-01-01 00:00:00.0")
ssrd <- ncvar_get(nc,"ssrd")

nc_close(nc)

ssrd.df <- melt(ssrd[pos.lon,pos.lat,]) %>%
  mutate(t = time) %>%
  mutate(month = month(t),
         day = day(t)) %>%
  mutate(ssrd = value/3600)


ssrd.df.sum <- ssrd.df %>% group_by(month) %>%
  summarise(sw = mean(ssrd))

lines(1:12,ssrd.df.sum$sw,col = "blue")

plot(datum$emean$rshort[1:12]/ssrd.df.sum$sw,type = "l")

ts <- readRDS("/home/femeunier/Documents/projects/YGB/data/ERA5_lat_9_lon_45_1//ERA5.RDS")
m = month((index(ts)))
y = year(index(ts))

TS <- data.frame(month = m[y == 1961],
           ssrd = ts[y == 1961,"ssrd"]/3600) %>% group_by(month) %>%
  summarise(sw = mean(ssrd))

plot(ts[1:8,"ssrd"]/3600)
# plot(PEcAn.data.atmosphere::cos_solar_zenith_angle(rep(1,8),
#                                                    -15, 12, 10800, seq(0,21,3)-1.5),
#      type = "l")
# SW <- as.numeric(ts[1:8,"ssrd"]/3600)

lines(TS$month,TS$sw, col = "green")

# Black = datum
# red = driver
# blue = ERA5 initial file
# green = era5

plot(ssrd.df.sum$sw/datum$emean$rshort[1:12])

df.SW <- ssrd.df %>% mutate(doy = yday(t))
SW <- df.SW$ssrd

cosz <- PEcAn.data.atmosphere::cos_solar_zenith_angle(df.SW$doy,
                                                      0, 25,86400/8 , rep(seq(0,23,3),365))
rpot <- 1366 * cosz
rpot <- rpot[1:length(SW)]
SW[rpot < SW] <- rpot[rpot < SW]
frac <- SW/rpot
frac[frac > 0.9] <- 0.9
frac[frac < 0] <- 0
frac[is.na(frac)] <- 0
frac[is.nan(frac)] <- 0

SWd <- SW * (1 - frac)
nbdsfA <- (SW - SWd) * 0.57
nddsfA <- SWd * 0.48
vbdsfA <- (SW - SWd) * 0.43
vddsfA <- SWd * 0.52

plot((nbdsfA + nddsfA + vbdsfA + vddsfA),
     SW)


#########################################################################

in.path =  "/home/femeunier/Documents/projects/YGB/data/ERA5_lat_0_lon_25_1"
in.prefix = "ERA5.1"
outfolder = "/home/femeunier/Documents/projects/YGB/data/ERA5_lat_0_lon_25_1/ED2"
start_date = "1961-01-01"
end_date = "1961-12-31"
lst = 0
lat = 0
lon = 25
overwrite = TRUE
verbose = FALSE
leap_year = TRUE

##########################################################################


overwrite <- as.logical(overwrite)
start_date <- as.POSIXlt(start_date, tz = "UTC")
end_date <- as.POSIXlt(end_date, tz = "UTC")
met_folder <- outfolder
met_header_file <- file.path(met_folder, "ED_MET_DRIVER_HEADER")
results <- data.frame(file = met_header_file, host = PEcAn.remote::fqdn(),
                      mimetype = "text/plain", formatname = "ed.met_driver_header files format",
                      startdate = start_date, enddate = end_date, dbfile.name = "ED_MET_DRIVER_HEADER",
                      stringsAsFactors = FALSE)
dir.create(met_folder, recursive = TRUE, showWarnings = FALSE)
dm <- c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305,
        335, 366)
dl <- c(0, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306,
        336, 367)
month <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL",
           "AUG", "SEP", "OCT", "NOV", "DEC")
mon_num <- c("01", "02", "03", "04", "05", "06", "07", "08",
             "09", "10", "11", "12")
day2mo <- function(year, day, leap_year) {
  mo <- rep(NA, length(day))
  if (!leap_year) {
    mo <- findInterval(day, dm)
    return(mo)
  }
  else {
    leap <- lubridate::leap_year(year)
    mo[leap] <- findInterval(day[leap], dl)
    mo[!leap] <- findInterval(day[!leap], dm)
    return(mo)
  }
}
start_year <- lubridate::year(start_date)
end_year <- lubridate::year(end_date)
day_secs <- udunits2::ud.convert(1, "day", "seconds")
for (year in start_year:end_year) {
  ncfile <- file.path(in.path, paste(in.prefix, year, "nc",
                                     sep = "."))
  if (!file.exists(ncfile)) {
    PEcAn.logger::logger.severe("Input file ", ncfile,
                                "(year ", year, ") ", "not found.")
  }
  nc <- ncdf4::nc_open(ncfile)
  flat <- try(ncdf4::ncvar_get(nc, "latitude"), silent = TRUE)
  if (!is.numeric(flat)) {
    flat <- nc$dim[[1]]$vals[1]
  }
  if (is.na(lat)) {
    lat <- flat
  }
  else if (lat != flat) {
    PEcAn.logger::logger.warn("Latitude does not match that of file",
                              lat, "!=", flat)
  }
  flon <- try(ncdf4::ncvar_get(nc, "longitude"), silent = TRUE)
  if (!is.numeric(flon)) {
    flat <- nc$dim[[2]]$vals[1]
  }
  if (is.na(lon)) {
    lon <- flon
  }
  else if (lon != flon) {
    PEcAn.logger::logger.warn("Longitude does not match that of file",
                              lon, "!=", flon)
  }
  lat <- eval(parse(text = lat))
  lon <- eval(parse(text = lon))
  sec <- nc$dim$time$vals
  Tair <- ncdf4::ncvar_get(nc, "air_temperature")
  Qair <- ncdf4::ncvar_get(nc, "specific_humidity")
  U <- try(ncdf4::ncvar_get(nc, "eastward_wind"), silent = TRUE)
  V <- try(ncdf4::ncvar_get(nc, "northward_wind"), silent = TRUE)
  Rain <- ncdf4::ncvar_get(nc, "precipitation_flux")
  pres <- ncdf4::ncvar_get(nc, "air_pressure")
  SW <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")
  LW <- ncdf4::ncvar_get(nc, "surface_downwelling_longwave_flux_in_air")
  CO2 <- try(ncdf4::ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air"),
             silent = TRUE)
  use_UV <- is.numeric(U) & is.numeric(V)
  if (!use_UV) {
    U <- try(ncdf4::ncvar_get(nc, "wind_speed"), silent = TRUE)
    if (is.numeric(U)) {
      PEcAn.logger::logger.info("eastward_wind and northward_wind are absent, using wind_speed to approximate eastward_wind")
      V <- rep(0, length(U))
    }
    else {
      PEcAn.logger::logger.severe("No eastward_wind and northward_wind or wind_speed in the met data")
    }
  }
  useCO2 <- is.numeric(CO2)
  sec <- udunits2::ud.convert(sec, unlist(strsplit(nc$dim$time$units,
                                                   " "))[1], "seconds")
  ncdf4::nc_close(nc)
  dt <- PEcAn.utils::seconds_in_year(year, leap_year)/length(sec)
  toff <- -as.numeric(lst) * 3600/dt
  slen <- seq_along(SW)
  Tair <- c(rep(Tair[1], toff), Tair)[slen]
  Qair <- c(rep(Qair[1], toff), Qair)[slen]
  U <- c(rep(U[1], toff), U)[slen]
  V <- c(rep(V[1], toff), V)[slen]
  Rain <- c(rep(Rain[1], toff), Rain)[slen]
  pres <- c(rep(pres[1], toff), pres)[slen]
  SW <- c(rep(SW[1], toff), SW)[slen]
  LW <- c(rep(LW[1], toff), LW)[slen]
  if (useCO2) {
    CO2 <- c(rep(CO2[1], toff), CO2)[slen]
  }
  skip <- FALSE
  nyr <- floor(length(sec) * dt/86400/365)
  yr <- NULL
  doy <- NULL
  hr <- NULL
  asec <- sec
  for (y in seq(year, year + nyr - 1)) {
    diy <- PEcAn.utils::days_in_year(y, leap_year)
    ytmp <- rep(y, udunits2::ud.convert(diy/dt, "days",
                                        "seconds"))
    dtmp <- rep(seq_len(diy), each = day_secs/dt)
    if (is.null(yr)) {
      yr <- ytmp
      doy <- dtmp
      hr <- rep(NA, length(dtmp))
    } else {
      yr <- c(yr, ytmp)
      doy <- c(doy, dtmp)
      hr <- c(hr, rep(NA, length(dtmp)))
    }
    rng <- length(doy) - length(ytmp):1 + 1
    if (!all(rng >= 0)) {
      skip <- TRUE
      PEcAn.logger::logger.warn(year, " is not a complete year and will not be included")
      break
    }
    asec[rng] <- asec[rng] - asec[rng[1]]
    hr[rng] <- (asec[rng] - (dtmp - 1) * day_secs)/day_secs *
      24
  }
  mo <- day2mo(yr, doy, leap_year)
  if (length(yr) < length(sec)) {
    rng <- (length(yr) + 1):length(sec)
    if (!all(rng >= 0)) {
      skip <- TRUE
      PEcAn.logger::logger.warn(paste(year, "is not a complete year and will not be included"))
      break
    }
    yr[rng] <- rep(y + 1, length(rng))
    doy[rng] <- rep(1:366, each = day_secs/dt)[1:length(rng)]
    hr[rng] <- rep(seq(0, length = day_secs/dt, by = dt/day_secs *
                         24), 366)[1:length(rng)]
  }
  if (skip) {
    print("Skipping to next year")
    next
  }
  cosz <- PEcAn.data.atmosphere::cos_solar_zenith_angle(doy,
                                                        lat, lon, dt, hr)
  stop()
  rpot <- 1366 * cosz
  rpot <- rpot[1:length(SW)]
  SW[rpot < SW] <- rpot[rpot < SW]
  frac <- SW/rpot
  frac[frac > 0.9] <- 0.9
  frac[frac < 0] <- 0
  frac[is.na(frac)] <- 0
  frac[is.nan(frac)] <- 0
  SWd <- SW * (1 - frac)
  n <- length(Tair)
  nbdsfA <- (SW - SWd) * 0.57
  nddsfA <- SWd * 0.48
  vbdsfA <- (SW - SWd) * 0.43
  vddsfA <- SWd * 0.52
  prateA <- Rain
  dlwrfA <- LW
  presA <- pres
  hgtA <- rep(50, n)
  ugrdA <- U
  vgrdA <- V
  shA <- Qair
  tmpA <- Tair
  if (useCO2) {
    co2A <- CO2 * 1e+06
  }
  for (y in year + 1:nyr - 1) {
    sely <- which(yr == y)
    for (m in unique(mo[sely])) {
      selm <- sely[which(mo[sely] == m)]
      mout <- paste(met_folder, "/", y, month[m], ".h5",
                    sep = "")
      if (file.exists(mout)) {
        if (overwrite) {
          file.remove(mout)
          ed_met_h5 <- hdf5r::H5File$new(mout)
        }
        else {
          PEcAn.logger::logger.warn("The file already exists! Moving to next month!")
          next
        }
      }
      else {
        ed_met_h5 <- hdf5r::H5File$new(mout)
      }
      dims <- c(length(selm), 1, 1)
      nbdsf <- array(nbdsfA[selm], dim = dims)
      nddsf <- array(nddsfA[selm], dim = dims)
      vbdsf <- array(vbdsfA[selm], dim = dims)
      vddsf <- array(vddsfA[selm], dim = dims)
      prate <- array(prateA[selm], dim = dims)
      dlwrf <- array(dlwrfA[selm], dim = dims)
      pres <- array(presA[selm], dim = dims)
      hgt <- array(hgtA[selm], dim = dims)
      ugrd <- array(ugrdA[selm], dim = dims)
      vgrd <- array(vgrdA[selm], dim = dims)
      sh <- array(shA[selm], dim = dims)
      tmp <- array(tmpA[selm], dim = dims)
      if (useCO2) {
        co2 <- array(co2A[selm], dim = dims)
      }
      ed_met_h5[["nbdsf"]] <- nbdsf
      ed_met_h5[["nddsf"]] <- nddsf
      ed_met_h5[["vbdsf"]] <- vbdsf
      ed_met_h5[["vddsf"]] <- vddsf
      ed_met_h5[["prate"]] <- prate
      ed_met_h5[["dlwrf"]] <- dlwrf
      ed_met_h5[["pres"]] <- pres
      ed_met_h5[["hgt"]] <- hgt
      ed_met_h5[["ugrd"]] <- ugrd
      ed_met_h5[["vgrd"]] <- vgrd
      ed_met_h5[["sh"]] <- sh
      ed_met_h5[["tmp"]] <- tmp
      if (useCO2) {
        ed_met_h5[["co2"]] <- co2
      }
      ed_met_h5$close_all()
    }
  }
  metvar <- c("nbdsf", "nddsf", "vbdsf", "vddsf", "prate",
              "dlwrf", "pres", "hgt", "ugrd", "vgrd", "sh", "tmp",
              "co2")
  metvar_table <- data.frame(variable = metvar, update_frequency = dt,
                             flag = 1)
  if (!useCO2) {
    metvar_table[metvar_table$variable == "co2", c("update_frequency",
                                                   "flag")] <- list(380, 4)
  }
  ed_metheader <- list(list(path_prefix = met_folder, nlon = 1,
                            nlat = 1, dx = 1, dy = 1, xmin = lon, ymin = lat,
                            variables = metvar_table))
  #check_ed_metheader(ed_metheader)
  write_ed_metheader(ed_metheader, met_header_file, header_line = shQuote("Made_by_PEcAn_met2model.ED2"))
}
PEcAn.logger::logger.info("Done with met2model.ED2")

##################################################

plot(PEcAn.data.atmosphere::cos_solar_zenith_angle(rep(1,8),
                                                   9, 30, 10800, seq(0,21,3)-1.5),
     type = "l")

