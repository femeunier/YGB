rm(list = ls())

library(ncdf4)

# Inputs

outfolder <- "/home/carya/output/dbfiles/Test"
outfolder <- "/data/gent/vo/000/gvo00074/ED_common_data/met/CRUNCEP/CB"

years <- 1921:2010

lat.in <- -15
Delta_lat <- 25
lon.in <- -10
Delta_lon <- 55

method = "ncss"
maxErrors = 10
sleep = 2
verbose = FALSE

var <- tibble::tribble(~DAP.name, ~CF.name, ~units,
                       "tair","air_temperature", "Kelvin",
                       "lwdown", "surface_downwelling_longwave_flux_in_air","W/m2",
                       "press","air_pressure", "Pascal",
                       "swdown","surface_downwelling_shortwave_flux_in_air", "W/m2",
                       "uwind", "eastward_wind", "m/s",
                       "vwind", "northward_wind","m/s",
                       "qair", "specific_humidity", "g/g",
                       "rain", "precipitation_flux","kg/m2/s")

var <- tibble::tribble(~DAP.name, ~CF.name, ~units,
                       "tair","air_temperature", "Kelvin",
                       "swdown","surface_downwelling_shortwave_flux_in_air", "W/m2",
                       "rain", "precipitation_flux","kg/m2/s")


ylist <- seq(years[1], years[length(years)], by = 1)
rows <- length(ylist)

results <- data.frame(file = character(rows), host = character(rows),
                      mimetype = character(rows), formatname = character(rows),
                      startdate = character(rows), enddate = character(rows),
                      dbfile.name = "CRUNCEP", stringsAsFactors = FALSE)

for (i in seq(1,length(years))){

  var.list <- list()
  dat.list <- list()
  for (j in seq(1,length(var$DAP.name))){

    year <- years[i]
    current_var <- var$DAP.name[j]
    print(paste("Reading variable",current_var,"for year",year))

    lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north",
                            vals = seq(lat.in,lat.in + Delta_lat,0.5), create_dimvar = TRUE)
    lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east",
                            vals = seq(lon.in,lon.in + Delta_lon,0.5), create_dimvar = TRUE)

    ntime <- PEcAn.utils::days_in_year(year) * 4
    days_elapsed <- (1:ntime) * 6/24 - 3/24

    time <- ncdf4::ncdim_def(name = "time", units = paste0("days since ",
                                                           year, "-01-01T00:00:00Z"), vals = as.array(days_elapsed),
                             create_dimvar = TRUE, unlim = TRUE)
    dim <- list(lon,lat, time)


    loc.file <- file.path(outfolder, paste("CRUNCEP", year,
                                           "nc", sep = "."))
    results$file[i] <- loc.file
    results$host[i] <- PEcAn.remote::fqdn()
    results$startdate[i] <- paste0(year, "-01-01 00:00:00")
    results$enddate[i] <- paste0(year, "-12-31 23:59:59")
    results$mimetype[i] <- "application/x-netcdf"
    results$formatname[i] <- "CF Meteorology"

    file_name <- "mstmip_driver_global_hd_climate_%1$s_%2$d_v1.nc4"
    dap_base <- switch(method, opendap = paste0("https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/",
                                                file_name), ncss = paste0("https://thredds.daac.ornl.gov/thredds/ncss/grid/ornldaac/1220/",
                                                                          file_name, "/dataset.html"))
    url <- sprintf(dap_base, current_var, year)
    ncss_query <- glue::glue(url, "?", "var={current_var}&",
                             "south={lat.in}&", "west={lon.in}&", "north={lat.in + Delta_lat}&",
                             "east={lon.in + Delta_lon}&", "time_start={year}-01-01T00:00:00Z&",
                             "time_end={year}-12-31T21:00:00Z&", "accept=netcdf")

    tmp_file <- tempfile()
    utils::download.file(ncss_query, tmp_file)
    dap <- ncdf4::nc_open(tmp_file)

    lats.in <- seq(lat.in,lat.in+Delta_lat,0.5)
    lons.in <- seq(lon.in,lon.in+Delta_lon,0.5)

    dat.list[[j]] <- PEcAn.utils::retry.func(ncdf4::ncvar_get(dap,
                                                              as.character(var$DAP.name[j]),
                                                              c(1,1,1),
                                                              c(length(lons.in),length(lats.in),ntime)),
                                             maxErrors = maxErrors,
                                             sleep = sleep)
    var.list[[j]] <- ncdf4::ncvar_def(name = as.character(var$CF.name[j]),
                                      units = as.character(var$units[j]), dim = dim,
                                      missval = -999, verbose = verbose)


    ncdf4::nc_close(dap)

  }


  dat.list[[8]] <- dat.list[[8]]/21600

  loc <- ncdf4::nc_create(filename = loc.file, vars = var.list,
                          verbose = verbose)
  for (j in seq_len(nrow(var))) {
    ncdf4::ncvar_put(nc = loc, varid = as.character(var$CF.name[j]),
                     vals = dat.list[[j]])
  }
  ncdf4::nc_close(loc)
}

# scp /home/femeunier/Documents/projects/YGB/scripts/Download.CRUNCEP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R



