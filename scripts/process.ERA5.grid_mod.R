rm(list = ls())

library(dplyr)
library(xts)
library(raster)
library(PEcAn.ED2)
library(furrr)
library(future)
library(pracma)
library(lubridate)

plan(sequential)

###############################################################################
# met2cf.ERA5
met2CF.ERA5<- function(lat,
                       long,
                       start_date,
                       end_date,
                       sitename,
                       outfolder,
                       out.xts,
                       overwrite = FALSE,
                       verbose = TRUE) {

  years <- seq(lubridate::year(start_date),
               lubridate::year(end_date),
               1)

  ensemblesN <- seq(1, 1)

  start_date <- paste0(lubridate::year(start_date),"-01-01")  %>% as.Date()
  end_date <- paste0(lubridate::year(end_date),"-12-31") %>% as.Date()
  # adding RH and converting rain

  out.new <- ensemblesN %>%
    purrr::map(function(ensi) {
      tryCatch({

        ens <- out.xts[[ensi]]
        # Solar radation conversions
        #https://confluence.ecmwf.int/pages/viewpage.action?pageId=104241513
        #For ERA5 daily ensemble data, the accumulation period is 3 hours. Hence to convert to W/m2:

        # Reanalysis have hourly time-step!

        ens[, "ssrd"] <- ens[, "ssrd"] / (1*3600)
        ens[, "strd"] <- ens[, "strd"] / (1*3600)
        #precipitation it's originaly in meters. Meters times the density will give us the kg/m2
        ens[, "tp"] <-
          ens[, "tp"] * 1000 / 1 # divided by 3 because we have 1 hours data --> mm/h
        ens[, "tp"] <-
          udunits2::ud.convert(ens[, "tp"], "kg m-2 hr-1", "kg m-2 s-1")  #There are 21600 seconds in 6 hours??
        #RH
        #Adopted from weathermetrics/R/moisture_conversions.R
        t <-
          udunits2::ud.convert(ens[, "t2m"] %>% as.numeric(), "K", "degC")
        dewpoint  <-
          udunits2::ud.convert(ens[, "d2m"] %>% as.numeric(), "K", "degC")
        beta <- (112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t))
        relative.humidity <- beta ^ 8
        #specific humidity
        specific_humidity <-
          PEcAn.data.atmosphere::rh2qair(relative.humidity,
                                         ens[, "t2m"] %>% as.numeric(),
                                         ens[, "sp"] %>% as.numeric()) # Pressure in Pa
      },
      error = function(e) {
        PEcAn.logger::logger.severe("Something went wrong during the unit conversion in met2cf ERA5.",
                                    conditionMessage(e))
      })


      #adding humidity
      xts::merge.xts(ens[, -c(3)], (specific_humidity)) %>%
        `colnames<-`(
          c(
            "air_temperature",
            "air_pressure",
            "precipitation_flux",
            "eastward_wind",
            "northward_wind",
            "surface_downwelling_shortwave_flux_in_air",
            "surface_downwelling_longwave_flux_in_air",
            "specific_humidity"
          )
        )

    })


  #These are the cf standard names
  cf_var_names = colnames(out.new[[1]])
  cf_var_units = c("K", "Pa", "kg m-2 s-1", "m s-1", "m s-1", "W m-2", "W m-2", "1")  #Negative numbers indicate negative exponents


  results_list <-  ensemblesN %>%
    purrr::map(function(i) {

      start_date <- min(zoo::index(out.new[[i]]))
      end_date <- max(zoo::index(out.new[[i]]))
      # Create a data frame with information about the file.  This data frame's format is an internal PEcAn standard, and is stored in the BETY database to
      # locate the data file.
      results <- data.frame(
        file = "",
        #Path to the file (added in loop below).
        host = PEcAn.remote::fqdn(),
        mimetype = "application/x-netcdf",
        formatname = "CF Meteorology",
        startdate = paste0(format(
          start_date , "%Y-%m-%dT%H:%M:00 %z"
        )),
        enddate = paste0(format(
          end_date , "%Y-%m-%dT%H:%M:00 %z"
        )),
        dbfile.name = paste0("ERA5.", i),
        stringsAsFactors = FALSE
      )

      # i is the ensemble number
      #Generating a unique identifier string that characterizes a particular data set.
      identifier <- paste("ERA5", sitename, i, sep = "_")

      identifier.file <- paste("ERA5",
                               i,
                               lubridate::year(start_date),
                               sep = ".")

      ensemble_folder <- file.path(outfolder, identifier)

      #Each file will go in its own folder.
      if (!dir.exists(ensemble_folder)) {
        dir.create(ensemble_folder,
                   recursive = TRUE,
                   showWarnings = FALSE)
      }

      flname <-file.path(ensemble_folder, paste(identifier.file, "nc", sep = "."))

      #Each ensemble member gets its own unique data frame, which is stored in results_list
      results$file <- flname

      years %>%
        purrr::map(function(year) {
          #
          identifier.file <- paste("ERA5",
                                   i,
                                   year,
                                   sep = ".")

          flname <-file.path(ensemble_folder, paste(identifier.file, "nc", sep = "."))
          # Spliting it for this year
          data.for.this.year.ens <- out.new[[i]]
          data.for.this.year.ens <- data.for.this.year.ens[year %>% as.character]


          #Each ensemble gets its own file.
          time_dim = ncdf4::ncdim_def(
            name = "time",
            paste(units = "hours since", format(start_date, "%Y-%m-%dT%H:%M")),
            seq(0, (length(zoo::index(
              data.for.this.year.ens
            )) * 3) - 1 , length.out = length(zoo::index(data.for.this.year.ens))),
            create_dimvar = TRUE
          )
          lat_dim = ncdf4::ncdim_def("latitude", "degree_north", lat, create_dimvar = TRUE)
          lon_dim = ncdf4::ncdim_def("longitude", "degree_east", long, create_dimvar = TRUE)

          #create a list of all ens
          nc_var_list <- purrr::map2(cf_var_names,
                                     cf_var_units,
                                     ~ ncdf4::ncvar_def(.x, .y, list(time_dim, lat_dim, lon_dim), missval = NA_real_))

          #results$dbfile.name <- flname


          if (!file.exists(flname) || overwrite) {
            tryCatch({
              nc_flptr <- ncdf4::nc_create(flname, nc_var_list, verbose = verbose)

              #For each variable associated with that ensemble
              for (j in seq_along(cf_var_names)) {
                # "j" is the variable number.  "i" is the ensemble number.
                ncdf4::ncvar_put(nc_flptr,
                                 nc_var_list[[j]],
                                 zoo::coredata(data.for.this.year.ens)[, nc_var_list[[j]]$name])
              }

              ncdf4::nc_close(nc_flptr)  #Write to the disk/storage
            },
            error = function(e) {
              PEcAn.logger::logger.severe("Something went wrong during the writing of the nc file.",
                                          conditionMessage(e))
            })

          } else {
            PEcAn.logger::logger.info(paste0(
              "The file ",
              flname,
              " already exists.  It was not overwritten."
            ))
          }


        })

      return(results)
    })
  #For each ensemble
  return(results_list )
}



################################################################################
# Extract

lats <- seq(-15,10,0.5)
lons <- seq(-10,45,0.5)

land.sea.mask <- readRDS("./data/LandSeaMask.RDS")

in.path = "/data/gent/vo/000/gvo00074/ED_common_data/met/CB/"
start_date = "1960-01-01"
end_date = "1969-12-31"
outfolder = "/data/gent/vo/000/gvo00074/ED_common_data/met/CB/extracted"
in.prefix = "ERA5_"

df.mask <- data.frame(lat = as.vector(meshgrid(lats,lons)[[1]]),
                      lon = as.vector(meshgrid(lats,lons)[[2]])) %>% left_join(land.sea.mask,
                                                                               by = c("lat","lon")) %>%
  filter(mask == 1) %>%
  mutate(still2do = TRUE)


# First we check that the files do not exist yet

for (i in seq(1,nrow(df.mask))){

  slat <- df.mask[i,"lat"] ; slon <- df.mask[i,"lon"]

  dest.folder <- file.path(outfolder,paste(in.prefix,"lat_",slat,"_lon_",slon,"_1",sep = ""),"ED2")

  init.file <- file.path(dest.folder,paste0(as.character(year(start_date)),"JAN.h5"))
  end.file <- file.path(dest.folder,paste0(as.character(year(end_date)),"DEC.h5"))

  if (dir.exists(dest.folder) & file.exists(init.file) & file.exists(end.file)){
    df.mask[i,"still2do"] <- FALSE
  }

}


all.lats <- df.mask %>% filter(still2do) %>% pull(lat)
all.lons <- df.mask %>% filter(still2do) %>% pull(lon)


future_map2(all.lats,all.lons,function(slat,slon){

  print("=======================================================================")
  print(paste0("Lat = ",slat,", Lon = ",slon))

  newsite = paste("lat",slat,"lon",slon,sep = "_")
  vars = NULL
  overwrite = TRUE

  years <- seq(lubridate::year(start_date),
               lubridate::year(end_date),
               1
  )
  ensemblesN <- seq(1, 1)


  tryCatch({
    #for each ensemble
    one.year.out <- years %>%
      purrr::map(function(year) {

        # for each year
        point.data <-  ensemblesN %>%
          purrr::map(function(ens) {


            ncfile <- file.path(in.path, paste0(in.prefix, year, ".nc"))

            PEcAn.logger::logger.info(paste0("Trying to open :", ncfile, " "))

            if (!file.exists(ncfile)){PEcAn.logger::logger.severe("The nc file was not found.")}

            #msg
            PEcAn.logger::logger.info(paste0(year, " is being processed ", "for ensemble #", ens, " "))
            #open the file
            nc_data <- ncdf4::nc_open(ncfile)
            # time stamp

            t <- ncdf4::ncvar_get(nc_data, "time")
            tunits <- ncdf4::ncatt_get(nc_data, 'time')
            tustr <- strsplit(tunits$units, " ")
            timestamp <-
              as.POSIXct(t * 3600, tz = "UTC", origin = tustr[[1]][3])
            try(ncdf4::nc_close(nc_data))


            # set the vars
            if (is.null(vars))
              vars <- names(nc_data$var)
            # for the variables extract the data

            all.data.point <- vars %>%
              purrr::map_dfc(function(vname) {
                PEcAn.logger::logger.info(paste0(" \t ",vname, "is being extracted ! "))

                brick.tmp <-
                  raster::brick(ncfile, varname = vname, level = ens)
                nn <-
                  raster::extract(brick.tmp,
                                  sp::SpatialPoints(cbind(slon, slat)),
                                  method = 'simple')

                if (!is.numeric(nn)) {
                  PEcAn.logger::logger.severe(paste0(
                    "Expected raster object to be numeric, but it has type `",
                    paste0(typeof(nn), collapse = " "),
                    "`"
                  ))
                }


                # replacing the missing/filled values with NA
                nn[nn == nc_data$var[[vname]]$missval] <- NA
                # send out the extracted var as a new col
                t(nn)

              }) %>%
              `colnames<-`(vars)
            #close the connection

            # send out as xts object
            xts::xts(all.data.point, order.by = timestamp)
          }) %>%
          setNames(paste0("ERA_ensemble_", ensemblesN))

        #Merge mean and the speard
        return(point.data)

      }) %>%
      setNames(years)


    # The order of one.year.out is year and then Ens - Mainly because of the spead  / I wanted to touch each file just once.
    # This now changes the order to ens - year
    point.data <- ensemblesN %>%
      purrr::map(function(Ensn) {
        one.year.out %>%
          purrr::map( ~ .x [[Ensn]]) %>%
          do.call("rbind.xts", .)
      })


    # Calling the met2CF inside extract bc in met process met2CF comes before extract !
    out <- met2CF.ERA5(
      slat,
      slon,
      start_date,
      end_date,
      sitename=newsite,
      outfolder,
      point.data,
      overwrite = FALSE,
      verbose = TRUE
    )

  }, error = function(e) {
    PEcAn.logger::logger.severe(paste0(conditionMessage(e)))
  })

  # saveRDS(point.data,file.path(outfolder,paste(in.prefix,"lat_",slat,"_lon_",slon,"_1",sep = ""),"TS.RDS"))

  PEcAn.ED2::met2model.ED2(in.path =  file.path(outfolder,paste0(in.prefix,newsite,"_1")),
                           in.prefix = "ERA5.1",
                           outfolder = file.path(outfolder,paste0(in.prefix,newsite,"_1"),"ED2"),
                           start_date = start_date,
                           end_date = end_date,
                           lat = slat,
                           lon = slon,
                           overwrite = FALSE)

  return(TRUE)
})

# scp /home/femeunier/Documents/projects/YGB/scripts/process.ERA5.grid_mod.R hpc:/data/gent/vo/000/gvo00074/felicien/R
