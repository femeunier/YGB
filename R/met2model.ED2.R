#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#' met2model wrapper for ED2
#'
#' If files already exist in 'Outfolder', the default function is NOT to
#' overwrite them and only gives user the notice that file already exists. If
#' user wants to overwrite the existing files, just change overwrite statement
#' below to TRUE.
#'
#' @export
#' @param in.path location on disk where inputs are stored
#' @param in.prefix prefix of input and output files
#' @param outfolder location on disk where outputs will be stored
#' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
#' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
#' @param lst timezone offset to GMT in hours
#' @param overwrite should existing files be overwritten
#' @param verbose should the function be very verbose
#' @param leap_year Enforce Leap-years? If set to TRUE, will require leap years to have 366 days. If set to false, will require all years to have 365 days. Default = TRUE.

met2model.ED2 <- function(inputs, outfolder, start_date, end_date, lst = 0, lat = NA,
                          lon = NA, overwrite = FALSE, verbose = FALSE, leap_year = TRUE, CO2constant = 410 ,...) {

  df.in <- inputs

  overwrite <- as.logical(overwrite)

  # results are stored in folder prefix.start.end
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date   <- as.POSIXlt(end_date, tz = "GMT")
  met_folder <- outfolder
  met_header_file <- file.path(met_folder, "ED_MET_DRIVER_HEADER")

  results <- data.frame(
    file = met_header_file,
    host = PEcAn.remote::fqdn(),
    mimetype = "text/plain",
    formatname = "ed.met_driver_header files format",
    startdate = start_date,
    enddate = end_date,
    dbfile.name = "ED_MET_DRIVER_HEADER",
    stringsAsFactors = FALSE
  )

  ## check to see if the outfolder is defined, if not create directory for output
  dir.create(met_folder, recursive = TRUE, showWarnings = FALSE)

  dm <- c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
  dl <- c(0, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 367)
  month <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  mon_num <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")


  day2mo <- function(year, day, leap_year) {
    mo <- rep(NA, length(day))
    if (!leap_year) {
      mo <- findInterval(day, dm)
      return(mo)
    } else {
      leap      <- lubridate::leap_year(year)
      mo[leap]  <- findInterval(day[leap], dl)
      mo[!leap] <- findInterval(day[!leap], dm)
      return(mo)
    }
  }

  # get start/end year since inputs are specified on year basis
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  year_seq <- seq(start_year, end_year)
  day_secs <- udunits2::ud.convert(1, "day", "seconds")



  # Check which years need to be processed
  # Need `floor_date` here to make sure we include all months
  # (otherwise, `seq.Date(..., by = "month")` might accidentally skip shorter months)
  month_seq <- seq(
    lubridate::floor_date(start_date, "month"),
    lubridate::floor_date(end_date, "month"),
    by = "1 month"
  )

  target_fnames <- paste0(toupper(strftime(month_seq, "%Y%b", tz = "UTC")), ".h5")
  target_out_files <- file.path(met_folder, target_fnames)
  have_target_out_files <- file.exists(target_out_files)
  if (any(have_target_out_files)) {
    if (overwrite) {
      PEcAn.logger::logger.warn(
        "The following existing target output files will be overwritten:",
        paste(sprintf("'%s'", target_out_files[have_target_out_files]), collapse = ", ")
      )
    } else {
      have_output_byyear <- split(have_target_out_files, lubridate::year(month_seq))
      complete_years <- vapply(have_output_byyear, all, logical(1))
      skip_years <- tryCatch(
        as.numeric(names(complete_years[complete_years])),
        warning = function(e) PEcAn.logger::logger.severe(e)
      )
      PEcAn.logger::logger.warn(
        "The following output files already exist:",
        paste(target_out_files[have_target_out_files]),
        ". This means the following complete years will be skipped: ",
        skip_years
      )
      year_seq <- setdiff(year_seq, skip_years)
    }
  }

  ## loop over files
  for (year in year_seq) {


    df.temp <- df.in %>% filter(y == year)

    lat  <- eval(parse(text = lat))
    lon  <- eval(parse(text = lon))
    sec  <- df.temp %>% pull(tot.s)
    Tair <- df.temp %>% pull(temp)
    Qair <- df.temp %>% pull(sh)
    U    <- df.temp %>% pull(ugrd)
    V    <- df.temp %>% pull(vgrd)
    Rain <- df.temp %>% pull(prate)
    pres <- df.temp %>% pull(pres)
    nbdsf   <- df.temp %>% pull(nbdsf)
    nddsf   <- df.temp %>% pull(nddsf)
    vbdsf   <- df.temp %>% pull(vbdsf)
    vddsf   <- df.temp %>% pull(vddsf)
    LW   <- df.temp %>% pull(dlwrf)
    CO2  <- CO2constant * ((df.temp %>% pull(temp))**0)


    use_UV <- is.numeric(U) & is.numeric(V)

    if(!use_UV){
      U <- try(ncdf4::ncvar_get(nc, "wind_speed"), silent = TRUE)
      if(is.numeric(U)){
        PEcAn.logger::logger.info("eastward_wind and northward_wind are absent, using wind_speed to approximate eastward_wind")
        V <- rep(0, length(U))
      }else{
        PEcAn.logger::logger.severe("No eastward_wind and northward_wind or wind_speed in the met data")
      }
    }
    useCO2 <- is.numeric(CO2)


    dt <- PEcAn.utils::seconds_in_year(year, leap_year) / length(sec)

    toff <- -as.numeric(lst) * 3600 / dt


    ## build time variables (year, month, day of year)
    skip <- FALSE
    nyr <- floor(length(sec) * dt / 86400 / 365)
    yr   <- NULL
    doy  <- NULL
    hr   <- NULL
    asec <- sec
    for (y in seq(year, year + nyr - 1)) {
      diy <- PEcAn.utils::days_in_year(y, leap_year)
      ytmp <- rep(y, udunits2::ud.convert(diy / dt, "days", "seconds"))
      dtmp <- rep(seq_len(diy), each = day_secs / dt)
      if (is.null(yr)) {
        yr  <- ytmp
        doy <- dtmp
        hr  <- rep(NA, length(dtmp))
      } else {
        yr  <- c(yr, ytmp)
        doy <- c(doy, dtmp)
        hr  <- c(hr, rep(NA, length(dtmp)))
      }
      rng <- length(doy) - length(ytmp):1 + 1
      if (!all(rng >= 0)) {
        skip <- TRUE
        PEcAn.logger::logger.warn(year, " is not a complete year and will not be included")
        break
      }
      asec[rng] <- asec[rng] - asec[rng[1]]
      hr[rng]   <- (asec[rng] - (dtmp - 1) * day_secs) / day_secs * 24
    }
    mo <- day2mo(yr, doy, leap_year)
    if (length(yr) < length(sec)) {
      rng <- (length(yr) + 1):length(sec)
      if (!all(rng >= 0)) {
        skip <- TRUE
        PEcAn.logger::logger.warn(paste(year, "is not a complete year and will not be included"))
        break
      }
      yr[rng]  <- rep(y + 1, length(rng))
      doy[rng] <- rep(1:366, each = day_secs / dt)[1:length(rng)]
      hr[rng]  <- rep(seq(0, length = day_secs / dt, by = dt / day_secs * 24), 366)[1:length(rng)]
    }
    if (skip) {
      print("Skipping to next year")
      next
    }


    ## calculate potential radiation in order to estimate diffuse/direct
    # cosz <- PEcAn.data.atmosphere::cos_solar_zenith_angle(doy, lat, lon, dt, hr)
    #
    # rpot <- 1366 * cosz
    # rpot <- rpot[1:length(SW)]
    #
    # SW[rpot < SW] <- rpot[rpot < SW]  ## ensure radiation < max
    # ### this causes trouble at twilight bc of missmatch btw bin avergage and bin midpoint
    # frac <- SW/rpot
    # frac[frac > 0.9] <- 0.9  ## ensure some diffuse
    # frac[frac < 0] <- 0
    # frac[is.na(frac)] <- 0
    # frac[is.nan(frac)] <- 0
    # SWd <- SW * (1 - frac)  ## Diffuse portion of total short wave rad

    ### convert to ED2.1 hdf met variables
    n      <- length(Tair)
    nbdsfA <- nbdsf
    nddsfA <- nddsf
    vbdsfA <- vbdsf
    vddsfA <- vddsf

    prateA <- Rain  # precipitation rate [kg_H2O/m2/s]
    dlwrfA <- LW  # downward long wave radiation [W/m2]
    presA  <- pres  # pressure [Pa]
    hgtA   <- rep(50, n)  # geopotential height [m]
    ugrdA <- U  # zonal wind [m/s]
    vgrdA <- V  # meridional wind [m/s]
    shA    <- Qair  # specific humidity [kg_H2O/kg_air]
    tmpA   <- Tair  # temperature [K]
    if (useCO2) {
      co2A <- CO2
    }

    ## create directory if(system(paste('ls',froot),ignore.stderr=TRUE)>0)
    ## system(paste('mkdir',froot))

    ## write by year and month
    for (y in year + 1:nyr - 1) {
      sely <- which(yr == y)
      for (m in unique(mo[sely])) {
        selm <- sely[which(mo[sely] == m)]
        mout <- paste(met_folder, "/", y, month[m], ".h5", sep = "")
        if (file.exists(mout)) {
          if (overwrite) {
            file.remove(mout)
            ed_met_h5 <- hdf5r::H5File$new(mout)
          } else {
            PEcAn.logger::logger.warn("The file already exists! Moving to next month!")
            next
          }
        } else {
          ed_met_h5 <- hdf5r::H5File$new(mout)
        }
        dims  <- c(length(selm), 1, 1)
        nbdsf <- array(nbdsfA[selm], dim = dims)
        nddsf <- array(nddsfA[selm], dim = dims)
        vbdsf <- array(vbdsfA[selm], dim = dims)
        vddsf <- array(vddsfA[selm], dim = dims)
        prate <- array(prateA[selm], dim = dims)
        dlwrf <- array(dlwrfA[selm], dim = dims)
        pres  <- array(presA[selm], dim = dims)
        hgt   <- array(hgtA[selm], dim = dims)
        ugrd  <- array(ugrdA[selm], dim = dims)
        vgrd  <- array(vgrdA[selm], dim = dims)
        sh    <- array(shA[selm], dim = dims)
        tmp   <- array(tmpA[selm], dim = dims)
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

    ## write DRIVER file
    metvar <- c("nbdsf", "nddsf", "vbdsf", "vddsf", "prate", "dlwrf",
                "pres", "hgt", "ugrd", "vgrd", "sh", "tmp", "co2")
    metvar_table <- data.frame(
      variable = metvar,
      update_frequency = dt,
      flag = 1
    )
    # if (!useCO2) {
    #   metvar_table[metvar_table$variable == "co2",
    #                c("update_frequency", "flag")] <- list(380, 4)
    # }

    if (!useCO2) {
      metvar_table_vars <- metvar_table[metvar_table$variable !=  "co2",]  ## CO2 optional in ED2
    }else{
      metvar_table_vars <- metvar_table
    }

    ed_metheader <- list(list(
      path_prefix = met_folder,
      nlon = 1,
      nlat = 1,
      dx = 1,
      dy = 1,
      xmin = lon,
      ymin = lat,
      variables = metvar_table_vars
    ))

    check_ed_metheader(ed_metheader)
    write_ed_metheader(ed_metheader, met_header_file,
                       header_line = shQuote("Made_by_PEcAn_met2model.ED2"))
  }  ### end loop over met files

  PEcAn.logger::logger.info("Done with met2model.ED2")
  return(invisible(results))
} # met2model.ED2
