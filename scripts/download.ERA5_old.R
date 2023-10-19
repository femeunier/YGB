download.ERA5 <- functions(outfolder,start_date,end_date,
                           product_types = "all",
                           overwrite = FALSE,reticulate_python = NULL){
  require("reticulate")

  if (!is.null(reticulate_python)) {
    reticulate::use_python()
  }

  tryCatch({
    cdsapi <- reticulate::import("cdsapi")
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "Failed to load `cdsapi` Python library. ",
      "Please make sure it is installed to a location accessible to `reticulate`.",
      "You should be able to install it with the following command: ",
      "`pip install --user cdsapi`.",
      "The following error was thrown by `reticulate::import(\"cdsapi\")`: ",
      conditionMessage(e)
    )
  })


  if (!file.exists(file.path(Sys.getenv("HOME"), ".cdsapirc"))){
    PEcAn.logger::logger.severe(
      "Please create a `${HOME}/.cdsapirc` file as described here:",
      "https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key ."
    )
  }



  tryCatch({
    cclient <- cdsapi$Client()
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "The following error was thrown by `cdsapi$Client()`: ",
      conditionMessage(e)
    )
  })

  all_products <- c("reanalysis", "ensemble_members",
                    "ensemble mean", "ensemble_spread")

  if (product_types == "all") {
    product_types <- all_products
  }

  if (any(!product_types %in% all_products)) {
    bad_products <- setdiff(product_types, all_products)
    PEcAn.logger::logger.severe(sprintf(
      "Invalid product types %s. Products must be one of the following: %s",
      paste0("`", bad_products, "`", collapse = ", "),
      paste0("`", all_products, "`", collapse = ", ")
    ))
  }

  # Full data documentation:
  # https://confluence.ecmwf.int/display/CKB/ERA5+data+documentation
  variables <- tibble::tribble(
    ~cf_name, ~units, ~api_name, ~ncdf_name,
    "air_temperature", "Kelvin", "2m_temperature", "t2m",
    "air_pressure", "Pa", "surface_pressure", "sp",
    NA_character_, "Kelvin", "2m_dewpoint_temperature", "d2m",
    "precipitation_flux", "kg/m2/s", "total_precipitation", "tp",
    "eastward_wind", "m/s", "10m_u_component_of_wind", "u10",
    "northward_wind", "m/s", "10m_v_component_of_wind", "v10",
    "surface_downwelling_shortwave_flux_in_air", "W/m2", "surface_solar_radiation_downwards", "ssrd",
    "surface_downwelling_longwave_flux_in_air", "W/m2", "surface_thermal_radiation_downwards", "strd"
  )
  nvar <- nrow(variables)

  # Spatial subset must be a bounding box (N, W, S, E). This sets the
  # bounding box to a single point -- the closest coordinate at the
  # 0.25 x 0.25 resolution of the product.
  area <- c(10,0,-10,40)

  files <- character()
  dir.create(outfolder, showWarnings = FALSE)


  # First, download all the files
  for (i in seq_len(nvar)) {
    var <- variables[["api_name"]][[i]]
    PEcAn.logger::logger.debug(glue::glue(
      "Downloading variable {i} of {nvar} ({var})."
    ))
    fname <- file.path(outfolder, paste("era5", var, "nc", sep = "."))
    if (file.exists(fname) && !overwrite) {
      PEcAn.logger::logger.warn(glue::glue(
        "File `{fname}` already exists, and `overwrite` is FALSE. ",
        "Skipping to next variable."
      ))
      next
    }
    do_next <- tryCatch({
      cclient$retrieve(
        "reanalysis-era5-single-levels",
        list(
          variable = var,
          product_type = 'ensemble_members',
          date = paste(start_date, end_date, sep = "/"),
          time = "00/to/23/by/1",
          area = area,
          grid = c(0.25, 0.25),
          format = "netcdf"
        ),
        fname
      )
      FALSE
    }, error = function(e) {
      PEcAn.logger::logger.warn(
        glue::glue(
          "Failed to download variable `{var}`. ",
          "Skipping to next variable. ",
          "Error message was:\n",
          conditionMessage(e)
        )
      )
      TRUE
    })

    if (isTRUE(do_next)) next
    files <- c(files, fname)
  }
}
