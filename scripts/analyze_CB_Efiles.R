rm(list = ls())

library(dplyr)
library(tidyr)
library(PEcAn.ED2)
library(stringr)
library(raster)
library(rhdf5)
library(pracma)

source("/data/gent/vo/000/gvo00074/felicien/R/h5read_opt.r")

ED_REG_LATMIN = -15
ED_REG_LATMAX = 10
ED_REG_LONMIN = -10
ED_REG_LONMAX = 45

GRID_RES = 1

day.sec = 86400

X = seq(ED_REG_LONMIN,ED_REG_LONMAX,GRID_RES)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX,GRID_RES)

land.sea.mask <- readRDS("./data/LandSeaMask.RDS")

df.mask <- data.frame(lat = as.vector(meshgrid(Y,X)[[1]]),
                      lon = as.vector(meshgrid(Y,X)[[2]])) %>%
  left_join(land.sea.mask,
            by = c("lat","lon")) %>%
  filter(mask == 1)

ref_dir <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/CB/run/"
rundir <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/CB/run/grid"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/CB/out"

df <- data.frame()
failed <- list()
ifailed <- 1

last.year <- 2000

for (i in seq(1,nrow(df.mask))){

  print(i/nrow(df.mask))

  clat <- df.mask[["lat"]][i]
  clon <- df.mask[["lon"]][i]

  run_name <- paste0("CB","_X_",abs(clon),ifelse(clon<0,"W","E"),"_Y_",abs(clat),ifelse(clat<0,"S","N"))

  run_ref <- file.path(rundir,run_name)
  out_ref <- file.path(outdir,run_name)

  if (!dir.exists(run_ref)) next()

  ed2in <- read_ed2in(file.path(run_ref,"ED2IN"))

  details.file <- file.info(list.files(path = file.path(out_ref,"analy"), full.names = TRUE,pattern = ".h5"))

  if (nrow(details.file) > 0){

    files.names <- rownames(details.file)
    years.E.files <- as.numeric(sapply(strsplit(basename(files.names),"-"," "),"[",3))
    months.E.files <- as.numeric(sapply(strsplit(basename(files.names),"-"," "),"[",4))

    cdf <- data.frame()

    for (imonth in seq(1,12)){

      h5file <- file.path(dirname(files.names)[1],
                          paste("analysis","E",as.character(last.year),sprintf("%02d",imonth),"00-000000-g01.h5",sep = "-"))
      analy.file <- file.path(dirname(files.names)[1],
                              paste("analysis","E",as.character(last.year),sprintf("%02d",imonth),"00-000000-g01.h5",sep = "-"))


      mymont    = lapply(h5read_opt(h5file),FUN=aperm)
      names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

      clat = mymont$LATITUDE
      clon = mymont$LONGITUDE

      evap = ( mymont$MMEAN.VAPOR.GC.PY
                + mymont$MMEAN.VAPOR.LC.PY
                + mymont$MMEAN.VAPOR.WC.PY ) * day.sec

      transp = mymont$MMEAN.TRANSP.PY * day.sec

      precip = mymont$MMEAN.PCPG.PY

      gpp = mymont$MMEAN.GPP.PY

      sw = mymont$MMEAN.ATM.RSHORT.PY

      cdf <- bind_rows(list(cdf,
                            data.frame(lat = clat,
                                       lon = clon,
                                       yr = last.year,
                                       month = imonth,

                                       AGB = sum(mymont$AGB.PY),
                                       AGB.trees = sum(mymont$AGB.PY[1,,2:4]),
                                       AGB.grass = sum(mymont$AGB.PY[1,,1]),

                                       LAI = sum(mymont$MMEAN.LAI.PY),
                                       LAI.trees = sum(mymont$MMEAN.LAI.PY[1,,2:4]),
                                       LAI.grass = sum(mymont$MMEAN.LAI.PY[1,,1]),

                                       evap = evap,
                                       transp = transp,

                                       et = evap + transp,

                                       precip = precip,

                                       sw = sw,

                                       gpp = gpp)))
    }



    df <- bind_rows(list(df,
                         cdf))


  } else {
    failed[[ifailed]] <- run_ref
    ifailed <- ifailed + 1
  }
}

saveRDS(df,"df_CB_Efiles.RDS")

# scp /home/femeunier/Documents/projects/YGB/scripts/analyze_CB_Efiles.R hpc:/data/gent/vo/000/gvo00074/felicien/R
