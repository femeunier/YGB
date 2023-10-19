rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)
library(cowplot)
library(pracma)

ED_REG_LATMIN = -15
ED_REG_LATMAX =  10
ED_REG_LONMIN = -10
ED_REG_LONMAX = 45

GRID_RES = 1

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

ed2in <- read_ed2in(file.path(ref_dir,"ED2IN"))
ed2in$IMONTHA = 1
ed2in$IDATEA = 1
ed2in$IYEARA = 1700

ed2in$IMONTHZ = 2
ed2in$IDATEZ = 1
ed2in$IYEARZ = 2002

ed2in$RUNTYPE <- "HISTORY"
ed2in$IMOUTPUT <- 3
ed2in$IED_INIT_MODE <- 6
ed2in$IYEARH <- 2000
ed2in$IMONTHH <- 1


list_dir <- list()

Nsimuperjob = 20
isimu = 0


for (i in seq(1,nrow(df.mask))){

  clat <- df.mask[["lat"]][i]
  clon <- df.mask[["lon"]][i]

  run_name <- paste0("CB","_X_",abs(clon),ifelse(clon<0,"W","E"),"_Y_",abs(clat),ifelse(clat<0,"S","N"))

  isimu = isimu + 1

  run_ref <- file.path(rundir,run_name)
  out_ref <- file.path(outdir,run_name)

  if(!dir.exists(run_ref)) dir.create(run_ref)
  if(!dir.exists(out_ref)) dir.create(out_ref)
  if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
  if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))

  # ED2IN
  ed2in_scenar <- ed2in
  ed2in_scenar$IEDCNFGF <- file.path(run_ref,"config.xml")
  ed2in_scenar$FFILOUT = file.path(out_ref,"analy","analysis")
  ed2in_scenar$SFILOUT = file.path(out_ref,"histo","history")
  ed2in_scenar$POI_LAT <- clat
  ed2in_scenar$POI_LON <- clon

  ed2in_scenar$SFILIN <-  ed2in_scenar$SFILOUT

  ed2in_scenar$ED_MET_DRIVER_DB <- file.path("/data/gent/vo/000/gvo00074/ED_common_data/met/CB/extracted",
                                             paste0("ERA5_lat_",clat,"_lon_",clon,"_1"),
                                             "ED2","ED_MET_DRIVER_HEADER")

  write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN_restart"))


  if (isimu == 1){
    isfirstjob = TRUE
    dir_joblauncher = run_ref
    list_dir[[run_name]] = run_ref
  } else{
    isfirstjob = FALSE
  }

  # job.sh

  write_joblauncher(file =  file.path(dir_joblauncher,"job.sh"),
                    nodes = 1,ppn = 18,mem = 16,walltime = 72,
                    prerun = "ml purge ; ml intel-compilers/2021.4.0 HDF5/1.12.1-iimpi-2021b UDUNITS/2.2.28-GCCcore-11.2.0; ulimit -s unlimited",
                    CD = run_ref,
                    ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2.2/ED2/ED/build/ed_2.2-opt-master-fa80dab6",
                    Rplot_function = NULL,
                    ED2IN = "ED2IN_restart",
                    firstjob = isfirstjob,
                    clean = TRUE,
                    in.line = 'ml purge; ml R/4.1.2-foss-2021b',
                    reload = TRUE)




  if (isimu == Nsimuperjob){
    isimu = 0
  }
}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")

# scp /home/femeunier/Documents/projects/YGB/scripts/restart_CB_simulations.R hpc:/data/gent/vo/000/gvo00074/felicien/R

