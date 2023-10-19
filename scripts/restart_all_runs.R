rm(list = ls())

library(dplyr)
library(LidarED)
library(purrr)
library(stringr)
library(PEcAn.ED2)
library(ED2scenarios)

rundir <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2_soil/ED2/ED/run/SA"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/Yoko/SA"

df.all <- df.param.all <- data.frame()
params <- c("stomatal_slope","Vm0","Delta_Vm0","stoma_psi_b","Delta_stoma_psi_b","clumping_factor")
Nparam <- length(params)

quantiles <- c(0.025,0.975)

# Multi jobs --> to group the simulations in a single job file
Nsimuperjob = 1
isimu = 0
list_dir <- list()

for (iparam in seq(1,Nparam)){
  for (iquantile in seq(1,length(quantiles))){

    isimu = isimu + 1

    param_name <- params[iparam]
    run_name <- paste0("SA_",param_name,"_",quantiles[iquantile])

    run_ref <- file.path(rundir,run_name)
    out_ref <- file.path(outdir,run_name)

    history.file <- file.path(out_ref,"histo","history")

    ed2in.file <- file.path(run_ref,"ED2IN")
    ed2in.file.restart <- file.path(run_ref,"ED2IN_history")

    ed2in <- read_ed2in(ed2in.file)
    ed2in.restart <- ed2in

    ed2in.restart$IYEARZ <- 2000
    ed2in.restart$RUNTYPE <- 'HISTORY'

    ed2in.restart$SFILIN <- history.file
    ed2in.restart$IYEARH <- 1800
    ed2in.restart$IMONTHH <- 1
    ed2in.restart$IDATEH <- 1
    ed2in.restart$ITIMEH <- 0

    write_ed2in(ed2in.restart,filename = ed2in.file.restart)

    if (isimu == 1){
      isfirstjob = TRUE
      dir_joblauncher = run_ref
      list_dir[[run_name]] = run_ref
    } else{
      isfirstjob = FALSE
    }

    write_joblauncher(file =  file.path(dir_joblauncher,"job_history.sh"),
                      nodes = 1, ppn = 18, mem = 16, walltime = 24,
                      prerun = "ml purge ; ml intel-compilers/2021.4.0 HDF5/1.12.1-iimpi-2021b UDUNITS/2.2.28-GCCcore-11.2.0; ulimit -s unlimited",
                      CD = run_ref,
                      ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/EDlatest/ED2/ED/build/ed_2.2-opt-master-43d3df20",
                      Rplot_function = '/data/gent/vo/000/gvo00074/felicien/R/read_and_plot_ED2.2_all_tspft_yearly.r',
                      ED2IN = "ED2IN_history",
                      firstjob = isfirstjob,
                      clean = TRUE,
                      in.line = 'ml purge; ml R/4.1.2-foss-2021b',
                      reload = TRUE)

    if (isimu >= Nsimuperjob){
      isimu = 0
    }
  }
}

dumb <- write_bash_submission(file = file.path(rundir,"jobs_restart.sh"),
                              list_files = list_dir,
                              job_name = "job_history.sh")

# scp /home/femeunier/Documents/projects/YGB/scripts/restart_all_runs.R hpc:/data/gent/vo/000/gvo00074/felicien/R
