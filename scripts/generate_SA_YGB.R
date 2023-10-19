rm(list = ls())

# To be run on the cluster

# devtools::install_github("pecanproject/pecan", subdir = 'base/utils')
# devtools::install_github("pecanproject/pecan", subdir = 'models/ed')
# devtools::install_github("femeunier/ED2scenarios")

# Libraries
library(dplyr)
library(tidyr)
library(purrr)
library(ED2scenarios)
library(PEcAn.ED2)
library(BayesianTools)

# Directories
ref_dir <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2_soil/ED2/ED/run"
ed2in <- read_ed2in(file.path(ref_dir,"ED2IN_YGB_new"))                                # reference ED2IN file

# No -T- Files
ed2in$ITOUTPUT = 0
ed2in$IYEARZ   = 2020

rundir <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2_soil/ED2/ED/run/SA"   # Directory for the run folders
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/SA"                       # Directory for the outfolders folders

# Parameters d
# Priors
pft_lowers <- c(stomatal_slope = 2,
                D0 = 0.005,
                Vm0 = 5,
                Delta_Vm0 = 0,
                stoma_psi_b = 250,
                Delta_stoma_psi_b = -150,
                vm_q10 = 1.8,
                clumping_factor = 0.4)

pft_uppers <- c(stomatal_slope = 16,
                D0 = 0.03,
                Vm0 = 35,
                Delta_Vm0 = 10,
                stoma_psi_b = 450,
                Delta_stoma_psi_b = -50,
                vm_q10 = 3,
                clumping_factor = 0.9)


# Global thresholds
global_min <- c(Vm0 = 5,stoma_psi_b = 100)
global_max <- c(Vm0 = 35,stoma_psi_b = 600)

prior <- map2(pft_lowers,pft_uppers,createUniformPrior)
param_names <- names(pft_lowers)
Nparam <- length(param_names)

# Multi jobs --> to group the simulations in a single job file
Nsimuperjob = 4
isimu = 0

# Config file
PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n"
defaults <- list_dir <- list()

# Default settings
settings <- list(model = list(revision = "git",
                              config.header = NULL),
                 pfts = list(pft = list(num = 2,
                                        ed2_pft_number = 2,
                                        name = "Early"),
                             pft = list(num = 3,
                                        ed2_pft_number = 3,
                                        name = "Mid"),
                             pft = list(num = 4,
                                        ed2_pft_number = 4,
                                        name = "Late")))

b1Ht = 1.2366; b2Ht = 0.5195; dbh_crit = 78.6525

# Default config
config <- list()

config[["Early"]] <- unlist(list(num = 2,
                                 Vm0 = 20,
                                 wood_psi50 = 400,
                                 stoma_psi_b = 350,
                                 phenology = 5,
                                 b1Rd = 4,
                                 mort1 = 0.0005,
                                 b1Ht = 1.2366,
                                 b2Ht = 0.5195,
                                 hgt_max = 33.26,
                                 leaf_psi_tlp = 200,
                                 b1Bs_small = 0.0346,
                                 b1Bs_large = 0.0346,
                                 b2Bs_small = 0.976,
                                 b2Bs_large = 0.976))

config[["Mid"]] <- unlist(list(num = 3,
                               Vm0 = 15,
                               wood_psi50 = 450,
                               stoma_psi_b = 400,
                               phenology = 5,
                               b1Rd = 4,
                               mort1 = 0.0005,
                               b1Ht = 1.2366,
                               b2Ht = 0.5195,
                               hgt_max = 33.26,
                               leaf_psi_tlp = 200,
                               b1Bs_small = 0.0514,
                               b1Bs_large = 0.0514,
                               b2Bs_small = 0.976,
                               b2Bs_large = 0.976))

config[["Late"]] <- unlist(list(num = 4,
                                Vm0 = 10,
                                wood_psi50 = 500,
                                stoma_psi_b = 450,
                                phenology = 5,
                                b1Rd = 4,
                                mort1 = 0.0005,
                                b1Ht = 1.2366,
                                b2Ht = 0.5195,
                                hgt_max = 33.26,
                                leaf_psi_tlp = 200,
                                wood_water_cap = 0.2,
                                b1Bs_small = 0.068,
                                b1Bs_large = 0.068,
                                b2Bs_small = 0.976,
                                b2Bs_large = 0.976))

# Which quantiles to be run (in addition to the median)
quantiles <- c(0.025,0.16,0.25,0.75,0.84,0.975)

#################################################################
# Reference run

# Directories
run_name <- "SA_reference"
isimu = isimu + 1

run_ref <- file.path(rundir,run_name)
out_ref <- file.path(outdir,run_name)

if(!dir.exists(run_ref)) dir.create(run_ref)
if(!dir.exists(out_ref)) dir.create(out_ref)
if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))

# ED2IN file
ed2in_scenar <- ed2in
ed2in_scenar$IEDCNFGF <- file.path(run_ref,"config.xml")
ed2in_scenar$FFILOUT = file.path(out_ref,"analy","analysis")
ed2in_scenar$SFILOUT = file.path(out_ref,"histo","history")

write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

# Config file
config_simu <- config

for (iparam in seq(1,Nparam)){
  if (param_names[iparam] %in% c("Vm0","stoma_psi_b")){ # Pft specific case

    param_name = param_names[iparam]
    param0 <- median(prior[[iparam]]$sample(n = 100000))
    delta_param <- median(prior[[paste0("Delta_",param_name)]]$sample(n = 100000))

    params <- param0 - delta_param*c(0,1,2)
    params_actual <- pmax(pmin(params,global_max[param_name]),global_min[param_name])

  } else if (param_names[iparam] %in% c("Delta_Vm0","Delta_stoma_psi_b")){ # We skip the delta
    next()
  } else { # Default case
    param_name <- param_names[iparam]
    samples <- prior[[iparam]]$sample(n = 100000)
    params_actual <- rep(median(samples),3)
  }

  for (ipft in seq(1,length(params_actual))){
    config_simu[[ipft]][param_name] <- params_actual[ipft]
  }
}

config_reference <- config_simu

# Write config file
xml <- write.config.xml.ED2(defaults = defaults,
                            settings = settings,
                            trait.values = config_simu)

XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
             prefix = PREFIX_XML)

# job.sh
if (isimu == 1){
  isfirstjob = TRUE
  dir_joblauncher = run_ref
  list_dir[[run_name]] = run_ref
} else{
  isfirstjob = FALSE
}

# Write job file
write_joblauncher(file = file.path(dir_joblauncher,"job.sh"), # job file name
                  nodes = 1,ppn = 18,mem = 16,walltime = 3,   # job config
                  prerun = "ml purge; ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",  # modules to load
                  CD = run_ref,  # path to the run folder
                  ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/EDlatest/ED2/ED/build/ed_2.2-opt-master-43d3df2", # ED2 executable name
                  ED2IN = "ED2IN", # ED2IN name
                  Rplot_function = '/data/gent/vo/000/gvo00074/felicien/R/read_and_plot_ED2.2_all_tspft.r',   # postprocessing R file
                  firstjob = isfirstjob,
                  clean = TRUE  # Do you want to clean some of the outputs
)

if (isimu == Nsimuperjob){
  isimu = 0
}

#################################################################
# Main loops for the SA runs (similar to above but one parameter is changed at the time)
# SA
for (iparam in seq(1,Nparam)){
  for (iquantile in seq(1,length(quantiles))){

    # Directories
    run_name <- paste0("SA_",param_names[iparam],"_",quantiles[iquantile])
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

    write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

    # Config
    config_simu <- config_reference

    for (iparam2 in seq(1,Nparam)){

      param_name = param_names[iparam2]

      if (param_name == param_names[iparam]){
        if (param_names[iparam] %in% c("Vm0","stoma_psi_b")){ # Pft specific case

          param0 <- quantile(prior[[iparam]]$sample(n = 100000),quantiles[iquantile])
          delta_param <- median(prior[[paste0("Delta_",param_name)]]$sample(n = 100000))

          params <- param0 - delta_param*c(0,1,2)
          params_actual <- pmax(pmin(params,global_max[param_name]),global_min[param_name])

        } else if (param_names[iparam] %in% c("Delta_Vm0","Delta_stoma_psi_b")){ # We skip the delta

          cparam <- substring(param_names[iparam],7)
          param0 <- median(prior[[cparam]]$sample(n = 100000))
          delta_param <- quantile(prior[[param_names[iparam]]]$sample(n = 100000),quantiles[iquantile])

          params <- param0 - delta_param*c(0,1,2)
          param_name <- cparam
          params_actual <- pmax(pmin(params,global_max[param_name]),global_min[param_name])


        } else { # Default case
          samples <- prior[[iparam]]$sample(n = 100000)
          params_actual <- rep(quantile(samples,quantiles[iquantile]),3)
        }

        for (ipft in seq(1,length(params_actual))){
          config_simu[[ipft]][param_name] <- params_actual[ipft]
        }
      }
    }

    xml <- write.config.xml.ED2(defaults = defaults,
                                settings = settings,
                                trait.values = config_simu)

    XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
                 prefix = PREFIX_XML)


    # job.sh

    if (isimu == 1){
      isfirstjob = TRUE
      dir_joblauncher = run_ref
      list_dir[[run_name]] = run_ref
    } else{
      isfirstjob = FALSE
    }

    write_joblauncher(file = file.path(dir_joblauncher,"job.sh"),
                      nodes = 1,ppn = 18,mem = 16,walltime = 3,
                      prerun = "ml purge; ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                      CD = run_ref,
                      ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/EDlatest/ED2/ED/build/ed_2.2-opt-master-43d3df2",
                      ED2IN = "ED2IN",
                      Rplot_function = '/data/gent/vo/000/gvo00074/felicien/R/read_and_plot_ED2.2_all_tspft.r',
                      firstjob = isfirstjob,clean = TRUE)

    if (isimu >= Nsimuperjob){
      isimu = 0
    }
  }
}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs_SA.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")


# To transfer the files
# scp /home/femeunier/Documents/projects/YGB/scripts/generate_SA_YGB.R hpc:/data/gent/vo/000/gvo00074/felicien/R
