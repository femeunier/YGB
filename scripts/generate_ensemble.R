rm(list = ls())

# Libraries
library(dplyr)
library(tidyr)
library(PEcAnRTM)
library(purrr)
library(rrtm)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)
library(ggplot2)
library(ggridges)
library(cowplot)
library(pracma)
library(BayesianTools)

# Directories

ref_dir <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2_soil/ED2/ED/run"
ed2in <- read_ed2in(file.path(ref_dir,"ED2IN_YGB_new"))

# No -T- Files
ed2in$ITOUTPUT = 0
ed2in$IYEARZ   = 2020

rundir <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2_soil/ED2/ED/run/SA"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/SA"

# Parameters defaults
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
                                        name = "Lata")))

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

# Main loop
ensemble.size = 40

# Multi jobs
Nsimuperjob = 4
isimu = 0

for (iens in seq(1,ensemble.size)){

  # Directories
  run_name <- paste0("SAensemble",iens)

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

  # Sample
  pft_samples <- map(1:length(prior), function(i){
    samples <- prior[[i]]$sample()
    names(samples) <- names(pft_lowers[[i]])
    return(samples)}) %>% set_names(names(prior))

  # Config
  config_simu <- config

  for (i in seq(1,Nparam)){
    if (param_names[i] %in% c("Vm0","stoma_psi_b")){ # Pft specific case

      param_name = param_names[i]
      param0 <- pft_samples[[param_name]]
      delta_param <- pft_samples[[paste0("Delta_",param_name)]]

      params <- param0 - delta_param*c(0,1,2)
      params_actual <- pmax(pmin(params,global_max[param_name]),global_min[param_name])

    } else if (param_names[i] %in% c("Delta_Vm0","Delta_stoma_psi_b")){ # We skip the delta
      next()
    } else { # Default case
      param_name = param_names[i]
      params_actual <- rep(pft_samples[[param_name]],3)
    }

    for (ipft in seq(1,length(params_actual))){
      config_simu[[ipft]][param_name] <- params_actual[ipft]
    }
  }

  xml <- write.config.xml.ED2(defaults = defaults,
                              settings = settings,
                              trait.values = config_simu)

  XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
               prefix = PREFIX_XML)

  if (isimu == 1){
    isfirstjob = TRUE
    dir_joblauncher = run_ref
    list_dir[[run_name]] = run_ref
  } else{
    isfirstjob = FALSE
  }

  # job.sh

  write_joblauncher(file = file.path(dir_joblauncher,"job_YGB_ensemble.sh"),
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


dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                              list_files = list_dir,
                              job_name = "job_YGB_ensemble.sh")


# scp /home/femeunier/Documents/projects/YGB/scripts/generate_ensemble.R hpc:/data/gent/vo/000/gvo00074/felicien/R




