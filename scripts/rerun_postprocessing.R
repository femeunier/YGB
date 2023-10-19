rm(list = ls())

library(stringr)

# ml purge; ml R/4.1.2-foss-2021b

fyear <- 1999

source('/data/gent/vo/000/gvo00074/felicien/R/read_and_plot_ED2.2_all_tspft_yearly.r')

# read_and_plot_ED2.2_all_tspft_yearly('/kyukon/scratch/gent/vo/000/gvo00074/felicien/Yoko/SA/SA_reference/analy',
#                                      'analysis','1700/01/01',paste0(fyear,'/01/01'))

params <- c("stomatal_slope","Vm0","Delta_Vm0","stoma_psi_b","Delta_stoma_psi_b","clumping_factor")
quantiles <- c(0.025,0.975)

for (iparam in seq(1,length(params))){
  for (iquant in seq(1,length(quantiles))){

    out_ref <- paste0("/kyukon/scratch/gent/vo/000/gvo00074/felicien/Yoko/SA/",
                     "SA_",params[iparam],"_",quantiles[iquant])
    analy.dir <- paste0(out_ref,"/analy")
    analy.file <- file.path(analy.dir,"analysis.RData")
    # if (!file.exists(analy.file)){

      details.file <- file.info(list.files(path = file.path(out_ref,"histo"), full.names = TRUE,pattern = ".h5"))
      files.OP.ordered <- details.file[with(details.file, order(as.POSIXct(mtime),decreasing = TRUE)), ]

      fyear <- as.numeric(str_split(basename(rownames(files.OP.ordered)[1]),pattern = "-")[[1]][3])

      read_and_plot_ED2.2_all_tspft_yearly(analy.dir,'analysis','1700/01/01',paste0(fyear,'/01/01'))
    # }

  }
}
