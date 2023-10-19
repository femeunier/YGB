rm(list = ls())

library(dplyr)
library(LidarED)
library(purrr)
library(stringr)

outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/SA"
month.t <- c(10:12)

df.all <- df.param.all <- data.frame()
params <- c("stomatal_slope","D0","Vm0","Delta_Vm0","stoma_psi_b","Delta_stoma_psi_b","vm_q10","clumping_factor")
Nparam <- length(params)

quantiles <- c(0.025,0.16,0.25,0.75,0.84,0.975)

for (iparam in seq(1,Nparam)){
  for (iquantile in seq(1,length(quantiles))){

    param_name <- params[iparam]
    run_name <- paste0("SA_",param_name,"_",quantiles[iquantile])
    out_ref <- file.path(outdir,run_name)

    datum.file <- file.path(out_ref,"analy","analysis.RData")

    if (file.exists(datum.file)){
      load(datum.file)

      pos.month <- which(datum$month %in% month.t)
      NEP <- apply(-t((datum$qmean$nee)[pos.month,]),1,mean)
      trans <- apply(t((datum$qmean$transp + pmax(-Inf,datum$qmean$evap))[pos.month,]),1,mean) *1000/18/86400*1000

      history.file <-  file.path(out_ref,"histo","history.xml")

      if (params[iparam] %in% c("Vm0","stoma_psi_b")) {
        df.param <- data.frame(pft = c(2, 3, 4),
                               param = params[iparam],
                               value = unlist(map(c(2, 3, 4), function(x)
                                 get_ED_default_pft(history.file, params[iparam], pft_target = x))))
      } else if (params[iparam] %in% c("Delta_Vm0","Delta_stoma_psi_b")){

        cparam <- substring(params[iparam],7)
        raw.values <- unlist(map(c(2, 3, 4), function(x)
          get_ED_default_pft(history.file, cparam, pft_target = x)))
        df.param <- data.frame(pft = c(2, 3, 4),
                               param = params[iparam],
                               value = rep(mean(diff(rev(raw.values))),3))
      } else {
        df.param <-
          data.frame(pft = c(2, 3, 4),
                     param = params[iparam],
                     value = rep(get_ED_default_pft(history.file, params[iparam], pft_target = 2), 3))
      }
      df.param.all <- bind_rows(list(df.param.all,
                                     df.param %>% mutate(quantile = quantiles[iquantile],
                                                         param = params[iparam])))

      df.all <- bind_rows(list(df.all,
                               data.frame(t = 1:48,
                                          NEP,
                                          ET = trans,
                                          quantile = quantiles[iquantile],
                                          param = params[iparam])))

    }
  }
}

# Reference run

run_name <- "SA_reference"
out_ref <- file.path(outdir,run_name)

datum.file <- file.path(out_ref,"analy","analysis.RData")

if (file.exists(datum.file)){
  load(datum.file)

  pos.month <- which(datum$month %in% month.t)
  NEP <- apply(-t((datum$qmean$nee)[pos.month,]),1,mean)
  trans <- apply(t((datum$qmean$transp + pmax(-Inf,datum$qmean$evap))[pos.month,]),1,mean) *1000/18/86400*1000

  history.file <-  file.path(out_ref,"histo","history.xml")

  for (iparam in seq(1,Nparam)){
    if (params[iparam] %in% c("Vm0","stoma_psi_b")) {
      df.param <- data.frame(pft = c(2, 3, 4),
                             param = params[iparam],
                             value = unlist(map(c(2, 3, 4), function(x)
                               get_ED_default_pft(history.file, params[iparam], pft_target = x))))
    } else if (params[iparam] %in% c("Delta_Vm0","Delta_stoma_psi_b")){

      cparam <- substring(params[iparam],7)
      raw.values <- unlist(map(c(2, 3, 4), function(x)
        get_ED_default_pft(history.file, cparam, pft_target = x)))
      df.param <- data.frame(pft = c(2, 3, 4),
                             param = params[iparam],
                             value = rep(mean(diff(rev(raw.values))),3))
    } else {
      df.param <-
        data.frame(pft = c(2, 3, 4),
                   param = params[iparam],
                   value = rep(get_ED_default_pft(history.file, params[iparam], pft_target = 2), 3))
    }
    df.param.all <- bind_rows(list(df.param.all,
                                   df.param %>% mutate(quantile = 0.5,
                                                       param = params[iparam])))
  }

  df.all <- bind_rows(list(df.all,
                           data.frame(t = 1:48,
                                      NEP,
                                      ET = trans,
                                      quantile = 0.5,
                                      param = "reference")))

}


saveRDS(object = df.all,file = file.path('.',"df_OP_SA_YGB.RDS"))
saveRDS(object = df.param.all,file = file.path('.',"df_param_SA_YGB.RDS"))

# scp /home/femeunier/Documents/projects/YGB/scripts/analyze_SA_YGB.R hpc:/data/gent/vo/000/gvo00074/felicien/R

