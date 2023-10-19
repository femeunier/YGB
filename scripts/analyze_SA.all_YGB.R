rm(list = ls())

library(dplyr)
library(LidarED)
library(purrr)
library(stringr)

outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/SA"
delta_time = 4

df.seasonal.C.all <- df.diurnal.C.all <- df.seasonal.Water.all <- df.diurnal.Water.all <-
  df.param.all <- df.others.all <- data.frame()

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

      ED2.others <- data.frame(year = datum$year,
                               month = datum$month,
                               AGB = datum$emean$agb,
                               LAI = datum$emean$lai,
                               GPP = datum$emean$gpp,
                               Reco = datum$emean$reco,
                               Rauto = datum$emean$plant.resp,
                               Rhetero = datum$emean$het.resp,
                               NEP = -datum$emean$nee,
                               NPP = datum$emean$npp,
                               ET = datum$emean$et,
                               evap = datum$emean$evap,
                               transp = datum$emean$transp)

      ED2.seasonal.C <- data.frame(year = datum$year,
                                   month = datum$month,
                                   GPP = datum$emean$gpp,
                                   Reco = datum$emean$reco,
                                   Rauto = datum$emean$plant.resp,
                                   Rhetero = datum$emean$het.resp,
                                   NEP = -datum$emean$nee/1000*1e-6*12*86400*365,
                                   NPP = datum$emean$npp)

      ED2.seasonal.Water <- data.frame(year = datum$year,
                                       month = datum$month,
                                       ET = datum$emean$et,
                                       evap = datum$emean$evap,
                                       transp = datum$emean$transp)


      ED2.diurnal.C <- data.frame(year = rep(datum$year,48),
                                  month = rep(datum$month,48),
                                  hour = sort(rep(seq(0,23.9,0.5),length(datum$year))),
                                  GPP = as.vector(datum$qmean$gpp),
                                  Reco = as.vector(datum$qmean$reco),
                                  Rauto = as.vector(datum$qmean$reco - datum$qmean$het.resp),
                                  Rhetero = as.vector(datum$qmean$het.resp),
                                  NEP = as.vector(-datum$qmean$nee)/1000*1e-6*12*86400*365,
                                  NPP = as.vector(datum$qmean$npp)) %>% mutate(hour = hour + delta_time/2) %>%
        mutate(hour = case_when(hour > 23.5 ~ (hour - 24),
                                TRUE ~ hour))

      ED2.diurnal.Water <- data.frame(year = rep(datum$year,48),
                                      month = rep(datum$month,48),
                                      hour = sort(rep(seq(0,23.9,0.5),length(datum$year))),
                                      ET = as.vector(datum$qmean$transp) + as.vector(datum$qmean$evap),
                                      evap = as.vector(datum$qmean$evap),
                                      transp = as.vector(datum$qmean$transp)) %>% mutate(hour = hour + delta_time/2) %>%
        mutate(hour = case_when(hour > 23.5 ~ (hour - 24),
                                TRUE ~ hour))

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

      df.seasonal.C.all <- bind_rows(list(df.seasonal.C.all,
                                          ED2.seasonal.C %>% mutate(quantile = quantiles[iquantile],
                                                                    param = params[iparam])))

      df.diurnal.C.all <- bind_rows(list(df.diurnal.C.all,
                                         ED2.diurnal.C %>% mutate(quantile = quantiles[iquantile],
                                                                  param = params[iparam])))

      df.seasonal.Water.all <- bind_rows(list(df.seasonal.Water.all,
                                              ED2.seasonal.Water %>% mutate(quantile = quantiles[iquantile],
                                                                            param = params[iparam])))

      df.diurnal.Water.all <- bind_rows(list(df.diurnal.Water.all,
                                             ED2.diurnal.Water %>% mutate(quantile = quantiles[iquantile],
                                                                          param = params[iparam])))

      df.others.all <- bind_rows(list(df.others.all,
                                      ED2.others %>% mutate(quantile = quantiles[iquantile],
                                                            param = params[iparam])))

    }
  }
}

# Reference run

run_name <- "SA_reference"
out_ref <- file.path(outdir,run_name)

datum.file <- file.path(out_ref,"analy","analysis.RData")
history.file <-  file.path(out_ref,"histo","history.xml")

if (file.exists(datum.file)){
  load(datum.file)

  ED2.others <- data.frame(year = datum$year,
                           month = datum$month,
                           AGB = datum$emean$agb,
                           LAI = datum$emean$lai,
                           GPP = datum$emean$gpp,
                           Reco = datum$emean$reco,
                           Rauto = datum$emean$plant.resp,
                           Rhetero = datum$emean$het.resp,
                           NEP = -datum$emean$nee,
                           NPP = datum$emean$npp,
                           ET = datum$emean$et,
                           evap = datum$emean$evap,
                           transp = datum$emean$transp)

  ED2.seasonal.C <- data.frame(year = datum$year,
                               month = datum$month,
                               GPP = datum$emean$gpp,
                               Reco = datum$emean$reco,
                               Rauto = datum$emean$plant.resp,
                               Rhetero = datum$emean$het.resp,
                               NEP = -datum$emean$nee/1000*1e-6*12*86400*365,
                               NPP = datum$emean$npp)

  ED2.seasonal.Water <- data.frame(year = datum$year,
                                   month = datum$month,
                                   ET = datum$emean$et,
                                   evap = datum$emean$evap,
                                   transp = datum$emean$transp)


  ED2.diurnal.C <- data.frame(year = rep(datum$year,48),
                              month = rep(datum$month,48),
                              hour = sort(rep(seq(0,23.9,0.5),length(datum$year))),
                              GPP = as.vector(datum$qmean$gpp),
                              Reco = as.vector(datum$qmean$reco),
                              Rauto = as.vector(datum$qmean$reco - datum$qmean$het.resp),
                              Rhetero = as.vector(datum$qmean$het.resp),
                              NEP = as.vector(-datum$qmean$nee)/1000*1e-6*12*86400*365,
                              NPP = as.vector(datum$qmean$npp)) %>% mutate(hour = hour + delta_time/2) %>%
    mutate(hour = case_when(hour > 23.5 ~ (hour - 24),
                            TRUE ~ hour))

  ED2.diurnal.Water <- data.frame(year = rep(datum$year,48),
                                  month = rep(datum$month,48),
                                  hour = sort(rep(seq(0,23.9,0.5),length(datum$year))),
                                  ET = as.vector(datum$qmean$transp) + as.vector(datum$qmean$evap),
                                  evap = as.vector(datum$qmean$evap),
                                  transp = as.vector(datum$qmean$transp)) %>% mutate(hour = hour + delta_time/2) %>%
    mutate(hour = case_when(hour > 23.5 ~ (hour - 24),
                            TRUE ~ hour))

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



  df.seasonal.C.all <- bind_rows(list(df.seasonal.C.all,
                                      ED2.seasonal.C %>% mutate(quantile = 0.5,
                                                                param = "reference")))

  df.diurnal.C.all <- bind_rows(list(df.diurnal.C.all,
                                     ED2.diurnal.C %>% mutate(quantile = 0.5,
                                                              param = "reference")))

  df.seasonal.Water.all <- bind_rows(list(df.seasonal.Water.all,
                                          ED2.seasonal.Water %>% mutate(quantile = 0.5,
                                                                        param = "reference")))

  df.diurnal.Water.all <- bind_rows(list(df.diurnal.Water.all,
                                         ED2.diurnal.Water %>% mutate(quantile = 0.5,
                                                                      param = "reference")))

  df.others.all <- bind_rows(list(df.others.all,
                                  ED2.others %>% mutate(quantile = 0.5,
                                                        param = "reference")))

}


saveRDS(object = df.diurnal.Water.all,file = file.path('.',"df.diurnal.Water.SA.RDS"))
saveRDS(object = df.seasonal.Water.all,file = file.path('.',"df.seasonal.Water.SA.RDS"))
saveRDS(object = df.diurnal.C.all,file = file.path('.',"df.diurnal.C.SA.RDS"))
saveRDS(object = df.seasonal.C.all,file = file.path('.',"df.seasonal.C.SA.RDS"))
saveRDS(object = df.param.all,file = file.path('.',"df_param_SA_YGB.RDS"))
saveRDS(object = df.others.all,file = file.path('.',"df_others_SA_YGB.RDS"))

# scp /home/femeunier/Documents/projects/YGB/scripts/analyze_SA.all_YGB.R hpc:/data/gent/vo/000/gvo00074/felicien/R

