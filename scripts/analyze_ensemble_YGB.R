rm(list = ls())

library(dplyr)
library(LidarED)
library(purrr)
library(stringr)

ensemble.size <- 40
delta_time <- 4
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/SA"
month.t <- c(10:12)

df.seasonal.C.all <- df.diurnal.C.all <- df.seasonal.Water.all <- df.diurnal.Water.all <- df.others.all <-
  df.param.all <- data.frame()

params <- c("stomatal_slope","D0","Vm0","Delta_Vm0","stoma_psi_b","Delta_stoma_psi_b","vm_q10","clumping_factor")

for (iens in seq(1,ensemble.size)){

  run_name <- paste0("SAensemble",iens)
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
                                 NPP = datum$emean$npp) %>% filter(year > min(year))

    ED2.seasonal.Water <- data.frame(year = datum$year,
                                     month = datum$month,
                                     ET = datum$emean$et,
                                     evap = datum$emean$evap,
                                     transp = datum$emean$transp) %>% filter(year > min(year))


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

    for (iparam in seq(1,length(params))){
      if (params[iparam] %in% c("Vm0","stoma_psi_b")){
        df.param <- data.frame(pft = c(2,3,4),
                               param = params[iparam],
                               value = unlist(map(c(2,3,4),function(x) get_ED_default_pft(history.file, params[iparam], pft_target = x))))
      } else if (params[iparam] %in% c("Delta_Vm0","Delta_stoma_psi_b")) {

        cparam <- substring(params[iparam],7)
        raw.values <- unlist(map(c(2, 3, 4), function(x)
          get_ED_default_pft(history.file, cparam, pft_target = x)))
        df.param <- data.frame(pft = c(2, 3, 4),
                               param = params[iparam],
                               value = rep(mean(diff(rev(raw.values))),3))

      } else{
        df.param <- data.frame(pft = c(2,3,4),
                               param = params[iparam],
                               value = rep(get_ED_default_pft(history.file, params[iparam], pft_target = 2),3))
      }
      df.param.all <- bind_rows(list(df.param.all,
                                     df.param %>% mutate(run = iens)))
    }

    df.seasonal.C.all <- bind_rows(list(df.seasonal.C.all,
                                        ED2.seasonal.C %>% mutate(run = iens)))

    df.diurnal.C.all <- bind_rows(list(df.diurnal.C.all,
                                       ED2.diurnal.C %>% mutate(run = iens)))

    df.seasonal.Water.all <- bind_rows(list(df.seasonal.Water.all,
                                            ED2.seasonal.Water %>% mutate(run = iens)))

    df.diurnal.Water.all <- bind_rows(list(df.diurnal.Water.all,
                                           ED2.diurnal.Water %>% mutate(run = iens)))

    df.others.all <- bind_rows(list(df.others.all,
                                    ED2.others %>% mutate(run = iens)))

  }
}

saveRDS(object = df.diurnal.Water.all,file = file.path('.',"df.diurnal.Water.ensemble.RDS"))
saveRDS(object = df.seasonal.Water.all,file = file.path('.',"df.seasonal.Water.ensemble.RDS"))
saveRDS(object = df.diurnal.C.all,file = file.path('.',"df.diurnal.C.ensemble.RDS"))
saveRDS(object = df.seasonal.C.all,file = file.path('.',"df.seasonal.C.ensemble.RDS"))

saveRDS(object = df.param.all,file = file.path('.',"df_param_ensemble_YGB.RDS"))
saveRDS(object = df.others.all,file = file.path('.',"df_others_ensemble_YGB.RDS"))

# scp /home/femeunier/Documents/projects/YGB/scripts/analyze_ensemble_YGB.R hpc:/data/gent/vo/000/gvo00074/felicien/R
