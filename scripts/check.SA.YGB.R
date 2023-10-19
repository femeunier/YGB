rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# Model outputs
files2transfer <- c("df.diurnal.Water.SA.RDS",
                    "df.seasonal.Water.SA.RDS",
                    "df.diurnal.C.SA.RDS",
                    "df.seasonal.C.SA.RDS",
                    "df_param_SA_YGB.RDS")

OP.list <- list()

for (ifile in seq(1,length(files2transfer))){
  system2("rsync",c("-avz",paste0("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",files2transfer[ifile]),
                    file.path(".","outputs",files2transfer[ifile])))
  OP.list[[tools::file_path_sans_ext(files2transfer[ifile])]] <- readRDS(file.path(".","outputs",files2transfer[ifile]))
}

df_OP_SA_YGB <- readRDS(file.path(".","outputs","df.diurnal.C.SA.RDS")) %>%
  left_join(readRDS(file.path(".","outputs","df.diurnal.Water.SA.RDS")))


df_OP_SA_YGB <- readRDS(file.path(".","outputs","df.seasonal.C.SA.RDS")) %>%
  left_join(readRDS(file.path(".","outputs","df.seasonal.Water.SA.RDS")) )

df_param_SA_YGB <- readRDS(file.path(".","outputs","df_param_SA_YGB.RDS"))

df_param_SA_YGB.sum <- df_param_SA_YGB %>% group_by(param,quantile) %>% summarise(value = mean(value))
OP <- df_OP_SA_YGB  %>% group_by(param,quantile) %>% summarise(GPP = mean(GPP,na.rm = TRUE),
                                                               ET = mean(ET,na.rm = TRUE),
                                                               NEP = mean(NEP,na.rm = TRUE),
                                                               WUE = mean(NEP/ET,na.rm = TRUE)) %>% ungroup()

paramSA <- df_param_SA_YGB.sum %>% left_join((OP %>% filter(quantile == 0.5) %>% dplyr::select(-c(param))),by = "quantile") %>%
  left_join((OP %>% filter(quantile != 0.5)),by = c("param","quantile")) %>% mutate(GPP = case_when(!is.na(GPP.x) ~ GPP.x,
                                                                                                   !is.na(GPP.y) ~ GPP.y,
                                                                                                   TRUE ~ NA_real_),
                                                                                    NEP = case_when(!is.na(NEP.x) ~ NEP.x,
                                                                                                    !is.na(NEP.y) ~ NEP.y,
                                                                                                    TRUE ~ NA_real_),
                                                                                    ET = case_when(!is.na(ET.x) ~ ET.x,
                                                                                                    !is.na(ET.y) ~ ET.y,
                                                                                                    TRUE ~ NA_real_),
                                                                                    WUE = case_when(!is.na(WUE.x) ~ WUE.x,
                                                                                                    !is.na(WUE.y) ~ WUE.y,
                                                                                                    TRUE ~ NA_real_)) %>%
  dplyr::select(-c(WUE.x,WUE.y,NEP.x,NEP.y,ET.x,ET.y,GPP.x,GPP.y ))

paramSA.long <- paramSA %>% rename(param.value = value) %>% pivot_longer(cols = c("GPP","NEP","ET","WUE")) %>% ungroup() %>% group_by(name) %>%
  mutate(rel.value = value/value[param == param[1] & quantile == 0.5])

ggplot(data = paramSA.long) +
  geom_line(aes(x = quantile,y = rel.value,color = param)) +
  geom_point(aes(x = quantile,y = rel.value,color = param)) +
  geom_hline(yintercept = 1, linetype = 2, color = "black") +
  labs(x = "relative change of the parameter",y = "relative change of the output") +
  facet_wrap(~ name) +
  theme_bw()


paramSA.long %>% filter(param == "vm_q10") %>% pull(param.value) %>% unique()


OP.selected <- df_OP_SA_YGB %>% filter(quantile == 0.975,param == "vm_q10")

# plot(OP.selected$year+OP.selected$month/12,OP.selected$NEP,type = 'l')
# plot(OP.selected$year+OP.selected$month/12,OP.selected$ET,type = 'l')
