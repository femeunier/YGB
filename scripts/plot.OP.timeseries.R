rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

files2transfer <- c("df_others_SA_YGB.RDS","df_param_SA_YGB.RDS")
files2transfer <- c("df_others_ensemble_YGB.RDS","df_param_ensemble_YGB.RDS")

type = "Ensemble"

OP.list <- list()

for (ifile in seq(1,length(files2transfer))){
  system2("rsync",c("-avz",paste0("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",files2transfer[ifile]),
                    file.path(".","outputs",files2transfer[ifile])))
  OP.list[[tools::file_path_sans_ext(files2transfer[ifile])]] <- readRDS(file.path(".","outputs",files2transfer[ifile]))
}

OP <- OP.list[[1]]

if (type == "SA"){
  OP.sum <- OP %>% pivot_longer(cols = -c(year,month,param,quantile),
                                names_to = "variable",
                                values_to = "value") %>%
    group_by(year,month,variable) %>% summarise(value.m = mean(value,na.rm = TRUE),
                                                value.sd = sd(value,na.rm = TRUE))
} else {
  OP.sum <- OP %>% pivot_longer(cols = -c(year,month,run),
                                names_to = "variable",
                                values_to = "value") %>%
    group_by(year,month,variable) %>% summarise(value.m = mean(value,na.rm = TRUE),
                                                value.sd = sd(value,na.rm = TRUE))
}


ggplot(data = OP.sum %>% ungroup %>% filter(year > min(year)),
       aes(x = year + month/12,
           y = value.m,
           ymin = value.m - value.sd,
           ymax = value.m + value.sd)) +
  geom_ribbon(alpha = 0.5, color = NA, fill = "grey") +
  geom_line(color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()
