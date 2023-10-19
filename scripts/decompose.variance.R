rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

params_name <- c("stomatal_slope","D0","Vm0","Delta_Vm0","stoma_psi_b","Delta_stoma_psi_b","vm_q10","clumping_factor")
Nparam <- length(params_name)

files2transfer <- c("df.diurnal.Water.ensemble.RDS","df.seasonal.Water.ensemble.RDS","df.diurnal.C.ensemble.RDS","df.seasonal.C.ensemble.RDS","df_param_ensemble_YGB.RDS")

OP.list <- list()

for (ifile in seq(1,length(files2transfer))){
  system2("rsync",c("-avz",paste0("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",files2transfer[ifile]),
                    file.path(".","outputs",files2transfer[ifile])))
  OP.list[[tools::file_path_sans_ext(files2transfer[ifile])]] <- readRDS(file.path(".","outputs",files2transfer[ifile]))
}

params <- OP.list[[5]]
OP <- OP.list[[4]] %>% group_by(run) %>% summarise(GPP = mean(GPP))

df.ensemble <- params %>% left_join(OP, by = "run")

df.ensemble2test <- df.ensemble %>% filter(pft == 2) %>% dplyr::select(param,value,GPP) %>% pivot_wider(names_from = "param",
                                                                                                        values_from = "value")

anovobj<-aov(lm(as.formula(paste0("GPP"," ~ .^2")),
                data = df.ensemble2test))

allssq<-summary(anovobj)[[1]][,2]
varn<-names((anovobj)$coefficients)[-1]
vars <- varn[1:Nparam]

sensivities<-sapply(vars,function(nn){

  SSQ <- sum(allssq,na.rm = T)
  SSQ_d <- sum(allssq[which(nn == varn)],na.rm = T)
  SSQ_i <- sum(allssq[which(!(nn == varn) & grepl(nn, varn, fixed=T))],na.rm = T)
  return(list(p.var = (SSQ_d+(SSQ_i/2))/SSQ,
              p.var.dir = SSQ_d/SSQ,
              p.var.indir = (SSQ_i/2)/SSQ))
  },USE.NAMES = T)


df2sort <- as.data.frame(t(sensivities)) %>% mutate(p.var = as.numeric(p.var)) %>% arrange(p.var)

df.SA.decomp <- as.data.frame(t(sensivities)) %>% mutate(params = factor(rownames(t(sensivities)),
                                                                         levels = rownames(df2sort))) %>%
  pivot_longer(cols = c("p.var","p.var.dir","p.var.indir"),
               names_to = "op.var",
               values_to = "value") %>% mutate(value = as.numeric(value))



ggplot(data = df.SA.decomp) +
  geom_point(aes(x = value,y = params)) +
  geom_segment(aes(x = 0, y = params,
                   xend = value,yend = params)) +
  # geom_vline(xintercept = 0,linetype = 2, color = "black") +
  facet_wrap(~ op.var) +
  theme_bw()
