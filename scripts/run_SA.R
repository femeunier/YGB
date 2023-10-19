rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(BayesianTools)

source("/home/femeunier/Documents/projects/DGVM.SA/R/sensitvity.analysis.R")

# Import outputs and parameter data.frame
system2("rsync",paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df.seasonal.C.SA.RDS",
                      "./outputs/"))
system2("rsync",paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df_param_SA_YGB.RDS",
                      "./outputs/"))

# Priors
pft_lowers <- c(stomatal_slope = 2,
                D0 = 0.005,
                Vm0 = 5,
                stoma_psi_b = 250,
                vm_q10 = 1.8,
                clumping_factor = 0.4)

pft_uppers <- c(stomatal_slope = 16,
                D0 = 0.03,
                Vm0 = 35,
                stoma_psi_b = 450,
                vm_q10 = 3,
                clumping_factor = 0.9)


# Global thresholds
global_min <- c(Vm0 = 5,stoma_psi_b = 100)
global_max <- c(Vm0 = 35,stoma_psi_b = 600)

prior <- map2(pft_lowers,pft_uppers,createUniformPrior)
param_names <- names(pft_lowers)
Nparam <- length(param_names)

# Trait samples
trait.samples <- list()
for (iparam in seq(1,Nparam)){
  trait.samples[[param_names[iparam]]] <- as.vector(prior[[iparam]]$sample(n = 100000))
}

# sa.samples

df.samples <- readRDS("./outputs/df_param_SA_YGB.RDS")

df.samples.current <- df.samples
df.samples.current.all <- df.samples.current %>% pivot_wider(names_from = param,
                                                             values_from = value) %>% arrange(quantile)
sa.samples <- df.samples.current.all

# SA outputs
df.SA <- readRDS("./outputs/df.seasonal.C.SA.RDS")

df.SA.sum <- df.SA %>% group_by(param,quantile) %>% summarise(m = mean(GPP))
df.SA.current <- df.SA.sum
df.SA.current.all <- bind_rows(list(do.call("rbind",df.SA.current %>% filter(param == "reference") %>% replicate(n = Nparam,simplify = FALSE)) %>%
                                      mutate(param = unique(df.SA.current %>% filter(param != "reference",
                                                                                     param %in% param_names) %>% pull(param))),
                                    df.SA.current %>% filter(param != "reference",
                                                             param %in% param_names))) %>%
  pivot_wider(names_from = param,
              values_from = m) %>% arrange(quantile)

sa.output <- df.SA.current.all
outdir <- file.path("./outputs/")

SA.op <- sensitivity.analysis(trait.samples,
                              sa.samples = sa.samples %>% filter(pft == 3) %>% mutate(stoma_psi_b = -stoma_psi_b),
                              sa.output,
                              outdir)

rel.I <-  left_join(sa.samples %>% filter(pft == 3) %>% dplyr::select(-pft) %>% pivot_longer(cols = -c(quantile),
                                                                                              names_to = "parameter",
                                                                                              values_to = "value"),
                     sa.output %>% pivot_longer(cols = -c(quantile),
                                                names_to = "parameter",
                                                values_to = "GPP"),
                     by = c("quantile","parameter")) %>% filter(parameter %in% param_names) %>% group_by(parameter) %>%
  mutate(rel.GPP = GPP/GPP[quantile == 0.5])


ggplot(data = rel.I,
       aes(x = quantile,y = rel.GPP, color = parameter)) +
  geom_line() +
  geom_point() +
  theme_bw()

# SA.op <- sensitivity.analysis(trait.samples %>% purrr::list_modify("Delta_Vm0" = NULL),
#                               sa.samples = sa.samples %>% filter(pft == 3) %>% select(-c("Delta_Vm0")) %>% mutate(stoma_psi_b = -stoma_psi_b),
#                               sa.output %>% select(-c("Delta_Vm0")), outdir)


df.SA.decomp <- data.frame(params = as.character(names(SA.op$variance.decomposition.output$coef.vars)),
                           CV = SA.op$variance.decomposition.output$coef.vars,
                           elast = SA.op$variance.decomposition.output$elasticities,
                           par.var = SA.op$variance.decomposition.output$partial.variances) %>% arrange(par.var) %>%
  mutate(params = factor(params,levels = params)) %>%
  pivot_longer(cols = c("CV","elast","par.var"),
               names_to = "type",
               values_to = "value")

print(sort(SA.op$variance.decomposition.output$partial.variances))

# Plot
ggplot(data = df.SA.decomp) +
  geom_point(aes(x = value,y = params)) +
  geom_segment(aes(x = 0, y = params,
                   xend = value,yend = params)) +
  # geom_vline(xintercept = 0,linetype = 2, color = "black") +
  facet_wrap(~ type,scales = "free_x") +
  theme_bw()

ggsave(filename = "./Figures/SAunivariate.png", plot = last_plot(),
       width = 20,height = 12,dpi = 300,units = "cm")

