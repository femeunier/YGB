rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(rhdf5)
library(lubridate)
library(wesanderson)

t.of.dist <- c(LPJ = 931,
               ED2 = 2200,
               ORCHIDEE = 17)


#######################################################################################################
# Data

data.wide <- readRDS("./data/Yoko_AGB.tot_dyn.RDS") %>%
  mutate(agb.av= agb.av/10,
         agb.sd = agb.sd/10) %>%
  rename(mean = agb.av,
         t = age.num,
         sd = agb.sd) %>%
  mutate(low = mean - sd,
         up = mean + sd) %>%
  mutate(t.since = case_when(t > 100 ~ -5,
                             TRUE ~ t))

#######################################################################################################
# LPJ
cmass <- read.table("./outputs/MIP/LPJ-GUESS/new_runs_14July/output_default/cmass.out",header = TRUE) %>%
  dplyr::select(-Lon,-Lat) %>%
  pivot_longer(cols = -Year,
               names_to = "pft",
               values_to = "cmass") %>%
  filter(pft == "Total") %>%
  mutate(t.since = Year - t.of.dist["LPJ"])

#######################################################################################################
# ED2

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_Yoko_chronosequence.RDS",
                      "./outputs/"))

df_OP_SA_Yoko.ref <- readRDS(file.path("./outputs/","df_Yoko_chronosequence.RDS")) %>%
  mutate(t = yr + (month - 1)/12/2) %>%
  rename(agb = AGB) %>%
  mutate(t.since = t - t.of.dist["ED2"]) %>%
  mutate(agb = case_when(t.since <= 0 ~ agb + 5,
                         TRUE ~ agb))

#######################################################################################################
# ORCHIDEE

h5file <- file.path('/home/femeunier/Documents/projects/YGB/outputs/MIP/ORCHIDEE','yoko_orc_10y_precut_stomate.nc')

# h5ls(h5file)
# h5readAttributes(h5file,"/time_centered")
# # Description of PFTs
# PFT_NAME__01=SoilBareGlobal
# PFT_NAME__02=BroadLeavedEvergreenTropical
# PFT_NAME__03=BroadLeavedRaingreenTropical
# PFT_NAME__04=NeedleleafEvergreenTemperate
# PFT_NAME__05=BroadLeavedEvergreenTemperate
# PFT_NAME__06=BroadLeavedSummergreenTemperate
# PFT_NAME__07=NeedleleafEvergreenBoreal
# PFT_NAME__08=BroadLeavedSummergreenBoreal
# PFT_NAME__09=LarixSpBoreal
# PFT_NAME__10=C3GrassTemperate
# PFT_NAME__11=C4GrassTemperate
# PFT_NAME__12=C3AgricultureTemperate
# PFT_NAME__13=C4AgricultureTemperate

# LEAF_M, SAP_M_AB, SAP_M_BE, HEART_M_AB, HEART_M_BE, ROOT_M, RESERVE_M, FRUIT_M

# Precut

time <- as.Date(h5read(h5file,"/time_centered")/86400,
                origin = "1991-01-01 00:00:00")
AGB <- h5read(h5file,"/SAP_M_AB")[1,1,,]/1000 +
  h5read(h5file,"/LEAF_M")[1,1,,]/1000 +
  h5read(h5file,"/HEART_M_AB")[1,1,,]/1000 +
  h5read(h5file,"/RESERVE_M")[1,1,,]/1000 +
  h5read(h5file,"/FRUIT_M")[1,1,,]/1000  # kgC/m²

# maxvegetfrac <-  h5read(h5file,"/maxvegetfrac")[1,1,,]
maxvegetfrac <- c( 0.0306,0.7394,0.0517,0.0055,0.0000,0.0000,0.0000,0.0000,0.0000,0.0695,0.0701,0.0271,0.0061)
AGB.tot <- sapply(1:dim(AGB)[2],function(i){
  forest <- c(1:11)
  return(weighted.mean(AGB[forest,i],maxvegetfrac[forest]))})

df_OP_ORCH_precut <- data.frame(times = time, agb = AGB.tot) %>%
  mutate(t = year(time)) %>%
  mutate(yr = t - min(t)) %>%
  group_by(yr) %>%
  summarise(agb = mean(agb),
            .groups = "keep")

# Postcut

h5files <- file.path('/home/femeunier/Documents/projects/YGB/outputs/MIP/ORCHIDEE',c('yoko_orc_postcut_stomate.nc',
                                                                                     'yoko_orc_postcut_stomate2.nc',
                                                                                    'yoko_orc_postcut_stomate3.nc'))
df_OP_ORCH_postcut <- data.frame() ; cyr <- 0

for (i in seq(1,length(h5files))){

  h5file <- h5files[i]

  Origin <- h5readAttributes(h5file,"/time_centered")[["time_origin"]]
  time <- as.Date(h5read(h5file,"/time_centered")/86400,
                  origin = Origin)
  AGB <- h5read(h5file,"/SAP_M_AB")[1,1,,]/1000 +
    h5read(h5file,"/LEAF_M")[1,1,,]/1000 +
    h5read(h5file,"/HEART_M_AB")[1,1,,]/1000 +
    h5read(h5file,"/RESERVE_M")[1,1,,]/1000 +
    h5read(h5file,"/FRUIT_M")[1,1,,]/1000  # kgC/m²

  # maxvegetfrac <-  h5read(h5file,"/maxvegetfrac")[1,1,,]
  AGB.tot <- sapply(1:dim(AGB)[2],function(i){

    forest <- c(1:11)
    return(weighted.mean(AGB[forest,i],maxvegetfrac[forest]))})

  temp.df <- data.frame(times = time, agb = AGB.tot) %>%
    mutate(t = year(time),
           m = month(time),
           d = day(time))  %>%
    filter(m == 1 & d == 1) %>%
    mutate(yr = t)

  df_OP_ORCH_postcut <- bind_rows(list(df_OP_ORCH_postcut,
                                       temp.df %>% dplyr::select(yr,agb)))

  cyr <- cyr
}


df_OP_ORCH <- bind_rows(list(df_OP_ORCH_precut,
                             df_OP_ORCH_postcut %>%
                               mutate(yr = 1:nrow(df_OP_ORCH_postcut)) %>%
                               ungroup() %>%
                               dplyr::select(yr,agb) %>%
                               mutate(yr = yr + max(df_OP_ORCH_precut$yr))
                             )) %>%
  mutate(t.since =  yr - t.of.dist["ORCHIDEE"]) %>% ungroup()

#######################################################################################################


OP.all <- bind_rows(list(
  cmass %>% dplyr::select(t.since,cmass) %>% rename(AGB = cmass) %>% mutate(model = "LPJ-GUESS"),
  df_OP_SA_Yoko.ref %>% dplyr::select(t.since,agb) %>% rename(AGB = agb) %>% mutate(model = "ED2"),
  df_OP_ORCH %>% dplyr::select(t.since, agb) %>% rename(AGB = agb) %>% mutate(model = "ORCHIDEE")))


########################################################################################################
pal <- wes_palette("Zissou1", 3, type = "continuous")

ggplot() +
  geom_point(data = data.wide,
             aes(x = t.since,
                 y = mean),
             color = "black",
             size = 2) +
  geom_errorbar(data = data.wide,
                aes(x = t.since,y = mean,ymin = low,ymax = up),
                width = 0,
                color = "black") +
  labs(x = "Time since disturbance (yr)", y = "AGB (kgC/m²)", color = "") +
  theme_bw() +
  scale_x_continuous(limits = c(-10,80)) +
  scale_y_continuous(limits = c(0,30)) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme(text = element_text(size = 22))

ggsave(last_plot(),filename = "./Figures/MI_regrowth_data.png",dpi = 300, width = 20, height = 10,unit = "cm")


ggplot(data = OP.all %>% filter(t.since >= -10,
                                t.since <= 80)) +
  geom_line(aes(x = t.since, y = AGB, color = model),show.legend = FALSE) +
  geom_point(data = data.wide,
             aes(x = t.since,
                 y = mean),
             color = "black",
             size = 2) +
  geom_errorbar(data = data.wide,
                aes(x = t.since,y = mean,ymin = low,ymax = up),
                width = 0,
                color = "black") +
  labs(x = "Time since disturbance (yr)", y = "AGB (kgC/m²)", color = "") +
  theme_bw() +
  scale_x_continuous(limits = c(-10,80)) +
  scale_y_continuous(limits = c(0,30)) +
  scale_color_manual(values = pal) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme(text = element_text(size = 22))

ggsave(last_plot(),filename = "./Figures/MI_regrowth_models.png",dpi = 300, width = 20, height = 10,unit = "cm")



MIP <- OP.all %>% filter(t.since >= -10,
                         t.since <= 80) %>%
  mutate(yr = floor(t.since)) %>%
  group_by(yr,model) %>%
  summarise(AGB = min(AGB),
            t.since = min(t.since),
            .groups = "keep") %>%
  filter(t.since %in% seq(-10,80,1)) %>%
  group_by(t.since) %>%
  summarise(AGB.m = mean(AGB),
            AGB.sd = sd(AGB),
            AGB.se = 1.96*sd(AGB)/sqrt(3),
            .groups = "keep")

ggplot(data = MIP %>% filter(t.since <= 77),
       aes(x = t.since, y = AGB.m)) +
  geom_line(color = "darkgrey",
            show.legend = FALSE) +
  geom_ribbon(aes(ymin = pmax(0,AGB.m - AGB.se), ymax = AGB.m + AGB.se), alpha = 0.4, fill = "grey",color = NA) +
  # geom_line(data = OP.all %>% filter(t.since >= -10,
  #                                    t.since <= 80),
  #           aes(x = t.since, y = AGB, color = model),
  #           show.legend = TRUE) +
  geom_point(data = data.wide,
             aes(x = t.since,
                 y = mean),
             color = "black",
             size = 2) +
  geom_errorbar(data = data.wide,
                aes(x = t.since,y = mean,ymin = low,ymax = up),
                width = 1,
                color = "black") +
  labs(x = "Time since disturbance (yr)", y = "AGB (kgC/m²)", color = "") +
  scale_x_continuous(limits = c(-10,80)) +
  scale_y_continuous(limits = c(0,30)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = 2) +
  theme(text = element_text(size = 22))




##################################################################################################################
# Fraction of recovery

OP.all.rel <- OP.all %>% filter(t.since >= -10,
                                t.since <= 80) %>%
  group_by(model) %>%
  mutate(AGB.OG = mean(AGB[t.since < 0])) %>%
  mutate(AGB.rel = AGB/AGB.OG)

data.wide.rel <- data.wide %>%
  rename(AGB = mean) %>%
  mutate(AGB.OG = mean(AGB[t.since < 0])) %>%
  mutate(AGB.rel = AGB/AGB.OG,
         low = low/AGB.OG,
         up = up/AGB.OG)


ggplot(data = OP.all.rel) +
  geom_line(aes(x = t.since, y = AGB.rel*100, color = model),show.legend = FALSE) +
  geom_point(data = data.wide.rel,
             aes(x = t.since,
                 y = AGB.rel*100),
             color = "black",
             size = 2) +
  geom_errorbar(data = data.wide.rel,
                aes(x = t.since,y = AGB.rel*100,ymin = low*100,ymax = up*100),
                width = 0,
                color = "black") +
  labs(x = "Time since disturbance (yr)", y = "Fraction of recovery (%)", color = "") +
  theme_bw() +
  scale_x_continuous(limits = c(-10,80)) +
  scale_y_continuous(limits = c(0,1.5*100)) +
  scale_color_manual(values = pal) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme(text = element_text(size = 22),
        legend.position = c(0.8,0.8))

ggsave(last_plot(),filename = "./Figures/MI_regrowth_recovery_models.png",dpi = 300, width = 20, height = 10,unit = "cm")

