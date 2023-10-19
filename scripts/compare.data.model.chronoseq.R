rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(minpack.lm)

##################################################################################################
# Model outputs

system2("rsync",c("-avz",
                  "hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/Yoko/analy/Yoko_default.RData",
                  "/home/femeunier/Documents/projects/YGB/outputs/"))

load("/home/femeunier/Documents/projects/YGB/outputs/Yoko_default.RData")

df.OP <- data.frame(
  yr = datum$year,
  month = datum$month,
  PFT = c(
    rep(2, length(datum$emean$agb)),
    rep(3, length(datum$emean$agb)),
    rep(4, length(datum$emean$agb)),
    rep(18, length(datum$emean$agb))
  ),
  LAI = as.vector(datum$szpft$lai[, c(12), c(2, 3, 4, 18)]),
  GPP = as.vector(datum$szpft$gpp[, c(12), c(2, 3, 4, 18)]),
  AGB = as.vector(datum$szpft$agb[, c(12), c(2, 3, 4, 18)]),
  NPP = as.vector(datum$szpft$npp[, c(12), c(2, 3, 4, 18)])) %>%
  mutate(time = yr + (month - 1) / 12 - yr[1]) %>%
  mutate(pft = case_when(PFT == 2 ~ "Pioneer",
                         PFT == 3 ~ "Mid-successional",
                         PFT == 4 ~ "Late-successional",
                         PFT == 18 ~ "Total"))

##################################################################################################
# Data

tmax = 500

file <- "/home/femeunier/Desktop/FWO/site.csv"
data.site <- read.csv(file)
data.wide <- data.site %>%
  mutate(agb = agb) %>%
  pivot_wider(names_from = type,
              values_from = agb) %>%
  mutate(t = case_when (t > 100 ~ as.integer(tmax),
                        TRUE ~ t))

m0 <- nlsLM(data = data.wide,
      mean ~ a*(1 - exp(-b*t)),
      start=list(a = 300,
                 b = 0.001),
      lower = c(0,0),
      upper = c(Inf,Inf),
      control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                            printEval = TRUE, warnOnly = TRUE))


df.fit <- data.frame(time = seq(0,tmax),
                     agb = coef(m0)[1]*(1 - exp(-coef(m0)[2]*seq(0,tmax))))

##################################################################################################
# Plot
# AGB

ggplot() +
  geom_area(data = df.OP %>% filter(PFT != 18),
            aes(x = time, y = AGB*10, color = as.factor(PFT), fill = as.factor(PFT)),
            color = "darkgrey",alpha = 0.8,
            position = position_stack(reverse = TRUE)) +
  geom_line(data = df.OP %>% filter(PFT == 18),
            aes(x = time, y = AGB*10), color = "darkgrey") +
  geom_point(data = data.wide,
             aes(x = t,
                 y = mean),
             color = "black",alpha = 0.7,
             size = 2) +
  geom_errorbar(data = data.wide,aes(x = t,y = mean,ymin = low,ymax = up),
                width = 1,
                color = "black",alpha = 0.7) +
  geom_line(data = df.fit,
            aes(x = time, y = agb),
            color = "black",linetype = 2) +
  scale_fill_manual(breaks = c(2,3,4),
                    values = c("#83CCC0","#44AA99","#186659")) +
  labs(x = 'Time since last disturbance (yr)',y = "Aboveground biomass \r\n (Mg C/ha)", fill = "PFT") +
  theme_bw() +
  theme(legend.position = c(0.1,0.8),
        text = element_text(size = 24),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 24),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 24)) +
  guides(fill = "none")

ggsave(filename = "./Figures/Chronosequence.png",
       dpi = 300,
       width = 30, height = 20,
       unit = "cm",
       plot = last_plot())

# NPP
ggplot() +
  geom_area(data = df.OP %>% filter(PFT != 18),
            aes(x = time, y = NPP, color = as.factor(PFT), fill = as.factor(PFT)),
            color = NA,alpha = 0.8,
            position = position_stack(reverse = TRUE)) +
  geom_line(data = df.OP %>% filter(PFT == 18),
            aes(x = time, y = NPP), color = "darkgrey") +
  scale_fill_manual(breaks = c(2,3,4),
                    values = c("#83CCC0","#44AA99","#186659")) +
  labs(x = 'Time since last disturbance (yr)',y = "NPP \r\n (kg C/m²/yr)", fill = "PFT") +
  theme_bw() +
  theme(legend.position = c(0.1,0.8),
        text = element_text(size = 24),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 24),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 24)) +
  guides(fill = "none")

# GPP
ggplot() +
  geom_area(data = df.OP %>% filter(PFT != 18),
            aes(x = time, y = GPP, color = as.factor(PFT), fill = as.factor(PFT)),
            color = NA,alpha = 0.8,
            position = position_stack(reverse = TRUE)) +
  geom_line(data = df.OP %>% filter(PFT == 18),
            aes(x = time, y = GPP), color = "darkgrey") +
  scale_fill_manual(breaks = c(2,3,4),
                    values = c("#83CCC0","#44AA99","#186659")) +
  labs(x = 'Time since last disturbance (yr)',y = "GPP \r\n (kg C/m²/yr)", fill = "PFT") +
  theme_bw() +
  theme(legend.position = c(0.1,0.8),
        text = element_text(size = 24),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 24),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 24)) +
  guides(fill = "none")

# LAI
ggplot() +
  geom_area(data = df.OP %>% filter(PFT != 18),
            aes(x = time, y = LAI, color = as.factor(PFT), fill = as.factor(PFT)),
            color = NA,alpha = 0.8,
            position = position_stack(reverse = TRUE)) +
  geom_line(data = df.OP %>% filter(PFT == 18),
            aes(x = time, y = LAI), color = "darkgrey") +
  scale_fill_manual(breaks = c(2,3,4),
                    values = c("#83CCC0","#44AA99","#186659")) +
  labs(x = 'Time since last disturbance (yr)',y = "LAI \r\n (m²/m²)", fill = "PFT") +
  theme_bw() +
  theme(legend.position = c(0.1,0.8),
        text = element_text(size = 24),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 24),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 24)) +
  guides(fill = "none")


df.soil <- data.frame(
  yr = datum$year,
  month = datum$month,
  fast.grnd.c = datum$emean$fast.grnd.c,
  fast.soil.c = datum$emean$fast.soil.c,
  struct.soil.c = datum$emean$struct.soil.c,
  struct.grnd.c = datum$emean$struct.grnd.c,
  microbe.soil.c = datum$emean$microbe.soil.c,   # 0 not resolved
  passive.soil.c = datum$emean$passive.soil.c,   # 0 not resolved
  slow.soil.c = datum$emean$slow.soil.c) %>%
  mutate(totalsc = fast.grnd.c + fast.soil.c + struct.soil.c + struct.grnd.c + microbe.soil.c + passive.soil.c + slow.soil.c)

df.soil.long <- df.soil %>% pivot_longer(cols = - c(yr,month),
                                         names_to = "var",
                                         values_to = "value")

ggplot(data = df.soil.long) +
  geom_line(aes(x = yr, y = value, color = var)) +
  theme_bw()
