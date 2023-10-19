rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

census.file   = '/home/femeunier/Documents/data/Yangambi/data/inventories/Yangambi_census.csv'
wood.density.brks <- c(0.53,0.71)

census.data <- read.csv(census.file) %>% mutate(pft = case_when(wood.dens < wood.density.brks[1] ~ 2,
                                                                wood.dens > wood.density.brks[2] ~ 4,
                                                                TRUE ~ 3),
                                                dbh_group = case_when(
                                                  dbh < 10 ~ 0,
                                                  dbh < 20 ~ 1,
                                                  dbh < 30 ~ 2,
                                                  dbh < 40 ~ 3,
                                                  dbh < 50 ~ 4,
                                                  dbh < 60 ~ 5,
                                                  dbh < 70 ~ 6,
                                                  dbh < 80 ~ 7,
                                                  dbh < 90 ~ 8,
                                                  dbh < 100 ~ 9,
                                                  TRUE ~ 10),
                                                n = 1/(20*20))

census.data.patch <- census.data  %>% group_by(pft,plots,dbh_group) %>% summarise(n = sum(n)) %>% ungroup() %>%
  complete(plots = 1:25,pft = c(2,3,4),dbh_group = seq(1,10),fill = list(n = 0)) %>% group_by(pft,dbh_group) %>%
  summarise(sd = sd(n, na.rm = TRUE),
            n = mean(n,na.rm = TRUE),
            N = length(dbh_group))

tot.sum <- census.data %>% group_by(plots,dbh_group) %>% summarise(n = sum(n)) %>% ungroup() %>%
  complete(plots = 1:25,dbh_group = seq(1,10),fill = list(n = 0)) %>% group_by(dbh_group) %>%
  summarise(sd = sd(n, na.rm = TRUE),
            n = mean(n,na.rm = TRUE),
            N = length(dbh_group),
            se = sd/sqrt(N))

barwidth = 0.9

ggplot() +
  geom_errorbar(data = tot.sum,
                mapping = aes(x = dbh_group, ymin = 0.2*(n)*10000, ymax = (n + sd)*10000),
                width = 0.2) +
  geom_bar(data = census.data.patch,
           mapping = aes(x = dbh_group, y = n*10000, fill = as.factor(pft)),
           stat="identity",
           position='stack',
           width = barwidth) +
  labs(fill = "Plant Functional Type", x = "DBH (cm)", y = "Tree density (ind/ha)") +
  scale_fill_manual(values = c("#9FFF8C","#44CC29","#137300"),
                    labels = c("Early-successional","Mid-successional","Late-successional")) +
  scale_x_continuous(breaks  = seq(1,(0+length(unique(tot.sum$dbh_group)))),
                     labels = c("10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","> 100") ) +
  theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.7, 0.75),
        text = element_text(size = 24))


ggsave(filename = "./Figures/ICdensity.png", plot = last_plot(),
      width = 20,height = 12,dpi = 300,units = "cm")

tot.sum2 <- census.data %>% group_by(dbh_group) %>% summarise(n = sum(n)/25) %>% ungroup() %>%
  complete(dbh_group = seq(1,10),fill = list(n = 0)) %>% group_by(dbh_group) %>%
  summarise(sd = sd(n, na.rm = TRUE),
            n = mean(n,na.rm = TRUE),
            N = length(dbh_group),
            se = sd/sqrt(N))

ggplot() +
  geom_bar(data = tot.sum2,
           mapping = aes(x = dbh_group, y = n*10000),
           stat="identity",
           width = barwidth) +
  scale_x_continuous(breaks  = seq(1,(0+length(unique(tot.sum$dbh_group)))),
                     labels = c("10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","> 100") ) +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Tree density (ind/ha)") +
  ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.7, 0.85),
        text = element_text(size = 24))

#################################################################################################################################

ggplot(data = census.data)+
  geom_histogram(aes(x = wood.dens,fill = as.factor(pft)),bins = 35) +
  geom_vline(xintercept = wood.density.brks,linetype = 2,color = "black") +
  labs(x = "Wood density (g/cm³)", y = "Count (#)", fill = "PFT") +
  scale_fill_manual(values = c("#9FFF8C","#44CC29","#137300"),
                    labels = c("Early","Mid","Late")) +
  theme_bw() +
  ggplot2::theme(legend.position = c(0.15, 0.74),
        text = element_text(size = 24))

ggsave(filename = "./Figures/IC.WD.png", plot = last_plot(),
       width = 20,height = 12,dpi = 300,units = "cm")
