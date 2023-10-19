rm(list = ls())

library(dplyr)
library(ggplot2)

WD.breaks <-  c((0.53+0.71)/2,(0.71+0.90)/2)

file <- "/home/femeunier/Documents/data/Yangambi/data/inventories/YGB-27 MIX-05 MCU.csv"
data <- read.csv(file,header = TRUE)

data.select <- data %>% dplyr::select(Tree.ID,X,Y,WD,DBH4.2,T1) %>% rename(dbh = DBH4.2) %>% mutate(dbh = dbh/10) %>%
  filter(X <= 20, X >=0,Y >= 0,dbh>0)  %>% rename(wood.dens = WD,
                                                  tag = Tree.ID) %>% rename(plots = T1)


ggplot(data = data.select) +
  geom_point(aes(x = X,y = Y, size = dbh,color = wood.dens),shape = 1) +
  facet_wrap(~ plots,nrow = 5) +
  theme_bw()

write.csv(x = data.select,file = "/home/femeunier/Documents/data/Yangambi/data/inventories/Yangambi_census_MIX05.csv")

# css.data <- data.frame(time = 2000,patch = 1,
#                        cohort = sprintf("0x%3.3X", seq(1,nrow(data.select))),
#                        dbh = data.select$dbh,hite = 10*data.select$dbh**0.25 ,pft = data.select$pft,
#                        n = 1/(20*20),bdead = 0.1, balive = 0.2, lai = 0.1)%>% arrange(desc(dbh))
#
# patch_file <- "/home/femeunier/Documents/R/ED2_Support_Files/pss+css_processing/sites/Gigante/Gigante_all.lat9.000lon-79.000.pss"
# pss.data <- read.table(patch_file,header = TRUE) %>% filter(patch < 2) %>% mutate(area = 1/length(area),
#                                                                                   time = 2000,
#                                                                                   patch = sprintf("0x%3.3X",patch))
# css.data$patch <- pss.data$patch
#
# write.table(x = css.data,file = "~/Documents/data/Yangambi/data/ED2/Yangambi.lat0.000lon24.000.css"
#             , append    = FALSE
#             , quote     = FALSE
#             , sep       = " "
#             , row.names = FALSE
#             , col.names = TRUE)
#
# write.table(x = pss.data,file = "~/Documents/data/Yangambi/data/ED2/Yangambi.lat0.000lon24.000.pss",sep = " ",
#             row.names = FALSE)
