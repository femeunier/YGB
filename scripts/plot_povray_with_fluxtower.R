rm(list = ls())

library(dplyr)

setwd("/home/femeunier/Downloads/pylon")

file.copy("/home/femeunier/Downloads/pylon/template.pov","/home/femeunier/Downloads/pylon/forest.pov")

data <- read.csv("/home/femeunier/Documents/data/Yangambi/data/inventories/YGB-27 MIX-05 MCU.csv",
                 header = TRUE,
                stringsAsFactors = FALSE)

data.select <- data %>% dplyr::select(Tree.ID,X,Y,WD,DBH4.2,T1) %>% rename(dbh = DBH4.2) %>% mutate(dbh = dbh/10) %>%
  filter(X <= 20, X >= 0,Y >= 0,dbh>0)  %>%
  rename(wood.dens = WD,
         tag = Tree.ID) %>%
  rename(plots = T1) %>%
  mutate(plots = as.numeric(plots)) %>%
  mutate(pft = case_when(wood.dens <= 0.4 ~ 2,
                         wood.dens <= 0.6 ~ 3,
                         TRUE ~ 4)) %>%
  mutate(height = (36.36-31.66*exp(-0.0221*dbh)))

xplot = sort(rep(1:5,5))
yplot = rep(1:5,5)
for (iplot in seq(1,length(unique(data.select$plots)))){

  data2plot <- data.select %>% filter(plots == iplot)

  Delta_X = 25 + (xplot[iplot] - 1)*20
  Delta_Y = -25 + (yplot[iplot] - 1)*20
  for (i in seq(1,nrow(data2plot))){
    write(paste0("plant(",data2plot$dbh[i],",",
                 data2plot$pft[i],",",
                 Delta_X + data2plot$X[i],",",
                 Delta_Y + data2plot$Y[i],",",data2plot$height[i],")"),
          "/home/femeunier/Downloads/pylon/forest.pov",
          append = TRUE)
  }

}


width  =  11.0
height =  8.5
depth = 300

povopts = paste("-D"
                ,"-V"
                ,"+UA"
                ,paste("+W",round(width*depth ),sep="")
                ,paste("+H",round(height*depth),sep="")
                ,sep = " ")

system2("chmod", paste("777","/home/femeunier/Downloads/pylon/forest.pov"))
system2("/usr/bin/povray", paste(povopts,"/home/femeunier/Downloads/pylon/forest.pov", sep = " "))

file.remove("/home/femeunier/Downloads/pylon/forest.pov")
