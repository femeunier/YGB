rm(list = ls())

load("./outputs/reference.RData")

# plot(datum$emean$agb,type = "l")

matplot(datum$szpft$agb[,12,c(2,3,4,18)],type = "l")

matplot((datum$szpft$sm.stress[,12,c(2,3,4)]),type = "l")
matplot((datum$szpft$mort[,12,c(2,3,4)]),type = "l")

matplot((datum$szpft$ncbmort[,12,c(2,3,4)]),type = "l")

matplot((datum$szpft$gpp[,12,c(2,3,4,18)]),type = "l")
matplot((datum$szpft$npp[,12,c(2,3,4,18)]),type = "l")
matplot((datum$szpft$lai[,12,c(2,3,4,18)]),type = "l")


# load("/home/femeunier/Documents/Gigante_control_long.RData")
#
# plot(datum$szpft$nplant[140,1:11,17])
