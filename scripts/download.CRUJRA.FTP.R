rm(list = ls())

library(R.utils)

credentials <- "mflicien:vyufgx(l7v*'"
dir <- "/badc/cru/data/cru_jra/cru_jra_2.0/data"
ftp <- "ftp.ceda.ac.uk"


vars <- c("dswrf","pre")
years <- seq(1901,20)
var <- "pre"
year <- 1903
filename <- paste0("crujra.v2.0.5d.",var,".",year,".365d.noc.nc.gz")

dest <- file.path("./outputs",filename)

url = paste0("ftp://",credentials,"@",ftp,dir,"/",var,"/",filename)
download.file(url,destfile = dest)

gunzip(dest)

dest.unziped <- file.path("./outputs",paste0("crujra.v2.0.5d.",var,".",year,".365d.noc.nc"))
dest.unziped.cropped <- file.path("./outputs",paste0("crujra.v2.0.5d.",var,".",year,".365d.noc_cropped.nc"))

system2("cdo",paste0("sellonlatbox",",",-10,",",45,",",-15,",",10," ",dest.unziped," ",dest.unziped.cropped))
# cdo ,lonmin,lonmax,latmin,latmax infile outfile
system2("rm",dest.unziped)
