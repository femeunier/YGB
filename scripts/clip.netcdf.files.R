rm(list = ls())

# ml purge
# ml R/4.1.0-foss-2021a
# ml CDO

vars <- c("dswrf","pre")
years <- seq(1911,2020)

dir.OP <- "/data/gent/vo/000/gvo00074/ED_common_data/met/CRUJRA"

for (ivar in seq(1,length(vars))){
  for (iyear in seq(1,length(years))){
    var = vars[ivar]
    year = years[iyear]

    filename <- paste0("crujra.v2.2.5d.",var,".",year,".365d.noc.nc")
    filename.cropped <- paste0("crujra.v2.2.5d.",var,".",year,".365d.noc.cropped.nc")

    dest.uncompressed <- file.path(dir.OP,var,filename)
    dest.unziped.cropped <- file.path(dir.OP,var,filename.cropped)

    system2("cdo",paste0("sellonlatbox",",",-15,",",50,",",-20,",",15," ",dest.uncompressed," ",dest.unziped.cropped))
    system2("rm",dest.uncompressed)


  }
}

