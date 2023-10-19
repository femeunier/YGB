rm(list = ls())

# Instructions go here

library(R.utils)

# curl  --cert /user/gent/425/vsc42558/ceda_pydap_cert_code/online_ca_client/contrail/security/onlineca/client/sh/creds.pem -L -c /dev/null https://dap.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v3.0/netcdf/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0.nc --output /data/gent/vo/000/gvo00074/felicien/ESA_Biomass/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0.nc
# https://dap.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v3.0/netcdf/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0.nc?download=1

perm <- "/user/gent/425/vsc42558/ceda_pydap_cert_code/online_ca_client/contrail/security/onlineca/client/sh/creds.pem"
dir <- "http://dap.ceda.ac.uk/thredds/fileServer/badc/cru/data/cru_jra/cru_jra_2.2/data"

vars <- c("dswrf","pre")
years <- seq(2012,2020)

dir.OP <- "/data/gent/vo/000/gvo00074/ED_common_data/met/CRUJRA"

for (ivar in seq(1,length(vars))){
  for (iyear in seq(1,length(years))){
    var = vars[ivar]
    year = years[iyear]

    filename <- paste0("crujra.v2.2.5d.",var,".",year,".365d.noc.nc")
    filename.compressed <- paste(filename,"gz",sep = ".")
    filename.cropped <- paste0("crujra.v2.2.5d.",var,".",year,".365d.noc.cropped.nc")

    source_file <- file.path(dir,var,filename.compressed)
    dest.file <- file.path(dir.OP,var,filename.compressed)
    dest.uncompressed <- file.path(dir.OP,var,filename)

    system2("curl",paste("--cert",
                         perm,"-L",
                         "-c /dev/null",
                         source_file,
                         "--output",
                         dest.file))

    gunzip(dest.file)
  }
}


# dest.unziped.cropped <- file.path("./outputs",filename.cropped)
# system2("cdo",paste0("sellonlatbox",",",-10,",",45,",",-15,",",10," ",dest.uncompressed," ",dest.unziped.cropped))
# system2("rm",dest.uncompressed)

