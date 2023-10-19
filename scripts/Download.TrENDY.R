rm(list = ls())

# system2("sshfs",
#         "trendy-v9@trendy.ex.ac.uk:/output /data/gent/vo/000/gvo00074/felicien/TrENDY/localdir")
# sshfs trendy-v9@trendy.ex.ac.uk:/output /data/gent/vo/000/gvo00074/felicien/TrENDY/localdir

cVeg.files <- list.files("/data/gent/vo/000/gvo00074/felicien/TrENDY/localdir",
                         pattern = "*cVeg.nc",
                         recursive = TRUE)

setwd("/data/gent/vo/000/gvo00074/felicien/TrENDY")

for (ifile in seq(1,length(cVeg.files))){
  system2('lftp',
          paste0('sftp://trendy-v9:gcb-2020@trendy.ex.ac.uk -e "get /output/',cVeg.files[ifile],' ; exit"'))

}

cRoot.files <- list.files("/data/gent/vo/000/gvo00074/felicien/TrENDY/localdir",
                         pattern = "*cRoot*",
                         recursive = TRUE)

for (ifile in seq(1,length(cRoot.files))){
  system2('lftp',
          paste0('sftp://trendy-v9:gcb-2020@trendy.ex.ac.uk -e "get /output/',cRoot.files[ifile],' ; exit"'))

}


