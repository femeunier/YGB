rm(list = ls())

library(raster)
library(gdalUtils)
library(gdalUtil)
gdalUtilities::gdalwarp()

file <- "/home/femeunier/Downloads/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0_cropped.nc"
tmp_raster <- brick(file, varname="agb")

# raster.aggregated.crop <- aggregate(tmp_raster, fact = 1/res(tmp_raster))

writeRaster(tmp_raster,
            "/home/femeunier/Downloads/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0_cropped.tif",
            datatype='FLT4S')
raster.aggregated.crop <- gdalwarp(file = tmp_raster,
                                   dstfile = "/home/femeunier/Downloads/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0_cropped_agg.tif", r='mode',
                                   multi=TRUE, tr=res(tmp_raster)*50)

# ncks -d lat,40.,70. -d lon,347.,10. in.nc out.nc
# cdo gridboxmean,50,50 ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0_cropped.nc ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0_cropped_aggr.nc

