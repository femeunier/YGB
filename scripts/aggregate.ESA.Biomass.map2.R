rm(list = ls())

library(raster)

file <- "/data/gent/vo/000/gvo00074/felicien/ESA_Biomass/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0.nc"
tmp_raster <- brick(file, varname="agb")

e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
raster.crop <- crop(tmp_raster, e, snap="out")

raster.aggregated.crop <- aggregate(raster.crop, fact = 0.05/res(tmp_raster))

writeRaster(raster.aggregated.crop,
            filename='/data/gent/vo/000/gvo00074/felicien/ESA_Biomass/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0.aggr.tif',
            format="GTiff",
            overwrite=TRUE,
            options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


file <- "/home/femeunier/Documents/projects/YGB/outputs/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0.aggr.tif"
tmp_raster <- brick(file, varname="agb")
plot(tmp_raster)

hist(tmp_raster)
