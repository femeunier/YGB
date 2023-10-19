rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(YGB)
library(raster)

##############################################################################

Avi.AFR <- stack("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/AGBavit_AFR.gri")

e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
Avi.AFR.crop <- crop(Avi.AFR, e)

##############################################################################
# system2("rsync",paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/pr.CMIP6.scenarssp585_yr.min_2015_yr.max_2100.RDS",
#                       "/home/femeunier/Documents/projects/CongoAS/outputs/"))
# system2("rsync",paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/pr.CMIP6.scenarhistorical_yr.min_1900_yr.max_2020.RDS",
#                       "/home/femeunier/Documents/projects/CongoAS/outputs/"))

pr.historical <- readRDS("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/pr.CMIP6.scenarhistorical_yr.min_1900_yr.max_2020.RDS")
pr.scenario <- readRDS("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/pr.CMIP6.scenarssp585_yr.min_2015_yr.max_2100.RDS")

pr.all <- bind_rows(list(pr.historical,
                         pr.scenario))


init.year <- min(pr.all$yr)
final.year <- max(pr.all$yr)

years <- seq(init.year+14,final.year-15,10)
models <- unique(pr.all$model)

all.models.years.MCWD <- data.frame()

for (iyear in seq(1,length(years))){

  print(paste0("- ",iyear/length(years)))

  for (imodel in seq(1,length(models))){
    print(paste0("-- ",imodel/length(models)))
    cdf <-  pr.all %>%
      filter(model == models[imodel],
             yr >= (years[iyear] - 15),  yr < (years[iyear] + 15))

    if (nrow(cdf) < 12) next()

    cdf <- cdf %>%
      dplyr::filter(lat <= 10, lat >= -15, lon <= 45, lon >= -10) %>%
      group_by(model,yr,lat,lon) %>%
      mutate(MAP = mean(pr)*86400*365,
             N = length(pr)) %>%
      filter(N == 12) %>%
      mutate(month = 1:12)


    # First calculate MCWD
    cdf.MCWD <- cdf %>%
      mutate(Pmm=pr*86400*30) %>%
      filter(!is.na(Pmm)) %>%
      group_by(model,yr,lat,lon,month) %>%
      dplyr::select(model,lon,lat,Pmm,yr,month) %>%
      group_by(model,month,lon,lat) %>%
      summarise(Pmm = mean(Pmm),
                .groups = "keep") %>%
      group_by(model,lon,lat) %>%
      mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
             E = 3.33,
             Etot = E*Ndays) %>%
      mutate(diff = Pmm - Etot) %>%
      mutate(wettest.month = which.max(diff)) %>%
      mutate(month.ord = 1 + (month - wettest.month)) %>%
      mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                                   TRUE ~ month.ord)) %>%
      arrange(month.ord) %>%
      mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                             TRUE ~ NA_real_)) %>%
      mutate(CWD = calc.CWD(diff,CWD[1])) %>%
      arrange(month) %>%
      mutate(MCWD = min(CWD))

    cdf.MCWD.sum <- cdf.MCWD %>%
      group_by(model,lat,lon) %>%
      summarise(MCWD = mean(MCWD,na.rm = TRUE),
                .groups = "keep")

    cdf.MAP.sum <- cdf.MCWD %>%
      group_by(model,lat,lon,month) %>%
      summarise(Pmonth = mean(Pmm,na.rm = TRUE),
                .groups = "keep") %>%
      group_by(model,lat,lon) %>%
      summarise(MAP = sum(Pmonth,na.rm = TRUE),
                .groups = "keep")

    # Then resample

    lat <- cdf.MCWD.sum %>% pull(lat)
    lon <- cdf.MCWD.sum %>% pull(lon)

    if (length(unique(diff(sort(unique(lat))))) > 1){
      res <- max(c(diff(sort(unique(lat))),diff(sort(unique(lon)))))
      dfr.cdf <- raster(SpatialPixelsDataFrame(points = cdf.MCWD.sum[c("lon","lat")],
                                               data = cdf.MCWD.sum["MCWD"],
                                               tolerance = res/10))

      dfr.MAP.cdf <- raster(SpatialPixelsDataFrame(points = cdf.MAP.sum[c("lon","lat")],
                                               data = cdf.MAP.sum["MAP"],
                                               tolerance = res/10))

    } else {
      dfr.cdf <- rasterFromXYZ(cdf.MCWD.sum  %>% ungroup() %>% dplyr::select(lon,lat,MCWD))
      dfr.MAP.cdf <- rasterFromXYZ(cdf.MAP.sum  %>% ungroup() %>% dplyr::select(lon,lat,MAP))
    }

    dfr.cdf.rspld <- resample(dfr.cdf,Avi.AFR.crop)
    dfr.cdf.rspld[is.na(Avi.AFR.crop)] <- NA

    dfr.cdf.MAP.rspld <- resample(dfr.MAP.cdf,Avi.AFR.crop)
    dfr.cdf.MAP.rspld[is.na(Avi.AFR.crop)] <- NA

    cdf.rspld <- as.data.frame(dfr.cdf.rspld,xy = TRUE) %>%
      rename(lon = x,
             lat = y) %>% filter(!is.na(MCWD))

    cdf.MAP.rspld <- as.data.frame(dfr.cdf.MAP.rspld,xy = TRUE) %>%
      rename(lon = x,
             lat = y) %>% filter(!is.na(MAP))

    all.models.years.MCWD <- bind_rows(list(all.models.years.MCWD,
                                            cdf.rspld %>% mutate(model = models[imodel],
                                                                 year = years[iyear]) %>%
                                              mutate(MAP = as.vector(cdf.MAP.rspld %>% pull(MAP)))
                                            ))


    # world <- ne_countries(scale = "medium", returnclass = "sf")
    # ggplot() +
    #   geom_tile(data = cdf.rspld ,
    #             aes(x = lon, y = lat,
    #                 fill = MCWD),
    #             alpha = 1) +
    #   geom_sf(data = world,fill = NA,color = "black") +
    #   theme_bw() +
    #   scale_x_continuous(limits = c(-10,45),expand = c(0,0)) +
    #   scale_y_continuous(limits = c(-16,10),expand = c(0,0)) +
    #   scale_fill_gradient(low = "red",high = "white",na.value = "transparent") +
    #   labs(x = "", y = "", fill = "AGB \r\n (kgC/m²)") +
    #   guides(size = "none") +
    #   theme(text = element_text(size = 20))
  }
}

saveRDS(all.models.years.MCWD,"./outputs/TS.climate.RDS")

# scp TS.climate.variable.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

# all.models.years.MCWD <- readRDS("/home/femeunier/Documents/projects/YGB/scripts/outputs/TS.climate.RDS")
#
# all.models.years.MCWD.sum <-
#   all.models.years.MCWD %>%
#   group_by(year,model) %>%
#   summarise(MCWD.m = mean(MCWD,na.rm = TRUE),
#             MAP.m = mean(MAP,na.rm = TRUE),
#             .groups = "keep")
#
# all.models.years.MCWD.sum.long <- all.models.years.MCWD.sum %>%
#   pivot_longer(cols = c(MCWD.m,MAP.m),
#                names_to = "var",
#                values_to = "value")
#
# all.models.years.MCWD.long <- all.models.years.MCWD %>%
#   pivot_longer(cols = c(MCWD,MAP),
#                names_to = "var",
#                values_to = "value")
#
#
# all.models.years.MCWD.long.all <- bind_rows(list())
#
# ggplot(data = all.models.years.MCWD.long %>% filter(year == 2005,
#                                                         !is.na(value)) ) +
#   geom_density(aes(x = value, color = as.factor(model)), fill = NA,alpha = 0.2, show.legend = FALSE) +
#   geom_density(aes(x = value), fill = NA,color = "black", alpha = 1, show.legend = FALSE) +
#   facet_wrap(~ as.factor(var),scales = "free") +
#   theme_bw()
#
#
# ggplot(data = all.models.years.MCWD.sum.long) +
#   geom_line(aes(x = year, y = value, color = model)) +
#   facet_wrap(~ var, scales = "free_y") +
#   theme_bw()
#
#
#
