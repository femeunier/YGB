rm(list = ls())

library(ggplot2)
library(ggrepel)
library(ggforce)

str_name<-'/home/femeunier/Desktop/FWO/ESA_tropical_africa_LC.tif'
imported_raster=raster(str_name)
imported_raster.na <- imported_raster
NAvalue(imported_raster.na) <- 210
plot(imported_raster.na)


getmode <- function(v,...) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# mod_raster <- reclassify(imported_raster,t(matrix(c(10,1,
#                                                     11,1,
#                                                     12,1,
#                                                     20,1,
#                                                     30,1,
#                                                     40,1,
#                                                     50,2,
#                                                     60,3,
#                                                     61,3,
#                                                     62,3,
#                                                     70,2,
#                                                     80,3,
#                                                     90,4,
#                                                     100,5,
#                                                     110,5,
#                                                     120,5,
#                                                     122,5,
#                                                     130,6,
#                                                     140,7,
#                                                     150,6,
#                                                     151,6,
#                                                     152,6,
#                                                     153,6,
#                                                     160,2,
#                                                     170,2,
#                                                     180,2,
#                                                     190,8,
#                                                     200,6,
#                                                     201,6,
#                                                     202,6,
#                                                     210,9),nrow = 2)))

# # https://maps.elie.ucl.ac.be/CCI/viewer/download/CCI-LC_Maps_Legend.pdf
# 10 = cropland,
# 11 = Herbaceous cover
# 12 = Tree or shrub cover
# 20 = Cropland, irrigated or post flooding
# 30 = Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# 40 = Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)
# 50 = Tree cover, broadleaved, evergreen, closed to open (>15%)
# 60 = Tree cover, broadleaved, deciduous, closed to open (>15%)
# 61 = Tree cover, broadleaved, deciduous, closed (>40%)
# 62 = Tree cover, broadleaved, deciduous, open (15‐40%)
# 70 = Tree cover, needleleaved, evergreen, closed to open (>15%)
# 80 = Tree cover, needleleaved, deciduous, closed to open (>15%)
# 90 = Tree cover, mixed leaf type (broadleaved and needleleaved)
# 100 = Mosaic tree and shrub (>50%) / herbaceous cover (<50%)
# 110 = Mosaic herbaceous cover (>50%) / tree and shrub (<50%)
# 120 = Shrubland
# 122 = Deciduous shrubland
# 130 = Grassland
# 140 = Lichens and mosses
# 150 = Sparse vegetation (tree, shrub, herbaceous cover) (<15%)
# 151 = Sparse tree (<15%)
# 152 = Sparse shrub (<15%)
# 153 = Sparse herbaceous cover (<15%)
# 160 = Tree cover, flooded, fresh or brakish water
# 170 = Tree cover, flooded, saline water
# 180 = Shrub or herbaceous cover, flooded, fresh/saline/brakish wate
# 190 = Urban areas
# 200 = Bare areas
# 201 = Consolidated bare areas
# 202 = Unconsolidated bare areas
# 210 = Water bodies

init.class <- data.frame(
  num = c(10:12,seq(20,60,10),61:62,
           seq(70,120,10),
           122,
           seq(130,150,10),
           151,152,153,
           seq(160,200,10),
           201,202,210),
  veg.type = c("cropland",
               "Herbaceous cover",
               "Tree or shrub cover",
               "Cropland, irrigated or post flooding",
               "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)",
               "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)",
               "Tree cover, broadleaved, evergreen, closed to open (>15%)",
               "Tree cover, broadleaved, deciduous, closed to open (>15%)",
               "Tree cover, broadleaved, deciduous, closed (>40%)",
               "Tree cover, broadleaved, deciduous, open (15‐40%)",
               "Tree cover, needleleaved, evergreen, closed to open (>15%)",
               "Tree cover, needleleaved, deciduous, closed to open (>15%)",
               "Tree cover, mixed leaf type (broadleaved and needleleaved)",
               "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)",
               "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)",
               "Shrubland",
               "Deciduous shrubland",
               "Grassland",
               "Lichens and mosses",
               "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)",
               "Sparse tree (<15%)",
               "Sparse shrub (<15%)",
               "Sparse herbaceous cover (<15%)",
               "Tree cover, flooded, fresh or brakish water",
               "Tree cover, flooded, saline water",
               "Shrub or herbaceous cover, flooded, fresh/saline/brakish water",
               "Urban areas",
               "Bare areas",
               "Consolidated bare areas",
               "Unconsolidated bare areas",
               "Water bodies"))

# Reclassify
# 1 = Grass/Savannah, 2 = Evergreen, 3 = Deciduous forest, 4 = Others

trans.mat <- t(matrix(c(10,4,

                        11,1,
                        12,1,

                        20,4,
                        30,4,
                        40,4,

                        50,2,

                        60,3,
                        61,3,
                        62,3,

                        70,2,

                        80,3,

                        90,4,

                        100,1,
                        110,1,
                        120,1,
                        122,1,
                        130,1,

                        140,4,

                        150,1,
                        151,1,
                        152,1,
                        153,1,

                        160,2,
                        170,2,

                        180,1,

                        190,4,
                        200,4,
                        201,4,
                        202,4,
                        210,4),
                      nrow = 2))


df.trans <- as.data.frame(trans.mat) %>% rename(num = V1,
                                                new = V2)
init.end.class <- init.class %>% left_join(df.trans,
                                           by = "num")


df.Afr <- data.frame(num = as.vector(imported_raster)) %>%
  left_join(init.end.class,
            by = "num")

df.Afr.sum <- df.Afr %>%
  filter(num != 210) %>%
  mutate(num = as.factor(num)) %>%
  group_by(veg.type,new) %>%
  summarise(N = length(new),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(prop = N/sum(N)) %>%
  arrange(desc(prop))

Afr.count.data <- df.Afr.sum %>%
  mutate(new = as.factor(new),
         veg.type = as.character(veg.type)) %>%
  mutate(new.cat = case_when(new == 1 ~ "Grass",
                             new == 2 ~ "Evergreen forests",
                             new == 3 ~ "Deciduous forests",
                             new == 4 ~ "Other")) %>%
  arrange(desc(new.cat)) %>%
  mutate(prop = prop*100) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
  mutate(labels = case_when(prop < 3 ~ NA_character_,
                            TRUE ~ veg.type)) %>%
  mutate(labels = case_when(labels == "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)" ~
                              "Mosaic cropland ",
                            labels == "Tree cover, broadleaved, deciduous, open (15‐40%)" ~ "Broadleaved deciduous open",
                            labels == "Tree cover, broadleaved, deciduous, closed to open (>15%)" ~
                              "Broadleaved deciduous",
                            labels == "Tree cover, broadleaved, evergreen, closed to open (>15%)" ~
                              "Broadleaved evergreen",
                              TRUE ~ labels)) %>%
  mutate(end = 2 * pi * cumsum(N)/sum(N),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

# ggplot(Afr.count.data, aes(x = "" , y = prop, fill = (new.cat))) +
#   geom_col(width = 1, color = 1) +
#   coord_polar(theta = "y") +
#   # geom_label_repel(aes(y = lab.ypos, label = labels),
#   #                  size = 4.5, nudge_x = 1, show.legend = FALSE) +
#   # geom_label_repel(aes(x = 1.4, y = lab.ypos, label = labels),
#   #                 nudge_x = .7,
#   #                 segment.size = 0.7,
#   #                 show.legend = FALSE) +
#   # geom_label_repel(aes(label = labels, y = lab.ypos),
#   #                  force = 0,nudge_x = 0.7, nudge_y = 0.7,show.legend = FALSE) +
#   # geom_label(aes(label = labels, y = lab.ypos), nudge_x = 1.2,
#   #            nudge_y = 2,show.legend = FALSE) +
#   scale_fill_manual(values = c("#448704","#005401","#c49402","grey")) +
#   guides(fill = guide_legend(title = "")) +
#   theme_void()


ggplot(Afr.count.data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = new.cat)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = labels,
                hjust = hjust, vjust = vjust)) +
  coord_fixed() +
  scale_x_continuous(limits = c(-2.5, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-2, 1.1),    # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_fill_manual(values = c("#448704","#005401","#c49402","grey")) +
  labs(fill = "") +
  theme_void()

imported_raster.mod <- imported_raster
NAvalue(imported_raster.mod) <- 210
mod_raster <- reclassify(imported_raster.mod,
                         trans.mat)

cuts=c(0,1,2,3,4) #set breaks
pal <- c("#c49402","#005401","#448704","grey")
plot(mod_raster, breaks=cuts, col = pal)

mod_raster_upscaled <- aggregate(mod_raster, fact = 0.25/res(mod_raster),
                                 fun = modal, na.rm = TRUE)
mod_raster_upscaled_na <- mod_raster_upscaled

df.LC.coarse <- as.data.frame(mod_raster_upscaled,
                              xy = TRUE) %>% filter(!is.na(ESA_tropical_africa_LC)) %>%
  rename(lon = x,
         lat = y,
         LC = ESA_tropical_africa_LC)

ggplot() +
  geom_tile(data = df.LC.coarse ,
            aes(x = lon, y = lat,
                fill = as.factor(LC)),
            alpha = 1) +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme_bw() +
  labs(x = "",y="",fill = "LC") +
  theme(legend.position = c(0.2,0.2)) +
  guides(fill = "none")


saveRDS(df.LC.coarse,file = "./data/LC.RDS")

# map <- raster("/home/femeunier/Downloads/FloristicType.tif")
# df.map <- as.data.frame(map, xy = T) %>%
#   mutate(Name = case_when(FloristicType == 1 ~ "Atlantic highland evergreen",
#                           FloristicType == 2 ~ "Atlantic coastal evergreen",
#                           FloristicType == 3 ~ "Atlantic inland evergeen",
#                           FloristicType == 4 ~ "Margin semideciduous",
#                           FloristicType == 5 ~ "Evergreen-semideciduous on sandstone",
#                           FloristicType == 6 ~ "Semideciduous",
#                           FloristicType == 7 ~ "Central evergreen",
#                           FloristicType == 8 ~ "Mixed evergreen",
#                           FloristicType == 9 ~ "Degraded semideciduous",
#                           FloristicType == 10 ~ "Semideciduous-evergreen transition"))
#
# world <- ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot() +
#   geom_tile(data = df.map %>% filter(!is.na(FloristicType)),
#             aes(x = x, y = y,
#                 fill = as.factor(Name))) +
#   geom_sf(data = world,fill = NA,color = "black") +
#   theme_void() +
#   labs(x = "",y="",fill = "Forest type") +
#   theme(panel.grid.major = element_blank(),
#         text = element_text(size = 24)) +
#   scale_x_continuous(limits = c(-10,40),expand = c(0,0)) +
#   scale_y_continuous(limits = c(-10,10),expand = c(0,0)) +
#   guides(size = "none")
#
# tmp <- resample(map,imported_raster)
