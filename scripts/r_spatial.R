# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
setwd("C:/APCE 2024/APCE 2024 GIS/apce2024gis")

# restore the libraries of the project 
renv::restore()


# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10))
grey.colors(10)  # shows hexadecimal codes
mycolors<-c("red","white","blue")
mycolors
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale around
barplot(rep(1,10), col = rev(terrain.colors(10)))
library(RColorBrewer)  # pre divined colors
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
barplot(rep(1,10), col = rev(viridis::viridis(10)))
barplot(rep(1,10), col = viridis::plasma(10))
barplot(rep(1,10), col = viridis::heat(10))
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                              layer="my_study_area")

# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
class(elevation) # see if it is raster or vector
plot(protected_areas)
plot(elevation)
plot(protected_areas,add=T) # plot on top of eachother

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict
plot(woodybiom) # scale is smaller, can better change with ggplot (easier)
ggplot() +
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colors=rev(terrain.colors(6)),
                       limits=c(0.77,6.55), # see in qgis
                       oob=squish, # everything larger than the used scale is the largest color value, don't omit the values.
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5)

# add study area, rivers and lakes.
# study area = red line, not filled (not fill=color)
# lake = light blue
# rivers are blue
woody_map<-ggplot() +
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colors=rev(terrain.colors(6)),
                       limits=c(0.77,6.55), # see in qgis
                       oob=squish, # everything larger than the used scale is the largest color value, don't omit the values.
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,color="red",linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA,color="blue",linewidth=0.5) +
  labs(title="Woody biomass") + 
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

woody_map

# make elevation map and show it
elevation_map<-ggplot() +
  tidyterra::geom_spatraster(data=elevation) +
  scale_fill_gradientn(colors=terrain.colors(10),
                       limits=c(500,2100), # see in qgis
                       oob=squish, # everything larger than the used scale is the largest color value, don't omit the values.
                       name="Meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,color="red",linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA,color="blue",linewidth=0.5) +
  labs(title="Elevation") + 
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

elevation_map

# can put them together, but not really nice
woody_map + elevation_map

# plot the rainfall map
rainfall_map<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall) +
  scale_fill_gradientn(colors=pal_zissou1,
                       limits=c(625,1375), # see in qgis
                       oob=squish, # everything larger than the used scale is the largest color value, don't omit the values.
                       name="mm/year") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,color="green",linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA,color="blue",linewidth=0.5) +
  labs(title="Rainfall") + 
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

rainfall_map


# can put them together, but not really nice
woody_map + elevation_map + rainfall_map

# combine the different maps into one composite map using the patchwork library and save it to a high resolution png
all_maps<- woody_map + elevation_map + rainfall_map +
  patchwork::plot_layout(ncol=2) # ncol places on top / next to each other, 3 means the 3 are next to each other and 1 means all are on top of each other 
all_maps
# save the plots, already done once
# ggsave("./figures/all_maps.png",width = 18, height = 18, units = "cm", dpi=300)


############################


### explore your study area #############
# set the limits of your study area
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)
saExt # why are the last four 0? UTM of 10 km

# crop the woody biomass to the extent of the studyarea
woody_sa<-terra::crop(woodybiom,saExt)

woody_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=woodybiom_sa) +
  scale_fill_gradientn(colors=rev(terrain.colors(6)),
                       limits=c(0.77,6.55), # see in qgis
                       oob=squish, # everything larger than the used scale is the largest color value, don't omit the values.
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA,color="blue",linewidth=0.5) +
  labs(title="Woody biomass") + 
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

woody_map_sa
# ggsave("./figures/woody_map_sa.png",width = 18, height = 18, units = "cm", dpi=300)


# Elevation map
elevation_sa<-terra::crop(elevation,saExt) # crop to study area

elevation_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=elevation_sa) +
  scale_fill_gradientn(colours=terrain.colors(10),
                       limits=c(1300,2100),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="elevation") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
elevation_map_sa
# ggsave("./figures/elevation_map_sa.png",width = 18, height = 18, units = "cm", dpi=300)


# plot the rainfall map study area
# plot rainfall map for the study area
# first you need to increase the raster resolution to 30 m
# Define the extent and resolution for the new raster
rainfall_30m <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")  
rainfall_sa<-terra::crop(rainfall_30m,saExt) # crop to study area
rainfall_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(650,1000),
                       oob=squish,
                       name="mm/yr") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Rainfall") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rainfall_map_sa
# ggsave("./figures/rainfall_map_sa.png",width = 18, height = 18, units = "cm", dpi=300)

# make distance to river map
# dist2river_sa<-terra::rast("./_MyData/rivers/DistanceToRiver.tif")
dist2river_sa<-terra::rast("C:/APCE 2024/APCE 2024 GIS/apce2024gis/2022_rivers/DistanceToRiver20sqm.tif")

dist2river_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=dist2river_sa) +
  scale_fill_gradientn(colours=topo.colors(6),
                       limits=c(0,6000),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Distance to river") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
dist2river_map_sa
# ggsave("./figures/dist2river_map_sa.png",width = 18, height = 18, units = "cm", dpi=300)


# burning frequency map from 2001 - 2016
burnfreq_sa<-terra::rast("C:/APCE 2024/APCE 2024 GIS/apce2024gis/_MyData/Fire/BurnFreq.tif")

burnfreq_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=burnfreq_sa) +
  scale_fill_gradientn(colours=pal_zissou2,
                       limits=c(0,16),
                       oob=squish,
                       name="years\nburned") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="n years burned") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
burnfreq_map_sa
# ggsave("./figures/burnfreq_map_sa.png",width = 18, height = 18, units = "cm", dpi=300)

# make a soil fertility map
cec_sa<-terra::rast("C:/APCE 2024/APCE 2024 GIS/apce2024gis/Soil fertility/CEC_5_15cm_SoilFertility.tif")
hist(cec_sa)
cec_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=cec_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(121,250),
                       oob=squish,
                       name="Soil\nCEC\n5-15cm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Soil CEC") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
cec_map_sa
# ggsave("./figures/cec_map_sa.png",width = 18, height = 18, units = "cm", dpi=300)

# landform hills
landform_h_sa<-terra::rast("C:/APCE 2024/APCE 2024 GIS/apce2024gis/_MyData/Landforms/hills.tif")
landform_map_h_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_h_sa)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("valleys\nand\nplains","hills")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="green") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Landform hills") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
landform_map_h_sa
# ggsave("./figures/landform_map_h_sa.png",width = 18, height = 18, units = "cm", dpi=300)

# landform valleys and plains
landform_vp_sa<-terra::rast("C:/APCE 2024/APCE 2024 GIS/apce2024gis/_MyData/Landforms/valleysPlains.tif")
landform_map_vp_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_vp_sa)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("valleys\nand\nplains","hills")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="green") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Landform valleys and plains") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
landform_map_vp_sa
# ggsave("./figures/landform_map_vp_sa.png",width = 18, height = 18, units = "cm", dpi=300)

# landform valleys 
landform_v_sa<-terra::rast("C:/APCE 2024/APCE 2024 GIS/apce2024gis/_MyData/Landforms/valleys.tif")
landform_map_v_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_v_sa)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("other","valleys")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="green") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Landform valleys") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
landform_map_v_sa
# ggsave("./figures/landform_map_v_sa.png",width = 18, height = 18, units = "cm", dpi=300)


# core_protected_areas  map 
r<-terra::rast("C:/APCE 2024/APCE 2024 GIS/apce2024gis/2022_protected_areas/CoreProtectedAreas.tif") 
CoreProtectedAreas_sa <- r |> #  replace NA by 0
  is.na() |>
  terra::ifel(0,r) 

CoreProtectedAreas_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(CoreProtectedAreas_sa)) +
  scale_fill_manual(values=c("grey","lightgreen"),
                    labels=c("no","yes")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Core protected areas") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
CoreProtectedAreas_map_sa
# ggsave("./figures/CoreProtectedAreas_map_sa.png",width = 18, height = 18, units = "cm", dpi=300)

# create 250 random points in your study area
set.seed(123)
rpoints <- terra::spatSample(studyarea, size = 250, 
                             method = "random")

# plot the points
rpoints_map_sa<-ggplot() +
  tidyterra::geom_spatvector(data=rpoints, size=0.5) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="250 random points") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rpoints_map_sa
# ggsave("./figures/rpoints_map_sa.png",width = 18, height = 18, units = "cm", dpi=300)

# combine the maps with patchwork
all_maps_sa<-woody_map_sa +dist2river_map_sa + elevation_map_sa + 
  CoreProtectedAreas_map_sa + rainfall_map_sa + 
  cec_map_sa + burnfreq_map_sa + landform_map_h_sa + rpoints_map_sa +
  patchwork::plot_layout(ncol=3)
all_maps_sa
# ggsave("./figures/all_maps_sa.png", width = 297, height = 210, units = "mm",dpi=300)


#########################


# extract your the values of the different raster layers to the points
# Extract raster values at the points
woody_points <- terra::extract(woody_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(woody=TBA_gam_utm36s)
woody_points
dist2river_points <- terra::extract(dist2river_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(dist2river=distance)
dist2river_points
elevation_points <- terra::extract(elevation, rpoints) |> 
  as_tibble() 
elevation_points
CorProtAr_points <- terra::extract(CoreProtectedAreas_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(CorProtAr=CoreProtectedAreas)
CorProtAr_points
rainfall_points <- terra::extract(rainfall_sa, rpoints) |> 
  as_tibble() |> 
  dplyr::rename(rainfall=CHIRPS_MeanAnnualRainfall)
rainfall_points
cec_points <- terra::extract(cec_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(cec='cec_5-15cm_mean')
cec_points
burnfreq_points <- terra::extract(burnfreq_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(burnfreq=burned_sum)
burnfreq_points
landform_h_points <- terra::extract(landform_h_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(hills=remapped)
landform_h_points




# merge the different variable into a single table
# use woody biomass as the last variable
pointdata<-cbind(dist2river_points[,2],elevation_points[,2],
                 CorProtAr_points[,2],rainfall_points[,2], 
                 cec_points[,2],burnfreq_points[,2],
                 landform_h_points[,2],woody_points[,2]) |>
  as_tibble()
pointdata

# plot how woody cover is predicted by different variables
# Create a correlation panel plot
library(psych)
psych::pairs.panels(
  pointdata ,
  method = "pearson",     # Correlation method (use "spearman" for rank correlation)
  hist.col = "lightblue",  # Color for histograms
  density = TRUE,          # Add density plots
  ellipses = F,         # Add correlation ellipses
  lm = TRUE,                # Add linear regression lines
  stars=T
)

# make long format
names(pointdata)
pointdata_long<-pivot_longer(data=pointdata,
                             cols = dist2river:hills, # all except woody
                             names_to ="pred_var",
                             values_to = "pred_val")
pointdata_long

# panel plot
ggplot(data=pointdata_long, mapping=aes(x=pred_val,y=woody,group=pred_var)) +
  geom_point() +
  geom_smooth() +
  ylim(0,40) +
  facet_wrap(~pred_var,scales="free") 

# do a pca
# Load the vegan package
library(vegan)
# Perform PCA using the rda() function
pca_result <- vegan::rda(pointdata,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)

# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites", pch=pointdata$CorProtAr+1, col = pointdata$hills+1, bg = "blue", cex = 1)
# Add arrows for variables
arrows(0, 0, scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
       length = 0.1, col = "red")
# Label the variables with arrows
text(scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
     labels = colnames(pointdata), col = "red", cex = 0.8, pos = 4)
# Add axis labels and a title
title(main = "PCA Biplot")
xlabel <- paste("PC1 (", round(pca_result$CA$eig[1] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(pca_result$CA$eig[2] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
title(xlab=xlabel)
title(ylab=ylabel)
# add contours for woody cover
vegan::ordisurf(pca_result, pointdata$woody, add = TRUE, col = "green4")


