library(sf)
library(ggplot2)
library(tmap)
library(dplyr)

data(World)
# package tmap has a syntax similar to ggplot. The functions start all with tm_
tm_shape(World) +
  tm_borders()

#simple examination
head(World)
names(World)
class(World)
dplyr::glimpse(World)

#What happens when you execute plot?
plot(World[4])
plot(World[,1])
plot(World[1,]) #primeira linha do dataframe
plot(World["pop_est"])

#geometry “column” is the geometries as objects
View(World)
head(World[, 1:4])

#classe
class(World)
class(World$geometry)

#extracting and assigning geometries to existant data frames and the possibility
#to drop the geometries
head(sf::st_coordinates(World))

no_geom <- sf::st_drop_geometry(World)
class(no_geom)

#bounding boxes
st_bbox(World)

###Manipulating sf objects
unique(World$continent)

World %>%
  filter(continent == "South America") %>%
  tm_shape() +
  tm_borders()

World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX"), "red", "grey")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")


###Loading, ploting, and saving a shapefile from the disk
#install.packages("rnaturalearth")
#install.packages("remotes")
#remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearth)
library(rnaturalearthhires)
bra <- ne_states(country = "brazil", returnclass = "sf")
plot(bra)

dir.create("data/shapefiles", recursive = TRUE)
st_write(obj = bra, dsn = "data/shapefiles/bra.shp", delete_layer = TRUE)

#Check the files that are created: .shp, .shx, .dbf, .cpg, prj.
#To read again this shapefile, you would execute:
bra2 <- read_sf("data/shapefiles/bra.shp")
class(bra2)

plot(bra)
plot(bra2)

###Loading, ploting, and saving a raster from the disk
library(raster)
dir.create(path = "data/raster/", recursive = TRUE)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")
plot(tmax_data)

is(tmax_data) #the data are a raster stack, several rasters piled

dim(tmax_data)

extent(tmax_data)

res(tmax_data)

###Palettes
#| eval: false
library(RColorBrewer)
display.brewer.all(type = "seq")
display.brewer.all(type = "div")
