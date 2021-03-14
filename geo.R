#### Library import ####
library(cholera)
library(tidyverse)

#### Datasets ####
# coordinates of streets
roads

# fatalities
head(fatalities)
head(regular.cases)

#### Plotting simple maps ####

roads2 <- roads %>% select(x, y, name) %>% filter(name != "Map Frame") %>% 
  arrange(name, x) %>% group_by(name) %>% 
  mutate(x_end = lag(x), y_end = lag(y)) %>% 
  filter(!is.na(x_end))

ggplot() + geom_point(data = fatalities, aes(x = x, y = y)) +
  geom_segment(data = roads2, 
               aes(x = x, y = y, xend = x_end, yend = y_end)) 

#### Using available tools to plot maps ####
streetNameLocator()
streetNameLocator(zoom = 1)

#### Walking distance ####
# the problem we encounter with geospatial data is that distances aren't usually a straight line

addWalkingPath(origin = 116, destination = 7)
addEuclideanPath(origin = 116, destination = 7)

#### Checking John Snow's theory ####
par(mfrow = c(1, 2))
plot(neighborhoodWalking())
plot(neighborhoodEuclidean())
# difference shows well with pumps 9 and 10
# but how do we convince the government that we're right?

#### Simulation ####
# Placing regularly spaced points
# regular.cases are the equally spaced points
ggplot(regular.cases, aes(x = x, y = y)) + geom_point()
par(mfrow = c(1,1))
plot(neighborhoodWalking(case.set = "expected"), "area.polygons")
addNeighborhoodCases(point.size = 1, alpha.level = 1)

# GIS - geographic information system
# maps most often in shp files
# sf simple features

library(sf)
# lets you know these are points
sf::read_sf("SnowGIS/Cholera_Deaths.shp")


library(rgdal)
image(readGDAL("SnowGIS/OSMap_Grayscale.tif"), col = hcl.colors(20, "Oslo"))
image(readGDAL("SnowGIS/OSMap_Grayscale.tif"), col = hcl.colors(20, "Grays"))
