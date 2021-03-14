#### Library import ####
library(cholera)
library(tidyverse)
library(OpenStreetMap)

#### Exploring Datasets ####
# Maps
head(road.segments)
head(pumps)

# Cases
head(fatalities)

#### Latitude and Longitude for Pump 7 ####
pump7 <- pumps[7, ]
pump7["lat"] <- 51.51335
pump7["long"] <- -0.1362
pump7["lat_ref"] <- 0.0008988113669
pump7["long_ref"] <- 0.001440502493

#### Transforming x and y to longitude and latitude for all 3 datasets ####
pumps_plt <- pumps %>% 
    mutate(
      # Add labels for pipes
      lbl = str_c("Pump ", id),
      lat = pump7$lat + pump7$lat_ref * (y - pump7$y),
      long = pump7$long + pump7$long_ref * (x - pump7$x)
      )

fatal_plt <- fatalities  %>% 
    mutate(lat = pump7$lat + pump7$lat_ref * (y - pump7$y),
           long = pump7$long + pump7$long_ref * (x - pump7$x))

roads_plt <- road.segments  %>% 
    mutate(lat1 = pump7$lat + pump7$lat_ref * (y1 - pump7$y),
           lat2 = pump7$lat + pump7$lat_ref * (y2 - pump7$y),
           long1 = pump7$long + pump7$long_ref * (x1 - pump7$x),
           long2 = pump7$long + pump7$long_ref * (x2 - pump7$x),)

#### Plotting a simple map of cases ####

ggplot() + geom_point(data = fatal_plt, aes(x = long, y = lat), size = 1.2) +
  geom_segment(data = roads_plt, 
               aes(x = long1, y = lat1, xend = long2, yend = lat2)) +
  geom_point(data = pumps_plt, aes(x = long, y = lat), 
             col = "red4", pch = 25, size = 3, fill = "red1") +
  geom_label(data = pumps_plt, aes(x = long, y = lat, label = lbl), 
             nudge_y = 0.0007) 

# To explore:
# ggplot themes to make the map the most appealing
# ggrepel for automatic label separation

# Simple density plot with geom_hex
ggplot() +
  geom_hex(data = fatal_plt, aes(x = long, y = lat), bins = 13) + 
  geom_segment(data = roads_plt, 
               aes(x = long1, y = lat1, xend = long2, yend = lat2)) +
  geom_point(data = pumps_plt, aes(x = long, y = lat), 
             col = "red4", fill = "red1", pch = 25, size = 3) +
  geom_label(data = pumps_plt, aes(x = long, y = lat, label = lbl), 
             nudge_y = 0.0007)

# To explore:
# geom_bin2d creates rectangles instead of hexagonsS
  
#### Loops for closest pump ####
# Setting empty vectors for the closest pump to each case
pump_euclid <- rep(0, nrow(fatalities))
pump_man <- rep(0, nrow(fatalities))

# Iterate for each fatality
for (i in 1:nrow(fatalities)){
  # Initial minimum value is set to infinity to be then changed to the actual minimum value encountered in the dataset
  min_euclid <- Inf
  min_man <- Inf

  # Iterate for each pump
  for (j in 1:nrow(pumps)){
    
    # Calculate Euclidean distance
  dist_euclid <- sqrt((fatalities$x[i] - pumps$x[j])**2 +
                        (fatalities$y[i] - pumps$y[j])**2)
  # If smaller than anything encountered previously, then set to the smallest distance and note the pump id number
  if (dist_euclid < min_euclid){
    min_euclid <- dist_euclid
    pump_euclid_id <- pumps$id[j]
  }  
  
  # Calculate Manhattan distance, repeat procedure
  dist_man <- abs(fatalities$x[i] - pumps$x[j]) + 
    abs(fatalities$y[i] - pumps$y[j])
  if (dist_man < min_man) {
    min_man <- dist_man
    pump_man_id <- pumps$id[j]
  }
  }
  
  # Fill in the initial vectors
  pump_euclid[i] <- pump_euclid_id
  pump_man[i] <- pump_man_id
}

# To explore:
# Combining euclidean and manhattan distance
# Using other distances

# Add columns with the closest pump to the original data
# Combine data for pipes furthest away into a single category (0)
fatal_plt["closest_euclid"] <- as.factor(ifelse(pump_euclid %in% c(6, 7, 8, 9, 10), pump_euclid, 0)) 
fatal_plt["closest_man"] <- as.factor(ifelse(pump_man %in% c(6, 7, 8, 9, 10), pump_man, 0))

# To explore:
# Include all pumps and see how it affects the graph

#### Grouping by closest pump on a graph ####

# Euclidean distance
ggplot() + 
  geom_point(data = fatal_plt, aes(x = long, y = lat, color = closest_euclid),
             size = 1.5) +
  geom_segment(data = roads_plt, 
               aes(x = long1, y = lat1, xend = long2, yend = lat2)) +
  geom_point(data = pumps_plt, aes(x = long, y = lat), col = "red4", pch = 25, 
             size = 3, fill = "red1") +
  geom_label(data = pumps_plt, aes(x = long, y = lat, label = lbl), 
             nudge_y = 0.0007) + 
  labs(title = "Pump 7 was the closest pump for the majority of cholera cases",
       color = "Closest Pump") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) + 
  scale_color_discrete(labels = c("Other", "Pump 6", "Pump 7", "Pump 8",
                                 "Pump 9", "Pump 10"))

# Manhattan distance
plt_man <- ggplot() +
  geom_point(data = fatal_plt, aes(x = long, y = lat, color = closest_man),
             size = 1.5) +
  geom_segment(data = roads_plt,
               aes(x = long1, y = lat1, xend = long2, yend = lat2)) +
  geom_point(data = pumps_plt, aes(x = long, y = lat), 
             col = "red4", fill = "red1", pch = 25, size = 3) +
  geom_label(data = pumps_plt, aes(x = long, y = lat, label = lbl),
             nudge_y = 0.0007)

#### Percentage of cases related to each pump ####
fatal_plt %>% 
  pivot_longer(cols = c(closest_euclid, closest_man), names_to = "metric",
               values_to = "pump_id") %>% 
  group_by(metric, pump_id) %>% 
  count() %>% 
  pivot_wider(names_from = metric, values_from = n) %>% 
  mutate(euclid_perc = round(closest_euclid / nrow(fatalities), 2),
         man_perc = round(closest_man / nrow(fatalities)), 2)

#### Combining with modern day maps ####

# Accessing a modern map of London for that area
london <- openmap(c(max(roads_plt$lat2), min(roads_plt$long1)),
        c(min(roads_plt$lat1), max(roads_plt$long2)), 
        type = "osm", minNumTiles = 10)

# Transforming the data into a Mercator projection
fatal_merc <- projectMercator(fatal_plt$lat, fatal_plt$long, drop = TRUE) %>%
  as.data.frame()

# Adding the fatalities onto a modern map
autoplot.OpenStreetMap(london) + 
  geom_point(data = fatal_merc, aes(x = x, y = y)) +
  labs(title = "1854 cholera fatalities on a map of modern London")
  
# To explore:
# How can the precision of the conversion be improved?
# Different types of maps, other than "osm"

# Smooth density on a modern map
autoplot.OpenStreetMap(london) + 
  stat_density2d(data = fatal_merc, 
                 aes(x = x, y = y, fill = ..density..), 
                 geom = "raster", contour = FALSE, alpha = 0.4) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  labs(title = "The density of cholera fatalities", fill = "Density")

# To explore:
# What colours would work the best with the gradient?