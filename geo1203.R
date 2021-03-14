#### Library import ####
library(cholera)
library(tidyverse)
library(gridExtra)

#### Datasets ####
# maps
head(roads)
head(road.segments)

# cases
head(fatalities)
head(regular.cases)

pump7 <- pumps[7, ]

pumps_plt <- pumps %>% mutate(lbl = str_c("Pump ", id)) %>% 
    mutate(lat = 51.51334 + 0.001440082004 * (y - pump7$y),
           long = -0.13667 + 0.0008983156581 * (x - pump7$x))

fatal_plt <- fatalities  %>% 
    mutate(lat = 51.51334 + 0.001440082004 * (y - pump7$y),
           long = -0.13667 + 0.0008983156581 * (x - pump7$x))

roads_plt <- road.segments  %>% 
    mutate(lat1 = 51.51334 + 0.001440082004 * (y1 - pump7$y),
           lat2 = 51.51334 + 0.001440082004 * (y2 - pump7$y),
           long1 = -0.13667 + 0.0008983156581 * (x1 - pump7$x),
           long2 = -0.13667 + 0.0008983156581 * (x2 - pump7$x),)

#### Plotting simple maps ####

ggplot() + geom_point(data = fatal_plt, aes(x = long, y = lat), size = 1.5) +
  geom_segment(data = roads_plt, 
               aes(x = long1, y = lat1, xend = long2, yend = lat2)) +
  geom_point(data = pumps_plt, aes(x = long, y = lat), col = "red4", pch = 25, 
             size = 3, fill = "red1") +
  geom_label(data = pumps_plt, aes(x = long, y = lat, label = lbl), nudge_y = 0.001) 
  
case1 <- fatalities[222, ]

pump_euclid <- rep(0, nrow(fatalities))
pump_man <- rep(0, nrow(fatalities))

for (i in 1:nrow(fatalities)){
  min_euclid <- Inf
  min_man <- Inf

  for (j in 1:nrow(pumps)){
  dist_euclid <- sqrt((fatalities$x[i] - pumps$x[j])**2 +
                        (fatalities$y[i] - pumps$y[j])**2)
  dist_man <- abs(fatalities$x[i] - pumps$x[j]) + 
    abs(fatalities$y[i] - pumps$y[j])
  if (dist_euclid < min_euclid){
    min_euclid <- dist_euclid
    pump_euclid_id <- pumps$id[j]
  }
  if (dist_man < min_man){
    min_man <- dist_man
    pump_man_id <- pumps$id[j]
  }
  }
  pump_euclid[i] <- pump_euclid_id
  pump_man[i] <- pump_man_id
  
}

fatalities["closest_euclid"] <- as.factor(pump_euclid)
fatalities["closest_man"] <- as.factor(pump_man)


plt_euclid <- ggplot() + 
  geom_point(data = fatalities, aes(x = x, y = y, color = closest_euclid),
             size = 1.5) +
  geom_segment(data = road.segments, 
               aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_point(data = pumps_plt, aes(x = x, y = y), col = "red4", pch = 25, 
             size = 3, fill = "red1") +
  geom_label(data = pumps_plt, aes(x = x, y = y, label = lbl), 
             nudge_y = 0.8) +
  geom_point(data = fatalities[222, ], aes(x = x, y = y), col = "blue") 

plt_man <- ggplot() + 
  geom_point(data = fatalities, aes(x = x, y = y, color = closest_man),
             size = 1.5) +
  geom_segment(data = road.segments, 
               aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_point(data = pumps_plt, aes(x = x, y = y), col = "red4", pch = 25, 
             size = 3, fill = "red1") +
  geom_label(data = pumps_plt, aes(x = x, y = y, label = lbl), 
             nudge_y = 0.8) +
  geom_point(data = fatalities[222, ], aes(x = x, y = y), col = "blue")
  


grid.arrange(plt_euclid, plt_man, ncol = 2)  

fatalities %>% 
  pivot_longer(cols = c(closest_euclid, closest_man), names_to = "metric",
               values_to = "pump_id") %>% 
  group_by(metric, pump_id) %>% count() %>% 
  pivot_wider(names_from = metric, values_from = n) %>% 
  mutate(euclid_perc = closest_euclid / nrow(fatalities),
         man_perc = closest_man / nrow(fatalities))

library(OpenStreetMap)

test <- openmap(c(max(roads_plt$lat2), min(roads_plt$long1)),
        c(min(roads_plt$lat1), max(roads_plt$long2)), 
        type = "osm", 
        minNumTiles = 4)

fatal_merc <- projectMercator(fatal_plt$lat, fatal_plt$long, drop = TRUE) %>% as.data.frame()
autoplot.OpenStreetMap(test) + geom_point(data = fatal_merc, aes(x = x, y = y), size = 1.5)
