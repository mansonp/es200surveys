##
#
# Field Data
#
##

library(readr)
library(tidyverse)
library(ggplot2)
library(sf)
library(mapview)
library(RC)
library(leaflet)


fieldwork <- read_csv("data/fieldwork.csv")

# Leaflet Version



# Create the leaflet map
map <- leaflet(fieldwork) %>%
   addTiles() %>%
   addMarkers(lng = ~longitude, lat = ~latitude,
              label = ~paste("Temperature: ", air_temp, "°F"))

# Display the map
map

# Mapview Version

reedmap <- fieldwork %>%
   mapview(., xcol = "longitude", ycol = "latitude", zcol="air_temp",
           crs = 4269, grid = FALSE)

reedmap@layer$settings$popup <- "air_temp"


fieldwork %>%
   group_by(team_name) %>%
   summarise(mean=mean(air_temp),
             sd=sd(air_temp))

fieldwork %>%
   group_by(site) %>%
   summarise(mean=mean(air_temp),
             sd=sd(air_temp)) 

fieldwork %>%
   ggplot(aes(y=air_temp, fill=team_name)) +
   geom_boxplot() +
   scale_fill_brewer(palette = "Dark2")

fieldwork %>%
   ggplot(aes(y=air_temp, fill=site)) +
   geom_boxplot() +
   scale_fill_brewer(palette = "Dark2")

fieldwork %>%
   ggplot(aes(x=air_temp, y=canopy_cover, color=team_name)) +
   geom_point() +
   scale_fill_brewer(palette = "Dark2")

fieldwork %>%
   ggplot(aes(x=air_temp, y=rh, color=team_name)) +
   geom_point() +
   scale_fill_brewer(palette = "Dark2")

fieldwork %>%
   ggplot(aes(x=canopy_cover, y=rh, color=team_name)) +
   geom_point() +
   scale_fill_brewer(palette = "Dark2")

fieldwork %>%
   ggplot(aes(x=canopy_cover, y=rh)) +
   geom_point() +
   scale_fill_brewer(palette = "Dark2") +
   geom_smooth(method = "lm")


ch.mod <- lm(data=fieldwork, rh~canopy_cover)
