library(readr)
library(tidyverse)
library(ggplot2)
library(sf)
library(mapview)




kestrelReadings032123PM <- read_csv("data/kestrelReadings.csv", 
                            skip = 4) %>%
   rename("Time" = "yyyy-MM-dd hh:mm:ss a",
          "Temp" = "°F...2") %>%
   mutate("datetimeK" = as.POSIXct(Time)+(12*60*60)) # Fix Times

kestrelReadings032223AM <- read_csv("data/kestrelReadings032223AM.csv", 
                                    skip = 4) %>%
   rename("Time" = "yyyy-MM-dd hh:mm:ss a",
          "Temp" = "°F...2") %>%
   mutate("datetimeK" = as.POSIXct(Time)+(12*60*60)) # Fix Times

gpsReadings <- read_csv("data/gpsReadings.txt", 
                            skip = 1,
                        col_names = FALSE) %>%
   rename("Lat"=X1,
          "Lon"=X2,
          "Brg" = X3,
          "Time"=X4,
          "Unkn" = X5) %>%
   mutate("datetimeG" = as.POSIXct(Time)-(7*60*60)) # Fix Times

kestrelReadings032223AM %>% 
   filter(Time > as.POSIXct("2023-03-22 08:14:10 AM")) %>%
   ggplot(aes(y=Temp, x=Time, group=1)) +
   geom_line() +
   theme_bw()


kestrel.geo <- kestrelReadings032123PM %>%
   left_join(gpsReadings, join_by(closest(datetimeK > Time))) # Time vs the new time I created is correct. Timezones?


kestrel.geo %>%
   filter(Time.x > as.POSIXct("2023-03-21 03:31:30 PM")) %>% # Filtered for warmth in house
   mapview(., xcol = "Lon", ycol = "Lat", zcol="Temp",
                                   crs = 4269, grid = FALSE)


