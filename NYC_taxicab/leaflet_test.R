library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(rgdal)

data_cleaning = function(df){
  # Do some things to kill the data we do not like.
  df$pickup_longitude = as.numeric(as.character(df$pickup_longitude))
  df$pickup_latitude = as.numeric(as.character(df$pickup_latitude))
  df$dropoff_longitude = as.numeric(as.character(df$dropoff_longitude))
  df$dropoff_latitude = as.numeric(as.character(df$dropoff_latitude))
  
  df$total_amount = as.numeric(as.character(df$total_amount))
  
  # Delete data with longitude/latitude = 0.00
  # Even more, we restrict our data to those around NYC.
  
  # Some trips have distance = 0. We will eliminate those.
  # The longest trip has distance > 1 million miles. This is not reasonable
  # as well.
  
  # Rate code id is a number from 1 to 6.
  # Some fares also looked weird. Currently, we remove the fares which are
  # free and above $300 (this might not be a good idea, though).
  
  # Also need to filter out the data with unreasonable duration.
  # Here, we call it unreasonable when...
  #   1. It's too speedy (usually when the timer has error and is too short)
  #   2. It takes too long and is too slow
  #      (Maybe the driver forgot to reset the timer!)
  df <- df %>% filter(pickup_longitude > -75 & pickup_longitude < -71.5 &
                      dropoff_longitude > -75 & dropoff_longitude < -71.5 &
                      pickup_latitude > 40 & pickup_latitude < 41.7 &
                      dropoff_latitude > 40 & dropoff_latitude < 41.7) %>%
    filter(trip_distance > 0.1 & trip_distance < 300 &
             RatecodeID > 0 & RatecodeID < 6 &
             total_amount <= 300 & total_amount >= 0.01 &
             passenger_count > 0) %>%
    filter(duration > 0 &
             speed < 0.025 & (duration < 10800 | speed > 0.004))
  return(df)
}

data_processing = function(df){
  # Here, we collect processing functions which processes date-time
  # information.
  df <- df %>% mutate(tpep_pickup_datetime = ymd_hms(tpep_pickup_datetime),
                    tpep_dropoff_datetime = ymd_hms(tpep_dropoff_datetime),
                    duration = tpep_dropoff_datetime - tpep_pickup_datetime,
                    speed = trip_distance/as.numeric(duration),
                    in_pocket = fare_amount + extra + tip_amount)
  return(df)
}

zone_lookup = function(st){
  return(match(1,st_contains(ny_areas, st)))
}

distance_processing = function(df){
  # Here, we collect processing functions which processes geospacial
  # information. Mainly, the zone info and distance info.
  
  # Adding the zone information. 
  # For this to be functioned properly, one must make sure that
  # ./NYC_taxicab/taxi_zones.shp has been read as ny_areas and processed.
  df <- df %>%
    mutate(pickup_sf = st_point(as.numeric(c(pickup_longitude,
                                             pickup_latitude))),
           dropoff_sf = st_point(as.numeric(c(dropoff_longitude,
                                             dropoff_latitude))),
           pickup_zone = match(1,st_contains(ny_areas, pickup_sf)),
           dropoff_zone = match(1,st_contains(ny_areas, dropoff_sf)))
  
  df <- df %>%
    mutate(earth_distance =
             as.numeric(st_length(st_sfc(st_linestring(
               rbind(pickup_sf, dropoff_sf), crs = 4326)))))
}

# A bad non-vectorized method to do all the zone mappings.
# Should be slow. Should be replaced
distance_processing_nv = function(df){
  df_patch = read.table(text = "",
                        col.names = c("pickup_zone", "dropoff_zone",
                                      "earth_distance"))
  
  for(i in 1:dim(df)[1]){
    pickup_sf = st_point(as.numeric(c(df$pickup_longitude[i],
                                      df$pickup_latitude[i])))
    dropoff_sf = st_point(as.numeric(c(df$dropoff_longitude[i],
                                       df$dropoff_latitude[i])))
    pickup_zone = match(1,st_contains(ny_areas, pickup_sf))
    dropoff_zone = match(1,st_contains(ny_areas, dropoff_sf))
    
    earth_distance = st_length(st_sfc(st_linestring(
      rbind(pickup_sf, dropoff_sf)), crs = 4326))
    
    df_patch = df_patch %>% add_row(pickup_zone = pickup_zone,
                            dropoff_zone = dropoff_zone,
                            earth_distance =
                              as.numeric(earth_distance)*0.000621371192)
  }
  return(cbind(df, df_patch))
}
# A few noticeable zones:
# Airport: EWR = 1, JFK = 132, LGA = 138, central park = 43

temp = trip[290001:300000,] #298649-308889
temp[1,"X"]
temp[10000,"X"]
ptm <- proc.time()
temp = distance_processing_nv(temp)
proc.time() - ptm
# 0.3-0.4s per entry... desperate

write.csv(temp %>% select(-1), "y1_290001-300000.csv", row.names = T)
# write.csv(test %>% select(-1,-2) , "y1_10001-20000.csv", row.names = T)

# Loading the ride data
trip = read.csv("./NYC_taxicab/y1.csv")

# Loading the zone map from NYC. The projection in that file is in
# EPSG:2908 (whatever it is).
# The Leaflet library only understand EPSG:4326
# (your daily friendly lat/lon), so you need to translate it.
ny_areas <- st_read("./NYC_taxicab/taxi_zones.shp") %>%
  st_transform(crs = 4326)

# You must process before you clean. Otherwise, you die.
trip = data_processing(trip)
# trip = distance_processing(trip)
trip = data_cleaning(trip)

test.df <- trip %>% head(10000) %>%
  select(lng = pickup_longitude, lat = pickup_latitude) 
test2.df <- trip %>% head(10000) %>%
  select(lng = dropoff_longitude, lat = dropoff_latitude) 

m <- leaflet(ny_areas) %>% addPolygons(color = "#004499", weight = 1,
      smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$GeoportailFrance.orthos) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 )
  # addCircles(data = test.df, weight = 0, color = "red") #%>%
  # addCircles(data = test2.df, weight = 0, color = "green")
  
m

# trip %>% arrange(desc(total_amount))

# Goal: Find a strategy to maximize profit for a taxi driver.
# Questions to ask:

# 1. Data exploration.
# 
