library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)

data_cleaning = function(df){
  # Do some things to kill the data we do not like.
  df$pickup_longitude = as.numeric(as.character(df$pickup_longitude))
  df$pickup_latitude = as.numeric(as.character(df$pickup_latitude))
  df$dropoff_longitude = as.numeric(as.character(df$dropoff_longitude))
  df$dropoff_latitude = as.numeric(as.character(df$dropoff_latitude))
  
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
  # Restructure the date/time
  df <- df %>% mutate(tpep_pickup_datetime = ymd_hms(tpep_pickup_datetime),
                    tpep_dropoff_datetime = ymd_hms(tpep_dropoff_datetime),
                    duration = tpep_dropoff_datetime - tpep_pickup_datetime,
                    speed = trip_distance/as.numeric(duration))
  # Add duration.
  return(df)
}

trip = read.csv("./NYC_taxicab/y1.csv")
trip$pickup_longitude = as.numeric(as.character(trip$pickup_longitude))
trip$pickup_latitude = as.numeric(as.character(trip$pickup_latitude))
trip$dropoff_longitude = as.numeric(as.character(trip$dropoff_longitude))
trip$dropoff_latitude = as.numeric(as.character(trip$dropoff_latitude))

trip$total_amount = as.numeric(as.character(trip$total_amount))

trip = data_processing(trip)
trip = data_cleaning(trip)

test.df <- trip %>% head(10000) %>%
  select(lng = pickup_longitude, lat = pickup_latitude) 
test2.df <- trip %>% head(10000) %>%
  select(lng = dropoff_longitude, lat = dropoff_latitude) 

m <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  # addTiles() %>% 
  # setView( lng = -74.0025225, lat = 40.7509353, zoom = 14 ) %>% 
  setView( lng = -73.7, lat = 40.84, zoom = 8 ) %>%
  # addProviderTiles("Esri.WorldImagery") %>%
  addCircles(data = test.df, weight = 0, color = "red") %>%
  addCircles(data = test2.df, weight = 0, color = "green")

m

trip %>% arrange(desc(total_amount))

# Goal: Find a strategy to maximize profit for a taxi driver.
# Questions to ask:

# 1. Data exploration.
# 
