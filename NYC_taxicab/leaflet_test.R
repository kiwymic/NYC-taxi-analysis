library(leaflet)
library(dplyr)

data_cleaning = function(df){
  # Do some things to kill the data we do not like.
  df$pickup_longitude = as.numeric(as.character(df$pickup_longitude))
  df$pickup_latitude = as.numeric(as.character(df$pickup_latitude))
  df$dropoff_longitude = as.numeric(as.character(df$dropoff_longitude))
  df$dropoff_latitude = as.numeric(as.character(df$dropoff_latitude))
  
  # Delete data with longitude/latitude = 0.00
  # Even more, we restrict our data to those aroung NYC.
  df <- df %>% filter(pickup_longitude > -75 & pickup_longitude < -71.5 &
                      dropoff_longitude > -75 & dropoff_longitude < -71.5 &
                      pickup_latitude > 40 & pickup_latitude < 41.7 &
                      dropoff_latitude > 40 & dropoff_latitude < 41.7)
  return(df)
}

trip = read.csv("./NYC_taxicab/y1.csv")
trip$pickup_longitude = as.numeric(as.character(trip$pickup_longitude))
trip$pickup_latitude = as.numeric(as.character(trip$pickup_latitude))
trip$dropoff_longitude = as.numeric(as.character(trip$dropoff_longitude))
trip$dropoff_latitude = as.numeric(as.character(trip$dropoff_latitude))

trip2 = data_cleaning(trip)

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
