library(plotly)
library(leaflet.extras)

# Constants fixed throughout this file.

# We only take dates withing this range to compute the statistics...
aggregate_start = ymd_hms("2016/01/01 00:00:00")
aggregate_end = ymd_hms("2016/01/28 23:59:59")

# Read and aggregate all cvs files again
num_files = 65 # Update whenever a new file is converted.

######################################################
azimuth = function(lon1, lat1, lon2, lat2){
  # Returns the azimuth on two gps coordinates, start at 1, ends at 2.
  # 0 degree = point north, clockwise augmenting
  rlon1 = lon1 * pi / 180
  rlat1 = lat1 * pi / 180
  rlon2 = lon2 * pi / 180
  rlat2 = lat2 * pi / 180
  
  return(atan2(sin(rlon2-rlon1)*cos(rlat2), cos(rlat1)*sin(rlat2) -
                 sin(rlat1)*cos(rlat2)*cos(rlon2-rlon1)))
}

additional_features = function(df){
  # Here, add whatever feature which seems to be useful
  
  # The borough of both the pickup and dropoff
  df$pickup_zone = as.numeric(df$pickup_zone)
  df$dropoff_zone = as.numeric(df$dropoff_zone)
  temp <- left_join(df, ny_zone_lookup, by = c("pickup_zone" = "ID"))
  temp <- left_join(temp, ny_zone_lookup, by = c("dropoff_zone" = "ID"))
  temp <- temp %>% rename(pickup_boro = borough.x,
                          dropoff_boro = borough.y) %>%
    select(-zone.x, -zone.y, -area.x, -area.y)
  
  # The traveling angle of the trip.
  # It is somewhat believed that in Manhattan, traveling vertically is
  # easier than traveling horizontally. Let's check
  temp <- temp %>% mutate(azimuth = azimuth(
    pickup_longitude, pickup_latitude, dropoff_longitude,dropoff_latitude),
    hourly_wage = in_pocket/duration*3600,
    hourly_wage = ifelse(hourly_wage>=2000,2000,hourly_wage))
  # Finally, the hourly wage!
  # (Hard cutoff at 2000, expecting cutting of 0.01% outliers)
  temp <- temp %>% mutate(dow = wday(tpep_pickup_datetime),
                          hr = hour(tpep_pickup_datetime))
  return(temp)
}

######################################################

trip = read.csv("./NYC_taxicab/y1_1-10000.csv")
for(i in c(2:num_files)){
  trip = rbind(trip, read.csv(paste0("./NYC_taxicab/y1_",
                             as.character(as.integer(i*10000-9999)), "-",
                             as.character(as.integer(i*10000)), ".csv")))
}
trip = trip %>% select(-X)

# After the zone mapping, we can tell that the NA means "not mapped".
# This means that the pickup/dropoff location is not in NYC or EWR airport.
# We have assigned a special location code, 0, to describe this.
trip <- trip %>% mutate(pickup_zone =
                          ifelse(is.na(pickup_zone),0,pickup_zone),
                        dropoff_zone =
                          ifelse(is.na(dropoff_zone),0,dropoff_zone))

trip <- trip %>% filter(tpep_pickup_datetime > aggregate_start &
                          tpep_pickup_datetime < aggregate_end)

# Construct a data.frame which integrates the required zone data
# area should be in square miles
ny_zone_lookup = data.frame(ID = ny_areas$OBJECTID, zone = ny_areas$zone,
                            borough = ny_areas$borough,
                            area = as.numeric(st_area(ny_areas))/2589988.11)
ny_zone_lookup <- rbind(ny_zone_lookup, c(0, "Not in NYC", "Other", 0))
ny_zone_lookup$ID = as.numeric(ny_zone_lookup$ID)

# Insert the analysis here.
# Note: 2016/01/01 is a Saturday.
trip <- additional_features(trip)

##############################################
# Tab I: Day of the week and hour of the day
# 1. Relation between day of the week and total ride
# full_trip <- read.csv("./NYC_taxicab/y1.csv")
# full_trip <- data_processing(full_trip)
# full_trip <- data_cleaning(full_trip)
# full_trip <- full_trip %>%
#   mutate(tpep_pickup_datetime = ymd_hms(tpep_pickup_datetime),
#          tpep_dropoff_datetime = ymd_hms(tpep_dropoff_datetime))
# full_trip <- full_trip %>%
#   filter(tpep_pickup_datetime > aggregate_start &
#            tpep_dropoff_datetime < aggregate_end) %>%
#   mutate(day = day(tpep_pickup_datetime))
# full_trip %>% group_by(day) %>% summarise(n())

# agg_I_1 <- trip %>% group_by(dow) %>% summarise(cnt = n())
g <- ggplot(data = trip, aes(x=dow))+
  geom_bar(aes(fill=pickup_boro)) + # , stat="identity"
  labs(x = "Day of the week", y = "Data count")
g

g <- ggplot(data = trip, aes(x=dow))+
  geom_bar(aes(fill=dropoff_boro)) + # , stat="identity"
  labs(x = "Day of the week", y = "Data count")
g

g <- ggplot(data = trip, aes(x=hr))+
  geom_bar(aes(fill=pickup_boro)) + # , stat="identity"
  labs(x = "Time of the day", y = "Data count")
g

g <- ggplot(data = trip, aes(x=hr))+
  geom_density(aes(color=as.factor(dow))) + # , stat="identity"
  labs(x = "Time of the day", y = "Data count")
g

# 2. What are the most popular pickup/dropoff regions?

# Pickup
temp <- trip %>% group_by(pickup_zone) %>% summarise(cnt = n()) %>%
  slice_max(cnt, n=20)
m <- leaflet(ny_areas %>% filter(OBJECTID %in% temp$pickup_zone)) %>%
  addPolygons(color = "#331166", weight = 1,                                                                                smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 )
m

# Dropoff
temp <- trip %>% group_by(dropoff_zone) %>% summarise(cnt = n()) %>%
  slice_max(cnt, n=20)
m <- leaflet(ny_areas %>% filter(OBJECTID %in% temp$dropoff_zone)) %>%
  addPolygons(color = "#116633", weight = 1,                                                                                smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 )
m

# 3. Scatter plot/heat map of specific TOD/DOW
# Pickup
TOD = 22
DOW = 6

trip_selected <- trip %>% filter(dow==DOW, hr==TOD) %>%
  select(longitude = pickup_longitude, latitude = pickup_latitude)

m <- leaflet(ny_areas) %>% addPolygons(color = "#331166", weight = 1,
  smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
  addCircles(data = trip_selected,
             weight = 0, color = "red")
m

# Dropoff
trip_selected <- trip %>% filter(dow==DOW, hr==TOD) %>%
  select(longitude = dropoff_longitude, latitude = dropoff_latitude)

m <- leaflet(ny_areas) %>% addPolygons(color = "#116633", weight = 1,
  smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
  addCircles(data = trip_selected,
             weight = 0, color = "green")
m

# The heat map thing
trip_selected <- trip %>% filter(dow==DOW, hr==TOD) %>%
  select(longitude = pickup_longitude, latitude = pickup_latitude)

m <- leaflet(ny_areas) %>% addPolygons(color = "#331166", weight = 1,
                                       smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
  addHeatmap(lng=trip_selected$longitude, lat=trip_selected$latitude,
             max=1, blur = 6, radius = 3)

m

trip_selected <- trip %>% filter(dow==DOW, hr==TOD) %>%
  select(longitude = dropoff_longitude, latitude = dropoff_latitude)

m <- leaflet(ny_areas) %>% addPolygons(color = "#116633", weight = 1,
                                       smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
  addHeatmap(lng=trip_selected$longitude, lat=trip_selected$latitude,
             max=1, blur = 6, radius = 3)

m

################################################
# Tab II. Issues related to price

# 1. What is the distribution of taxi fare?
g <- ggplot(data = trip, aes(x=in_pocket))+
  geom_histogram(aes(fill=pickup_boro), binwidth = 3) + 
  coord_cartesian(xlim=c(0,75)) +
  labs(x = "Taxi fare getting into drivers' pockets", y = "Data count")
ggplotly(g, tooltip="text")

# 2. What is the distribution of (credit card) tips?
g <- ggplot(data = trip, aes(x=tip_amount))+
  geom_histogram(aes(fill=pickup_boro), binwidth = 1) + 
  coord_cartesian(xlim=c(0,20)) +
  labs(x = "Tips <3", y = "Data count")
g

# 3. What is the distribution of distance?
g <- ggplot(data = trip, aes(x=trip_distance))+
  geom_histogram(aes(fill=pickup_boro), binwidth = 1) + 
  coord_cartesian(xlim=c(0,20)) +
  labs(x = "Distance traveled (miles)", y = "Data count")
g

g <- ggplot(data = trip, aes(x=as.factor(dow), y=trip_distance))+
  geom_boxplot(aes(fill=dow)) + coord_cartesian(ylim=c(0,8)) + 
  labs(x = "Day of week", y = "Distancee traveled (miles)")
g

# 4. What is the distribution of the hourly wage?
g <- ggplot(data = trip, aes(x=hourly_wage))+
  geom_histogram(aes(fill=pickup_boro), binwidth = 5) + 
  coord_cartesian(xlim=c(0,500)) +
  labs(x = "Hourly wage", y = "Data count")
g

g <- ggplot(data = trip, aes(x=as.factor(dow), y=hourly_wage))+
  geom_boxplot(aes(fill=dow)) + coord_cartesian(ylim=c(0,150)) + 
  labs(x = "Day of week", y = "Hourly wage")
g

temp <- trip %>% group_by(dow, hr) %>%
  summarise(avg = mean(hourly_wage))
g <- ggplot(data = temp, aes(x=hr, y=avg))+
  geom_line(aes(color=as.character(dow))) + # , stat="identity"
  labs(x = "Time of the day", y = "Hourly wage") # + facet_wrap(~ dow)
g

# 5. Travelling in Manhattan
temp <- trip %>% filter(pickup_boro=="Manhattan" &
                          dropoff_boro=="Manhattan" & hourly_wage < 200)
g <- ggplot(data = temp, aes(x=earth_distance,
                             y=azimuth))+
  geom_point(aes(color=hourly_wage)) + 
  labs(x = "Teleport distance", y = "Azimuth") # + facet_wrap(~ dow)
g

# 6. A baseline comparison: Traveling to and from JFK
# Many of those have flat rate = $52. How profitable is this?
temp <- trip %>% filter(pickup_zone==132 |
                          dropoff_boro==132) %>%
  filter(hourly_wage < 200)
g <- ggplot(data = temp, aes(x=fare_amount))+
  geom_histogram(binwidth = 3) + 
  coord_cartesian(xlim=c(0,99)) +
  labs(x = "Taxi fare (tips excluded)", y = "Data count")
ggplotly(g, tooltip="text")

g <- ggplot(data = temp, aes(x=trip_distance, y=hourly_wage)) +
  geom_point(aes(color=hourly_wage)) + 
  labs(x = "Trip distance", y = "Hourly wage") +
  coord_cartesian(xlim=c(0,50)) + 
  geom_density2d()
ggplotly(g, tooltip="text")

# Compute an average JFK wage table and
temp_wage <- temp %>% mutate(other_zone =
                  ifelse(pickup_zone==132, dropoff_zone, pickup_zone)) %>%
  group_by(other_zone) %>% summarize(avg = mean(hourly_wage))

pal <- colorBin("YlOrRd", domain = temp_wage$avg)
m <- leaflet(ny_areas %>% filter(OBJECTID %in% temp_wage$other_zone)) %>%
addPolygons(fillColor = ~pal(temp_wage$avg), weight = 1,
              smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%                                                                             
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
  addLegend(pal = pal, values = ~temp_wage$avg, opacity = 0.7,
            title = "Wage, pu/do JFK", position = "bottomright")
m

# As a comparison, make a map for pickup at each region...
region_wage <- trip %>% group_by(pickup_zone) %>%
  summarize(avg = mean(hourly_wage))

m <- leaflet(ny_areas %>% filter(OBJECTID %in% region_wage$pickup_zone)) %>%
  addPolygons(fillColor = ~pal(region_wage$avg), weight = 1,
              smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%                                                                             
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
  addLegend(pal = pal, values = ~temp_wage$avg, opacity = 0.7,
            title = "Wage, pu", position = "bottomright")
m

# Same, but dropoff
dropoff_wage <- trip %>% group_by(dropoff_zone) %>%
  summarize(avg = mean(hourly_wage))

m <- leaflet(ny_areas %>% filter(OBJECTID %in% dropoff_wage$dropoff_zone)) %>%
  addPolygons(fillColor = ~pal(dropoff_wage$avg), weight = 1,
              smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%                                                                             
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
  addLegend(pal = pal, values = ~temp_wage$avg, opacity = 0.7,
            title = "Wage, do", position = "bottomright")
m

########################################
# Free playground!

# 1. Zone + DOW
ZONE=132
DOW = 3

trip_selected <- trip %>% filter(dow==DOW, pickup_zone==ZONE) %>%
  select(longitude = dropoff_longitude, latitude = dropoff_latitude,
         density = hourly_wage)

m <- leaflet(ny_areas %>% filter(OBJECTID==ZONE)) %>%
  addPolygons(color = "#331166", weight = 1,
              smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
  addHeatmap(lng=trip_selected$longitude, lat=trip_selected$latitude,
             max=1, blur = 6, radius = 3, intensity = 100*trip_selected$density)
m

# 2. Zone + HR
ZONE=43
HR = 17

trip_selected <- trip %>% filter(hr==HR, pickup_zone==ZONE) %>%
  select(longitude = dropoff_longitude, latitude = dropoff_latitude,
         density = hourly_wage)

m <- leaflet(ny_areas %>% filter(OBJECTID==ZONE)) %>%
  addPolygons(color = "#331166", weight = 1,
              smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
  addHeatmap(lng=trip_selected$longitude, lat=trip_selected$latitude,
             max=1, blur = 6, radius = 3, intensity = 100*trip_selected$density)
m

# 1     1 33464
# 2     2 30431
# 3     3 29371
# 4     4 30830
# 5     5 33405
# 6     6 34111
# 7     7 35765
# 8     8 38003
# 9     9 39405
# 10    10 34269
# 11    11 33246
# 12    12 35778
# 13    13 38494
# 14    14 38653
