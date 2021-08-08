# Constants fixed throughout this file.

# We only take dates withing this range to compute the statistics...
aggregate_start = ymd_hms("2016/01/01 00:00:00")
aggregate_end = ymd_hms("2016/01/14 23:59:59")

# Read and aggregate all cvs files again
num_files = 20 # Update whenever a new file is converted.

######################################################
azimuth = function(lon1, lat1, lon2, lat2){
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

# Constuct a data.frame which integrates the required zone data
# area should be in square miles
ny_zone_lookup = data.frame(ID = ny_areas$OBJECTID, zone = ny_areas$zone,
                            borough = ny_areas$borough,
                            area = as.numeric(st_area(ny_areas))/2589988.11)
ny_zone_lookup <- rbind(ny_zone_lookup, c(0, "Not in NYC", "Other", 0))
ny_zone_lookup$ID = as.numeric(ny_zone_lookup$ID)

# Insert the analysis here.
# Note: 2016/01/01 is a Saturday.