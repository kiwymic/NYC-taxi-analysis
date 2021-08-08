# Read and aggregate all cvs files again
num_files = 10 # Update whenever a new file is converted.

trip = read.csv("./NYC_taxicab/y1_1-10000.csv")
for(i in c(2:num_files)){
  trip = rbind(trip, read.csv(paste0("./NYC_taxicab/y1_",
                             as.character(as.integer(i*10000-9999)), "-",
                             as.character(as.integer(i*10000)), ".csv")))
}
trip = trip %>% select(-X)

# Constuct a data.frame which integrates the required zone data
# area should be in square miles
ny_zone_lookup = data.frame(ID = ny_areas$LocationID, zone = ny_areas$zone,
                            borough = ny_areas$borough,
                            area = as.numeric(st_area(ny_areas))/2589988.11)
ny_zone_lookup <- rbind(ny_zone_lookup, c(0, "Not in NYC", "Other", 0))


# Insert the analysis here.
# Note: 2016/01/01 is a Saturday.