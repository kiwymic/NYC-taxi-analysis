# The first urgent thing is to save 
test = read.csv("./NYC_taxicab/yellow_tripdata_2016-01.csv")

colnames(test)
unique(test$passenger_count)

# Save the first 1 million rows to a temp file.
# write.csv(test[1:1000000,], "./NYC_taxicab/F1mil.csv", row.names = T)

# Split the rows randomly to 10 files, so each file contains ~1 mil rows.
# Just in case we cannot deal with the largest file, we shall pick a
# subset of the randomly split file.
library(dplyr)

N = dim(test)[1] # The number of rows of the dataframe (10906858)
test["mask"] = sample(1:10, N, replace = T)

for(i in 1:10){
  
  write.csv(test %>% filter(mask == i) %>% select(-mask),
            paste0("./NYC_taxicab/y", as.character(i), ".csv"),
            row.names = T)
}
