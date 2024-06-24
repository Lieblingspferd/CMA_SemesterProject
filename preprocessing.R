######### Preprocessing ##########

# Computational Movement Analysis, Spring Semester 24
# ZHAW / UZH
# Annika Hirsch

# Project Title: Have I gotten faster? - An analysis of Mountainbiking Data

"
This file contains all the preprocessing that I do on the data.

Which is mainly: 
* Importing and converting it to LV95
* Crop the data to contain only the relevant area of analysis
* Map Matching the trajectories
* And finally: exporting the segments (to continue the analysis in the main document)
"

######### Package dependencies ##########
library(moveVis)
library(FITfileR)
library(dplyr)
library(sf)

# And all my self written functions (they are also in the sript here, 
# but additionally in this file, so that I have all functions by executing 
# only one line of code)
source("functions.R")

######### Data Import ##########
a1 <- readFitFile("data/strava_records/kurz_auf_den_harder.fit")
a2 <- readFitFile("data/strava_records/2_Anlauf_Bleiki_DH_Sturz_.fit")
a3 <- readFitFile("data/strava_records/2x_Harder_.fit")
a4 <- readFitFile("data/strava_records/Bleiki_.fit")
a5 <- readFitFile("data/strava_records/Bleiki_aber_diesmal_trocken.fit")
a6 <- readFitFile("data/strava_records/Bleiki_DH.fit")
a7 <- readFitFile("data/strava_records/Bleiki_Liebe_.fit")
a8 <- readFitFile("data/strava_records/Einmal_schnell_zum_einkaufen.fit")
a9 <- readFitFile("data/strava_records/Erholunsfahrt.fit")
a10 <- readFitFile("data/strava_records/Harder_.fit")
a11 <- readFitFile("data/strava_records/Hike_a_Bike_auf_den_harder.fit")
a12 <- readFitFile("data/strava_records/Rutschige_nasse_Bleiki.fit")
a13 <- readFitFile("data/strava_records/Sturz_Platten_und_kein_GPS.fit")

# function to transform the fitfile into a usable sf
transform_activity <- function(dataframe){
  dataframe <- records(dataframe)
  dataframe <- st_as_sf(dataframe[["record_1"]],
                        coords = c("position_long", "position_lat"),
                        crs = 4326)
  dataframe <- st_transform(dataframe, crs = 2056)
  coords <- st_coordinates(dataframe)
  dataframe <- cbind(dataframe, coords)
  return(dataframe)
}

# Transform all fitfiles
a1 <- transform_activity(a1)
a2 <- transform_activity(a2)
a3 <- transform_activity(a3)
a4 <- transform_activity(a4)
a5 <- transform_activity(a5)
a6 <- transform_activity(a6)
a7 <- transform_activity(a7)
a8 <- transform_activity(a8)
a9 <- transform_activity(a9)
a10 <- transform_activity(a10)
a11 <- transform_activity(a11)
a12 <- transform_activity(a12)
a13 <- transform_activity(a13)


######### Crop to the area of analysis #########
# Shorten the files to only contain Bleiki DH 
# Buffer around segment
# Manually drawn a line on map.geo.admin, exported and then imported to here
segment <- read_sf("data/segmentation/Bleiki_DH_Segment.kml")
buffer <- st_buffer(segment, 20, endCapStyle = "flat")#20m buffer
buffer <- st_transform(buffer, crs = 2056)

# This solution seems really slow... But hey, let's accept it
a1 <- a1 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a2 <- a2 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))         
a3 <- a3 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a4 <- a4 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a5 <- a5 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a6 <- a6 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a7 <- a7 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a8 <- a8 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a9 <- a9 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a10 <- a10 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a11 <- a11 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a12 <- a12 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))
a13 <- a13 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))

# Only include the longest segment 
longest_consecutive_timestamps <- function(dataframe, threshold = 30){
  temp <- dataframe %>%
    mutate(
      timelag = as.numeric(difftime(timestamp, lag(timestamp), units = "secs"))
    )
  temp$is_new_segment <- temp$timelag > threshold
  temp <- temp[2:length(temp$timestamp),]
  temp$segment <- cumsum(temp$is_new_segment) + 1
  max <- which.max(table(temp$segment))
  temp <- temp[temp$segment == max,]
  return(temp)
}

a1 <- longest_consecutive_timestamps(a1)
a2 <- longest_consecutive_timestamps(a2)
a3 <- longest_consecutive_timestamps(a3)
a4 <- longest_consecutive_timestamps(a4)
a5 <- longest_consecutive_timestamps(a5)
a6 <- longest_consecutive_timestamps(a6)
a7 <- longest_consecutive_timestamps(a7)
a8 <- longest_consecutive_timestamps(a8)
a9 <- longest_consecutive_timestamps(a9)
a10 <- longest_consecutive_timestamps(a10)
a11 <- longest_consecutive_timestamps(a11)
a12 <- longest_consecutive_timestamps(a12)
a13 <- longest_consecutive_timestamps(a13)

# To test how that worked, I'll plot them all ...
library(tmap)
tmap_mode("view")

# Create a line
create_line <- function(dataframe){
  line <- dataframe %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")
}

a1_line <- create_line(a1)
a2_line <- create_line(a2)
a3_line <- create_line(a3)
a4_line <- create_line(a4)
a5_line <- create_line(a5)
a6_line <- create_line(a6)
a7_line <- create_line(a7)
a8_line <- create_line(a8)
a9_line <- create_line(a9)
a10_line <- create_line(a10)
a11_line <- create_line(a11)
a12_line <- create_line(a12)
a13_line <- create_line(a13)


# Change basemap
tmap_options(basemaps = "OpenStreetMap")
# Plot map
tm_shape(a1_line) +
  tm_lines(col = "black") +
  tm_shape(a2_line) +
  tm_lines(col = "grey") +
  tm_shape(a3_line) +
  tm_lines(col = "brown") +
  tm_shape(a4_line) +
  tm_lines(col = "yellow") +
  tm_shape(a5_line) +
  tm_lines(col = "orange") +
  tm_shape(a6_line) +
  tm_lines(col = "red") +
  tm_shape(a7_line) +
  tm_lines(col = "pink") +
  tm_shape(a8_line) +
  tm_lines(col = "purple") +
  tm_shape(a9_line) +
  tm_lines(col = "blue") +
  tm_shape(a10_line) +
  tm_lines(col = "turquoise") +
  tm_shape(a11_line) +
  tm_lines(col = "green") +
  tm_shape(a12_line) +
  tm_lines(col = "darkgreen") +
  tm_shape(a13_line) +
  tm_lines(col = "magenta") 

# What a nice plot... It really shows the inaccuracy of the GPS of my phone

# Add a column for the time difference seconds (since the start of the activity)
test <- a1
timediff_since_start <- function(dataframe){
  starttime <- dataframe$timestamp[1]
  timediff <- as.numeric(difftime(dataframe$timestamp, starttime), units = "secs")
  return(timediff)
}

a1$timediff_since_start <- timediff_since_start(a1)
a2$timediff_since_start <- timediff_since_start(a2)
a3$timediff_since_start <- timediff_since_start(a3)
a4$timediff_since_start <- timediff_since_start(a4)
a5$timediff_since_start <- timediff_since_start(a5)
a6$timediff_since_start <- timediff_since_start(a6)
a7$timediff_since_start <- timediff_since_start(a7)
a8$timediff_since_start <- timediff_since_start(a8)
a9$timediff_since_start <- timediff_since_start(a9)
a10$timediff_since_start <- timediff_since_start(a10)
a11$timediff_since_start <- timediff_since_start(a11)
a12$timediff_since_start <- timediff_since_start(a12)
a13$timediff_since_start <- timediff_since_start(a13)

# Export all of these not mapmatched dataframes...
# So that I can share them on github, since I removed the part about it that was sensible information
st_write(a1, "data/strava_records/a1.gpkg")
st_write(a2, "data/strava_records/a2.gpkg")
st_write(a3, "data/strava_records/a3.gpkg")
st_write(a4, "data/strava_records/a4.gpkg")
st_write(a5, "data/strava_records/a5.gpkg")
st_write(a6, "data/strava_records/a6.gpkg")
st_write(a7, "data/strava_records/a7.gpkg")
st_write(a8, "data/strava_records/a8.gpkg")
st_write(a9, "data/strava_records/a9.gpkg")
st_write(a10, "data/strava_records/a10.gpkg")
st_write(a11, "data/strava_records/a11.gpkg")
st_write(a12, "data/strava_records/a12.gpkg")
st_write(a13, "data/strava_records/a13.gpkg")



######## Re-importing the preprocessed files ##########
# To run the script only from here, I need the package dependencies:
library(dplyr)
library(sf)
source("functions.R")

a1 <- st_read("data/strava_records/a1.gpkg")
a2 <- st_read("data/strava_records/a2.gpkg")
a3 <- st_read("data/strava_records/a3.gpkg")
a4 <- st_read("data/strava_records/a4.gpkg")
a5 <- st_read("data/strava_records/a5.gpkg")
a6 <- st_read("data/strava_records/a6.gpkg")
a7 <- st_read("data/strava_records/a7.gpkg")
a8 <- st_read("data/strava_records/a8.gpkg")
a9 <- st_read("data/strava_records/a9.gpkg")
a10 <- st_read("data/strava_records/a10.gpkg")
a11 <- st_read("data/strava_records/a11.gpkg")
a12 <- st_read("data/strava_records/a12.gpkg")
a13 <- st_read("data/strava_records/a13.gpkg")
# On top of not needing to rerun the preprocessing, 
# another advantage is that reading .gpkg files is 
# a lot faster than reading .fit files

######## Distance according to the actual position of the GPS fixes ###########
distance <- # euclidean distance
  function (x1, y1, x2, y2){
    dist <- ((x1 - x2)^2 + (y1 - y2)^2)^(1/2)
    return(dist)
    }

# Function to calculate the distance from one point to the next 
# (and add it to the dataframe as a new column)
add_dist_col <- function(dataframe){
  dataframe$distance <- distance(lead(dataframe$X), lead(dataframe$Y), dataframe$X, dataframe$Y)
  return(dataframe)
}

a1 <- add_dist_col(a1)
a2 <- add_dist_col(a2)
a3 <- add_dist_col(a3)
a4 <- add_dist_col(a4)
a5 <- add_dist_col(a5)
a6 <- add_dist_col(a6)
a7 <- add_dist_col(a7)
a8 <- add_dist_col(a8)
a9 <- add_dist_col(a9)
a10 <- add_dist_col(a10)
a11 <- add_dist_col(a11)
a12 <- add_dist_col(a12)
a13 <- add_dist_col(a13)


######## Speed based on actual position of GPS-fixes ###########
speed2 <- #calculates the speed based on two consecutive fixes
  function(dataframe){
    dataframe$speed2 <- dataframe$distance / 
      (as.numeric(difftime(lead(dataframe$timestamp), dataframe$timestamp, units = "secs")))
    return(dataframe)
  }

test <- a1
test <- speed2(test)
# Works, but I do have some fixes that have the same timestamp... 
# So it might be good to delete the rows with the timelag 0 
# (Otherwise I get some inf values for the speed)

# Remove rows with timelag == 0
test <- subset(test, test$timelag != 0)
# Retry the speed calculation
test <- speed2(test)

plot(test$speed2) # Looks like I took a nice break at the beginning 

# Removing the rows with timelag == 0
a1 <- subset(a1, a1$timelag != 0)
a2 <- subset(a2, a2$timelag != 0)
a3 <- subset(a3, a3$timelag != 0)
a4 <- subset(a4, a4$timelag != 0)
a5 <- subset(a5, a5$timelag != 0)
a6 <- subset(a6, a6$timelag != 0)
a7 <- subset(a7, a7$timelag != 0)
a8 <- subset(a8, a8$timelag != 0)
a9 <- subset(a9, a9$timelag != 0)
a10 <- subset(a10, a10$timelag != 0)
a11 <- subset(a11, a11$timelag != 0)
a12 <- subset(a12, a12$timelag != 0)
a13 <- subset(a13, a13$timelag != 0)

# And calculating the speed between two fixes
a1 <- speed2(a1)
a2 <- speed2(a2)
a3 <- speed2(a3)
a4 <- speed2(a4)
a5 <- speed2(a5)
a6 <- speed2(a6)
a7 <- speed2(a7)
a8 <- speed2(a8)
a9 <- speed2(a9)
a10 <- speed2(a10)
a11 <- speed2(a11)
a12 <- speed2(a12)
a13 <- speed2(a13)


# Speed calculated for 5 fixes (analog to Laube and Purves, 2011 and as done in the lecture in Week 4)
speed5 <- function(dataframe){
  temp <- dataframe
  temp$nPlus2 <- distance(lead(temp$X, 2), lead(temp$Y, 2), temp$X, temp$Y)/
    (as.numeric(difftime(lead(temp$timestamp, 2), temp$timestamp, units = "secs")))
  temp$nPlus1 <- distance(lead(temp$X), lead(temp$Y), temp$X, temp$Y)/
    (as.numeric(difftime(lead(temp$timestamp), temp$timestamp, units = "secs")))
  temp$nMinus1 <- distance(temp$X, temp$Y, lag(temp$X), lag(temp$Y))/
    (as.numeric(difftime(temp$timestamp, lag(temp$timestamp), units = "secs")))
  temp$nMinus2 <- distance(temp$X, temp$Y, lag(temp$X, 2), lag(temp$Y, 2))/
    (as.numeric(difftime(temp$timestamp, lag(temp$timestamp, 2), units = "secs")))
  temp$speed5 <- (temp$nMinus2 + temp$nMinus1 + temp$nPlus1 + temp$nPlus2)/4
  dataframe$speed5 <- temp$speed5
  return(dataframe)
}

# Test the function
test <- speed5(test)
plot(test$speed5)

# Calculating the speed of 5 fixes for all dataframes: 
a1 <- speed5(a1)
a2 <- speed5(a2)
a3 <- speed5(a3)
a4 <- speed5(a4)
a5 <- speed5(a5)
a6 <- speed5(a6)
a7 <- speed5(a7)
a8 <- speed5(a8)
a9 <- speed5(a9)
a10 <- speed5(a10)
a11 <- speed5(a11)
a12 <- speed5(a12)
a13 <- speed5(a13)

# Speed calculated for 11 fixes (analog to Laube and Purves, 2011 and as done in the lecture in Week 4)
speed11 <- function(dataframe){
  temp <- dataframe
  temp$nPlus5 <- distance(lead(temp$X, 5), lead(temp$Y, 5), temp$X, temp$Y)/
    (as.numeric(difftime(lead(temp$timestamp, 5), temp$timestamp, units = "secs")))
  temp$nPlus4 <- distance(lead(temp$X, 4), lead(temp$Y, 4), temp$X, temp$Y)/
    (as.numeric(difftime(lead(temp$timestamp, 4), temp$timestamp, units = "secs")))
  temp$nPlus3 <- distance(lead(temp$X, 3), lead(temp$Y, 3), temp$X, temp$Y)/
    (as.numeric(difftime(lead(temp$timestamp, 3), temp$timestamp, units = "secs")))
  temp$nPlus2 <- distance(lead(temp$X, 2), lead(temp$Y, 2), temp$X, temp$Y)/
    (as.numeric(difftime(lead(temp$timestamp, 2), temp$timestamp, units = "secs")))
  temp$nPlus1 <- distance(lead(temp$X), lead(temp$Y), temp$X, temp$Y)/
    (as.numeric(difftime(lead(temp$timestamp, 1), temp$timestamp, units = "secs")))
  temp$nMinus1 <- distance(temp$X, temp$Y, lag(temp$X), lag(temp$Y))/
    (as.numeric(difftime(temp$timestamp, lag(temp$timestamp, 1), units = "secs")))
  temp$nMinus2 <- distance(temp$X, temp$Y, lag(temp$X, 2), lag(temp$Y, 2))/
    (as.numeric(difftime(temp$timestamp, lag(temp$timestamp, 2), units = "secs")))
  temp$nMinus3 <- distance(temp$X, temp$Y, lag(temp$X, 3), lag(temp$Y, 3))/
    (as.numeric(difftime(temp$timestamp, lag(temp$timestamp, 3), units = "secs")))
  temp$nMinus4 <- distance(temp$X, temp$Y, lag(temp$X, 4), lag(temp$Y, 4))/
    (as.numeric(difftime(temp$timestamp, lag(temp$timestamp, 4), units = "secs")))
  temp$nMinus5 <- distance(temp$X, temp$Y, lag(temp$X, 5), lag(temp$Y, 5))/
    (as.numeric(difftime(temp$timestamp, lag(temp$timestamp, 5), units = "secs")))
  temp$speed11 <- (temp$nMinus5 + temp$nMinus4 + 
                    temp$nMinus3 + temp$nMinus2 + 
                    temp$nMinus1 + temp$nPlus1 + 
                    temp$nPlus2 + temp$nPlus3 + 
                    temp$nPlus4 + temp$nPlus5)/10
  dataframe$speed11 <- temp$speed11
  return(dataframe)
}

# Test the function
test <- speed11(test)
plot(test$speed11)

# Calculating the speed of 11 fixes for all dataframes: 
a1 <- speed11(a1)
a2 <- speed11(a2)
a3 <- speed11(a3)
a4 <- speed11(a4)
a5 <- speed11(a5)
a6 <- speed11(a6)
a7 <- speed11(a7)
a8 <- speed11(a8)
a9 <- speed11(a9)
a10 <- speed11(a10)
a11 <- speed11(a11)
a12 <- speed11(a12)
a13 <- speed11(a13)

# Filtering out the static points:
# I choose 1 m/s as a threshold for the static points

# Function to create columns that store if the different speeds indicate static fixes
static <- function(dataframe){
  dataframe$static2 <- dataframe$speed2 < 1
  dataframe$static5 <- dataframe$speed5 < 1
  dataframe$static11 <- dataframe$speed11 < 1
  return(dataframe)
}

a1 <- static(a1)
a2 <- static(a2)
a3 <- static(a3)
a4 <- static(a4)
a5 <- static(a5)
a6 <- static(a6)
a7 <- static(a7)
a8 <- static(a8)
a9 <- static(a9)
a10 <- static(a10)
a11 <- static(a11)
a12 <- static(a12)
a13 <- static(a13)

# Function to filter out static points
static_filter <- function(dataframe, static){
  filtered <- dataframe %>%
    filter(!static)
  return(filtered)
}

# Filtered out static points from speed2 
a1_static2 <- static_filter(a1, a1$static2)
a2_static2 <- static_filter(a2, a2$static2)
a3_static2 <- static_filter(a3, a3$static2)
a4_static2 <- static_filter(a4, a4$static2)
a5_static2 <- static_filter(a5, a5$static2)
a6_static2 <- static_filter(a6, a6$static2)
a7_static2 <- static_filter(a7, a7$static2)
a8_static2 <- static_filter(a8, a8$static2)
a9_static2 <- static_filter(a9, a9$static2)
a10_static2 <- static_filter(a10, a10$static2)
a11_static2 <- static_filter(a11, a11$static2)
a12_static2 <- static_filter(a12, a12$static2)
a13_static2 <- static_filter(a13, a13$static2)

# Filtered out static points from speed5 
a1_static5 <- static_filter(a1, a1$static5)
a2_static5 <- static_filter(a2, a2$static5)
a3_static5 <- static_filter(a3, a3$static5)
a4_static5 <- static_filter(a4, a4$static5)
a5_static5 <- static_filter(a5, a5$static5)
a6_static5 <- static_filter(a6, a6$static5)
a7_static5 <- static_filter(a7, a7$static5)
a8_static5 <- static_filter(a8, a8$static5)
a9_static5 <- static_filter(a9, a9$static5)
a10_static5 <- static_filter(a10, a10$static5)
a11_static5 <- static_filter(a11, a11$static5)
a12_static5 <- static_filter(a12, a12$static5)
a13_static5 <- static_filter(a13, a13$static5)

# Filtered out static points from speed11 
a1_static11 <- static_filter(a1, a1$static11)
a2_static11 <- static_filter(a2, a2$static11)
a3_static11 <- static_filter(a3, a3$static11)
a4_static11 <- static_filter(a4, a4$static11)
a5_static11 <- static_filter(a5, a5$static11)
a6_static11 <- static_filter(a6, a6$static11)
a7_static11 <- static_filter(a7, a7$static11)
a8_static11 <- static_filter(a8, a8$static11)
a9_static11 <- static_filter(a9, a9$static11)
a10_static11 <- static_filter(a10, a10$static11)
a11_static11 <- static_filter(a11, a11$static11)
a12_static11 <- static_filter(a12, a12$static11)
a13_static11 <- static_filter(a13, a13$static11)

######## Smoothness #########

# Calculate Acceleration and jerk
smoothness2 <- function(dataframe){
  temp <- dataframe %>% 
    mutate(acceleration2 = c(NA, diff(speed2)) / 
             (as.numeric(difftime(timestamp, lag(timestamp), units = "secs"))),
           jerk2 = c(NA, diff(acceleration2)) / 
             (as.numeric(difftime(timestamp, lag(timestamp), units = "secs")))
    )
  return(temp)
}


test <- smoothness2(a1)

# Calculate the smoothness for all dataframes
a1 <- smoothness2(a1)
a2 <- smoothness2(a2)
a3 <- smoothness2(a3)
a4 <- smoothness2(a4)
a5 <- smoothness2(a5)
a6 <- smoothness2(a6)
a7 <- smoothness2(a7)
a8 <- smoothness2(a8)
a9 <- smoothness2(a9)
a10 <- smoothness2(a10)
a11 <- smoothness2(a11)
a12 <- smoothness2(a12)
a13 <- smoothness2(a13)

a1_static2 <- smoothness2(a1_static2)
a2_static2 <- smoothness2(a2_static2)
a3_static2 <- smoothness2(a3_static2)
a4_static2 <- smoothness2(a4_static2)
a5_static2 <- smoothness2(a5_static2)
a6_static2 <- smoothness2(a6_static2)
a7_static2 <- smoothness2(a7_static2)
a8_static2 <- smoothness2(a8_static2)
a9_static2 <- smoothness2(a9_static2)
a10_static2 <- smoothness2(a10_static2)
a11_static2 <- smoothness2(a11_static2)
a12_static2 <- smoothness2(a12_static2)
a13_static2 <- smoothness2(a13_static2)

# Function to get the metrics (mean and standard deviation) of 
# speed, acceleration and jerk of the respective dataframe
get_metrics <- function(dataframe){
  metrics <- dataframe %>%
    summarise(
      total_time = as.numeric(difftime(max(dataframe$timestamp, na.rm = TRUE), 
                                       min(dataframe$timestamp, na.rm = TRUE), 
                                       units = "mins")), 
      mean_speed = mean(speed2, na.rm = TRUE), 
      sd_speed = sd(speed2, na.rm = TRUE), 
      mean_acceleration = mean(acceleration2, na.rm = TRUE),
      sd_acceleration = sd(acceleration2, na.rm = TRUE),
      mean_jerk = mean(jerk2, na.rm = TRUE),
      sd_jerk = sd(jerk2, na.rm = TRUE)
    )
}

# Calculate the metrics for all dataframes

metrics_a1_static2 <- a1_static2 %>% get_metrics()
metrics_a2_static2 <- a2_static2 %>% get_metrics()
metrics_a3_static2 <- a3_static2 %>% get_metrics()
metrics_a4_static2 <- a4_static2 %>% get_metrics()
metrics_a5_static2 <- a5_static2 %>% get_metrics()
metrics_a6_static2 <- a6_static2 %>% get_metrics()
metrics_a7_static2 <- a7_static2 %>% get_metrics()
metrics_a8_static2 <- a8_static2 %>% get_metrics()
metrics_a9_static2 <- a9_static2 %>% get_metrics()
metrics_a10_static2 <- a10_static2 %>% get_metrics()
metrics_a11_static2 <- a11_static2 %>% get_metrics()
metrics_a12_static2 <- a12_static2 %>% get_metrics()
metrics_a13_static2 <- a13_static2 %>% get_metrics()

# Add name of the dataframe to the each table so that I can then combine them
metrics_a1_static2$name <- deparse(substitute(a1_static2))
metrics_a2_static2$name <- deparse(substitute(a2_static2))
metrics_a3_static2$name <- deparse(substitute(a3_static2))
metrics_a4_static2$name <- deparse(substitute(a4_static2))
metrics_a5_static2$name <- deparse(substitute(a5_static2))
metrics_a6_static2$name <- deparse(substitute(a6_static2))
metrics_a7_static2$name <- deparse(substitute(a7_static2))
metrics_a8_static2$name <- deparse(substitute(a8_static2))
metrics_a9_static2$name <- deparse(substitute(a9_static2))
metrics_a10_static2$name <- deparse(substitute(a10_static2))
metrics_a11_static2$name <- deparse(substitute(a11_static2))
metrics_a12_static2$name <- deparse(substitute(a12_static2))
metrics_a13_static2$name <- deparse(substitute(a13_static2))

# Combine all together
metrics_static2 <- rbind(metrics_a1_static2,
                         metrics_a2_static2,
                         metrics_a3_static2, 
                         metrics_a4_static2, 
                         metrics_a5_static2, 
                         metrics_a6_static2, 
                         metrics_a7_static2, 
                         metrics_a8_static2, 
                         metrics_a9_static2, 
                         metrics_a10_static2, 
                         metrics_a11_static2, 
                         metrics_a12_static2, 
                         metrics_a13_static2) 

# The total time cannot be correct. I suspect that since I usually rest at the
# end of the trail - some points static - before I then ride away (and leave 
# the buffer zone)- movement again - it still counts this time...
# How can I delete this?
# recalculate timelag?:
a1_static2$timelag <- as.numeric(difftime(lead(a1_static2$timestamp), a1_static2$timestamp))

# Function that removes any timelag longer than the specified threshold
remove_timelag <- function(dataframe, threshold = 25){
  while (any(na.omit(dataframe$timelag) > threshold)){
    dataframe <- dataframe[which(dataframe$timelag <= threshold),]
    dataframe <- dataframe %>% 
      mutate(
        timelag = as.numeric(difftime(lead(timestamp), timestamp, units = "secs")) 
      )
  }
  return(dataframe)
}
# This function does still not really work, since it will only keep the last 
# part without a timegap...

# Let me try again with the previous longest_consecutive_timestamps() function
# It might work here as well

a1_static2 <- longest_consecutive_timestamps(a1_static2, 25)
a2_static2 <- longest_consecutive_timestamps(a2_static2, 25)
a3_static2 <- longest_consecutive_timestamps(a3_static2, 25)
a4_static2 <- longest_consecutive_timestamps(a4_static2, 25)
a5_static2 <- longest_consecutive_timestamps(a5_static2, 25)
a6_static2 <- longest_consecutive_timestamps(a6_static2, 25)
a7_static2 <- longest_consecutive_timestamps(a7_static2, 25)
a8_static2 <- longest_consecutive_timestamps(a8_static2, 25)
a9_static2 <- longest_consecutive_timestamps(a9_static2, 25)
a10_static2 <- longest_consecutive_timestamps(a10_static2, 25)
a11_static2 <- longest_consecutive_timestamps(a11_static2, 25)
a12_static2 <- longest_consecutive_timestamps(a12_static2, 25)
a13_static2 <- longest_consecutive_timestamps(a13_static2, 25)

# Recalculation of the metrics
# Calculate the metrics for all dataframes

metrics_a1_static2 <- a1_static2 %>% get_metrics()
metrics_a2_static2 <- a2_static2 %>% get_metrics()
metrics_a3_static2 <- a3_static2 %>% get_metrics()
metrics_a4_static2 <- a4_static2 %>% get_metrics()
metrics_a5_static2 <- a5_static2 %>% get_metrics()
metrics_a6_static2 <- a6_static2 %>% get_metrics()
metrics_a7_static2 <- a7_static2 %>% get_metrics()
metrics_a8_static2 <- a8_static2 %>% get_metrics()
metrics_a9_static2 <- a9_static2 %>% get_metrics()
metrics_a10_static2 <- a10_static2 %>% get_metrics()
metrics_a11_static2 <- a11_static2 %>% get_metrics()
metrics_a12_static2 <- a12_static2 %>% get_metrics()
metrics_a13_static2 <- a13_static2 %>% get_metrics()

# Add name of the dataframe to the each table so that I can then combine them
metrics_a1_static2$name <- deparse(substitute(a1_static2))
metrics_a2_static2$name <- deparse(substitute(a2_static2))
metrics_a3_static2$name <- deparse(substitute(a3_static2))
metrics_a4_static2$name <- deparse(substitute(a4_static2))
metrics_a5_static2$name <- deparse(substitute(a5_static2))
metrics_a6_static2$name <- deparse(substitute(a6_static2))
metrics_a7_static2$name <- deparse(substitute(a7_static2))
metrics_a8_static2$name <- deparse(substitute(a8_static2))
metrics_a9_static2$name <- deparse(substitute(a9_static2))
metrics_a10_static2$name <- deparse(substitute(a10_static2))
metrics_a11_static2$name <- deparse(substitute(a11_static2))
metrics_a12_static2$name <- deparse(substitute(a12_static2))
metrics_a13_static2$name <- deparse(substitute(a13_static2))

# Combine all together
metrics_static2 <- rbind(metrics_a1_static2,
                         metrics_a2_static2,
                         metrics_a3_static2, 
                         metrics_a4_static2, 
                         metrics_a5_static2, 
                         metrics_a6_static2, 
                         metrics_a7_static2, 
                         metrics_a8_static2, 
                         metrics_a9_static2, 
                         metrics_a10_static2, 
                         metrics_a11_static2, 
                         metrics_a12_static2, 
                         metrics_a13_static2)

# Better... But there are some suspiciously low times... I might have cropped it too much
# Markdown table that I can use in the report:
library(simplermarkdown)
md_table(st_drop_geometry(metrics_static2))

plot(metrics_static2)

# Maybe alright, but somethings seem off
# Should I technically recalculate speed for the current versions of the dataframes?


######## Animating the Movement ########
library(moveVis)
library(move)

# Convert data to Move object
# Turns out, the data needs to be in EPSG 4326.
test <- a1_static2

test <- st_transform(test, crs = 4326)
coords <- st_coordinates(test)
test <- cbind(test, coords) 
coordinates(a1) <- ~X+Y
move_obj <- move(x = test$X.1, 
                 y = test$Y.1, 
                 time = test$timestamp, 
                 proj = CRS("+proj=longlat +ellps=WGS84"))

move_obj_aligned <- align_move(move_obj, res = 1, unit = "secs")
  
frames <- frames_spatial(move_obj_aligned, 
                         map_service = "osm",  
                         map_type = "topographic", 
                         alpha = 0.5) 

# I somehow need to make sure that all animations run at the same time. 
# Since I could not find out, how that works with MoveVis specifically, 
# I am setting all timestamps to an arbitrary date (01.01.2024, 12:00)
# so that they all start then. 
library(lubridate)

# function to create the move
create_move <- function(dataframe){
  timezero <- ymd_hms("2024-01-01 12:00:00")
  temp <- dataframe
  starttime <- temp$timestamp[1]
  temp$timediff <- as.numeric(difftime(temp$timestamp, starttime), units = "secs")
  temp$newtime <- timezero + temp$timediff
  temp <- st_transform(temp, crs = 4326)
  coords <- st_coordinates(temp)
  temp <- cbind(temp, coords) 
  move_obj <- move(x = temp$X.1, 
                   y = temp$Y.1, 
                   time = temp$newtime, 
                   animal = deparse(substitute(dataframe)),
                   proj = CRS("+proj=longlat +ellps=WGS84"))
  move_obj_aligned <- align_move(move_obj, res = 1, unit = "secs")
  return(move_obj_aligned)
}

# All dataframes into moves
a1_move <- create_move(a1_static2)
a2_move <- create_move(a2_static2)
a3_move <- create_move(a3_static2)
a4_move <- create_move(a4_static2)
a5_move <- create_move(a5_static2)
a6_move <- create_move(a6_static2)
a7_move <- create_move(a7_static2)
a8_move <- create_move(a8_static2)
a9_move <- create_move(a9_static2)
a10_move <- create_move(a10_static2)
a11_move <- create_move(a11_static2)
a12_move <- create_move(a12_static2)
a13_move <- create_move(a13_static2)

# Manually adjust times, so that all moves that start lower only start, when 
# a1 passes them

# a2 + 197
a2_move@timestamps <- a2_move@timestamps + 197
# a7 + 29
a7_move@timestamps <- a7_move@timestamps + 29
# a13 + 305
a13_move@timestamps <- a13_move@timestamps + 305
# a10 + 269
a10_move@timestamps <- a10_move@timestamps + 269
# a4 + 396
a4_move@timestamps <- a4_move@timestamps + 396
# a12 + 408
a12_move@timestamps <- a12_move@timestamps + 408

# Combining them all together
stack <- moveStack(a1_move, a2_move, a3_move, a4_move, a5_move, a6_move, a7_move, 
            a8_move, a9_move, a10_move, a11_move, a12_move, a13_move 
            #names("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13")
            )

plot(stack)

# Align temporally
stack_aligned <- align_move(stack, res = 1, unit = "secs")

frames_stack <- frames_spatial(stack_aligned, 
                         map_service = "osm",  
                         map_type = "topographic", 
                         alpha = 0.5)

# Edit frames
frames_stack <- add_labels(frames_stack, x = "Longitude", y = "Latitude")
frames_stack <- add_progress(frames_stack) 
frames_stack <- add_scalebar(frames_stack, height = 0.015) 
frames_stack <- add_northarrow(frames_stack)

frames_stack[[3]]
# Unfortunately some of the moves start lower... and are therefore not comparable.

# Animate the frames
animation <- animate_frames(frames_stack, out_file = "gps_movement3.gif", 
                            width = 800, height = 600, fps = 24)  # Frames per second

# Display the animation in R
animation

######## Visualize the results ######## 
# Individual plots of speed, acceleration and jerk for each dataframe

# Load required libraries
library(ggplot2)

# Function to get the maxspeed from all dataframes. 
maxspeed <- function(){
  maxval <- max(a1_static2$speed2, na.rm = TRUE)
  if (max(a2_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a2_static2$speed2, na.rm = TRUE)
  }
  if (max(a3_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a3_static2$speed2, na.rm = TRUE)
  }
  if (max(a4_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a4_static2$speed2, na.rm = TRUE)
  }
  if (max(a5_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a5_static2$speed2, na.rm = TRUE)
  }
  if (max(a6_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a6_static2$speed2, na.rm = TRUE)
  }
  if (max(a7_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a7_static2$speed2, na.rm = TRUE)
  }
  if (max(a8_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a8_static2$speed2, na.rm = TRUE)
  }
  if (max(a9_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a9_static2$speed2, na.rm = TRUE)
  }
  if (max(a10_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a10_static2$speed2, na.rm = TRUE)
  }
  if (max(a11_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a11_static2$speed2, na.rm = TRUE)
  }
  if (max(a12_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a12_static2$speed2, na.rm = TRUE)
  }
  if (max(a13_static2$speed2, na.rm = TRUE) >= maxval){
    maxval <- max(a13_static2$speed2, na.rm = TRUE)
  }
  return(maxval)
}

maxspeed2 <- maxspeed()
# This function works great, but doing this for the minimum and maximum of 
# speed, acceleration and jerk is taking to long. 
# There must be more efficient ways (or more like, it can probably be done in less lines of code)

# Global max and min of speed
speed_lim <- function(){
  speed2 <- c(a1_static2$speed2, a6_static2$speed2, a10_static2$speed2,
                   a2_static2$speed2, a7_static2$speed2, a11_static2$speed2,
                   a3_static2$speed2, a8_static2$speed2, a12_static2$speed2, 
                   a4_static2$speed2, a9_static2$speed2, a13_static2$speed2, 
                   a5_static2$speed2)
  maxspeed <- max(speed2, na.rm = TRUE)
  minspeed <- min(speed2, na.rm = TRUE)
  return(c(maxspeed, minspeed))
}

speed_lim2 <- speed_lim()
# Less lines of code... But I have not thought about the computational efficiency

# Global Max and min of acceleration
acceleration_lim <- function(){
  acceleration2 <- c(a1_static2$acceleration2, a6_static2$acceleration2, a10_static2$acceleration2,
              a2_static2$acceleration2, a7_static2$acceleration2, a11_static2$acceleration2,
              a3_static2$acceleration2, a8_static2$acceleration2, a12_static2$acceleration2, 
              a4_static2$acceleration2, a9_static2$acceleration2, a13_static2$acceleration2, 
              a5_static2$acceleration2)
  maxacc <- max(acceleration2, na.rm = TRUE)
  minacc <- min(acceleration2, na.rm = TRUE)
  return(c(maxacc, minacc)) 
}

acc_lim <- acceleration_lim()

# Global Max and min of jerk
jerk_lim <- function(){
  jerk2 <- c(a1_static2$jerk2, a6_static2$jerk2, a10_static2$jerk2,
                     a2_static2$jerk2, a7_static2$jerk2, a11_static2$jerk2,
                     a3_static2$jerk2, a8_static2$jerk2, a12_static2$jerk, 
                     a4_static2$jerk2, a9_static2$jerk2, a13_static2$jerk2, 
                     a5_static2$jerk2)
  maxjerk <- max(jerk2, na.rm = TRUE)
  minjerk <- min(jerk2, na.rm = TRUE)
  return(c(maxjerk, minjerk)) 
}

jerk_lim2 <- jerk_lim()

plot_speed <- function(dataframe){
  p_speed <- ggplot(dataframe, aes(x = timestamp, y = speed2)) +
    geom_line() + 
    ylim(speed_lim2[2], speed_lim2[1]) +
    labs(title = paste(deparse(substitute(dataframe))), x = "Time", y = "Speed")
  return(p_speed)
}

plot_speed_all <- function(){
  p_speed <- ggplot() +
    geom_line(aes(x = timediff_since_start(a1_static2), y = a1_static2$speed2), color = "red", lty = "dashed") + 
    geom_line(aes(x = timediff_since_start(a2_static2), y = a2_static2$speed2), color = "orange", lty = "dashed") + 
    geom_line(aes(x = timediff_since_start(a3_static2), y = a3_static2$speed2), color = "yellow", lty = "dashed") + 
    geom_line(aes(x = timediff_since_start(a4_static2), y = a4_static2$speed2), color = "lightgreen", lty = "dashed") + 
    geom_line(aes(x = timediff_since_start(a5_static2), y = a5_static2$speed2), color = "green", lty = "dashed") + 
    geom_line(aes(x = timediff_since_start(a6_static2), y = a6_static2$speed2), color = "darkgreen", lty = "dashed") + 
    geom_line(aes(x = timediff_since_start(a7_static2), y = a7_static2$speed2), color = "lightblue", lty = "dotted") + 
    geom_line(aes(x = timediff_since_start(a8_static2), y = a8_static2$speed2), color = "royalblue", lty = "dotted") + 
    geom_line(aes(x = timediff_since_start(a9_static2), y = a9_static2$speed2), color = "darkblue", lty = "dotted") + 
    geom_line(aes(x = timediff_since_start(a10_static2), y = a10_static2$speed2), color = "purple", lty = "dotted") + 
    geom_line(aes(x = timediff_since_start(a11_static2), y = a11_static2$speed2), color = "magenta", lty = "dotted") +
    geom_line(aes(x = timediff_since_start(a12_static2), y = a12_static2$speed2), color = "pink", lty = "dotted") + 
    geom_line(aes(x = timediff_since_start(a13_static2), y = a13_static2$speed2), color = "salmon", lty = "dotted") + 
    ylim(speed_lim2[2], 25) +
    labs(title = "Speed over the duration of the ride", x = "Time", y = "Speed")
  return(p_speed)
}
# There is only one speed over 50 m/s, considering that this is more than 
# 150 km/h, this is most likely due to an error in the GPS position, so to see 
# the lines better, I am setting the ylim to 25 instead. 

plot_speed_all()
# Nice try, but this plot actually displays nothing

# I am going all out and make the plot interactive

test <- plot_speed(a1_static2)
test
# Plot Speed
p_speed <- ggplot(combined_a, aes(x = timestamp, y = speed2, color = id)) +
  geom_line() +
  facet_wrap(~ id, ncol = 5) +
  ylim(y_limits$speed_min, y_limits$speed_max) +
  theme_minimal() +
  labs(title = "Speed over Time", x = "Time", y = "Speed")

# Plot Acceleration
p_accel <- ggplot(combined_a, aes(x = timestamp, y = acceleration2, color = id)) +
  geom_line() +
  facet_wrap(~ id, ncol = 5) +
  ylim(y_limits$accel_min, y_limits$accel_max) +
  theme_minimal() +
  labs(title = "Acceleration over Time", x = "Time", y = "Acceleration")

# Plot Jerk
p_jerk <- ggplot(combined_a, aes(x = timestamp, y = jerk2, color = id)) +
  geom_line() +
  facet_wrap(~ id, ncol = 5) +
  ylim(y_limits$jerk_min, y_limits$jerk_max) +
  theme_minimal() +
  labs(title = "Jerk over Time", x = "Time", y = "Jerk")

# Combine plots vertically
combined_plot <- plot_grid(p_speed, p_accel, p_jerk, align = 'v', ncol = 1)

combined_plot


######## Segmentation ########
# import segments
# difficulty segmentation
segment1 <- st_read("data/segmentation/difficulty_segmentation/segment1.gpkg")
segment2 <- st_read("data/segmentation/difficulty_segmentation/segment2.gpkg")
segment3 <- st_read("data/segmentation/difficulty_segmentation/segment3.gpkg")
segment4 <- st_read("data/segmentation/difficulty_segmentation/segment4.gpkg")
segment5 <- st_read("data/segmentation/difficulty_segmentation/segment5.gpkg")
segment6 <- st_read("data/segmentation/difficulty_segmentation/segment6.gpkg")
segment7 <- st_read("data/segmentation/difficulty_segmentation/segment7.gpkg")
segment8 <- st_read("data/segmentation/difficulty_segmentation/segment8.gpkg")
segment9 <- st_read("data/segmentation/difficulty_segmentation/segment9.gpkg")
segment10 <- st_read("data/segmentation/difficulty_segmentation/segment10.gpkg")

# natural segmentation
n_segment1 <- st_read("data/segmentation/natural_segmentation/n_segment1.gpkg")
n_segment2 <- st_read("data/segmentation/natural_segmentation/n_segment2.gpkg")
n_segment3 <- st_read("data/segmentation/natural_segmentation/n_segment3.gpkg")
n_segment4 <- st_read("data/segmentation/natural_segmentation/n_segment4.gpkg")
n_segment5 <- st_read("data/segmentation/natural_segmentation/n_segment5.gpkg")
n_segment6 <- st_read("data/segmentation/natural_segmentation/n_segment6.gpkg")
n_segment7 <- st_read("data/segmentation/natural_segmentation/n_segment7.gpkg")
n_segment8 <- st_read("data/segmentation/natural_segmentation/n_segment8.gpkg")


# Create buffer for all segments
buf1 <- st_buffer(segment1, 3, endCapStyle = "flat")
buf2 <- st_buffer(segment2, 3, endCapStyle = "flat")
buf3 <- st_buffer(segment3, 3, endCapStyle = "flat")
buf4 <- st_buffer(segment4, 3, endCapStyle = "flat")
buf5 <- st_buffer(segment5, 3, endCapStyle = "flat")
buf6 <- st_buffer(segment6, 3, endCapStyle = "flat")
buf7 <- st_buffer(segment7, 3, endCapStyle = "flat")
buf8 <- st_buffer(segment8, 3, endCapStyle = "flat")
buf9 <- st_buffer(segment9, 3, endCapStyle = "flat")
buf10 <- st_buffer(segment10, 3, endCapStyle = "flat")

n_buf1 <- st_buffer(n_segment1, 3, endCapStyle = "flat")
n_buf2 <- st_buffer(n_segment2, 3, endCapStyle = "flat")
n_buf3 <- st_buffer(n_segment3, 3, endCapStyle = "flat")
n_buf4 <- st_buffer(n_segment4, 3, endCapStyle = "flat")
n_buf5 <- st_buffer(n_segment5, 3, endCapStyle = "flat")
n_buf6 <- st_buffer(n_segment6, 3, endCapStyle = "flat")
n_buf7 <- st_buffer(n_segment7, 3, endCapStyle = "flat")
n_buf8 <- st_buffer(n_segment8, 3, endCapStyle = "flat")

# Adding a column, that states the column 
# I will go from the top to the bottom, so
a1 <- a1 %>% filter(apply(st_within(., buffer, sparse = FALSE), 1, any))

  
######## Map Matching ########
# I did start to map match, but since this mixes up the order of the points 
# (The fixes are sometimes to far off, and get therefore matched to the wrong 
# parts of the trail, and this would introduce a lot of errors into the calculation 
# of speed or smoothness) I did the analysis on the original GPS fixes

# I followed the tutorial from class

# I used this data: https://www.swisstopo.admin.ch/de/landeskarte-swiss-map-vector-10
# For the road network
# Specifically the layer "DKM10_STRASSE"
paths <- st_read("data/paths.gpkg")

# union them
paths <- st_union(paths)

test <- a1
nearest <- st_nearest_points(test, paths)
near_p <- st_cast(nearest, "POINT")
near_from <- near_p[c(TRUE, FALSE)]
near_to <- near_p[c(FALSE,TRUE)]:

tm_shape(paths) + tm_lines() +
  tm_shape(nearest) + tm_lines(lty = 3) +
  tm_shape(near_from) + tm_dots() +
  tm_shape(near_to) + tm_dots(col = "red")

# This is nice... But it does sometimes match the points to the wrong path.
# Since I mostly took the same path, I am going to go back to QGIS and delete
# the roads, that I did not use

# Here is the new road, which just contains the correct path
# In the bottom I included a variant, since judging from my records, 
# I did take that at least once
only_bleiki <- st_read("data/only_bleiki.gpkg")
only_bleiki <- st_union(only_bleiki)

nearest2 <- st_nearest_points(test, only_bleiki)
near_p2 <- st_cast(nearest2, "POINT")
near_from2 <- near_p2[c(TRUE, FALSE)]
near_to2 <- near_p2[c(FALSE,TRUE)]

tm_shape(only_bleiki) + tm_lines() +
  tm_shape(nearest2) + tm_lines(lty = 3) +
  tm_shape(near_from2) + tm_dots() +
  tm_shape(near_to2) + tm_dots(col = "red")

# Better but the problem is still, that in some switchbacks it does match the point 
# to the wrong path... Which might give me problems when calculating the speed and the 
# smoothness... 
# And to be honest... The path displayed on the open street map seems to match the GPS
# fixes better than the hiking path from SwissTopo... 
# Will ask a Geomatiker, how that could be

# So apparently the hiking paths do not get checked everytime and could be copied 
# from old maps. Additionally, it might have more of an informative character and does
# therefore not need to be too accurate. I did check with the DEM and according to 
# this, the OSM paths match the DEM more than the Swisstopo one. Therefore I will
# be using the OSM paths and not the Swisstopo paths
osm_bleiki <- st_read("data/QGIS-stuff/osm_lines.gpkg")
osm_bleiki <- st_transform(osm_bleiki, crs=2056)
osm_bleiki <- st_union(osm_bleiki)

nearest3 <- st_nearest_points(test, osm_bleiki)
near_p3 <- st_cast(nearest3, "POINT")
near_from3 <- near_p3[c(TRUE, FALSE)]
near_to3 <- near_p3[c(FALSE,TRUE)]

tm_shape(osm_bleiki) + tm_lines() +
  tm_shape(nearest3) + tm_lines(lty = 3) +
  tm_shape(near_from3) + tm_dots() +
  tm_shape(near_to3) + tm_dots(col = "red")

# Still a bit problematic in some parts... But better
# Only other option that I can come up with right now is not doing mapmatching...
# I will start with doing my analysis with the matched points first and then try it 
# with the not matched data and check how the results make sense

# Next how to store the matched points ...
# The points did not change the order compared to the original dataframe...
# So I should be able to just add the column to the dataframe 
test_matched <- st_drop_geometry(test)
test_matched$geometry <- near_to3
test_matched <- st_set_geometry(test_matched, test_matched$geometry)

map_match <- function(dataframe, path) {
  nearest <- st_nearest_points(dataframe, path)
  near_p <- st_cast(nearest, "POINT")
  near_to <- near_p[c(FALSE, TRUE)]
  temp <- st_drop_geometry(dataframe)
  temp$geometry <- near_to
  temp <- st_set_geometry(temp, temp$geometry)
  coords <- st_coordinates(temp)
  temp <- cbind(temp, coords)
  return(temp)
}

a1_matched <- map_match(a1, osm_bleiki)
a2_matched <- map_match(a2, osm_bleiki)
a3_matched <- map_match(a3, osm_bleiki)
a4_matched <- map_match(a4, osm_bleiki)
a5_matched <- map_match(a5, osm_bleiki)
a6_matched <- map_match(a6, osm_bleiki)
a7_matched <- map_match(a7, osm_bleiki)
a8_matched <- map_match(a8, osm_bleiki)
a9_matched <- map_match(a9, osm_bleiki)
a10_matched <- map_match(a10, osm_bleiki)
a11_matched <- map_match(a11, osm_bleiki)
a12_matched <- map_match(a12, osm_bleiki)
a13_matched <- map_match(a13, osm_bleiki)

a1_m_line <- create_line(a1_matched)
a2_m_line <- create_line(a2_matched)
a3_m_line <- create_line(a3_matched)
a4_m_line <- create_line(a4_matched)
a5_m_line <- create_line(a5_matched)
a6_m_line <- create_line(a6_matched)
a7_m_line <- create_line(a7_matched)
a8_m_line <- create_line(a8_matched)
a9_m_line <- create_line(a9_matched)
a10_m_line <- create_line(a10_matched)
a11_m_line <- create_line(a11_matched)
a12_m_line <- create_line(a12_matched)
a13_m_line <- create_line(a13_matched)

tm_shape(a1_m_line) +
  tm_lines(col = "black") +
  tm_shape(a1_line) +
  tm_lines(col = "black", lty = "dotted") 

tm_shape(a2_m_line) +
  tm_lines(col = "grey") +
  tm_shape(a2_line) +
  tm_lines(col = "grey", lty="dotted") 

tm_shape(a3_m_line) +
  tm_lines(col = "brown") +
  tm_shape(a3_line) +
  tm_lines(col = "brown", lty = "dotted") 
  
tm_shape(a4_m_line) +
  tm_lines(col = "yellow") +
  tm_shape(a4_line) +
  tm_lines(col = "yellow", lty = "dotted") 

tm_shape(a5_m_line) +
  tm_lines(col = "orange") +
  tm_shape(a5_line) +
  tm_lines(col = "orange", lty = "dotted") 

tm_shape(a6_m_line) +
  tm_lines(col = "red") +
  tm_shape(a6_line) +
  tm_lines(col = "red", lty = "dotted") 

tm_shape(a7_m_line) +
  tm_lines(col = "pink") +
  tm_shape(a7_line) +
  tm_lines(col = "pink", lty = "dotted") 

tm_shape(a8_m_line) +
  tm_lines(col = "purple") +
  tm_shape(a8_line) +
  tm_lines(col = "purple", lty = "dotted") 

tm_shape(a9_m_line) +
  tm_lines(col = "blue") +
  tm_shape(a9_line) +
  tm_lines(col = "blue", lty = "dotted") 

tm_shape(a10_m_line) +
  tm_lines(col = "turquoise") +
  tm_shape(a10_line) +
  tm_lines(col = "turquoise", lty = "dotted") 

tm_shape(a11_m_line) +
  tm_lines(col = "green") +
  tm_shape(a11_line) +
  tm_lines(col = "green", lty = "dotted") 

tm_shape(a12_m_line) +
  tm_lines(col = "darkgreen") +
  tm_shape(a12_line) +
  tm_lines(col = "darkgreen", lty = "dotted") 

tm_shape(a13_m_line) +
  tm_lines(col = "magenta") +
  tm_shape(a13_line) +
  tm_lines(col = "magenta", lty = "dotted")
