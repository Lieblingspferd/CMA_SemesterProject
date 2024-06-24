######### Functions #########
# This R script stores the functions that I created in order to process my data.
# These functions are often very specific, and only work with my data structure.

# In order to have all functions available in the preprocessing script, without 
# running every line of code, I have moved them here additionally and will refer
# back to this script in the preprocessing script. They were developed, written,
# tested and used in the preprocessing script. 


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

# Create a line
create_line <- function(dataframe){
  line <- dataframe %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")
}

# Add a column for the time difference seconds (since the start of the activity)
timediff_since_start <- function(dataframe){
  starttime <- dataframe$timestamp[1]
  timediff <- as.numeric(difftime(dataframe$timestamp, starttime), units = "secs")
  return(timediff)
}

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

speed2 <- #calculates the speed based on two consecutive fixes
  function(dataframe){
    dataframe$speed2 <- dataframe$distance / 
      (as.numeric(difftime(lead(dataframe$timestamp), dataframe$timestamp, units = "secs")))
    return(dataframe)
  }

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

# Function to create columns that store if the different speeds indicate static fixes
static <- function(dataframe){
  dataframe$static2 <- dataframe$speed2 < 1
  dataframe$static5 <- dataframe$speed5 < 1
  dataframe$static11 <- dataframe$speed11 < 1
  return(dataframe)
}

# Function to filter out static points
static_filter <- function(dataframe, static_col){
  filtered <- dataframe %>%
    filter(!static_col)
  return(filtered)
}

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

# Function that removes any timelag longer than the specified threshold
# Careful: Does only work for few cases...
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

# Function to add the different dataframes to the plot
add_data_to_fig <- function(dataframe, color2, label){
  fig <- fig %>% add_trace(data = dataframe, x = timediff_since_start(dataframe), y = ~speed2, 
                           name = paste0("Speed ", label), mode = 'lines', type = 'scatter', line = list(color = color2, width = 2))
  fig <- fig %>% add_trace(data = dataframe, x = timediff_since_start(dataframe),  
                           y = ~acceleration2, name = paste0("Acceleration ", label), line = list(color = color2, width = 2, dash = 'dash')) 
  fig <- fig %>% add_trace(data = dataframe, x = timediff_since_start(dataframe),
                           y = ~jerk2, name = paste0("Jerk ", label), line = list(color = color2, width = 2, dash = 'dot'))
  return(fig)
}

# Adding a column, that states the segment 
# I will go from the top to the bottom, so if any segment is in multiple, 
# it will be matched to the one with the highest number
match_to_segment <- function(dataframe, segmentation = "difficulty"){
  temp <- dataframe
  temp$segment <- "none"
  if (segmentation == "difficulty"){
    temp$bool_segment <- apply(st_within(dataframe, buf1, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "segment 1"
    temp$bool_segment <- apply(st_within(dataframe, buf2, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "segment 2"
    temp$bool_segment <- apply(st_within(dataframe, buf3, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "segment 3"
    temp$bool_segment <- apply(st_within(dataframe, buf4, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "segment 4"
    temp$bool_segment <- apply(st_within(dataframe, buf5, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "segment 5"
    temp$bool_segment <- apply(st_within(dataframe, buf6, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "segment 6"
    temp$bool_segment <- apply(st_within(dataframe, buf7, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "segment 7"
    temp$bool_segment <- apply(st_within(dataframe, buf8, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "segment 8"
    temp$bool_segment <- apply(st_within(dataframe, buf9, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "segment 9"
    temp$bool_segment <- apply(st_within(dataframe, buf10, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "segment 10"
    temp$bool_segment <- NULL
    return(temp)
  } else if (segmentation == "natural"){
    temp$bool_segment <- apply(st_within(dataframe, n_buf1, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "N segment 1"
    temp$bool_segment <- apply(st_within(dataframe, n_buf2, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "N segment 2"
    temp$bool_segment <- apply(st_within(dataframe, n_buf3, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "N segment 3"
    temp$bool_segment <- apply(st_within(dataframe, n_buf4, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "N segment 4"
    temp$bool_segment <- apply(st_within(dataframe, n_buf5, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "N segment 5"
    temp$bool_segment <- apply(st_within(dataframe, n_buf6, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "N segment 6"
    temp$bool_segment <- apply(st_within(dataframe, n_buf7, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "N segment 7"
    temp$bool_segment <- apply(st_within(dataframe, n_buf8, sparse = FALSE), 1, any)
    temp$segment[temp$bool_segment == TRUE] <- "N segment 8"
    temp$bool_segment <- NULL
    return(temp)
  } else {print("This type of segmentation is not supported")}
}

# Function to get the metrics (mean and standard deviation) of 
# speed, acceleration and jerk of the respective dataframe by segment
get_metrics_by_segment <- function(dataframe){
  metrics <- dataframe %>%
    group_by(segment) %>%
    summarise(
      total_time = as.numeric(difftime(max(timestamp, na.rm = TRUE), 
                                       min(timestamp, na.rm = TRUE), 
                                       units = "secs")),
      mean_speed = mean(speed2, na.rm = TRUE), 
      sd_speed = sd(speed2, na.rm = TRUE), 
      mean_acceleration = mean(acceleration2, na.rm = TRUE),
      sd_acceleration = sd(acceleration2, na.rm = TRUE),
      mean_jerk = mean(jerk2, na.rm = TRUE),
      sd_jerk = sd(jerk2, na.rm = TRUE)
    )
}

# Map Matching (without incorporating the timestamp)
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