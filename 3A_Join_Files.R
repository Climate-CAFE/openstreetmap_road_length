##Author: Allison James
##Contrib: Zach Popp
##Date Created: 09/16/2024
##Date Modified: 09/10/2024
##Overview: The goal is this script is to process roads data from OpenStreetMap
##          to an administrative area level road length measure by road class.
##          This tutorial can be used for any spatial resolution, but this
##          example calculates road length measures at the municipality level
##          for each state in Mexico.
##          This script uses OpenStreetMap data downloaded locally from 
##          Geofabrik in a shapefile format and uses GADM data for the 
##          administrative areas, which is downloaded for the whole country.
##          The roads are intersected with administrative areas using 
##          st_intersection, and then sum road lengths are calculated for each
##          of the unique road classifications. The number of road intersections
##          in each administrative area are also calculated.
##
##Purpose: 
##          Join the road lengths and intersections for each state together
##          to create a final, country-wide file.


# Reading in packages
library(tidyverse)
library(sf)

roads_state_dir <- "" # This is where each state-wide file is kept.
roads_country_dir <- "" # This is where you would like your final output file to go.

# %%%%%%%%%%%%%%%%%%%% JOIN ROAD LENGTHS %%%%%%%%%%%%%%%%%%%%%%% #

# Create list of all county-wide tract-level road measures data
#
roads_files <- dir(roads_state_dir, full.names=TRUE, pattern="osm_road_len_by_fclass_.*.rds")

# Looping through files to merge into one file with all years included
#
for (i in 1:length(roads_files)) {
  
  # Indicate progress
  #
  cat("Processing", i, "of", length(roads_files), "files \n")
  
  # Read in roads at state level
  #
  input <- readRDS(roads_files[i])
  
  # The state's road length was calculated with distinct projected coordinate 
  # systems based its longitude and hemisphere. We cannot create a nationwide
  # shapefile from components with distinct coordinate systems. To create 
  # the nationwide file, we will convert each shapefile to a consistent 
  # coordinate system. We use WGS 84, as it is the coordinate system which the 
  # states were initially read in as. It won't change the calculated lengths,
  # it just makes it possible to join the files together and map them.
  #
  input <- st_transform(input, 4326)
  
  if (i == 1) { final_roads <- input; next }
  
  # Bind each new state to a nationwide dataset
  #
  final_roads <- rbind(final_roads, input)
}

# For all NA in the dataset for road categories, we replace with 0.
# Because there are no roads of a particular type intersecting with a given state tract, 
# the length will be calculated as NA, but this represents a 0 road length, 
# so we substitute the 0 for clarity.
#
road_len_vars <- c("prim_len", "sec_len", "track_len", "other_len", "total_len")

for (i in road_len_vars) {
  final_roads[i] <- ifelse(is.na(final_roads[[i]]), 0, final_roads[[i]])
}


# %%%%%%%%%%%%%%%%%%%% JOIN INTERSECTIONS %%%%%%%%%%%%%%%%%%%%%%% #

# Create list of all county-wide tract-level intersections data
#
int_files <- dir(roads_state_dir, full.names=TRUE, pattern="osm_intersections_.*.rds")

# Looping through files to merge into one file with all years included
#
for (i in 1:length(int_files)) {
  
  # Indicate progress
  #
  cat("Processing", i, "of", length(int_files), "files \n")
  
  # Read in roads at state level
  #
  input <- readRDS(int_files[i])
  
  # We will also convert these to WGS 84 for the same reason as the roads
  #
  input <- st_transform(input, 4326)
  
  if (i == 1) { final_int <- input; next }
  
  # Bind each new municipality to a nationwide dataset
  #
  final_int <- rbind(final_int, input)
}

# For all NA in the dataset for road categories, we replace with 0.
# Because there are no roads of a particular type intersecting with a given state tract, 
# the length will be calculated as NA, but this represents a 0 road length, 
# so we substitute the 0 for clarity.
#

final_int$num_intersections <- ifelse(is.na(final_int$num_intersections), 0, 
                                      final_int$num_intersections)


# %%%%%%%%%%%%%%%%%%% COMBINE ROADS AND INTERSECTIONS %%%%%%%%%%%%%%%%%%%%%%% #

# Note: since both the road and intersections contain spatial data, the spatial
# data must be dropped from one of the files. I will drop the intersection points,
# since the roads file contains the municipality outlines.
final_int_no_geom <- st_drop_geometry(final_int)

final <- final_roads %>% 
  left_join(final_int_no_geom) # by = "GID_2"


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXPORT DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# We will export the data in several formats that might be applicable for different
# end users - R users, non-R users, spatial data users, non-spatial data users.

# Export rds file
#
saveRDS(final, paste0(roads_country_dir, "Nationwide_OSM_Roads_Sum_Length_GADM.rds"))

# Export gpkg file
#
st_write(final, paste0(roads_country_dir, "Nationwide_OSM_Roads_Sum_Length_GADM.gpkg"))

# Remove geometry for tabular export
#
final_tab <- as.data.frame(final)
final_tab$geom <- NULL

# Export csv file
#
write.csv(final_tab, paste0(roads_country_dir, 
                            "Nationwide_OSM_Roads_Sum_Length_GADM.csv"),
          row.names=FALSE)


