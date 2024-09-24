##Author: Allison James
##Contrib: Zach Popp
##Date Created: 09/03/2024
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
##          For a bash script to run on each administrative area, we need to
##          make a numeric column for the administrative area name.


# Reading in packages
#
library(tidyverse)
library(sf)
library(data.table)

# Set directories where you want to input and output data
#
gadm_dir <- "YOUR LOCAL PATH"

# %%%%%%%%%%%%%%%%%%%% READ IN GADM DATA %%%%%%%%%%%%%%%%%%%%%%% #
# Read in data for GADM Level 2.
# Level 0 is just an outline of the country. 
# Level 1 is the largest administrative division (eg state).
# For example, the 31 states of Mexico + Mexico city comprise Level 1 for Mexico.
# Level 2 are the next sublevel of administrative divisions (eg municipality).
# The ENGTYPE_2 column details the (English) names of Level 2-
# not every country's administrative divisions will be named the same.
# The USA has counties rather than municipalities, for example.
#
filename <- "gadm41_MEX_2.shp" # Replace with the name of your GADM file
gadm_raw_data <- st_read(paste0(gadm_dir, "/", filename))

# Replace periods in the ID column with underscores
#
gadm_raw_data$GID_1 <- gsub("\\.", "_", gadm_raw_data$GID_1)
gadm_raw_data$GID_2 <- gsub("\\.", "_", gadm_raw_data$GID_2)

# Assign a number to both GID_1 and GID_2. Note that these numbers likely will
# not match those found in the GID's, but we just need a numeric identifier if
# we want to run all areas at once using bash scripting.
#
gadm_raw_data$GID_1_num <- as.integer(factor(gadm_raw_data$GID_1))
gadm_raw_data$GID_2_num <- as.integer(factor(gadm_raw_data$GID_2))

gadm_cleaned_data <- gadm_raw_data[,c("GID_0", "GID_1", "GID_1_num",  "GID_2",
                                      "GID_2_num", "NAME_1", "NAME_2", 
                                      "ENGTYPE_2")]

# Save the updated data.
# Note: make sure it's in a spatial data format (.shp, .gpkg, .rds)
#
st_write(gadm_cleaned_data, paste0(gadm_dir, "/", "gadm_clean.gpkg"))

