##Author: Allison James
##Contrib: Zach Popp
##Date Created: 09/03/2024
##Date Modified: 09/16/2024
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
##          Process GADM data for a particular country and subset it for a
##          particular administrative area.

# Reading in packages
#
library(tidyverse)
library(sf)
library(data.table)

# Set directories where you want to input and output data
#
gadm_dir <- "YOUR PATH FOR ADMINISTRATIVE BOUNDARY DATA"
roads_indir <- "YOUR PATH FOR ROADS DATA"
roads_outdir <- "YOUR PATH FOR OUTPUT ROADS"

# The following variables come from the command line when running as bash. Bash
# scripting is a method for expediting processing using a computing cluster.
# To use this code, you would need to separately write a bash script
# that specifies a series of indices which will be used to process multiple 
# states simultaneously.
#
# If you are only processing a single state (or other administrative area), 
# this is not necessary. The bash script language below can be removed, and you 
# can add a line to specify the state you would like to process,
# such as the example line below.
#
args <- commandArgs(trailingOnly = TRUE)
index <- as.numeric(args[1]) # index from 1 to length of unique geographies
# Note: Make sure you use a numeric identifier

# If you are not using a bash script than you can use the line below.
#GID <- "MEX_1_1" # Example here for Aguascalientes state Geo ID code

# %%%%%%%%%%%%%%%%%%%% READ IN GADM DATA %%%%%%%%%%%%%%%%%%%%%%% #
# Read in administrative boundary data
#
filename_gadm <- "gadm41_MEX_2.shp" # Replace with the name of your GADM file
gadm_poly <- st_read(paste0(gadm_dir, "/", filename_gadm))

# Replace periods in the ID column with underscores
#
gadm_poly$GID_1 <- gsub("\\.", "_", gadm_poly$GID_1)
gadm_poly$GID_2 <- gsub("\\.", "_", gadm_poly$GID_2)

# If you are not using a bash script than you can use the code below to subset
# your GADM file as needed, OR set gadm_state to be your full gadm file. Note
# that processing large areas with many roads will be computationally 
# intensive. If not using batch scripting, set your gadm_state file and then
# skip to the READ IN OPENSTREETMAP DATA section
#
# gadm_state <- gadm_poly[gadm_poly$GID_1 == "STATE OF INTEREST", ]
# gadm_state <- gadm_poly[gadm_poly$GID_1 == "MEX_1_1", ] # Example here for Aguascalientes state Geo ID code
#

# List all unique geographic identifiers. We want to isolate the file by a 
# region larger than the administrative boundary to which we will aggregate,
# as this will allow us to subset the processing
#
GID_1_list <- unique(gadm_poly$GID_1)     #integer; does not contain leading zeroes

# Use index input to subset to single "state" or "district" for reduction
# of processing time
#
GID_in <- GID_1_list[index]

# Select the data for the chosen state. 
# Make sure the subsequent number for the GID column name matches the level
# at which you are doing your analysis. In this example, analysis is done
# for each municipality (Level 2) in a particular state (Level 1), so the chosen
# column is GID_1 rather than GID_2 (municipality) or GID_0 (country).
# 
gadm_state <- gadm_poly[gadm_poly$GID_1 == GID_in, ]


# %%%%%%%%%%%%%%%%%%%% READ IN OPENSTREETMAP DATA %%%%%%%%%%%%%%%%%%%%%%% #
# Read in OSM roads
# The file may take a minute or two to load, as it's a big file.
# There may be millions of observations, depending on the size of the region.
#
filename_osm <- "gis_osm_roads_free_1.shp" # Replace with your file's name
osm_roads <- st_read(paste0(roads_indir, "/", filename_osm))

# The process below is conducted to determine the appropriate projected coordinate
# system for both the roads and census tract data. Projected coordinate systems
# allow faster spatial analysis This faster computation
# has the drawback of producing a distortion of the features which can, 
# in this case, lead to a lengthening or shortening of road lenghts. Distortion
# can be minimized by selecting a projection that is specific to a given region.
# Here, we use Universal Travel Mercator (UTM) zones to project the data. For 
# more information, see: https://www.geographyrealm.com/universal-transverse-mercator/
#
# First, we determine in which UTM zone the state is in.  The formula is based 
# on the post linked below, and added here by Dennis Milechin
# https://gis.stackexchange.com/questions/209267/r-return-the-utm-zone-that-a-wgs84-point-belongs-to
#

# Get the bounding box of the state
#
state_bbox <- st_bbox(gadm_state)

# Determine the center longitude
#
long_center <- (state_bbox$xmax + state_bbox$xmin)/2
lat_center <- (state_bbox$ymax + state_bbox$ymin)/2

# Determine the UTM Zone for the longitude center
#
utm_zone <- floor((long_center+180)/6)+1

# Extract the EPSG code
#
epsg_code <- ifelse(
  lat_center >= 0,
  utm_zone + 32600,
  utm_zone + 32700
)


# Transform both GIS data layers to UTM
#
gadm_state <- st_transform(gadm_state, epsg_code)
osm_roads <- st_transform(osm_roads, epsg_code)

# Subset column names
#
gadm_state <- gadm_state[c("GID_1", "GID_2", "geom")] # or "geography"
osm_roads <- osm_roads[c("fclass", "geometry")]

# Apply the intersection between each municipality (level 2) and road
# To speed up processing, we can turn off spherical geometry. This will
# affect the precision of the spatial intersection, but we can expect the 
# changes to be relatively minimal in comparing sum road length across 
# districts
#
sf_use_s2(FALSE)
gadm_level2_full_osm <- st_intersection(gadm_state, osm_roads)

# The intersection results in points being generated at intersection, which is not
# relevant for length calculations. The code below will remove the point data
#
# gadm_tract_full_osm_nopt <- gadm_tract_full_osm[st_geometry_type(gadm_tract_full_osm$geometry) %in% c("LINESTRING", "MULTILINESTRING"),]

# Calculate length for  after cropping to polygon fit
#
gadm_level2_full_osm$len_m <- st_length(gadm_level2_full_osm$geom)

# Set as data.table
#
gadm_level2_full_osm_dt <- setDT(gadm_level2_full_osm)

# Summation of length measures by municipality and road type
#
gadm_level2_sum <- gadm_level2_full_osm[, .(length = (sum(len_m))), 
                                        by=.(GID_2, fclass)]

#*******************************************************************************
#**************** ADD REFERENCE FOR ROAD CLASSES/OSM DATA DICTIONARY ***********
#*******************************************************************************
#*******************************************************************************
# Estimate of total length by class
#
rd_gadm_primary <- gadm_level2_sum[fclass %in% c("primary", "primary_link", 
                                                 "trunk", "trunk_link",
                                                 "motorway", "motorway_link"), 
                                   .(prim_len = sum(length)), by=.(GID_2)]
rd_gadm_secondary <- gadm_level2_sum[fclass %in% c("secondary", "secondary_link", 
                                                   "tertiary", "tertiary_link"), 
                                     .(sec_len = sum(length)), by=.(GID_2)]
rd_gadm_local <- gadm_level2_sum[fclass %in% c("unclassified", "residential", 
                                               "living_street"), 
                                 .(local_len = sum(length)), by=.(GID_2)]
rd_gadm_track <- gadm_level2_sum[fclass %in% c("track", "track_grade2", 
                                               "track_grade3", "track_grade4",
                                               "track_grade5"), 
                                 .(track_len = sum(length)), by=.(GID_2)]

#define other- cycleway, footway, path, pedestrian, service, steps
rd_gadm_other <- gadm_level2_sum[fclass %in% c("cycleway", "footway", "steps",
                                               "path", "pedestrian", "service"), 
                                 .(other_len = sum(length)), by=.(GID_2)]

gadm_tract_total <- gadm_level2_sum[, .(total_len = sum(length)), by = GID_2]

# Rejoin class level lengths to municipalities
#
level1_rd_length <- left_join(gadm_state, rd_gadm_primary, by = "GID_2")
level1_rd_length <- left_join(level1_rd_length, rd_gadm_secondary, by = "GID_2")
level1_rd_length <- left_join(level1_rd_length, rd_gadm_local, by = "GID_2")
level1_rd_length <- left_join(level1_rd_length, rd_gadm_track, by = "GID_2")
level1_rd_length <- left_join(level1_rd_length, rd_gadm_other, by = "GID_2")
level1_rd_length <- left_join(level1_rd_length, gadm_tract_total, by = "GID_2")

# Save joined dataset
#
saveRDS(level1_rd_length, paste0(roads_outdir, "osm_road_len_by_fclass_",
                                 GID, ".rds"))

# %%%%%%%%%%%%%%%%%% CALCULATE NUMBER OF INTERSECTIONS %%%%%%%%%%%%%%%%%%%%%%% #

# Apply intersection to road segments
road_intersections <- st_intersection(st_as_sf(gadm_level2_full_osm))

# Filter out road segments that overlap and only keep points, which are 
# road intersections.
road_intersections <- road_intersections %>% 
  filter(st_geometry_type(.) == "POINT")

# # Join the intersections back with the GID_2 roads
# road_intersections_with_gid2 <- st_join(road_intersections, 
#                                         st_as_sf(gadm_level2_full_osm))

# Identify observations with NA value for GID_2, if they exist
# na_gid2 <- road_intersections %>% 
#   filter(is.na(GID_2))
# 
# # Assign these observations to the nearest GID_2 polygon
# nearest_gid2 <- st_nearest_feature(na_gid2, 
#                                    st_as_sf(gadm_level2_full_osm))
# nearest_gid2_values <- gadm_level2_full_osm$GID_2[nearest_gid2]
# 
# road_intersections$GID_2[is.na(road_intersections$GID_2)] <- nearest_gid2_values



# Count the number of intersections in each GID_2
intersection_count <- road_intersections %>%
  group_by(GID_2) %>%
  summarise(num_intersections = n())

# Save points
#
saveRDS(intersection_count, paste0(roads_outdir, "osm_intersections_",
                                   GID, ".rds"))
