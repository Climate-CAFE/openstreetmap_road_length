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
##          Process GADM data for a particular country and subset it for a
##          particular administrative area.


# Reading in packages
library(tidyverse)
library(sf)
library(data.table)

# Set directories where you want to input and output data
gadm_dir <- "/projectnb/gislane/allison/Mexico/GDAM/"
# directory where your administrative boundaries data is stored
roads_indir <- "/projectnb/gislane/allison/Mexico/mexico-latest-free/"
# directory where your roads data is stored
roads_outdir <- "/projectnb/gislane/allison/Mexico/roads_out/"
# directory where you want to output your data after aggregation

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
GID <- as.numeric(args[1]) # ID of the area
# Note: Make sure you use a numeric identifier

# If you are not using a bash script than you can use the line below.
#GID <- "MEX_1_1" # Example here for Aguascalientes state Geo ID code

# %%%%%%%%%%%%%%%%%%%% READ IN GADM DATA %%%%%%%%%%%%%%%%%%%%%%% #
# Read in cleaned data from Step 1.
# This data should have a NUMERIC column for the GID of interest.
#
filename_gadm <- "gadm_clean.gpkg" # Replace with the name of your GADM file
gadm_poly <- st_read(paste0(gadm_dir, filename_gadm))

# Replace periods in the ID column with underscores
gadm_poly$GID_1 <- gsub("\\.", "_", gadm_poly$GID_1)
gadm_poly$GID_2 <- gsub("\\.", "_", gadm_poly$GID_2)

# Select the data for the chosen state. 
# Make sure the subsequent number for the GID column name matches the level
# at which you are doing your analysis. In this example, analysis is done
# for each municipality (Level 2) in a particular state (Level 1), so the chosen
# column is GID_1_num rather than GID_2_num (municipality) or GID_0 (country).
# 
gadm_state <- gadm_poly %>% filter(
  GID_1_num == GID
)

# If you are not bash scripting, there is no need to subset by the numeric
# column. You can uncomment and use the code below instead:
# gadm_state <- gadm_poly %>% filter(
#   GID_1 == GID
# )

# %%%%%%%%%%%%%%%%%%%% READ IN OPENSTREETMAP DATA %%%%%%%%%%%%%%%%%%%%%%% #
# Read in OSM roads
# The file may take a minute or two to load, as it's a big file.
# There may be millions of observations, depending on the size of the region.
#
filename_osm <- "gis_osm_roads_free_1.shp" # Replace with your file's name
osm_roads <- st_read(paste0(roads_indir, filename_osm))


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
gadm_state <- gadm_state[c("GID_1", "GID_2", "geometry")]
osm_roads <- osm_roads[c("fclass", "geometry")]

# Apply the intersection between each municipality (level 2) and road
#
gadm_level2_full_osm <- st_intersection(gadm_state, osm_roads)


# The intersection results in points being generated at intersection, which is not
# relevant for length calculations. The code below will remove the point data
#
# gadm_tract_full_osm_nopt <- gadm_tract_full_osm[st_geometry_type(gadm_tract_full_osm$geometry) %in% c("LINESTRING", "MULTILINESTRING"),]

# Calculate length for  after cropping to polygon fit
#
gadm_level2_full_osm$len_m <- st_length(gadm_tract_full_osm$geometry) 

# Set as data.table
#
gadm_level2_full_osm_dt <- setDT(gadm_level2_full_osm)

# Summation of length measures by municipality and road type
#
gadm_level2_sum <- gadm_level2_full_osm[, .(length = (sum(len_m))), 
                                        by=.(GID_2, fclass)]

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
road_intersections <- st_intersection(st_as_sf(gadm_tract_full_osm))

# Filter out road segments that overlap and only keep points, which are 
# road intersections.
road_intersections <- road_intersections %>% 
  filter(st_geometry_type(.) == "POINT")

# Join the intersections back with the GID_2 roads
road_intersections_with_gid2 <- st_join(road_intersections, 
                                        st_as_sf(gadm_tract_full_osm))

# Identify observations with NA value for GID_2
na_gid2 <- road_intersections_with_gid2 %>% 
  filter(is.na(GID_2))

# Assign these observations to the nearest GID_2 polygon
nearest_gid2 <- st_nearest_feature(na_gid2, 
                                   st_as_sf(gadm_tract_full_osm))
nearest_gid2_values <- gadm_tract_full_osm$GID_2[nearest_gid2]

road_intersections_with_gid2$GID_2[is.na(road_intersections_with_gid2$GID_2)] <- nearest_gid2_values

# Count the number of intersections in each GID_2
intersection_count <- road_intersections_with_gid2 %>%
  group_by(GID_2) %>%
  summarise(num_intersections = n())

# Save points
#
saveRDS(intersection_count, paste0(roads_outdir, "osm_intersections_",
                                 GID, ".rds"))
