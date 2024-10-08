# Road Length Sums using OpenStreetMap data
This tutorial describes how to download and process OpenStreetMap roads data to calculate road sums by type for a particular administrative level. While the example code will calculate road sums for every municipality in each of Mexico's 32 states, it should be applicable for any country, as long as administrative levels are downloaded from GADM.

# Project Overview
This project calculates the total road lengths by type (e.g., highways, local streets, unpaved tracks) for each Level 2 administrative division (e.g., municipalities) within Mexico’s 32 states using OpenStreetMap (OSM) data. The process can be easily adapted for other countries and administrative levels by modifying the inputs. Administrative boundaries are sourced from the Global Administrative Areas Database (GADM).

# Usage
To use this pipeline, you will need to download shapefiles for both roads and administrative boundaries. The road data will come from OpenStreetMap, and administrative boundaries from GADM. The steps involve:
1. Downloading and unzipping the OSM road data for the country of interest (ensure adequate disk space—about 6x the zipped file size).
2. Downloading GADM administrative boundaries at the appropriate level (Level 2 for municipalities in this example).
3. Using RStudio to calculate total road lengths by type for each administrative division.

# Data Sources:
- OpenStreetMap (OSM): Roads data can be downloaded from Geofabrik (see below).
- GADM (Global Administrative Areas Database): Administrative boundary shapefiles are available for almost every country.

# Workflow
1. Download OpenStreetMap data
[OpenStreetMap](https://www.openstreetmap.org/about) is a collaborative, open-source platform that provides maps and geographic data. It contains a wide range of types of geographic data, including buildings, roads, points of interest, and natural features, among others. It can be accessed from a number of sources. The historical data for the full planet is available, but is not recommended to download, as it is extremely large. A more convenient way to access OSM data is through [Geofabrik](https://download.geofabrik.de/), a consulting firm based in Germany that provides free files that are updated daily. No purchase or membership is required to access either source.
To download data for a particular region or country, click on a region name to see the available sub-regions. Highlighting the name will also highlight each region on the map. For example, Mexico is part of North America rather than Central America.
![Screenshot 2024-09-16 at 3 35 35 PM](https://github.com/user-attachments/assets/7769ecbb-6639-4d15-8f22-897525bd83b4)
Choose the shp.zip if it is available. It’s typically only available for smaller regions, such as individual countries. Click the link of the desired file format and it will directly download onto your local machine. Once it is downloaded, you can “decompress” or “unzip” the folder, which will open a folder with the contents of the compressed file. You should be aware that the unzipped folder will be larger than the zipped file. Ensure that you have roughly 6x the space available as the zipped file just to be safe. For example, the Mexico folder was 1.1GB when zipped and 3.3GB after unzipping. The folder will have a shapefile for each type of geographic data. For explanations and definitions of each map features, refer to the [OpenStreetMap wiki]([url](https://wiki.openstreetmap.org/wiki/Map_features)). The shapefile we would like to work with contains all road types: highways, local streets, unpaved tracks, and others. This file will be called "gis_osm_roads_free_1.shp" or something similar.

2. Download administrative boundaries
To calculate road sums and intersections for a particular area within a country, we need shapefiles containing the administrative boundaries of that country. The source of this data we will use in the tutorial is from GADM (Global Administrative Areas Database), which provides geographic boundaries and information on administrative subdivisions for almost every country (or special region, such as American Samoa) in the world. Level 0 files contain an outline of the country/region itself. Level 1 files contain boundaries for the first-level administrative divisions, usually states, provinces, or regions. For example, the United States and Mexico both have states. Level 2 files contain boundaries for the second-level subdivisions within the first-level divisions, such as counties, districts, or municipalities. The United States has counties, but Mexico has municipalities. GADM provides both the local-language name and the English name for these subdivisions. Some countries have Level 3 subdivisions, such as Estonia which has boundaries for every city, town, and village. Our tutorial will calculate the road length sums by type for each Level 2 subdivision in Mexico, but could be adapted for Level 1 or 3 divisions in another country. To download this data, [select the country of choice](https://gadm.org/download_country.html) from the drop-down menu and select the "Shapefile" option. The Level 2 file will be called "gadm41_{country 3-letter abbreviation}_2.shp".

3. Process data in RStudio
Open the provided scripts (1A_OSM_Road_Length.R and 2A_Join_Files.R) in RStudio. 1A_OSM_Road_Length.R, which calculates road lengths for each administrative area, can either be run locally or by bash scripting (1B_OSM_Road_Length.sh) to run the script for all administrative areas at once. 2A_Join_Files.R creates one nationwide file by binding the outputs from 1A_OSM_Road_Length.R together.

# Dependencies
- R with tidyverse, sf, and data.table packages

# Contact Information
For questions and further information, contact Allison James (alliej@bu.edu)
