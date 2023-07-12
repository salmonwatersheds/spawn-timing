###############################################################################
# Code to read in NuSEDS Conservation Unit System sites
# and match to PSE regions, and add elevation and distance to ocean.
# Created by Steph Peacock, Analyst, Salmon Watersheds Program
# Contact: speacock at psf dot ca
# Date: Feb 16, 2023
###############################################################################

library(dplyr)
library(httr)
library(jsonlite)

###############################################################################
# Read-in NuSEDS data
###############################################################################

# Read in Conservation Unit System Sites
cu_url <- "https://open.canada.ca/data/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6/resource/fc475853-b599-4e68-8d80-f49c03ddc01c/download/conservation_unit_system_sites.csv"

dat <- read.csv(cu_url) %>% select("SYSTEM_SITE", "SPECIES_QUALIFIED",  "Y_LAT", "X_LONGT", "CU_NAME", "CU_ACRO", "CU_TYPE", "CU_INDEX",  "FULL_CU_IN", "POP_ID", "IS_INDICATOR")

###############################################################################
# Extract unique locations
###############################################################################

dat$latlon <- paste0("lat=", dat$Y_LAT, "&lon=", dat$X_LONGT)
latlon <- sort(unique(dat$latlon)) # 2312 unique locations

points_dat <- data.frame(
  lat = dat$Y_LAT[match(latlon, dat$latlon)],
  lon = dat$X_LONGT[match(latlon, dat$latlon)],
  lat1 = NA, 
  lon1 = NA,
  elevation = NA,
  distance_to_ocean = NA
)

###############################################################################
# Create lookup file linking points_dat to populations and CUs
###############################################################################

dat$csv_id <- match(dat$latlon, latlon)

# Add PSF's cuid
pse_cu <- read.csv("/Users/stephaniepeacock/Salmon Watersheds Dropbox/Stephanie Peacock/X Drive/1_PROJECTS/Population Methods and Analysis/decoders/tables/all_regions_cu_du_smu_decoder.csv")

dat$cuid <- pse_cu$cuid[match(dat$FULL_CU_IN, pse_cu$FULL_CU_IN)]

cus <- unique(dat$FULL_CU_IN[which(dat$CU_TYPE == "Current" & is.na(dat$cuid))])

dat[match(cus, dat$FULL_CU_IN), c("CU_NAME", "CU_ACRO", "CU_TYPE", "CU_INDEX", "FULL_CU_IN")]

write.csv(dat, file = "output/csv_id_decoder.csv", row.names = FALSE)
###############################################################################
# Get elevation from Canadian Digital Elevation Model
# https://natural-resources.canada.ca/science-and-data/science-and-research/earth-sciences/geography/topographic-information/web-services/elevation-api/17328
###############################################################################

url <- "http://geogratis.gc.ca/services/elevation/cdem/altitude" # ?lat=45.5&lon=-71.5

# Fix two poionts on Taku and Alsek which are JUST on US side of border and therefore
# come back as NULL altitude
latlon[2242] <- "lat=58.580573&lon=-133.652400"
latlon[2251] <- "lat=59.444&lon=-137.99"

for(i in 1875:length(latlon)){
  if(grepl("NA", latlon[i]) == FALSE){
    el <- GET(paste0(url, "?", latlon[i]))
    dum <- fromJSON(rawToChar(el$content))
    if(length(dum$altitude)){
      points_dat$elevation[i] <- dum$altitude
      points_dat$lat1[i] <- dum$geometry$coordinates[2]
      points_dat$lon1[i] <- dum$geometry$coordinates[1]
    }
  }}



write.csv(points_dat[, c("lat", "lon", "elevation", "distance_to_ocean")], file = "output/NuSEDS_spawn_timing_points.csv")

# ###############################################################################
# # Match PSE regions
# ###############################################################################
# # Note: decoder GitHub repo not yet up and running so use local version in Dropbox
# 
# # Read in PSF CU list
# # decoder_url <- "https://raw.githubusercontent.com/salmonwatersheds/decoders/main/all_regions_cu_du_smu_decoder.csv"
# decoder_url <- "~/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/Population\ Methods\ and\ Analysis/decoders/tables/all_regions_cu_du_smu_decoder.csv"
# psf_cu <- read.csv(decoder_url) %>% select("Species", "cuid", "cuname", "cu_acronym", "trtc_cu", "FULL_CU_IN", ,"Conservation Unit", "duid", "du_number", "DU_name", "du_acronym",  "gen_length", "COSEWIC_status", "Area", "SMU")
# 
#                                           
# # region_url <- "https://raw.githubusercontent.com/salmonwatersheds/decoders/main/CU_list_by_region.csv"
# region_url <- "~/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/Population\ Methods\ and\ Analysis/decoders/tables/CU_list_by_region.csv"
# regions <- read.csv(region_url)
# 
# #------------------------------------------------------------------------------
# # Aside: Matching CUs to PSF's database
# #------------------------------------------------------------------------------
# # CU's in PSF but not NuSEDS
# sort(unique(psf_cu$FULL_CU_IN))[sort(unique(psf_cu$FULL_CU_IN)) %in% sort(unique(dat$FULL_CU_IN)) == FALSE]
# 
# # CUs in NuSEDS but not PSF
# sort(unique(dat$FULL_CU_IN))[sort(unique(dat$FULL_CU_IN)) %in% sort(unique(psf_cu$FULL_CU_IN)) == FALSE]
# 
# # Correct what ones I can
# # **Question for Eric: Does it make sense to add a "FULL_CU_IN_NuSEDS" column to the decoder?
# cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKE-5")] <- "PKE-05"
# cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKE-6")] <- "PKE-06"
# cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKE-7")] <- "PKE-07"
# psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "PKE-7")] <- "PKE-07"
# cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKE-8")] <- "PKE-08"
# cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKE-9")] <- "PKE-09"
# 
# cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKO-8")] <- "PKO-08"
# cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKO-9")] <- "PKO-09"
# 
# psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "SER-010")] <- "SER-10"
# psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "SER-011")] <- "SER-11"
# psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "SER-022")] <- "SER-22"
# psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "SER-023")] <- "SER-23"
# psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "SER-024")] <- "SER-24"
# 
# 
# # Chinook ones extinct?
# unique(cu$CU_NAME[cu$FULL_CU_IN %in% c("CK-9002", "CK-9004", "CK-9005", "CK-9006", "CK-9007", "CK-9008")])
# 
# psf_cu$cuname[psf_cu$FULL_CU_IN %in% c("CK-30", "CK-59", "CK-62", "CK-66", "CK-70", "CK-77", "CK-79")] # Probably have no run timing info. Ok.
# 
# 
# # Coho
