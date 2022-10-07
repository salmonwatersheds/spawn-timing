###############################################################################
# Code to extract spawn timing data from NuSEDS and merge with info on CU
# and PSE region for analysis.
#
# Created by Steph Peacock, Analyst, Salmon Watersheds Program
# Contact: speacock at psf dot ca
# Date: October 7, 2022
###############################################################################

library(dplyr)

###############################################################################
# Read-in NuSEDS data
###############################################################################

# Spawn timing information is not included in Conservation_Unit_Data_20220902.csv
# so must read in original dataset
nuseds <- read.csv("data/NuSEDS_20220902.csv") %>% select("AREA", "WATERBODY", "GAZETTED_NAME", "LOCAL_NAME_1", "POPULATION","RUN_TYPE", "SPECIES", "ANALYSIS_YR", "START_DTT", "END_DTT", "ESTIMATE_CLASSIFICATION", "ESTIMATE_METHOD", "STREAM_ARRIVAL_DT_FROM", "STREAM_ARRIVAL_DT_TO", "START_SPAWN_DT_FROM", "START_SPAWN_DT_TO", "PEAK_SPAWN_DT_FROM", "PEAK_SPAWN_DT_TO", "END_SPAWN_DT_FROM", "END_SPAWN_DT_TO", "ADULT_PRESENCE", "JACK_PRESENCE", "MAX_ESTIMATE", "POP_ID") 


# Merge with Conservation Unit System Sites
cu <- read.csv("data/conservation_unit_system_sites.csv") %>% select("SYSTEM_SITE", "SPECIES_QUALIFIED",  "Y_LAT", "X_LONGT", "CU_NAME", "CU_ACRO", "CU_TYPE", "CU_INDEX",  "FULL_CU_IN", "POP_ID", "IS_INDICATOR")

nuseds_cu <- left_join(nuseds, cu, by = "POP_ID") 

# Which entries have some spawn timing info?
has_info <- apply(nuseds_cu[, c("STREAM_ARRIVAL_DT_FROM", "STREAM_ARRIVAL_DT_TO", "START_SPAWN_DT_FROM", "START_SPAWN_DT_TO", "PEAK_SPAWN_DT_FROM", "PEAK_SPAWN_DT_TO", "END_SPAWN_DT_FROM", "END_SPAWN_DT_TO")] != "", 1, sum) > 0

nuseds_cu <- nuseds_cu[has_info == TRUE, ]

###############################################################################
# How representative are the data?
###############################################################################

# Read in PSF CU list
psf_cu <- read.csv("data/CU_list_by_region.csv")

write.csv(sort(unique(psf_cu$CUIndex)), "output/CU_xref.csv", row.names = FALSE)
write.csv(sort(unique(cu$FULL_CU_IN)), "output/CU_xref2.csv", row.names = FALSE)

# Proportion of CUs with data for each piece 