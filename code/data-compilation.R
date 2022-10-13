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
nuseds <- read.csv("data/NuSEDS_20220902.csv") %>% select("ACT_ID", "AREA", "WATERBODY", "GAZETTED_NAME", "LOCAL_NAME_1", "POPULATION","RUN_TYPE", "SPECIES", "ANALYSIS_YR", "START_DTT", "END_DTT", "ESTIMATE_CLASSIFICATION", "ESTIMATE_METHOD", "STREAM_ARRIVAL_DT_FROM", "STREAM_ARRIVAL_DT_TO", "START_SPAWN_DT_FROM", "START_SPAWN_DT_TO", "PEAK_SPAWN_DT_FROM", "PEAK_SPAWN_DT_TO", "END_SPAWN_DT_FROM", "END_SPAWN_DT_TO", "ADULT_PRESENCE", "JACK_PRESENCE", "MAX_ESTIMATE", "POP_ID") 


# Merge with Conservation Unit System Sites
cu <- read.csv("data/conservation_unit_system_sites.csv") %>% select("SYSTEM_SITE", "SPECIES_QUALIFIED",  "Y_LAT", "X_LONGT", "CU_NAME", "CU_ACRO", "CU_TYPE", "CU_INDEX",  "FULL_CU_IN", "POP_ID", "IS_INDICATOR")


###############################################################################
# Match PSE regions
###############################################################################

# Read in PSF CU list
psf_cu <-read.csv("data/all_regions_cu_du_smu_decoder.csv")#
regions <- read.csv("data/CU_list_by_region.csv")

#------------------------------------------------------------------------------
# Aside: Matching CUs to PSF's database
#------------------------------------------------------------------------------
# CU's in PSF but not NuSEDS
sort(unique(psf_cu$FULL_CU_IN))[sort(unique(psf_cu$FULL_CU_IN)) %in% sort(unique(cu$FULL_CU_IN)) == FALSE]

# CUs in NuSEDS but not PSF
sort(unique(cu$FULL_CU_IN))[sort(unique(cu$FULL_CU_IN)) %in% sort(unique(psf_cu$FULL_CU_IN)) == FALSE]

# Correct what ones I can
cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKE-5")] <- "PKE-05"
cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKE-6")] <- "PKE-06"
cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKE-7")] <- "PKE-07"
psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "PKE-7")] <- "PKE-07"
cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKE-8")] <- "PKE-08"
cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKE-9")] <- "PKE-09"

cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKO-8")] <- "PKO-08"
cu$FULL_CU_IN[which(cu$FULL_CU_IN == "PKO-9")] <- "PKO-09"

psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "SER-010")] <- "SER-10"
psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "SER-011")] <- "SER-11"
psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "SER-022")] <- "SER-22"
psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "SER-023")] <- "SER-23"
psf_cu$FULL_CU_IN[which(psf_cu$FULL_CU_IN == "SER-024")] <- "SER-24"


# Chinook ones extinct?
unique(cu$CU_NAME[cu$FULL_CU_IN %in% c("CK-9002", "CK-9004", "CK-9005", "CK-9006", "CK-9007", "CK-9008")])

psf_cu$cuname[psf_cu$FULL_CU_IN %in% c("CK-30", "CK-59", "CK-62", "CK-66", "CK-70", "CK-77", "CK-79")] # Proabbly have no run timing info. Ok.


# Coho


#------------------------------------------------------------------------------
# Adding PSE region to nuseds data
#------------------------------------------------------------------------------
nuseds_cu <- left_join(nuseds, cu, by = "POP_ID") 

# Which entries have some spawn timing info?
varNames <- c("STREAM_ARRIVAL_DT_FROM", "STREAM_ARRIVAL_DT_TO", "START_SPAWN_DT_FROM", "START_SPAWN_DT_TO", "PEAK_SPAWN_DT_FROM", "PEAK_SPAWN_DT_TO", "END_SPAWN_DT_FROM", "END_SPAWN_DT_TO")

nuseds_cu$REGION <- regions$Region[match(nuseds_cu$FULL_CU_IN, psf_cu$FULL_CU_IN[match(regions$CUID, psf_cu$cuid)])]

nuseds_cu[is.na(nuseds_cu$REGION), ]

# Replace "" with NA
nuseds_cu[which(nuseds_cu == "", arr.ind = TRUE)] <- NA

# Fill in missing regions
unique(nuseds_cu$CU_NAME[is.na(nuseds_cu$REGION)])

nuseds_cu$REGION[nuseds_cu$CU_NAME %in% c("INTERIOR FRASER<<BIN>>", "FRASER RIVER<<BIN>>","FRASER RIVER MIGRATORY COUNTS<<BIN>>", "FRASER-CROSS-CU SUPPLEMENTATION EXCLUSION<<BIN>>", "FRASER-EARLY SUMMER TIMING", "MIDDLE FRASER", "UPPER FRASER", "FRANCOIS-LATE TIMING", "CARIBOO-SUMMER TIMING", "FRASER-HARRISON FALL TRANSPLANT_FA_0.3<<BIN>>", "ALOUETTE_EARLY SUMMER<<EXTIRPATED>>", "FRASER-MISCELLANEOUS<<BIN>>", "FRANCOIS-EARLY SUMMER TIMING")] <- "Fraser"

nuseds_cu$REGION[nuseds_cu$CU_NAME %in% c("OSOYOOS")] <- "Columbia"

nuseds_cu$REGION[nuseds_cu$CU_NAME %in% c("OWIKENO-LATE TIMING")] <- "Central Coast"

nuseds_cu$REGION[nuseds_cu$CU_NAME %in% c("BABINE")] <- "Skeena"

#------------------------------------------------------------------------------
# Change spawn timing dates into DOY
#------------------------------------------------------------------------------

for(j in 1:length(varNames)){
  nuseds_cu[, varNames[j]] <- as.numeric(nuseds_cu[, varNames[j]] %>% as.Date(format="%d-%b-%y") %>% format("%j"))
 }

#------------------------------------------------------------------------------
# QA/QC
#------------------------------------------------------------------------------

# Check that from < to for all variables

range(nuseds_cu$STREAM_ARRIVAL_DT_TO - nuseds_cu$STREAM_ARRIVAL_DT_FROM, na.rm = TRUE)
nuseds_cu[which(nuseds_cu$STREAM_ARRIVAL_DT_TO < nuseds_cu$STREAM_ARRIVAL_DT_FROM), c("ACT_ID", 'STREAM_ARRIVAL_DT_FROM', "STREAM_ARRIVAL_DT_TO")]

# Remove data that don't make sense
nuseds_cu[nuseds_cu$ACT_ID == 2027391231, 'STREAM_ARRIVAL_DT_FROM'] <- NA
nuseds_cu[nuseds_cu$ACT_ID == 48829881, "STREAM_ARRIVAL_DT_FROM"] <- NA

# Swap start and from (1 day apart)
nuseds_cu[nuseds_cu$ACT_ID == 1002102506, 'STREAM_ARRIVAL_DT_FROM'] <- 353
nuseds_cu[nuseds_cu$ACT_ID == 1002102506, 'STREAM_ARRIVAL_DT_TO'] <- 354

#------------
nuseds_cu[which(nuseds_cu$START_SPAWN_DT_TO < nuseds_cu$START_SPAWN_DT_FROM), c("ACT_ID", 'START_SPAWN_DT_FROM', "START_SPAWN_DT_TO")]
nuseds_cu[nuseds_cu$ACT_ID == 1453232816, "START_SPAWN_DT_TO"] <- NA # 51, Doesn't make sense

#------------
nuseds_cu[which(nuseds_cu$PEAK_SPAWN_DT_TO < nuseds_cu$PEAK_SPAWN_DT_FROM), c("ACT_ID", 'PEAK_SPAWN_DT_FROM', "PEAK_SPAWN_DT_TO")]

nuseds_cu[nuseds_cu$ACT_ID == 83631370, "PEAK_SPAWN_DT_TO"] <- 291 # Swap to and from
nuseds_cu[nuseds_cu$ACT_ID == 83631370, "PEAK_SPAWN_DT_FROM"] <- 288 # Swap to and from

nuseds_cu[nuseds_cu$ACT_ID == 1734648730, "PEAK_SPAWN_DT_TO"] <- 326 # Swap to and from
nuseds_cu[nuseds_cu$ACT_ID == 1734648730, "PEAK_SPAWN_DT_FROM"] <- 309 # Swap to and from

nuseds_cu[nuseds_cu$ACT_ID == 94527917, "PEAK_SPAWN_DT_TO"] <- 354 # Swap to and from
nuseds_cu[nuseds_cu$ACT_ID == 94527917, "PEAK_SPAWN_DT_FROM"] <- 306 # Swap to and from

nuseds_cu[nuseds_cu$ACT_ID == 68420541, "PEAK_SPAWN_DT_FROM"] <- NA # 364 doesnt make sense

#------------
nuseds_cu[which(nuseds_cu$END_SPAWN_DT_TO < nuseds_cu$END_SPAWN_DT_FROM), c("ACT_ID", 'END_SPAWN_DT_FROM', "END_SPAWN_DT_TO")] # All make sense (end of the year)

#------------------------------------------------------------------------------
# Write output data file
#------------------------------------------------------------------------------
has.info <- apply(!is.na(nuseds_cu[, varNames]), 1, sum)
write.csv(nuseds_cu[which(has.info > 0), ], "output/NuSEDS-spawn-timing.csv", row.names = FALSE)

###############################################################################
# Summarize available data by region
###############################################################################

regionNames <- sort(unique(nuseds_cu$REGION))
colRegions <- PNWColors::pnw_palette("Bay", n = length(regionNames))

data_availability <- array(NA, dim = c(length(regionNames), length(varNames)), dimnames = list(regionNames, varNames))

# Proportion of CUs in each region that have data
has.dat <- function(x) sum(!is.na(x))/length(x)

for(i in 1:length(regionNames)){
  nuseds_cu.i <- nuseds_cu[which(nuseds_cu$REGION == regionNames[i]), ]
  cu.i <- unique(nuseds_cu.i$FULL_CU_IN)
  for(j in 1:length(varNames)){
    dum <- tapply(nuseds_cu.i[, varNames[j]], nuseds_cu.i$FULL_CU_IN, has.dat)
    data_availability[i, j] <- length(which(dum == 0))/length(dum)
  }
}

quartz(width = 10, height = 4, pointsize = 10)
par(mfcol = c(2,4), mar = c(1,2,2,0), oma = c(6,1,1,0))
for(j in 1:length(varNames)){
  if((j %% 2) != 0){
    barplot(data_availability[, j], ylim = c(0,1), col = colRegions, gap = 0, border = NA, las = 2, main = varNames[j], names.arg = NA)
  } else {
    barplot(data_availability[, j], ylim = c(0,1), col = colRegions, gap = 0, border = NA, las = 2, main = varNames[j])
  }
}

# Summarize timing by region/species
speciesNames <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")

s <- 1
quartz(width = 10, height = 4, pointsize = 10)
par(mfcol = c(1,4), mar = c(1,2,2,0), oma = c(6,1,1,0))
for(i in 1:length(regionNames)){
  ind <- which(nuseds_cu$REGION == regionNames[i] & nuseds_cu$SPECIES == speciesNames[s])
    
    # Arrival date
    hist(c(nuseds_cu$STREAM_ARRIVAL_DT_FROM[ind], nuseds_cu$STREAM_ARRIVAL_DT_TO[ind]), xlim = range(nuseds_cu$STREAM_ARRIVAL_DT_FROM, nuseds_cu$STREAM_ARRIVAL_DT_TO, na.rm = TRUE), col = NA, border = NA, breaks = seq(0, 366, 7))
    hist(nuseds_cu$STREAM_ARRIVAL_DT_FROM[ind], add = TRUE, col = paste0(colRegions[i], 50), breaks = seq(0, 366, 7))
    hist(nuseds_cu$STREAM_ARRIVAL_DT_TO[ind], add = TRUE, col = colRegions[i], breaks = seq(0, 366, 7))
    
    # Start spawm date
    hist(c(nuseds_cu$START_SPAWN_DT_FROM[ind], nuseds_cu$START_SPAWN_DT_TO[ind]), xlim = range(nuseds_cu$START_SPAWN_DT_FROM, nuseds_cu$START_SPAWN_DT_TO, na.rm = TRUE), col = NA, border = NA, breaks = seq(0, 366, 7))
    hist(nuseds_cu$START_SPAWN_DT_FROM[ind], add = TRUE, col = paste0(colRegions[i], 50), breaks = seq(0, 366, 7))
    hist(nuseds_cu$START_SPAWN_DT_TO[ind], add = TRUE, col = colRegions[i], breaks = seq(0, 366, 7))
    
    # Peak spawm date
    hist(c(nuseds_cu$PEAK_SPAWN_DT_FROM[ind], nuseds_cu$PEAK_SPAWN_DT_TO[ind]), xlim = range(nuseds_cu$PEAK_SPAWN_DT_FROM, nuseds_cu$PEAK_SPAWN_DT_TO, na.rm = TRUE), col = NA, border = NA, breaks = seq(0, 366, 7))
    hist(nuseds_cu$PEAK_SPAWN_DT_FROM[ind], add = TRUE, col = paste0(colRegions[i], 50), breaks = seq(0, 366, 7))
    hist(nuseds_cu$PEAK_SPAWN_DT_TO[ind], add = TRUE, col = colRegions[i], breaks = seq(0, 366, 7))
    
    # End spawm date
    hist(c(nuseds_cu$END_SPAWN_DT_FROM[ind], nuseds_cu$END_SPAWN_DT_TO[ind]), xlim = range(nuseds_cu$END_SPAWN_DT_FROM, nuseds_cu$END_SPAWN_DT_TO, na.rm = TRUE), col = NA, border = NA, breaks = seq(0, 366, 7))
    hist(nuseds_cu$END_SPAWN_DT_FROM[ind], add = TRUE, col = paste0(colRegions[i], 50), breaks = seq(0, 366, 7))
    hist(nuseds_cu$END_SPAWN_DT_TO[ind], add = TRUE, col = colRegions[i], breaks = seq(0, 366, 7))
    
    
}
