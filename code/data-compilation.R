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

# Spawn timing information is not included in Conservation_Unit_Data
# so must read in original dataset
nuseds <- read.csv("data/all_areas_nuseds_20230404.csv") %>% select("ACT_ID", "AREA", "WATERBODY", "GAZETTED_NAME", "LOCAL_NAME_1", "POPULATION","RUN_TYPE", "SPECIES", "ANALYSIS_YR", "START_DTT", "END_DTT", "ESTIMATE_CLASSIFICATION", "ESTIMATE_METHOD", "STREAM_ARRIVAL_DT_FROM", "STREAM_ARRIVAL_DT_TO", "START_SPAWN_DT_FROM", "START_SPAWN_DT_TO", "PEAK_SPAWN_DT_FROM", "PEAK_SPAWN_DT_TO", "END_SPAWN_DT_FROM", "END_SPAWN_DT_TO", "ADULT_PRESENCE", "JACK_PRESENCE", "TOTAL_RETURN_TO_RIVER", "POP_ID") 
# If using NuSEDS datasets prior to 2023, need to specify "MAX_ESTIMATE" instead of "TOTAL_RETURN_TO_RIVER"

# # Merge with Conservation Unit System Sites
# cu_url <- "https://open.canada.ca/data/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6/resource/fc475853-b599-4e68-8d80-f49c03ddc01c/download/conservation_unit_system_sites.csv"
# 
# cu <- read.csv(cu_url) %>% select("SYSTEM_SITE", "SPECIES_QUALIFIED",  "Y_LAT", "X_LONGT", "CU_NAME", "CU_ACRO", "CU_TYPE", "CU_INDEX",  "FULL_CU_IN", "POP_ID", "IS_INDICATOR")

# Correct Bella Coola chum pop'ns assigned to wrong CU (pers. comm. Carrie Holt, May 2023)
cu <- read.csv("data/conserv_unit_system_sites_mv.csv") %>% select("SYSTEM_SITE", "SPECIES_QUALIFIED",  "Y_LAT", "X_LONGT", "CU_NAME", "CU_ACRO", "CU_TYPE", "CU_INDEX",  "FULL_CU_IN", "POP_ID", "IS_INDICATOR")

# ###############################################################################
# # Match PSE regions
# ###############################################################################
# Note: decoder GitHub repo not yet up and running so use local version in Dropbox

# Read in PSF CU list
# decoder_url <- "https://raw.githubusercontent.com/salmonwatersheds/decoders/main/all_regions_cu_du_smu_decoder.csv"
decoder_url <- "~/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/Population\ Methods\ and\ Analysis/decoders/tables/all_regions_cu_du_smu_decoder.csv"
psf_cu <- read.csv(decoder_url) %>% select("spp", "cuid", "cuname", "cu_acronym", "FULL_CU_IN", "gen_length", "COSEWIC_status", "Area", "SMU")

# Trim out duplicate CUs from CU-DU matching
cuid <- sort(unique(psf_cu$cuid))
psf_cu <- psf_cu[match(cuid, psf_cu$cuid), ]

# region_url <- "https://raw.githubusercontent.com/salmonwatersheds/decoders/main/CU_list_by_region.csv"
region_url <- "~/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/Population\ Methods\ and\ Analysis/decoders/tables/CU_list_by_region.csv"
regions <- read.csv(region_url)

#------------------------------------------------------------------------------
# Aside: Matching CUs to PSF's database
#------------------------------------------------------------------------------
# CU's in PSF but not NuSEDS
sort(unique(psf_cu$FULL_CU_IN))[sort(unique(psf_cu$FULL_CU_IN)) %in% sort(unique(cu$FULL_CU_IN)) == FALSE]

# CUs in NuSEDS but not PSF
sort(unique(cu$FULL_CU_IN))[sort(unique(cu$FULL_CU_IN)) %in% sort(unique(psf_cu$FULL_CU_IN)) == FALSE]

# Match cuid to NuSEDS cu data
cu$cuid <- psf_cu$cuid[match(cu$FULL_CU_IN, psf_cu$FULL_CU_IN)]

cu$FULL_CUNAME <- paste(cu$SPECIES_QUALIFIED, cu$CU_NAME, sep = "::")
sum(is.na(cu$cuid))
unique(cu$FULL_CUNAME[is.na(cu$cuid)])

regions[grep("Alsek", regions$CULabel..PSE.Display.Name.), ]

cu$cuid[cu$FULL_CUNAME == "SEL::OSOYOOS"] <- 1300
cu$cuid[cu$FULL_CUNAME == "SEL::BABINE" ] <- 180
cu$cuid[cu$FULL_CUNAME == "SEL::TAHLO/MORRISON"] <- 189
cu$cuid[cu$FULL_CUNAME == "SEL::CLEMENTS"] <- 418
cu$cuid[cu$FULL_CUNAME == "SEL::BOWSER"] <- 420
cu$cuid[cu$FULL_CUNAME == "SEL::OWEEGEE"] <- 425
cu$cuid[cu$FULL_CUNAME == "SER::SOUTHERN FJORDS"] <- 971
cu$cuid[cu$FULL_CUNAME == "SER::WEST VANCOUVER ISLAND"] <- 972
cu$cuid[cu$FULL_CUNAME == "SER::NW VANCOUVER ISLAND"] <- 974
cu$cuid[cu$FULL_CUNAME == "SER::NORTHERN TRANSBOUNDARY FJORDS"] <- 1023
cu$cuid[cu$FULL_CUNAME == "SER::ALSEK RIVER"] <- 1021

#------------------------------------------------------------------------------
# Adding PSE region to nuseds data
#------------------------------------------------------------------------------
nuseds_cu <- left_join(nuseds, cu, by = "POP_ID")

# Need to remove duplicate pink salmon observations
nuseds_cu$POP_ID[which(nuseds_cu$SPECIES_QUALIFIED == "PKO" & nuseds_cu$ANALYSIS_YR %in% seq(1920, 2021, 2))] <- NA
nuseds_cu$POP_ID[which(nuseds_cu$SPECIES_QUALIFIED == "PKE" & nuseds_cu$ANALYSIS_YR %in% seq(1921, 2021, 2))] <- NA
nuseds_cu <- nuseds_cu[!is.na(nuseds_cu$POP_ID), ]

# Which entries have some timing info?
varNames <- c("START_DTT", "END_DTT", "STREAM_ARRIVAL_DT_FROM", "STREAM_ARRIVAL_DT_TO", "START_SPAWN_DT_FROM", "START_SPAWN_DT_TO", "PEAK_SPAWN_DT_FROM", "PEAK_SPAWN_DT_TO", "END_SPAWN_DT_FROM", "END_SPAWN_DT_TO")

nuseds_cu$REGION <- regions$Region[match(nuseds_cu$cuid, regions$cuid)]

# Replace "" with NA
nuseds_cu[which(nuseds_cu == "", arr.ind = TRUE)] <- NA

# Fill in missing regions
cus_no_region <- unique(nuseds_cu$CU_NAME[is.na(nuseds_cu$REGION)])

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("INTERIOR FRASER<<BIN>>", "FRASER RIVER<<BIN>>","FRASER RIVER MIGRATORY COUNTS<<BIN>>", "FRASER-CROSS-CU SUPPLEMENTATION EXCLUSION<<BIN>>", "FRASER-EARLY SUMMER TIMING", "MIDDLE FRASER", "UPPER FRASER", "FRANCOIS-LATE TIMING", "CARIBOO-SUMMER TIMING", "FRASER-HARRISON FALL TRANSPLANT_FA_0.3<<BIN>>", "ALOUETTE_EARLY SUMMER<<EXTIRPATED>>", "FRASER-MISCELLANEOUS<<BIN>>", "FRANCOIS-EARLY SUMMER TIMING", "INDIAN/KRUGER-EARLY SUMMER TIMING"))] <- "Fraser"

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("OSOYOOS"))] <- "Columbia"

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("OWIKENO-LATE TIMING", "WHALEN", "(N)SYLVIA CREEK", "HECATE STRAIT-FJORDS", "HECATE LOWLANDS"))] <- "Central Coast"

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("BABINE", "PRUDHOMME", "SHAWATLAN", "TAHLO/MORRISON", "BOWSER", "(N)ONERKA", "NASS-SKEENA ESTUARY", "MIDDLE-UPPER SKEENA"))] <- "Skeena"

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("CLEMENTS", "SPLIT MOUNTAIN/LEVERSON", "OWEEGEE"))] <- "Nass"


nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("SOUTHERN FJORDS", "SOUTHERN BC-CROSS-CU SUPPLEMENTATION EXCLUSION<<BIN>>", "(N)GLENDALE", "OWOSSITSA", "GREAT CENTRAL/SPROAT<<BIN>>", "PACK", "SOUTH-MISCELLANEOUS<<BIN>>", "HOMATHKO-KLINAKLINI-SMITH-RIVERS-BELLA COOLA-DEAN"))] <- "Vancouver Island & Mainland Inlets"

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("(P)HATCHERY EXCLUSION-PALLANT CREEK", "(N)MAYER", "NORTH HAIDA GWAII", "EAST HAIDA GWAII"))] <- "HaidaGwaii"


# Somehow some populations with no CU were assigned to the wrong region sometimes. Reassign.
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "WHALEN CREEK" & nuseds_cu$SPECIES == "Chinook")] <- "Central Coast"
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "OWEEGEE CREEK" & nuseds_cu$SPECIES %in% c("Pink", "Chum"))] <- "Nass"
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "SYLVIA CREEK" & nuseds_cu$SPECIES == "Chinook")] <- "Central Coast"
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "SOMASS-SPROAT-GC SYSTEM")] <- "Vancouver Island & Mainland Inlets"
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "PACK LAKE CREEK")] <- "Vancouver Island & Mainland Inlets"
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "SHAWNIGAN CREEK")] <- "Vancouver Island & Mainland Inlets"


# # Loop through to manually correct missing regions
# cus_no_region <- unique(nuseds_cu$CU_NAME[is.na(nuseds_cu$REGION)])
# wb <- unique(nuseds_cu$WATERBODY[which(nuseds_cu$CU_NAME == cus_no_region[1])])
# i <- 1
# unique(nuseds_cu$REGION[which(nuseds_cu$WATERBODY == wb[i])])
# 
# # write.csv(nuseds_cu[which(nuseds_cu$WATERBODY == wb[i]), ], "output/regionConflic.csv")

#------------------------------------------------------------------------------
# Change spawn timing dates into DOY
#------------------------------------------------------------------------------

for(j in 1:length(varNames)){
  nuseds_cu[, varNames[j]] <- as.numeric(nuseds_cu[, varNames[j]] %>% as.Date(format="%d-%b-%y") %>% format("%j"))
 }

#------------------------------------------------------------------------------
# QA/QC
#------------------------------------------------------------------------------

#----
# Check that from < to for survey date variables
range(nuseds_cu$START_DTT - nuseds_cu$END_DTT, na.rm = TRUE)
# hist(nuseds_cu$START_DTT, xlim = c(0, 365), breaks = seq(0, 53*7, 7), xlab = "DOY", main = "Survey dates")
# hist(nuseds_cu$END_DTT, breaks = seq(0, 53*7, 7), col = "#FF000030", border =2, add = TRUE)
# legend("topleft", fill = c(grey(0.8), "#FF000030"), border = c(1, 2), bty = "n", legend = c( "START", "END"))
# Add 365 to put these in the next year
ind <- which((nuseds_cu$END_DTT < nuseds_cu$START_DTT) & nuseds_cu$END_DTT < 100)
nuseds_cu$END_DTT[ind] <- nuseds_cu$END_DTT[ind] + 365

# Check that from < to for all variables

range(nuseds_cu$STREAM_ARRIVAL_DT_TO - nuseds_cu$STREAM_ARRIVAL_DT_FROM, na.rm = TRUE)
nuseds_cu[which(nuseds_cu$STREAM_ARRIVAL_DT_TO < nuseds_cu$STREAM_ARRIVAL_DT_FROM), c("ACT_ID", 'STREAM_ARRIVAL_DT_FROM', "STREAM_ARRIVAL_DT_TO")]

# Remove data that don't make sense: FROM before TO
nuseds_cu[nuseds_cu$ACT_ID == 2027391231, 'STREAM_ARRIVAL_DT_FROM'] <- NA
nuseds_cu[nuseds_cu$ACT_ID == 48829881, "STREAM_ARRIVAL_DT_FROM"] <- NA

# Swap start and from (1 day apart)
nuseds_cu[nuseds_cu$ACT_ID == 1002102506, 'STREAM_ARRIVAL_DT_FROM'] <- 353
nuseds_cu[nuseds_cu$ACT_ID == 1002102506, 'STREAM_ARRIVAL_DT_TO'] <- 354

# Add 365 to remaining TO dates
nuseds_cu$STREAM_ARRIVAL_DT_TO[which(nuseds_cu$STREAM_ARRIVAL_DT_TO < nuseds_cu$STREAM_ARRIVAL_DT_FROM)] <- nuseds_cu$STREAM_ARRIVAL_DT_TO[which(nuseds_cu$STREAM_ARRIVAL_DT_TO < nuseds_cu$STREAM_ARRIVAL_DT_FROM)] + 365


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

nuseds_cu[nuseds_cu$ACT_ID == 751065, "PEAK_SPAWN_DT_TO"] <- 272 # Swap to and from
nuseds_cu[nuseds_cu$ACT_ID == 751065, "PEAK_SPAWN_DT_FROM"] <- 245 # Swap to and from

nuseds_cu[nuseds_cu$ACT_ID == 68420541, "PEAK_SPAWN_DT_FROM"] <- NA # 364 doesnt make sense

#------------
nuseds_cu[which(nuseds_cu$END_SPAWN_DT_TO < nuseds_cu$END_SPAWN_DT_FROM), c("ACT_ID", 'END_SPAWN_DT_FROM', "END_SPAWN_DT_TO")] # All make sense (end of the year)

nuseds_cu$END_SPAWN_DT_TO[which(nuseds_cu$END_SPAWN_DT_TO < nuseds_cu$END_SPAWN_DT_FROM)] <- nuseds_cu$END_SPAWN_DT_TO[which(nuseds_cu$END_SPAWN_DT_TO < nuseds_cu$END_SPAWN_DT_FROM)] + 365

###############################################################################
# Revisit pops with missing cuid
###############################################################################

unique(nuseds_cu$FULL_CUNAME[which(is.na(nuseds_cu$cuid))])

dum <- nuseds_cu[match(unique(nuseds_cu$FULL_CUNAME[which(is.na(nuseds_cu$cuid))]), nuseds_cu$FULL_CUNAME), c("FULL_CUNAME", "FULL_CU_IN", "cuid", "CU_TYPE")]

dum[which(dum$CU_TYPE == "Current"),]
###############################################################################
# Write output data file
###############################################################################
# Don't include if only START_DTT and END_DTT have data
has.info <- apply(!is.na(nuseds_cu[, varNames[3:10]]), 1, sum)
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

