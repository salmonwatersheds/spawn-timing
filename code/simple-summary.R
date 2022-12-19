###############################################################################
# Simple averages of arrival date, mean and sd of spawn timing for each CU
# that has data in NuSEDS
#
# Created by Steph Peacock, Analyst, Salmon Watersheds Program
# Contact: speacock at psf dot ca
# Date: December 19, 2022
###############################################################################

###############################################################################
# Read-in compiled spawn-timing data
# See data-compilation.R for how these data were compiled from raw NuSEDS 
###############################################################################

dat <- read.csv("output/NuSEDS-spawn-timing.csv")

# Create unique Species Qualified CU Name
dat$SQ_CU_NAME <- paste(dat$SPECIES_QUALIFIED, dat$CU_NAME, sep = " ")

###############################################################################
# Simple average of timing for each CU
###############################################################################

FULL_CU_IN <- sort(unique(dat$FULL_CU_IN))

# Match each unique CU to first row in dat for metadata
ind1 <- match(FULL_CU_IN, dat$FULL_CU_IN)

# Create summary dataframe
sumDat <- dat[ind1, c("FULL_CU_IN", "CU_NAME", "CU_ACRO", "CU_TYPE", "CU_INDEX", "SPECIES_QUALIFIED", "REGION")]

# Match each row in dat to a CU for summarizing timing
ind2 <- match(dat$FULL_CU_IN, sumDat$FULL_CU_IN)

#Average of arrival date (from, to) and end spawn (from, to) per CU
sumDat$STREAM_ARRIVAL_DT <- tapply(apply(dat[, c("STREAM_ARRIVAL_DT_FROM", "STREAM_ARRIVAL_DT_TO")], 1, mean, na.rm = TRUE), ind2, mean, na.rm = TRUE)
                                        
sumDat$END_SPAWN_DT <- tapply(apply(dat[, c("END_SPAWN_DT_FROM", "END_SPAWN_DT_TO")], 1, mean, na.rm = TRUE), ind2, mean, na.rm = TRUE)

###############################################################################
# Write to csv
###############################################################################

write.csv(sumDat, file = "output/CU-spawn-timing-simple.csv", row.names = FALSE)
