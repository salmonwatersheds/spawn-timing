###############################################################################
# Code to fit a model to spawn timing data from NuSEDS to estimate
# arrival date, mean and sd of spawn timing for each CU
#
# Created by Steph Peacock, Analyst, Salmon Watersheds Program
# Contact: speacock at psf dot ca
# Date: October 12, 2022
###############################################################################

library(dplyr)
library(dclone)

###############################################################################
# Read-in compiled spawn-timing data
# See data-compilation.R for how these data were compiled from raw NuSEDS 
###############################################################################

dat <- read.csv("output/NuSEDS-spawn-timing.csv")

# Create unique Species Qualified CU Name
dat$SQ_CU_NAME <- paste(dat$SPECIES_QUALIFIED, dat$CU_NAME, sep = " ")

###############################################################################
# Select species and create JAGS data list
###############################################################################

speciesNames <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")

# Selected species (ss)
ss <- "Pink"

# Start with arrival date for now
ind.ss <- which(dat$SPECIES == ss & !is.na(dat$CU_NAME) & apply(!is.na(dat[, c("STREAM_ARRIVAL_DT_FROM", "STREAM_ARRIVAL_DT_TO")]), 1, sum) > 0 )

# Extract regions and CUs that have data
regions <- sort(unique(dat$REGION[ind.ss]))
CUs <- sort(unique(dat$SQ_CU_NAME[ind.ss]))

# Match each CU to a region
regions_CU <- rep(NA, length(CUs))
names(regions_CU) <- CUs
for(i in 1:length(CUs)){
  dum <- unique(dat$REGION[which(dat$SQ_CU_NAME == CUs[i])])
  if(length(dum) > 1) stop("Not 1:1") else regions_CU[i] <- dum
}

#------------------------------------------------------------------------------
# Create data list for JAGS
#------------------------------------------------------------------------------

jagsDat <- list(
  
  # Counters
  # n = length(ind.ss),
  nCUs = length(CUs),
  nRegions = length(regions),
  
  # Other variables
  # year = dat$ANALYSIS_YR[ind.ss],
  region = as.numeric(factor(dat$REGION[ind.ss], levels = regions)),
  CU = as.numeric(factor(dat$SQ_CU_NAME[ind.ss], levels = CUs)),
  region_CU = as.numeric(factor(regions_CU, levels = regions))
  
  
  
  # # Spawning
  # cens_start = cbind(dat$START_SPAWN_DT_FROM[ind.ss], dat$START_SPAWN_DT_TO[ind.ss]),
  # cens_peak = cbind(dat$PEAK_SPAWN_DT_FROM[ind.ss],dat$PEAK_SPAWN_DT_TO[ind.ss]),
  # cens_end = cbind(dat$END_SPAWN_DT_FROM[ind.ss], dat$END_SPAWN_DT_TO[ind.ss]),
)

# Set up data needed for censored analysis: Arrival time
# Arrival interval 
jagsDat$cens_arrival = cbind(dat$STREAM_ARRIVAL_DT_FROM[ind.ss], dat$STREAM_ARRIVAL_DT_TO[ind.ss])

# Create time variable if arrival was observed exactly (uncensored) and NA 
# if exact arrival time is unknown (censored)
jagsDat$time_arrival <- rep(NA, dim(jagsDat$cens_arrival)[1])
jagsDat$time_arrival[which(jagsDat$cens_arrival[, 1] == jagsDat$cens_arrival[, 2])] <- jagsDat$cens_arrival[which(jagsDat$cens_arrival[, 1] == jagsDat$cens_arrival[, 2]), 2]

# Create dummy variable indicating censor type
jagsDat$Y_arrival <- rep(NA, dim(jagsDat$cens_arrival)[1])

##    Y = 1 is interval censored (from and to provided) BUT not known exactly
jagsDat$Y_arrival[!is.na(jagsDat$cens_arrival[, 1]) & !is.na(jagsDat$cens_arrival[, 2]) & is.na(jagsDat$time_arrival)] <- 1

##    Y = 0 is left censoring (to = NA)
jagsDat$Y_arrival[is.na(jagsDat$cens_arrival[, 2])] <- 0
jagsDat$cens_arrival[is.na(jagsDat$cens_arrival[, 2]), 2] <- Inf 

##    Y = 2 is right censoring (from = NA)
jagsDat$Y_arrival[is.na(jagsDat$cens_arrival[, 1])] <- 2
jagsDat$cens_arrival[is.na(jagsDat$cens_arrival[, 1]), 1] <- -Inf 

# Create index variable for sensoring
jagsDat$not.censored_arrival <- which(!is.na(jagsDat$time_arrival))
jagsDat$censored_arrival <- which(is.na(jagsDat$time_arrival))

# For dates that cross new year, make end date > DOY 365
for(i in 1:4){
  if(length(which(jagsDat$cens_arrival[, 2] - jagsDat$cens_arrival[, 1] < 0)) > 0){
    jagsDat$cens_arrival[which(jagsDat$cens_arrival[, 2] - jagsDat$cens_arrival[, 1] < 0), 2] <- jagsDat$cens_arrival[which(jagsDat$cens_arrival[, 2] - jagsDat$cens_arrival[, 1] < 0), 2] + 365
  }
}

###############################################################################
# Define model
###############################################################################

model <- function(){
  
  # Define priors on arrival timing and spawn timing
  for(i in 1:nRegions){
    muArrive[i] ~ dnorm(230, 10^-2)
    sdArrive[i] ~ dlnorm(log(20), 3^-2)
    # muSpawn[i] ~ dnorm(255, 10^-2)
    # sdSpawn[i] ~ dlnorm(log(20), 3^-2)
  }
  
  for(j in 1:nCUs){
    muArrive_CU[j] ~ dnorm(muArrive[region_CU[j]], 1)
    # t_peak[j] ~ dnorm(muSpawn[region_CU[j]], 3^-2)
    # sd_peak[j] ~ dlnorm(log(sdSpawn[region_CU[j]]), 3^(-2))
   }
  
  # Likelihood
  for(z in not.censored_arrival){# time_arrival[z] != NA
    time_arrival[z] ~ dnorm(muArrive_CU[CU[z]], sdArrive[region[z]])
  }
  
  for(z in censored_arrival){ # time_arrival[z] = NA
    Y_arrival[z] ~ dinterval(time_arrival[z], cens_arrival[z,])
    time_arrival[z] ~ dnorm(muArrive_CU[CU[z]], sdArrive[region[z]])
  }
  
} # end model

# **8 TO do: Need 

###############################################################################
# Fit model
###############################################################################

# Need initial values for t_arrive, t_start, t_peak, and t_end for each CU
inits <- list(
  time_arrival = apply(jagsDat$cens_arrival, 1, mean, na.rm = TRUE),
  muArrive = rep(230, jagsDat$nRegions),
  sdArrive = rep(log(20), jagsDat$nRegions),
  muArrive_CU = rep(230, jagsDat$nCUs)
)

# Choose reasonable values for initial censored arrival times
inits$time_arrival[which(jagsDat$Y_arrival == 0)] <- jagsDat$cens_arrival[which(jagsDat$Y_arrival == 0), 1] + 1
inits$time_arrival[which(jagsDat$Y_arrival == 2)] <- jagsDat$cens_arrival[which(jagsDat$Y_arrival == 2), 2] - 1

# Set known inits to NA
inits$time_arrival[jagsDat$not.censored_arrival] <- NA

# Fit model
fit <- jags.fit(
  data = jagsDat, 
  params = c("muArrive", "sdArrive", "muArrive_CU"), 
  model = model,
  inits, 
  progress.bar = "text")

summary(fit)
