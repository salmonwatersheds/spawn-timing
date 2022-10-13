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

ind.ss <- which(dat$SPECIES == ss & !is.na(dat$CU_NAME) & apply(!is.na(dat[, c("STREAM_ARRIVAL_DT_FROM", "STREAM_ARRIVAL_DT_TO")]), 1, sum) > 0 )

regions <- sort(unique(dat$REGION[ind.ss]))
CUs <- sort(unique(dat$SQ_CU_NAME[ind.ss]))

# Match each CU to a region
regions_CU <- rep(NA, length(CUs))
names(regions_CU) <- CUs
for(i in 1:length(CUs)){
  dum <- unique(dat$REGION[which(dat$SQ_CU_NAME == CUs[i])])
  if(length(dum) > 1) stop("Not 1:1") else regions_CU[i] <- dum
}

jagsDat <- list(
  
  # Arrival
  arrival = cbind(dat$STREAM_ARRIVAL_DT_FROM[ind.ss], dat$STREAM_ARRIVAL_DT_TO[ind.ss]),
  
  # Spawning
  start = cbind(dat$START_SPAWN_DT_FROM[ind.ss], dat$START_SPAWN_DT_TO[ind.ss]),
  peak = cbind(dat$PEAK_SPAWN_DT_FROM[ind.ss],dat$PEAK_SPAWN_DT_TO[ind.ss]),
  end = cbind(dat$END_SPAWN_DT_FROM[ind.ss], dat$END_SPAWN_DT_TO[ind.ss]),
  
  # Other variables
  year = dat$ANALYSIS_YR[ind.ss],
  region = as.numeric(factor(dat$REGION[ind.ss], levels = regions)),
  CU = as.numeric(factor(dat$SQ_CU_NAME[ind.ss], levels = CUs)),
  region_CU = as.numeric(factor(regions_CU, levels = regions)),
  
  # Counters
  n = length(ind.ss),
  nCUs = length(CUs),
  nRegions = length(regions)
)

# Set end
jagsDat$arrival[is.na(jagsDat$arrival[, 1]), 1] <- -Inf
jagsDat$arrival[is.na(jagsDat$arrival[, 2]), 2] <- Inf


# For dates that cross new year, make end date > DOY 365
for(i in 1:4){
  if(length(which(jagsDat[[i]][, 2] - jagsDat[[i]][, 1] < 0)) > 0){
    jagsDat[[i]][which(jagsDat[[i]][, 2] - jagsDat[[i]][, 1] < 0), 2] <- jagsDat[[i]][which(jagsDat[[i]][, 2] - jagsDat[[i]][, 1] < 0), 2] + 365
  }
}

# Create variable for censoring
nvar0 <- length(jagsDat)
for(i in 1:4){
  nvar <- length(jagsDat)
  jagsDat[[nvar + 1]] <- rep(NA, jagsDat$n)
  jagsDat[[nvar + 1]][which(is.na(jagsDat[[i]][, 1]) & !is.na(jagsDat[[i]][, 2]))] <- 0 # if no start, just know that is less than to
  jagsDat[[nvar + 1]][which(is.na(jagsDat[[i]][, 2]) & !is.na(jagsDat[[i]][, 1]))] <- 2 # if no end, just know that is more than from
  jagsDat[[nvar + 1]][which(!is.na(jagsDat[[i]][, 1]) & !is.na(jagsDat[[i]][, 2]))] <- 1 # if have both to and from
}

names(jagsDat)[(nvar0 + 1):(nvar+1)] <- c("y_arrival", "y_start", "y_peak", "y_end")

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
  
  # for(j in 1:nCUs){
  #   t_start[j] <- pnorm(0.025, t_peak[j], sd_peak[j]^(-2))
  #   t_end[z] <- pnorm(0.975, t_peak[j], sd_peak[j]^(-2))
  # }
  
  # Model prediction
  for(z in 1:n){
    t_arrival[z] ~ dnorm(muArrive_CU[CU[z]], sdArrive[region_CU[CU[z]]]^(-2))
  }
  
  # Likelihood
  for(z in 1:n){
     y_arrival[z] ~ dinterval(t_arrival[z], arrival[z, ])
    # y_start[z] ~ dinterval(t_start[CU[z]], start[z, ])
    # y_peak[z] ~ dinterval(t_peak[CU[z]], peak[z, ])
    # y_end[z] ~ dinterval(t_end[CU[z]], end[z, ])
  }
  
} # end model

# **8 TO do: Need 

###############################################################################
# Fit model
###############################################################################

# Need initial values for t_arrive, t_start, t_peak, and t_end for each CU
inits <- list(
  t_arrival = apply(jagsDat$arrival, 1, mean, na.rm = TRUE)
)

# If righ or left censored, these need to fall in the range so add=or subtract
inits$t_arrival[which(jagsDat$y_arrival == 2)] <- inits$t_arrival[which(jagsDat$y_arrival == 2)] + 1
inits$t_arrival[which(jagsDat$y_arrival == 0)] <- inits$t_arrival[which(jagsDat$y_arrival == 0)] - 1

# Fit model
fit <- jags.fit(
  data = jagsDat, 
  params = list("muArrive", "sdArrive", "muArrive_CU"), 
  model = model,
  inits, 
  progress.bar = "text")

summary(fit)
