###############################################################################
# Code to fit a model to spawn timing data from NuSEDS to estimate
# arrival date, mean and sd of spawn timing for each CU
#
# Created by Steph Peacock, Analyst, Salmon Watersheds Program
# Contact: speacock at psf dot ca
# Date: January 17, 2023
###############################################################################

library(dplyr)
library(dclone)
library(viridis)     
###############################################################################
# Read-in compiled spawn-timing data
# See data-compilation.R for how these data were compiled from raw NuSEDS 
###############################################################################

dat <- read.csv("output/NuSEDS-spawn-timing.csv")

# Create unique Species Qualified CU Name
dat$SQ_CU_NAME <- paste(dat$SPECIES_QUALIFIED, dat$CU_NAME, sep = " ")

# Separate river- and lake-type sockeye for estimating hyperparamters
dat$SPECIES[which(dat$SPECIES_QUALIFIED == "SEL")] <- "SockeyeLake"
dat$SPECIES[which(dat$SPECIES_QUALIFIED == "SER")] <- "SockeyeRiver"

###############################################################################
# Define JAGS model
###############################################################################

model <- function(){
  
  #------------
  # Regional hyperparameters
  #------------
  for(i in 1:nRegions){
    alpha_R[i] ~ dnorm(230, 20^(-2)) # Mean arrival time for each region
    mu_R[i] ~ dnorm(230, 20^(-2)) # Mean spawn time for each region
    sig_R[i] ~ dlnorm(log(20), 1) # SD in spawn timing for each region
   }
  
  #------------
  # CU-level parameters
  #------------
  for(j in 1:nCUs){
    alpha_CU[j] ~ dnorm(alpha_R[region_CU[j]], upsilon[2]^(-2)) # Mean arrival time for each CU, from regional hyperdistribution
    mu_CU[j] ~ dnorm(mu_R[region_CU[j]], upsilon[2]^(-2)) # Mean spawn time for each CU, from regional hyperdistribution
    sig_CU[j] ~ dlnorm(log(sig_R[region_CU[j]]), upsilon[2]^(-2)) # Standard deviation in spawn time for each CU
  }
  
  #------------
  # Likelihood
  #------------
  # Separate for each variable to ensure NAs don't cause issues
  for(z in z.arrival){
    arrival[z] ~ dnorm(alpha_CU[CU[z]], upsilon[1]^(-2))
  }
  
  # Start date is the date when 2.5% of fish have started spawning
  # Defined in terms of the mean and sd of normal distribution of spawn timing
  for(z in z.start){
    start[z] ~ dnorm(mu_CU[CU[z]] - q*sig_CU[CU[z]], upsilon[1]^(-2))
  }
  
  # Peak spawning is the mean of normal distribution of spawn timing
  for(z in z.peak){
    peak[z] ~ dnorm(mu_CU[CU[z]], upsilon[1]^(-2))
  }
  
  # End date is the date when 97.5% of fish have spawned
  # Defined in terms of the mean and sd of normal distribution of spawn timing
  for(z in z.end){
    end[z] ~ dnorm(mu_CU[CU[z]] + q*sig_CU[CU[z]], upsilon[1]^(-2))
  }
  
} # end model

###############################################################################
# Loop through and fit model to each species
###############################################################################

speciesNames <- c("Chinook", "Chum", "Coho", "Pink", "SockeyeLake", "SockeyeRiver")#, "Steelhead")
# Leave Steelhead out for now; only 5 years of data for any steelhead population

for(s in 1:length(speciesNames)){
  
  # Selected species (ss)
  ss <- speciesNames[s]

  # Subset dat for that species
  dat.ss <- dat[which(dat$SPECIES == ss & !is.na(dat$CU_NAME)), ]
  
  # Alternative: subset for species qualified, separating lake- and river-type sockeye
  # ss <- c("SEL", "SER")[s]
  # ss <- c("PKO", "PKE")[s]
  # dat.ss <- dat[which(dat$SPECIES_QUALIFIED == ss & !is.na(dat$CU_NAME)), ]
  
  # Extract regions and CUs that have data
  regions <- sort(unique(dat.ss$REGION))
  CUs <- sort(unique(dat.ss$SQ_CU_NAME))
  
  # Match each CU to a region
  regions_CU <- rep(NA, length(CUs))
  names(regions_CU) <- CUs
  for(i in 1:length(CUs)){
    dum <- unique(dat.ss$REGION[which(dat.ss$SQ_CU_NAME == CUs[i])])
    if(length(dum) > 1) stop("Not 1:1") else regions_CU[i] <- dum
  }
  
  ###############################################################################
  # Create data list for JAGS
  ###############################################################################
  
  jagsDat <- list(
    
    # Counters
    n = nrow(dat.ss),
    nCUs = length(CUs),
    nRegions = length(regions),
    
    # Other variables
    region = as.numeric(factor(dat.ss$REGION, levels = regions)),
    CU = as.numeric(factor(dat.ss$SQ_CU_NAME, levels = CUs)),
    region_CU = as.numeric(factor(regions_CU, levels = regions)),
   
    # Variance terms
    upsilon = c(0.1, 1)
  )
  
  # Arrival
  arrival <- cbind(as.numeric(dat.ss$STREAM_ARRIVAL_DT_FROM), as.numeric(dat.ss$STREAM_ARRIVAL_DT_TO))
  # Check: Are any from < to
  if(length(which(arrival[, 2] - arrival[, 1] < 0)) > 0){
    warning(paste0(ss, ": STREAM_ARRIVAL_DT_TO < STREAM_ARRIVAL_DT_FROM"))
    arrival[which(arrival[, 2] - arrival[, 1] < 0), 2] <- 365 + arrival[which(arrival[, 2] - arrival[, 1] < 0), 2]
  }
  jagsDat$arrival <- apply(arrival, 1, mean, na.rm = TRUE)
  jagsDat$z.arrival <- which(!is.na(jagsDat$arrival))
  
  # Start 
  start <- cbind(as.numeric(dat.ss$START_SPAWN_DT_FROM), as.numeric(dat.ss$START_SPAWN_DT_TO))
  # Check: Are any from < to 
  if(length(which(start[, 2] - start[, 1] < 0)) > 0) stop("START_SPAWN_DT_TO < START_SPAWN_DT_FROM")
  jagsDat$start <- apply(start, 1, mean, na.rm = TRUE)
  
  # If start (or end) is < DOY100, add 365 so that the fitting doesn't get confused.
  jagsDat$start[which(jagsDat$start < 100)] <- jagsDat$start[which(jagsDat$start < 100)] + 365
  
  jagsDat$z.start <- which(!is.na(jagsDat$start))
  
  # Peak
  peak <- cbind(as.numeric(dat.ss$PEAK_SPAWN_DT_FROM), as.numeric(dat.ss$PEAK_SPAWN_DT_TO)) 
  # Check: Are any from < to 
  
  # For sockeye there is one where the TO doesn't make sense (Adams SEL) -> change to NA
  if(s == 5){
    peak[which((peak[, 2] - peak[, 1]) < 0), 2] <- NA
  }
  
  if(length(which(peak[, 2] - peak[, 1] < 0)) > 0) stop("PEAK_SPAWN_DT_TO < PEAK_SPAWN_DT_FROM")
  jagsDat$peak <- apply(peak, 1, mean, na.rm = TRUE)
  jagsDat$z.peak <- which(!is.na(jagsDat$peak))
  
  # End
  end <- cbind(as.numeric(dat.ss$END_SPAWN_DT_FROM), as.numeric(dat.ss$END_SPAWN_DT_TO)) # Check: Are any from < to 
  if(length(which(end[, 2] - end[, 1] < 0)) > 0){
    end[which(end[, 2] - end[, 1] < 0), 2] <- 365 + end[which(end[, 2] - end[, 1] < 0), 2]
    warning(paste0(ss, ": END_SPAWN_DT_TO < END_SPAWN_DT_FROM"))
  }
  jagsDat$end <- apply(end, 1, mean, na.rm = TRUE)
  # If end is < DOY100, add 365 so that the fitting doesn't get confused.
  jagsDat$end[which(jagsDat$end < 100)] <- jagsDat$end[which(jagsDat$end < 100)] + 365
  
  jagsDat$z.end<- which(!is.na(jagsDat$end))
  
  # Define the quantile for start and end of spawning
  p <- 0.95
  jagsDat$q <- qnorm(1 - (1 - p)/2)
  
  saveRDS(jagsDat, file = paste0("output/jagsDat_", ss, ".rds"))
  
  ###############################################################################
  # Fit model
  ###############################################################################

  #------------------------------------------------------------------------------
  # Initial values
  #------------------------------------------------------------------------------

  # Need initial values for t_arrive, t_start, t_peak, and t_end for each CU
  inits <- list(
    alpha_R = tapply(jagsDat$arrival, jagsDat$region, mean, na.rm = TRUE),
    mu_R = tapply(jagsDat$peak, jagsDat$region, mean, na.rm = TRUE),
    sig_R = tapply(jagsDat$peak, jagsDat$region, sd, na.rm = TRUE),
    alpha_CU = tapply(jagsDat$arrival, jagsDat$CU, mean, na.rm = TRUE),
    mu_CU = tapply(jagsDat$peak, jagsDat$CU, mean, na.rm = TRUE),
    sig_CU = pmin(tapply(jagsDat$peak, jagsDat$CU, sd, na.rm = TRUE), 100)
  )

  # Sockeye: no data for spawn timing from AlaskaTransboundary
  # Use arrival timing and add mean difference
  inits$mu_R[is.na(inits$mu_R)] <- inits$alpha_R[is.na(inits$mu_R)] + mean(inits$mu_R - inits$alpha_R, na.rm = TRUE)
  # # Use average sd among other regions
  inits$sig_R[is.na(inits$sig_R)] <- mean(inits$sig_R, na.rm = TRUE)

  # Replace NAs at CU level with region
  ind <- which(is.na(inits$alpha_CU))
  inits$alpha_CU[ind] <- inits$alpha_R[jagsDat$region_CU[ind]]

  ind <- which(is.na(inits$mu_CU))
  inits$mu_CU[ind] <- inits$mu_R[jagsDat$region_CU[ind]]

  ind <- which(is.na(inits$sig_CU))
  inits$sig_CU[ind] <- 20#inits$sig_R[jagsDat$region_CU[ind]]

  # Can't have zero sd
  inits$sig_CU[which(inits$sig_CU == 0)] <- inits$sig_R[jagsDat$region_CU[which(inits$sig_CU == 0)]]


  #------------------------------------------------------------------------------
  # Fit model
  #------------------------------------------------------------------------------
  cl3 <- makeCluster(3)

  fit <- jags.parfit(
    cl = cl3,
    data = jagsDat,
    params = c("alpha_R", "mu_R", "sig_R", "alpha_CU", "mu_CU", "sig_CU"), #
    model = model,
    inits = inits,
    n.chains = 3,
    progress.bar = "text")

  stopCluster(cl3)

  saveRDS(object = fit, file = paste0("output/simple-model-fit_", ss, ".rds"))

} # end species
