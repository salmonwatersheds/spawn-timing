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
library(viridis)     
###############################################################################
# Read-in compiled spawn-timing data
# See data-compilation.R for how these data were compiled from raw NuSEDS 
###############################################################################

dat <- read.csv("output/NuSEDS-spawn-timing.csv")

# Create unique Species Qualified CU Name
dat$SQ_CU_NAME <- paste(dat$SPECIES_QUALIFIED, dat$CU_NAME, sep = " ")

###############################################################################
# Select species 
###############################################################################

speciesNames <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")

# Selected species (ss)
ss <- "Pink"

# Start with arrival date for now
ind.ss <- which(dat$SPECIES == ss & !is.na(dat$CU_NAME))# & apply(!is.na(dat[, c("STREAM_ARRIVAL_DT_FROM", "STREAM_ARRIVAL_DT_TO")]), 1, sum) > 0 )

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

###############################################################################
# Create data list for JAGS
###############################################################################

jagsDat <- list(
  
  # Counters
  n = length(ind.ss),
  nCUs = length(CUs),
  # nRegions = length(regions),
  
  # Other variables
  # year = dat$ANALYSIS_YR[ind.ss],
  region = as.numeric(factor(dat$REGION[ind.ss], levels = regions)),
  CU = as.numeric(factor(dat$SQ_CU_NAME[ind.ss], levels = CUs))
  region_CU = as.numeric(factor(regions_CU, levels = regions))

)

#------------------------------------------------------------------------------
# Set up data needed for censored analysis: Arrival time
#------------------------------------------------------------------------------
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

#------------------------------------------------------------------------------
# Set up data needed for censored analysis: Spawn time
#------------------------------------------------------------------------------
# Spawn timing interval s
jagsDat$cens_start = cbind(dat$START_SPAWN_DT_FROM[ind.ss], dat$START_SPAWN_DT_TO[ind.ss])
jagsDat$cens_peak = cbind(dat$PEAK_SPAWN_DT_FROM[ind.ss], dat$PEAK_SPAWN_DT_TO[ind.ss])
jagsDat$cens_end = cbind(dat$END_SPAWN_DT_FROM[ind.ss], dat$END_SPAWN_DT_TO[ind.ss])

# Create time variable if arrival was observed exactly (uncensored) and NA 
# if exact arrival time is unknown (censored)
jagsDat$time_start <- rep(NA, dim(jagsDat$cens_start)[1])
jagsDat$time_start[which(jagsDat$cens_start[, 1] == jagsDat$cens_start[, 2])] <- jagsDat$cens_start[which(jagsDat$cens_start[, 1] == jagsDat$cens_start[, 2]), 2]

jagsDat$time_peak <- rep(NA, dim(jagsDat$cens_peak)[1])
jagsDat$time_peak[which(jagsDat$cens_peak[, 1] == jagsDat$cens_peak[, 2])] <- jagsDat$cens_peak[which(jagsDat$cens_peak[, 1] == jagsDat$cens_peak[, 2]), 2]

jagsDat$time_end <- rep(NA, dim(jagsDat$cens_end)[1])
jagsDat$time_end[which(jagsDat$cens_end[, 1] == jagsDat$cens_end[, 2])] <- jagsDat$cens_end[which(jagsDat$cens_end[, 1] == jagsDat$cens_end[, 2]), 2]

# Create dummy variable indicating censor type
jagsDat$Y_start <- rep(NA, dim(jagsDat$cens_start)[1])
jagsDat$Y_peak <- rep(NA, dim(jagsDat$cens_peak)[1])
jagsDat$Y_end <- rep(NA, dim(jagsDat$cens_end)[1])

##    Y = 1 is interval censored (from and to provided) BUT not known exactly
jagsDat$Y_start[!is.na(jagsDat$cens_start[, 1]) & !is.na(jagsDat$cens_start[, 2]) & is.na(jagsDat$time_start)] <- 1

jagsDat$Y_peak[!is.na(jagsDat$cens_peak[, 1]) & !is.na(jagsDat$cens_peak[, 2]) & is.na(jagsDat$time_peak)] <- 1

jagsDat$Y_end[!is.na(jagsDat$cens_end[, 1]) & !is.na(jagsDat$cens_end[, 2]) & is.na(jagsDat$time_end)] <- 1

##    Y = 0 is left censoring (to = NA)
jagsDat$Y_start[is.na(jagsDat$cens_start[, 2])] <- 0
jagsDat$cens_start[is.na(jagsDat$cens_start[, 2]), 2] <- Inf 

jagsDat$Y_peak[is.na(jagsDat$cens_peak[, 2])] <- 0
jagsDat$cens_peak[is.na(jagsDat$cens_peak[, 2]), 2] <- Inf 

jagsDat$Y_end[is.na(jagsDat$cens_end[, 2])] <- 0
jagsDat$cens_end[is.na(jagsDat$cens_end[, 2]), 2] <- Inf 

##    Y = 2 is right censoring (from = NA)
jagsDat$Y_start[is.na(jagsDat$cens_start[, 1])] <- 2
jagsDat$cens_start[is.na(jagsDat$cens_start[, 1]), 1] <- -Inf 

jagsDat$Y_peak[is.na(jagsDat$cens_peak[, 1])] <- 2
jagsDat$cens_peak[is.na(jagsDat$cens_peak[, 1]), 1] <- -Inf 

jagsDat$Y_end[is.na(jagsDat$cens_end[, 1])] <- 2
jagsDat$cens_end[is.na(jagsDat$cens_end[, 1]), 1] <- -Inf 

# Create index variable for sensoring
jagsDat$not.censored_peak <- which(!is.na(jagsDat$time_peak)) # Not censored
jagsDat$censored_peak <- which(abs(jagsDat$cens_peak[, 1]) != abs(jagsDat$cens_peak[, 2])) # Censored but some information

jagsDat$not.censored_start <- which(!is.na(jagsDat$time_start))
jagsDat$censored_start <- which(abs(jagsDat$cens_start[, 1]) != abs(jagsDat$cens_start[, 2]))

jagsDat$not.censored_end <- which(!is.na(jagsDat$time_end))
jagsDat$censored_end <- which(abs(jagsDat$cens_end[, 1]) != abs(jagsDat$cens_end[, 2]))

# For dates that cross new year, make end date > DOY 365
for(i in 1:4){
  if(length(which(jagsDat$cens_start[, 2] - jagsDat$cens_start[, 1] < 0)) > 0){
    jagsDat$cens_start[which(jagsDat$cens_start[, 2] - jagsDat$cens_start[, 1] < 0), 2] <- jagsDat$cens_start[which(jagsDat$cens_start[, 2] - jagsDat$cens_start[, 1] < 0), 2] + 365
  }
  
  if(length(which(jagsDat$cens_peak[, 2] - jagsDat$cens_peak[, 1] < 0)) > 0){
    jagsDat$cens_peak[which(jagsDat$cens_peak[, 2] - jagsDat$cens_peak[, 1] < 0), 2] <- jagsDat$cens_peak[which(jagsDat$cens_peak[, 2] - jagsDat$cens_peak[, 1] < 0), 2] + 365
  }
  
  if(length(which(jagsDat$cens_end[, 2] - jagsDat$cens_end[, 1] < 0)) > 0){
    jagsDat$cens_end[which(jagsDat$cens_end[, 2] - jagsDat$cens_end[, 1] < 0), 2] <- jagsDat$cens_end[which(jagsDat$cens_end[, 2] - jagsDat$cens_end[, 1] < 0), 2] + 365
  }
}

# Add zero data
n <- length(ind.ss)
jagsDat$mu.zero <- rep(0, n)
jagsDat$sd1.zero <- rep(0, n)
jagsDat$sd2.zero <- rep(0, n)



###############################################################################
# Fit model
###############################################################################

source("code/models.R")

model <- model_spawn
#------------------------------------------------------------------------------
# Initial values
#------------------------------------------------------------------------------

# Need initial values for t_arrive, t_start, t_peak, and t_end for each CU
inits <- list(
  time_start = apply(jagsDat$cens_start, 1, mean, na.rm = TRUE),
  time_peak = apply(jagsDat$cens_peak, 1, mean, na.rm = TRUE),
  time_end = apply(jagsDat$cens_end, 1, mean, na.rm = TRUE),
  # muSpawn = 230, #rep(230, jagsDat$nRegions),
  # sdArrive = log(20), #rep(log(20), jagsDat$nRegions),
  muSpawn_CU = rep(230, jagsDat$nCUs),
  sdSpawn_CU = rep(log(20), jagsDat$nCUs)
)

# Choose reasonable values for initial censored arrival times
# inits$time_arrival[which(jagsDat$Y_arrival == 0)] <- jagsDat$cens_arrival[which(jagsDat$Y_arrival == 0), 1] + 1
# inits$time_arrival[which(jagsDat$Y_arrival == 2)] <- jagsDat$cens_arrival[which(jagsDat$Y_arrival == 2), 2] - 1

inits$time_start[which(jagsDat$Y_start == 0)] <- jagsDat$cens_start[which(jagsDat$Y_start == 0), 1] + 1
inits$time_start[which(jagsDat$Y_start == 2)] <- jagsDat$cens_start[which(jagsDat$Y_start == 2), 2] - 1

inits$time_peak[which(jagsDat$Y_peak == 0)] <- jagsDat$cens_peak[which(jagsDat$Y_peak == 0), 1] + 1
inits$time_peak[which(jagsDat$Y_peak == 2)] <- jagsDat$cens_peak[which(jagsDat$Y_peak == 2), 2] - 1

inits$time_end[which(jagsDat$Y_end == 0)] <- jagsDat$cens_end[which(jagsDat$Y_end == 0), 1] + 1
inits$time_end[which(jagsDat$Y_end == 2)] <- jagsDat$cens_end[which(jagsDat$Y_end == 2), 2] - 1

# Set known inits to NA
# inits$time_arrival[jagsDat$not.censored_arrival] <- NA
inits$time_start[jagsDat$not.censored_start] <- NA
inits$time_peak[jagsDat$not.censored_peak] <- NA
inits$time_end[jagsDat$not.censored_end] <- NA


#------------------------------------------------------------------------------
# Fit model
#------------------------------------------------------------------------------

fit <- jags.fit(
  data = jagsDat, 
  params = c("muSpawn_CU", "sdSpawn_CU"), 
  model = model,
  inits, 
  progress.bar = "text")

###############################################################################
# Look at output
###############################################################################

S <- summary(fit)

R <- gelman.diag(fit)

pnames <- rownames(S[[1]])

# # Region hyperparameter
# ind <- list(
#   muArrive = match(paste0("muArrive[", 1:jagsDat$nRegions, "]"), pnames),
#   sdArrive = match(paste0("sdArrive[", 1:jagsDat$nRegions, "]"), pnames),
#   muArrive_CU = match(paste0("muArrive_CU[", 1:jagsDat$nCUs, "]"), pnames),
#   sdArrive_CU = match(paste0("sdArrive_CU[", 1:jagsDat$nCUs, "]"), pnames)
# )

ind <- list(
  muArrive = match("muArrive", pnames),
  sdArrive = match("sdArrive", pnames),
  muArrive_CU = match(paste0("muArrive_CU[", 1:jagsDat$nCUs, "]"), pnames)
  )

# Plot regional output
colsCU <- viridis(jagsDat$nCUs)
par(mfrow = c(3,2))
for(i in 1:6){ # for each region
  x <- seq(min(S[[1]][ind$muArrive_CU[unique(jagsDat$CU[jagsDat$region == i])], 'Mean']), max(S[[1]][ind$muArrive_CU[unique(jagsDat$CU[jagsDat$region == i])], 'Mean']), 0.1)
  if(length(x) == 1) x <- seq(x - 15, x + 15, 0.1)
  y <- dnorm(x, mean = S[[1]][ind$muArrive[i], 'Mean'], sd = S[[1]][ind$sdArrive[i], "Mean"])
  plot(x, y/max(y), "l", lwd = 4, col = grey(0.8), xlab = "", ylab = "Arrival time", bty = "l", las = 1, yaxt = "n", main = regions[i])
  for(j in unique(jagsDat$CU[jagsDat$region == i])){
   abline(v = S[[1]][ind$muArrive_CU[j], 'Mean'], col = colsCU[j])
  } # end j
} # end i

# No regional hyperparameters
x <- seq(150, 300, 0.1)
y <- dnorm(x, mean = S[[1]][ind$muArrive, 'Mean'], sd = S[[1]][ind$sdArrive, "Mean"])
plot(x, y/max(y), "l", lwd = 4, col = grey(0.8), xlab = "", ylab = "Arrival time", bty = "l", las = 1, yaxt = "n")
for(j in 1:jagsDat$nCU){
  abline(v = S[[1]][ind$muArrive_CU[j], 'Mean'], col = colsCU[j])
} # end j


#########################3

jagsDat$cens_peak[jagsDat$CU == 1,]
