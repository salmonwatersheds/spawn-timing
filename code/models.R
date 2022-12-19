###############################################################################
# Model 1: No regional hyperparamters
###############################################################################

model_noRegion <- function(){
  
  muArrive ~ dnorm(230, 20)
  sdArrive ~ dlnorm(log(20), 1)
  
  for(j in 1:nCUs){
    muArrive_CU[j] ~ dnorm(muArrive, sdArrive)
  }
  
  # Likelihood
  for(z in not.censored_arrival){# time_arrival[z] != NA
    time_arrival[z] ~ dnorm(muArrive_CU[CU[z]], 100)
  }
  
  for(z in censored_arrival){ # time_arrival[z] = NA
    Y_arrival[z] ~ dinterval(time_arrival[z], cens_arrival[z,])
    time_arrival[z] ~ dnorm(muArrive_CU[CU[z]], sdArrive)
  }
  
} # end model

###############################################################################
# Model 2: regional hyperparamters
###############################################################################

model <- function(){
  
  # Define priors on arrival timing and spawn timing
  for(i in 1:nRegions){
    muArrive[i] ~ dnorm(230, 10^-2)
    sdArrive[i] ~ dunif(0, 100)
    # muSpawn[i] ~ dnorm(255, 10^-2)
    # sdSpawn[i] ~ dlnorm(log(20), 3^-2)
  }
  
  for(j in 1:nCUs){
    muArrive_CU[j] ~ dnorm(muArrive[region_CU[j]], sdArrive[region_CU[j]])
    sdArrive_CU[j] ~ dlnorm(0.01, 10)
    # t_peak[j] ~ dnorm(muSpawn[region_CU[j]], 3^-2)
    # sd_peak[j] ~ dlnorm(log(sdSpawn[region_CU[j]]), 3^(-2))
  }
  
  # Likelihood
  for(z in not.censored_arrival){# time_arrival[z] != NA
    time_arrival[z] ~ dnorm(muArrive_CU[CU[z]], sdArrive_CU[CU[z]])
  }
  
  for(z in censored_arrival){ # time_arrival[z] = NA
    Y_arrival[z] ~ dinterval(time_arrival[z], cens_arrival[z,])
    time_arrival[z] ~ dnorm(muArrive_CU[CU[z]], sdArrive_CU[CU[z]])
  }
  
} # end model

###############################################################################
# Model 3: spawn-timing
###############################################################################

model_spawn <- function(){
  
  # # Define priors on arrival timing and spawn timing
  # for(i in 1:nRegions){
  #   muSpawn[i] ~ dnorm(255, 10^-2)
  #   sdSpawn[i] ~ dlnorm(log(20), 3^-2)
  # }
  
  #------------------------------------
  # Prior distributions on CU-level parameters
  #------------------------------------
  for(j in 1:nCUs){
    muSpawn_CU[j] ~ dnorm(255, 10^-2) #dnorm(muSpawn[region_CU[j]], sdSpawn[region_CU[j]])
    sdSpawn_CU[j] ~ dlnorm(log(20), 3^-2)
  }
  
  #------------------------------------
  # Peak spawn timing
  #------------------------------------
  for(z in not.censored_peak){# time_arrival[z] != NA
    mu.zero[z] ~ dnorm(muSpawn_CU[CU[z]] - time_peak[z], 1000)
  }
  
  for(z in censored_peak){ # time_arrival[z] = NA
    Y_peak[z] ~ dinterval(time_peak[z], cens_peak[z,])
    mu.zero[z] ~ dnorm(muSpawn_CU[CU[z]] - time_peak[z], 1000)
  }
  
  #------------------------------------
  # SD spawn timing
  #------------------------------------
  
  for(z in censored_start){ 
    Y_start[z] ~ dinterval(time_start[z], cens_start[z,])
    sd1[z] <- (time_start[z] - time_peak[z])/qnorm(0.025, 0, 1) - sdSpawn_CU[CU[z]]
    sd1.zero[z] ~ dnorm(0, 1000)
  }
  
  for(z in not.censored_start){
    sd1[z] <- (time_start[z] - time_peak[z])/qnorm(0.025, 0, 1) - sdSpawn_CU[CU[z]]
    sd1.zero[z] ~ dnorm(0, 1000)
  }
  
  for(z in not.censored_end){
    sd2[z] <- (time_end[z] - time_peak[z])/qnorm(0.975, 0, 1) - sdSpawn_CU[CU[z]]
    sd2.zero[z] ~ dnorm(0, 1000)
  }

  for(z in censored_end){
    Y_end[z] ~ dinterval(time_end[z], cens_end[z,])
    sd2[z] <- (time_end[z] - time_peak[z])/qnorm(0.975, 0, 1) - sdSpawn_CU[CU[z]]
    sd2.zero[z] ~ dnorm(0, 1000)
  }
  
  
} # end model