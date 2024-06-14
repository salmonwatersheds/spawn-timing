###############################################################################
# Code to look at model fit to spawn timing data from NuSEDS to estimate
# arrival date, mean and sd of spawn timing for each CU
#
# Created by Steph Peacock, Analyst, Salmon Watersheds Program
# Contact: speacock at psf dot ca
# Date: October 12, 2022
###############################################################################

library(MCMCglmm)

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

#------------------------------------------------------------------------------
# Set up dataframe to hold arrival and spawn timing (st) for each region/species (hyperparameters)
#------------------------------------------------------------------------------

regionNames <- sort(unique(dat$REGION))
speciesNames <- c("Chinook", "Chum", "Coho", "Pink", "SockeyeLake", "SockeyeRiver")
n1 <- length(regionNames) * length(speciesNames)

st_hyper <- data.frame(
  region = rep(regionNames, each = length(speciesNames)),
  species = rep(speciesNames, length(regionNames)),
  
  # # Mean
  # arrival_mean = rep(NA, n1),
  # mu_spawn_mean = rep(NA, n1),
  # sd_spawn_mean = rep(NA, n1),
  # start_spawn_mean = rep(NA, n1),
  # end_spawn_mean = rep(NA, n1),
  # 
  # #Mode
  # arrival_mode = rep(NA, n1),
  # mu_spawn_mode = rep(NA, n1),
  # sd_spawn_mode = rep(NA, n1),
  # start_spawn_mode = rep(NA, n1),
  # end_spawn_mode = rep(NA, n1),
  
  # 50th quantile - USE THIS
  arrival_50 = rep(NA, n1),
  mu_spawn_50 = rep(NA, n1),
  sd_spawn_50 = rep(NA, n1),
  start_spawn_50 = rep(NA, n1),
  end_spawn_50 = rep(NA, n1)
)

#------------------------------------------------------------------------------
# Set up dataframe to hold arrival and spawn timing (st) for each CU
#------------------------------------------------------------------------------

cuNames <- sort(unique(dat$SQ_CU_NAME[!is.na(dat$CU_NAME)]))
n2 <- length(cuNames)

st_CU <- data.frame(
  region = rep(NA, n2),
  SPECIES_QUALIFIED = rep(NA, n2),
  SQ_CU_NAME = rep(NA, n2),
  FULL_CU_IN = rep(NA, n2),
  cuid = rep(NA, n2), # PSE-assigned cuid 
  
  # # Mean
  # arrival_mean = rep(NA, n2),
  # mu_spawn_mean = rep(NA, n2),
  # sd_spawn_mean = rep(NA, n2),
  # start_spawn_mean = rep(NA, n2),
  # end_spawn_mean = rep(NA, n2),
  # 
  # #Mode
  # arrival_mode = rep(NA, n2),
  # mu_spawn_mode = rep(NA, n2),
  # sd_spawn_mode = rep(NA, n2),
  # start_spawn_mode = rep(NA, n2),
  # end_spawn_mode = rep(NA, n2),
  
  # 50th quantile - USE THIS!
  arrival_50 = rep(NA, n2),
  mu_spawn_50 = rep(NA, n2),
  sd_spawn_50 = rep(NA, n2),
  start_spawn_50 = rep(NA, n2),
  end_spawn_50 = rep(NA, n2),
  
  n.arrival = rep(NA, n2), # Number of year-location observations for arrival time for that CUn.peak = rep(NA, n2), # Number of year-location observations for spawn time for that CU
  n.sd = rep(NA, n2)
)

# Match each CU to info from dat
for(i in 1:n2){
  ind <- which(dat$SQ_CU_NAME == cuNames[i])
  
  # Region
  dum <- unique(dat$REGION[ind])
  if(length(dum) > 1) stop("Not 1:1") else st_CU$region[i] <- dum
  
  # Species
  dum <- unique(dat$SPECIES_QUALIFIED[ind])
  if(length(dum) > 1) stop("Not 1:1") else st_CU$SPECIES_QUALIFIED[i] <- dum
  
  # CU_NAME
  dum <- unique(dat$SQ_CU_NAME[ind])
  if(length(dum) > 1) stop("Not 1:1") else st_CU$SQ_CU_NAME[i] <- dum
  
  # cuid
  dum <- unique(dat$cuid[ind])
  if(length(dum) > 1) stop("Not 1:1") else st_CU$cuid[i] <- dum
  
  
  # FULL_CU_IN
  dum <- unique(dat$FULL_CU_IN[ind])
  if(length(dum) > 1) stop("Not 1:1") else st_CU$FULL_CU_IN[i] <- dum
  
}

###############################################################################
# Read in mcmc output from model fits and extract posterior info
###############################################################################

# June 28, 2023: Use 50th quantile of posteriors, which is between mean and HPD
# and represents time when the majority of salmon will have started/ended
# spawning. Separate hyper-parameters for lake- and river-sockeye based on 
# significant differences between the two life-histories in spawn timing for
# some regions.

for(s in 1:length(speciesNames)){
  
  # Selected species (ss)
  ss <- speciesNames[s]
  
  fit <- readRDS(file = paste0("output/simple-model-fit_", ss, ".rds"))

  # Extract parameter names
  pnames <- colnames(fit[[1]])
  
  # Check convergence
  R <- gelman.diag(fit)
  if(length(which(R[[1]][, 1] > 1.1)) > 0){
    ind <- which(R[[1]][, 1] > 1.1)
    warning(paste0("Convergence issues for ", ss, " parameters."))
    cat(paste(ss, ":", pnames[ind], "\n"))
  }
  
  # # Turn list of chains into array
  # fit2 <- array(NA, dim = c(length(fit), nrow(fit[[1]]), ncol(fit[[1]])))
  # for(i in 1:length(fit)){
  #   fit2[i, , ] <- fit[[i]]
  # }

  # Rbind list of chains into single matrix for HPDInterval to avoid list output
  fit3 <- as.mcmc(rbind(fit[[1]], fit[[2]], fit[[3]]))

  # modes <- posterior.mode(fit)
  # means <- apply(fit3, 2, mean)

  # Extract output
  # Subset dat for that species
  dat.ss <- dat[which(dat$SPECIES == ss & !is.na(dat$CU_NAME)), ]
  jagsDat <- readRDS(paste0("output/jagsDat_", ss, ".rds"))
  
  regionNames.ss <- sort(unique(dat.ss$REGION))
  cuNames.ss <- sort(unique(dat.ss$SQ_CU_NAME)) 
  
  #----------------------------------------------------------------------------
  # Regional hyperparameters
  #----------------------------------------------------------------------------
  
  for(j in 1:length(regionNames.ss)){
    
    # Input arrival estimates (mean, mode, 50th quantile)
    # st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("arrival_mean")] <- c(as.numeric(means[paste0("alpha_R[", j, "]")]))
    # st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("arrival_mode")] <- c(as.numeric(modes[paste0("alpha_R[", j, "]")]))
    st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("arrival_50")] <- quantile(fit3[, paste0("alpha_R[", j, "]")], 0.5)
    
    # Input spawn timing distribution (mean, mode, 50th quantile)
    # st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("mu_spawn_mean", "sd_spawn_mean")] <- c(as.numeric(means[paste0("mu_R[", j, "]")]), as.numeric(means[paste0("sig_R[", j, "]")]))
    # st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("mu_spawn_mode", "sd_spawn_mode")] <- c(as.numeric(modes[paste0("mu_R[", j, "]")]), as.numeric(modes[paste0("sig_R[", j, "]")]))
    st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("mu_spawn_50", "sd_spawn_50")] <- c(quantile(fit3[, paste0("mu_R[", j, "]")], 0.5), quantile(fit3[, paste0("sig_R[", j, "]")], 0.5))
    
    #---
    # Start spawn timing (mean)
     # st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("start_spawn_mean")] <- mean(fit3[, paste0("mu_R[", j, "]")] - 1.96 * fit3[, paste0("sig_R[", j, "]")])
     # st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("start_spawn_mode")] <- posterior.mode(fit3[, paste0("mu_R[", j, "]")] - 1.96 * fit3[, paste0("sig_R[", j, "]")])
     st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("start_spawn_50")] <- quantile(fit3[, paste0("mu_R[", j, "]")] - 1.96 * fit3[, paste0("sig_R[", j, "]")], 0.5)
    
    # Plotting
    if(1 == 2){
      # Peak
      hist(fit3[, paste0("mu_R[", j, "]")], breaks = seq(-2500, 500, 7), xlim = c(1, 365), xaxs = "i")
      abline(v = mean(fit3[, paste0("mu_R[", j, "]")]), col = 5, lwd =2)
      abline(v = posterior.mode(fit3[, paste0("mu_R[", j, "]")]), col = 2, lwd = 2)
      
      # Start
      hist(fit3[, paste0("mu_R[", j, "]")] - 1.96 * fit3[, paste0("sig_R[", j, "]")], breaks = seq(-2500, 500, 7), xlim = c(1, 365), xaxs = "i")
      abline(v = temp_mode, col = 2, lwd = 2)
      abline(v = temp_hpdi, col = 4, lwd = 2, lty = 2)
      abline(v = quantile(fit3[, paste0("mu_R[", j, "]")] - 1.96 * fit3[, paste0("sig_R[", j, "]")], 0.5), col = 3, lwd =2)
      abline(v = mean(fit3[, paste0("mu_R[", j, "]")] - 1.96 * fit3[, paste0("sig_R[", j, "]")]), col = 5, lwd =2)
    }
    
    #---
    # End spawn timing incl. 95% CI
     st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("end_spawn_mean")] <- mean(fit3[, paste0("mu_R[", j, "]")] + 1.96 * fit3[, paste0("sig_R[", j, "]")])
     st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("end_spawn_mode")] <- posterior.mode(fit3[, paste0("mu_R[", j, "]")] + 1.96 * fit3[, paste0("sig_R[", j, "]")])
     st_hyper[which(st_hyper$region == regionNames.ss[j] & st_hyper$species == ss), c("end_spawn_50")] <- quantile(fit3[, paste0("mu_R[", j, "]")] + 1.96 * fit3[, paste0("sig_R[", j, "]")], 0.5)
    
  } # end regions
  
  
  #----------------------------------------------------------------------------
  # Conservation Units summaries
  #----------------------------------------------------------------------------
  
  for(i in 1:length(cuNames.ss)){
  
    # Input arrival estimates
    # st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("arrival_mean")] <- c(as.numeric(means[paste0("alpha_CU[", i, "]")]))
    # st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("arrival_mode")] <- c(as.numeric(modes[paste0("alpha_CU[", i, "]")]))
    st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("arrival_50")] <- quantile(fit3[, paste0("alpha_CU[", i, "]")], 0.5)
    
    # Input spawn timing distribution
    
    # st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("mu_spawn_mean", "sd_spawn_mean")] <- c(as.numeric(means[paste0("mu_CU[", i, "]")]), as.numeric(means[paste0("sig_CU[", i, "]")]))
    # st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("mu_spawn_mode", "sd_spawn_mode")] <- c(as.numeric(modes[paste0("mu_CU[", i, "]")]), as.numeric(modes[paste0("sig_CU[", i, "]")]))
    st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("mu_spawn_50", "sd_spawn_50")] <- c(quantile(fit3[, paste0("mu_CU[", i, "]")], 0.5), quantile(fit3[,paste0("sig_CU[", i, "]")], 0.5))
    
    #---
    # Start spawn timing incl. 95% CI
    # st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("start_spawn_mean")] <- mean(fit3[, paste0("mu_CU[", i, "]")] - 1.96 * fit3[, paste0("sig_CU[", i, "]")])
    # st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("start_spawn_mode")] <- posterior.mode(fit3[, paste0("mu_CU[", i, "]")] - 1.96 * fit3[, paste0("sig_CU[", i, "]")])
    st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("start_spawn_50")] <- quantile(fit3[, paste0("mu_CU[", i, "]")] - 1.96 * fit3[, paste0("sig_CU[", i, "]")], 0.5)
    
    #---
    # End spawn timing incl. 95% CI
    # st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("end_spawn_mean")] <- mean(fit3[, paste0("mu_CU[", i, "]")] + 1.96 * fit3[, paste0("sig_CU[", i, "]")])
    # st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("end_spawn_mode")] <- posterior.mode(fit3[, paste0("mu_CU[", i, "]")] + 1.96 * fit3[, paste0("sig_CU[", i, "]")])
    st_CU[which(st_CU$SQ_CU_NAME == cuNames.ss[i]), c("end_spawn_50")] <- quantile(fit3[, paste0("mu_CU[", i, "]")] + 1.96 * fit3[, paste0("sig_CU[", i, "]")], 0.5)
    
    st_CU$n.arrival[which(st_CU$SQ_CU_NAME == cuNames.ss[i])] <- sum(!is.na(jagsDat$arrival[jagsDat$CU == i]))
    st_CU$n.peak[which(st_CU$SQ_CU_NAME == cuNames.ss[i])] <- sum(!is.na(jagsDat$peak[jagsDat$CU == i]))
    st_CU$n.sd[which(st_CU$SQ_CU_NAME == cuNames.ss[i])] <- sum(!is.na(jagsDat$start[jagsDat$CU == i]), !is.na(jagsDat$end[jagsDat$CU == i]))
    
  } # end CUs

} # end species



write.csv(st_hyper, file = paste0("output/spawn-timing_regions_50_lakeriversep_brks2_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv(st_CU, file = paste0("output/spawn-timing_CUs_50_lakeriversep_brks2_", Sys.Date(), ".csv"), row.names = FALSE)

# ###############################################################################
# # Compare mean, mode 50
# ###############################################################################
# 
# plot(st_CU$start_spawn_mode, st_CU$start_spawn_50)
# abline(a = 0, b = 1, lty = 2)
# length(which(abs(st_CU$start_spawn_mode - st_CU$start_spawn_50) > 1)) #26
# length(which(abs(st_CU$start_spawn_mode - st_CU$start_spawn_50) > 14)) #9
# st_CU[which(abs(st_CU$start_spawn_mode - st_CU$start_spawn_50) > 14), c("SQ_CU_NAME", "start_spawn_mean", "start_spawn_mode", "start_spawn_50")]
# 
# plot(st_CU$end_spawn_mode, st_CU$end_spawn_50)
# abline(a = 0, b = 1, lty = 2)
# length(which(abs(st_CU$end_spawn_mode - st_CU$end_spawn_50) > 1)) #26
# length(which(abs(st_CU$end_spawn_mode - st_CU$end_spawn_50) > 14)) #9
# st_CU[which(abs(st_CU$end_spawn_mode - st_CU$end_spawn_50) > 14), c("SQ_CU_NAME", "end_spawn_mean", "end_spawn_mode", "end_spawn_50")]
# 
# # Which CUs have more than a day difference?
# ind <- which(abs(st_CU$sd_spawn_mean - st_CU$sd_spawn_mode) > 40)
# abs(st_CU$sd_spawn_mean - st_CU$sd_spawn_mode)[ind]
# st_CU[ind, c("SQ_CU_NAME", "sd_spawn_mean", "sd_spawn_mode", "sd_spawn_50")]
# st_CU[ind, c("SQ_CU_NAME", "start_spawn_mean", "start_spawn_mode")]
# 
# 
# for(i in ind){
#   
#   # Selected species (ss)
#   ss <- speciesNames[as.numeric(factor(st_CU$SPECIES_QUALIFIED[i], levels = unique(st_CU$SPECIES_QUALIFIED)))]
#   
#   # Extract output
#   # Subset dat for that species
#   dat.ss <- dat[which(dat$SPECIES == ss & !is.na(dat$CU_NAME)), ]
#   jagsDat <- readRDS(paste0("output/jagsDat_", ss, ".rds"))
#   
#   regionNames.ss <- sort(unique(dat.ss$REGION))
#   cuNames.ss <- sort(unique(dat.ss$SQ_CU_NAME)) 
#   
#   
#   fit <- readRDS(file = paste0("output/simple-model-fit_", ss, ".rds"))
#   fit3 <- as.mcmc(rbind(fit[[1]], fit[[2]], fit[[3]]))
#   
#   st_CU$cuid[i]
#   j <- which(cuNames.ss == st_CU$SQ_CU_NAME[i])
#   hist(fit3[, paste0("mu_CU[", j, "]")] - 1.96 * fit3[, paste0("sig_CU[", j, "]")], breaks = seq(-10000, 10000, 7), xlim = c(0, 365), xaxs = 'i')
#   abline(v = st_CU$start_spawn_mean[i], col = 2, lwd = 2)
#   abline(v = st_CU$start_spawn_mode[i], col = 4, lwd = 2)
#   abline(v = st_CU$start_spawn_50[i], col = 5, lwd = 2)
#   
# }
# 
# ###############################################################################
# ###############################################################################
# # # Region hyperparameter
# # nCU <- jagsDat$nCUs
# # nR <- jagsDat$nRegions
# # 
# # ind <- list(
# #   muArrive = match(paste0("muArrive[", 1:nR, "]"), pnames),
# #   # sdArrive = match(paste0("sdArrive[", 1:nR, "]"), pnames),
# #   muArrive_CU = match(paste0("muArrive_CU[", 1:nCU, "]"), pnames),
# #   muSpawn = match(paste0("muSpawn[", 1:nR, "]"), pnames),
# #   # sdSpawn = match(paste0("sdSpawn[", 1:nR, "]"), pnames),
# #   muSpawn_CU = match(paste0("muSpawn_CU[", 1:nCU, "]"), pnames),
# #   sdSpawn_CU = match(paste0("sdSpawn_CU[", 1:nCU, "]"), pnames)
# # )
# 
# # Plot regional output
# colsCU <- viridis(jagsDat$nCUs)
# 
# quartz(width = 8, height = 6, pointsize = 10)
# par(mfrow = c(3,2))
# 
# for(i in 1:nR){ # for each region
#   
#   x <- seq(min(S[[1]][ind$muArrive_CU[unique(jagsDat$CU[jagsDat$region == i])], 'Mean']), max(S[[1]][ind$muArrive_CU[unique(jagsDat$CU[jagsDat$region == i])], 'Mean']), 0.1)
#   
#   if(length(x) == 1) x <- seq(x - 15, x + 15, 0.1)
#   
#   y <- dnorm(x, mean = S[[1]][ind$muArrive[i], 'Mean'], sd = S[[1]][ind$sdArrive[i], "Mean"])
#   
#   plot(x, y/max(y), "l", lwd = 4, col = grey(0.8), xlab = "", ylab = "Arrival time", bty = "l", las = 1, yaxt = "n", main = regions[i])
#   
#   for(j in unique(jagsDat$CU[jagsDat$region == i])){
#     abline(v = S[[1]][ind$muArrive_CU[j], 'Mean'], col = colsCU[j])
#   } # end j
# } # end i
# 
# 
# 
# ####
# 
# # Choose region
# r <- 1
# 
# # Choose CU
# cu_ind <- which(regions_CU == regions[r])
# 
# cols1 <- viridis(length(cu_ind), alpha = 0.3)
# cols2 <- viridis(length(cu_ind), alpha = 1)
# 
# hist(arrival[jagsDat$CU %in% cu_ind, ], freq = FALSE, breaks = seq(0, 366, 7), yaxs = "i", border = NA)
# for(i in 1:length(cu_ind)){
#   hist(arrival[jagsDat$CU == cu_ind[i], ], freq = FALSE, add = TRUE, border = NA, col = cols1[i], xpd = NA, yaxs = "i", breaks = seq(0, 366, 7))
#   
#   y <- dnorm(dumx, mean = modes[ind$muArrive_CU[cu_ind[i]]], 0.5*modes[ind$sdArrive[r]])
#   lines(dumx, y/max(y)*3500, col = cols2[i])
#   
# }
# 
# # Mean times match but not SD
# # What about spawn timing?
# i <- 2
# 
# hist(peak[jagsDat$CU == cu_ind[i], ], freq = FALSE, breaks = seq(0, 366, 7), yaxs = "i", border = "white", xlim = c(200, 300))
# lines(density(peak[jagsDat$CU == cu_ind[i], ], na.rm = TRUE), col = grey(0.6))
# 
# hist(start[jagsDat$CU == cu_ind[i], ], freq = FALSE, breaks = seq(0, 366, 7), yaxs = "i", border = "white", col = "#00FF0030", add = TRUE)
# hist(end[jagsDat$CU == cu_ind[i], ], freq = FALSE, breaks = seq(0, 366, 7), yaxs = "i", border = "white", col = "#FF000030", add = TRUE)
# 
# y <- dnorm(dumx, mean = modes[ind$muSpawn_CU[cu_ind[i]]], modes[ind$sdSpawn_CU[cu_ind[i]]])
# lines(dumx, y/max(y)*0.04, lwd =1.5)
# 
# ##
# # Look at regional muArrive compared to prior
# 
# hist(fit[[1]][, 1], freq=FALSE)
# dumx <- seq(1, 365, 0.1)
# lines(dumx, dnorm(dumx, 230, 20)*10)
# 
# #------------------------------------------------------------------------------
# # compare approaches
# #------------------------------------------------------------------------------
# spQ <- data.frame(SPECIES_QUALIFIED = c( "CK","CM","CO","PKE", "PKO", "SEL", "SER"),
#                   species = c("Chinook", "Chum", "Coho", "Pink", "Pink", "Sockeye", "Sockeye"))
# st_CU$species <- spQ$species[match(st_CU$SPECIES_QUALIFIED, spQ$SPECIES_QUALIFIED)]
# regionCols <- rainbow(9)
# regionNames <- c("Yukon", "AlaskaTransboundary", "Nass" , "Skeena", "HaidaGwaii", "Central Coast",  "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")
# 
# for(s in 1:5){
#   n.s <- length(which(st_CU$species == unique(spQ$species)[s]))
#   plot(c(0,365), c(1, n.s), "n", xlim = c(100, 365))
#   n.sr <- 0
#   for(r in 1:length(regionNames)){
#     st <- st_CU[which(st_CU$species == unique(spQ$species)[s] & st_CU$region == regionNames[r]), ]
#     if(nrow(st) > 0){
#       # 90% hpdi
#       segments(x0 = st$start_spawn_lower90, 
#                x1 = st$end_spawn_upper90, 
#                y0 = n.s - n.sr - c(1:nrow(st)), 
#                y1 = n.s - n.sr - c(1:nrow(st)), 
#                lwd = 10, col = paste0(regionCols[r], 30))
#       # 80% hpdi
#       segments(x0 = st$start_spawn_lower80, 
#                x1 = st$end_spawn_upper80, 
#                y0 = n.s - n.sr - c(1:nrow(st)), 
#                y1 = n.s - n.sr - c(1:nrow(st)), 
#                lwd = 6, col = paste0(regionCols[r], 40))
#       
#       # mode
#       segments(x0 = st$start_spawn, 
#                x1 = st$end_spawn, 
#                y0 = n.s - n.sr - c(1:nrow(st)), 
#                y1 = n.s - n.sr - c(1:nrow(st)), 
#                lwd = 2, col = regionCols[r])
#     }
#     n.sr <- n.sr + nrow(st)
#   } # end region
#   mtext(side = 3, line = 1, unique(spQ$species)[s])
# }
