###############################################################################
# Do regional hyperparameters differ when separating out lake-type and river-
# type sockeye populations?
# 
# June 27, 2023
# Steph Peacock
###############################################################################
dat <- read.csv("output/NuSEDS-spawn-timing.csv")

# Create unique Species Qualified CU Name
dat$SQ_CU_NAME <- paste(dat$SPECIES_QUALIFIED, dat$CU_NAME, sep = " ")

dat <- subset(dat, dat$SPECIES == "Pink")

species <- c("Pink", "PKO", "PKE")
allRegions <- sort(unique(dat$REGION))

# Load JAGS mcmc output for sockeye fits
fit_Pink <- readRDS("output/simple-model-fit_Pink.rds")
fit_PKO <- readRDS("output/simple-model-fit_PKO.rds")
fit_PKE <- readRDS("output/simple-model-fit_PKE.rds")
fit <- list(fit_Pink, fit_PKO, fit_PKE)

###############################################################################
# Extract hyper parameters
###############################################################################

hyperpar <- array(NA, 
                  dim = c(length(species), length(allRegions), 3, 5000, 4), 
                  dimnames = list(species, allRegions, NULL, NULL, c("mu", "sig", "start", "end")))

for(i in 1:3){ # For each species
  ss <- c("Sockeye", "SEL", "SER")[i]
  if(i == 1){
    dat.ss <- dat[which(dat$SPECIES == ss & !is.na(dat$CU_NAME)), ]
  } else {
    dat.ss <- dat[which(dat$SPECIES_QUALIFIED == ss & !is.na(dat$CU_NAME)), ]
  }
  
  regions <- sort(unique(dat.ss$REGION))
  nR <- length(regions)
  indR <- match(regions, allRegions)
  
  # Extract parameter names
  pnames <- colnames(fit[[i]][[1]])
  
  for(j in 1:nR){ # For each region
    hyperpar[i, indR[j], , , "mu"] <- fit[[i]][[1]][, grep("mu_R", pnames)[j]]
    hyperpar[i, indR[j], , , "sig"] <- fit[[i]][[1]][, grep("sig_R", pnames)[j]]
  }

}

# Calculate start and end days
hyperpar[, , , , "start"] <- hyperpar[, , , , "mu"] - 1.96 * hyperpar[, , , , "sig"]
hyperpar[, , , , "end"] <- hyperpar[, , , , "mu"] + 1.96 * hyperpar[, , , , "sig"]

###############################################################################
# Compare start and end dates for each region
###############################################################################
cols <- c("#2c7bb6", "#d7191c")

DOY <- c(1:366)
xDate <- as.Date(paste(2000, DOY, sep = "-"), format = "%Y-%j")

quartz(width = 8.5, height = 9, pointsize = 12)
par(mfrow  = c(3,2), mar = c(3, 3, 1, 1), oma = c(2, 2, 1, 0))

for(j in 1:length(allRegions)){ # for each region
  
  if(j == 2){
    ns <- 2
    densStart <- list(density(hyperpar[1, j, , , "start"]), 
                      density(hyperpar[2, j, , , "start"]), 
                      list(x = NA, y = NA))
    
    # densEnd <- list(density(hyperpar[1, j, , , "end"]), 
    #                 density(hyperpar[2, j, , , "end"]), 
    #                 list(x = NA, y = NA))
    
  } else {
    ns <- 3
  densStart <- list(density(hyperpar[1, j, , , "start"]), 
               density(hyperpar[2, j, , , "start"]), 
               density(hyperpar[3, j, , , "start"]))
  
  # densEnd <- list(density(hyperpar[1, j, , , "end"]), 
  #                   density(hyperpar[2, j, , , "end"]), 
  #                   density(hyperpar[3, j, , , "end"]))
  }
  
  plot(c(150, 280), c(0, max(densStart[[1]]$y, densStart[[2]]$y, densStart[[3]]$y, na.rm = TRUE)), "n") #, densEnd[[1]]$y, densEnd[[2]]$y, densEnd[[3]]$y
  for(i in 1:ns){
    lines(densStart[[i]]$x, densStart[[i]]$y, lty = i)##, col = cols[1]
    abline(v = quantile(hyperpar[i, j, , , "start"], 0.5), lty = i, lwd = 2)#, col = cols[1], lwd = 2)
    # lines(densEnd[[i]]$x, densEnd[[i]]$y, col = cols[2], lty = i)
    # abline(v = quantile(hyperpar[i, j, , , "end"], 0.5), lty = i, col = cols[2], lwd = 2)
    
  }
  legend("topleft", lty = 1:3, legend = species, bty = "n")
  mtext(side = 3, allRegions[j])
  
  u <- par('usr')
  indPKO <- which(dat$SPECIES_QUALIFIED == "PKO" & dat$ANALYSIS_YR %in% seq(1901, 2021, 2) & dat$REGION == allRegions[j])
  indPKE <- which(dat$SPECIES_QUALIFIED == "PKE" & dat$ANALYSIS_YR %in% seq(1900, 2022, 2) & dat$REGION == allRegions[j])
  
  nPKO <- c(length(unique(dat$POP_ID[indPKO])), length(unique(dat$cuid[indPKO])))
  nPKE <- c(length(unique(dat$POP_ID[indPKE])), length(unique(dat$cuid[indPKE])))
  text(u[1], mean(u[3:4]), paste0("  PKO: ", nPKO[1], " pops, ", nPKO[2], " CUs \n   PKE:", nPKE[1], " pops, ", nPKE[2], " CUs"), adj = 0)
  }