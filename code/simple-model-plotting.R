###############################################################################
# Code to plot estimates of arrival and spawn timing by region and CU
#
# Created by Steph Peacock, Analyst, Salmon Watersheds Program
# Contact: speacock at psf dot ca
# Date: January 18, 2023
###############################################################################
library(gplots)
###############################################################################
# Read in summarized output
###############################################################################

st_hyper <- read.csv("output/spawn-timing_regions.csv")
st_CU <- read.csv("output/spawn-timing_CUs.csv")

dat <- read.csv("output/NuSEDS-spawn-timing.csv")

# Create unique Species Qualified CU Name
dat$SQ_CU_NAME <- paste(dat$SPECIES_QUALIFIED, dat$CU_NAME, sep = " ")

#------------------------------------------------------------------------------
# Set up dataframe to hold arrival and spawn timing (st) for each region/species (hyperparameters)
#------------------------------------------------------------------------------

regionNames <- sort(unique(dat$REGION))
speciesNames <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye")
SQ <- list("CK", "CM", "CO", c("PKE", "PKO"), c("SEL", "SER"))

x <- c(1:365)
xDate <- as.Date(paste(1999, x, sep = "-"), format = "%Y-%j")

###############################################################################
# Region hyperparameters
###############################################################################

latOrder <- rev(c(9,1,6,7,5,2,8,4,3))

regionNames[latOrder]


colR <- PNWColors::pnw_palette("Bay", n = length(regionNames))

pdf(file = "output/figures/spawn-timing_byRegion.pdf", width = 8.5, height = 11, pointsize = 12)
par(mfrow = c(3,2), mar = c(3,4,1,1), oma = c(4,4,5,2))
for(s in 1:length(speciesNames)){
  # plot(xDate[c(200, 280)], c(0,0.1), "n", xlab = "", ylab = "", main = speciesNames[s], las = 1, bty = "l")
  plot(xDate, x, "n", xlim = xDate[c(159,300)], ylim = c(0,1), xlab = "", ylab = "", main = speciesNames[s], las = 1, bty = "l")
  axis(side = 1, at = xDate[which(as.numeric(strftime(xDate, "%d")) %in% c(1, 15))], labels = FALSE, tck = -0.02)
  for(i in 1:length(regionNames)){
    # Select parameters (par) for region and species
    par <- st_hyper[which(st_hyper$region == regionNames[latOrder[i]] & st_hyper$species == speciesNames[s]), ]
    y <- dnorm(x, mean = par$mu_spawn, sd = par$sd_spawn)
    # segments(x0 = x, x1 = x, y0 = 0, y1 = y, col = colR[i], lwd = 5)
    lines(xDate, y/max(y), col = colR[i], lwd = 2)
    # points(par$arrival, 0.1 - 0.01*i, col = colR[i], pch = 19)
    # segments(x0 = par$arrival_lower, x1 = par$arrival_upper, y0 = 0.1 - 0.01*i, y1 = 0.1 - 0.01*i, col = colR[i])
    if(!is.na(par$arrival)){
      points(as.Date(paste(1999, par$arrival, sep = "-"), format = "%Y-%j"), -0.01, bg = paste0(colR[i], 30), pch = 24, col = colR[i], cex = 1.5)
      # arrows(
      #   x0 = as.Date(paste(1999, par$arrival, sep = "-"), format = "%Y-%j"), 
      #   x1 = as.Date(paste(1999, par$arrival, sep = "-"), format = "%Y-%j"), 
      #   y0 = 1, 
      #   y1 = y[which(x == round(par$arrival))]/max(y), 
      #   col = colR[i], length = 0.08)
    }

  } # End regions
} # end species

plot(1,1,"n", bty = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
legend("center", lwd = 2, col = colR, regionNames[latOrder], cex = 1, bg = "white")#bty = "n")
mtext(side = 3, outer = TRUE, "Regional hyperparameters for spawn timing (curve) and arrival date (triangle)", line = 1)

dev.off()

###############################################################################
# Plot CUs
###############################################################################

# dummy data for CU distributions
yy <- array(99, dim = c(nrow(st_CU), length(x)))
for(i in 1:nrow(st_CU)){
  if(!is.na(st_CU$mu_spawn[i])){
    if(!is.na(st_CU$cuid[i])){
    dum <- dnorm(x, mean = st_CU$mu_spawn[i], sd = st_CU$sd_spawn[i])
    yy[i, ] <- dum/max(dum)
    }
  }
}


for(s in 1:length(SQ)){
  
  # Subset dat for that species
  
  jagsDat <- readRDS(paste0("output/jagsDat_", speciesNames[s], ".rds"))
  
  dat.ss <- dat[which(dat$SPECIES == speciesNames[s] & !is.na(dat$CU_NAME)), ]
  
  # Extract regions and CUs that have data
  regions <- sort(unique(dat.ss$REGION))
  CUs_all <- sort(unique(dat.ss$SQ_CU_NAME))
  
  for(r in 1:length(regions)){
    
    # Regional hyperparamters
    par <- st_hyper[which(st_hyper$region == regions[r] & st_hyper$species == speciesNames[s]), ]
    y <- dnorm(x, mean = par$mu_spawn, sd = par$sd_spawn)
    
    # Subset data
    ind <- which(st_CU$SPECIES_QUALIFIED %in% SQ[[s]] & st_CU$region == regions[r])
    if(length(ind) == 1){
      yyy <- t(as.matrix(yy[ind,]))
    } else {
      yyy <- yy[ind,]
    }
    
   CUs <- sort(unique(st_CU$SQ_CU_NAME[st_CU$SPECIES_QUALIFIED %in% SQ[[s]] & st_CU$region == regions[r]]))
  
   
   colCU <- PNWColors::pnw_palette("Bay", n = length(CUs))
   
   # Start new pdf
   pdf(file = paste0("output/figures/spawn-timing_", speciesNames[s], "_", regions[r], ".pdf"), width = 8.5, height = 11, pointsize = 12)
   par(mfrow = c(4, 2), mar = c(4,4,2,1), oma = c(4,4,5,2))
   
   
  # Legend
   plot(1,1,"n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
   legend("center", pch = c(1, NA, NA, NA), lwd = c(NA, 3, 0.8, 2), col = c(paste0(colCU[1], 50), grey(0.6), grey(0.8), colCU[1]), legend = c("NuSEDS data", "regional hyperdistribution", "other CUs", "focal CU"), bty = 'n')
   
   mtext(side = 3, outer = TRUE, paste("Spawn timing for", regions[r], speciesNames[s]))
   for(i in 1:length(CUs)){
     if(is.na(yyy[i,1]) | yyy[i,1] != 99){
     
     plot(xDate, x, "n", ylim = c(0,1), xlab = "", ylab = "", las = 1, bty = "l", yaxt = "n")
     
     cux <- strsplit(CUs[i], split = " ")[[1]]
     cux <- paste(cux[2:length(cux)], collapse = " ")
     mtext(side = 3, paste0(cux, " - ", strftime(xDate[round(st_CU$mu_spawn[which(st_CU$SQ_CU_NAME == CUs[i])])], format = "%b %d")), col = colCU[i], font = 2, cex = 0.8, adj = 0)
     
     axis(side = 1, at = xDate[which(as.numeric(strftime(xDate, "%d")) %in% c(1, 15))], labels = FALSE, tck = -0.02)
     
     
     # Add all CUs for given species/region in grey
     if(length(CUs) > 1){
       for(j in 1:length(CUs)){
       lines(xDate, yyy[j, ], col = grey(0.8), lwd = 0.8)
       }
     }
     
     # Plot regional hyperparamter in thick grey
     lines(xDate, y/max(y), col = grey(0.6), lwd = 3)
     
     
     # Highlight selected CU and add data
     lines(xDate, yyy[i,], col = colCU[i], lwd = 2)
     
     ind.i <- which(jagsDat$CU == match(CUs[i], CUs_all))
     abline(h = c(0.2, 0.4, 0.6, 0.8), col = paste0(colCU[i], 30))
     text(rep(xDate[1], 4), c(0.2, 0.4, 0.6, 0.8), c("arrival", "start", "peak", "end"), adj = 0, col = colCU[i], cex = 0.8)
     points(xDate[jagsDat$arrival[ind.i]], runif(length(ind.i), 0.15, 0.25), col = paste0(colCU[i], 50))
     points(xDate[jagsDat$start[ind.i]], runif(length(ind.i), 0.35, 0.45), col = paste0(colCU[i], 50))
     points(xDate[jagsDat$peak[ind.i]], runif(length(ind.i), 0.55, 0.65), col = paste0(colCU[i], 50))
     points(xDate[jagsDat$end[ind.i]], runif(length(ind.i), 0.75, 0.85), col = paste0(colCU[i], 50))
    
    
   }} # end CU
  dev.off()
  } # end region
  } # end species


#------------------------------------------------------------------------------