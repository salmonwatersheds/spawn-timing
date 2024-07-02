###############################################################################
# Code to summarize the start, end, and peak spawning dates for each population 
# and CU and assign data quality scores
#
# Created by Steph Peacock, Analyst, Salmon Watersheds Program
# Contact: speacock at psf dot ca
# Date: May 11, 2023
###############################################################################

library(dplyr)
library(dclone)
library(viridis)     

###############################################################################
# Read-in compiled spawn-timing data
# See data-compilation.R for how these data were compiled from raw NuSEDS 
###############################################################################

dat <- read.csv("output/NuSEDS-spawn-timing.csv")

# Need to have unique population-cu because even/odd pink populations
# must be split into two CUs
dat$POPULATION_CU <- paste(dat$POPULATION, dat$FULL_CU_IN, sep = "_")

#------------------------------------------------------------------------------
# Calculate average start, peak, and end per year
#------------------------------------------------------------------------------
# Jan 23, 2023 Need to account for year-end changes in DOY

# Start 
start <- cbind(as.numeric(dat$START_SPAWN_DT_FROM), as.numeric(dat$START_SPAWN_DT_TO))
# Check: Are any from < to 
if(length(which(start[, 2] - start[, 1] < 0)) > 0) stop("START_SPAWN_DT_TO < START_SPAWN_DT_FROM")
dat$START_SPAWN_DT <- apply(start, 1, mean, na.rm = TRUE)

# If start (or end) is < DOY100, add 365 so that the fitting doesn't get confused.
# I don't think this actually applies to any CUs
dat$START_SPAWN_DT[which(dat$START_SPAWN_DT < 100)] <- dat$START_SPAWN_DT[which(dat$START_SPAWN_DT < 100)] + 365

hist(dat$START_SPAWN_DT, breaks = seq(0, 465, 7), main = "START_SPAWN_DT", xlab = "", yaxs = "i", border = NA, col = grey(0.8), ylim = c(0, 1000))
# Peak
peak <- cbind(as.numeric(dat$PEAK_SPAWN_DT_FROM), as.numeric(dat$PEAK_SPAWN_DT_TO)) 
# Check: Are any from < to 
if(length(which(peak[, 2] - peak[, 1] < 0)) > 0){
  print(paste("START_SPAWN_DT_TO < START_SPAWN_DT_FROM for\n", dat$POPULATION_CU[which(peak[, 2] - peak[, 1] < 0)], collapse = ', '))
}
# For sockeye there is one where the TO doesn't make sense (Adams SEL) -> change to NA
peak[which((peak[, 2] - peak[, 1]) < 0), 2] <- NA

dat$PEAK_SPAWN_DT <- apply(peak, 1, mean, na.rm = TRUE)
dat$PEAK_SPAWN_DT[is.na(dat$PEAK_SPAWN_DT)] <- NA

hist(dat$PEAK_SPAWN_DT, breaks = seq(0, 465, 7), main = "PEAK_SPAWN_DT", xlab = "", yaxs = "i", border = NA, col = grey(0.8), ylim = c(0, 1000))


# If end is < DOY100, add 365 so that the fitting doesn't get confused.
dat$PEAK_SPAWN_DT[which(dat$PEAK_SPAWN_DT < 100)] <- dat$PEAK_SPAWN_DT[which(dat$PEAK_SPAWN_DT < 100)] + 365
hist(dat$PEAK_SPAWN_DT, breaks = seq(0, 465, 7), add= TRUE, border = NA, col = "#FF000050")


# End - this one has numerous populations with end[, 2] < end[, 1]
end <- cbind(as.numeric(dat$END_SPAWN_DT_FROM), as.numeric(dat$END_SPAWN_DT_TO)) # Check: Are any from < to 

if(length(which(end[, 2] - end[, 1] < 0)) > 0){
  end[which(end[, 2] - end[, 1] < 0), 2] <- 365 + end[which(end[, 2] - end[, 1] < 0), 2]
  warning(paste0(ss, ": END_SPAWN_DT_TO < END_SPAWN_DT_FROM"))
}
dat$END_SPAWN_DT <- apply(end, 1, mean, na.rm = TRUE)
hist(dat$END_SPAWN_DT, breaks = seq(0, 465, 7), main = "END_SPAWN_DT", xlab = "", yaxs = "i", border = NA, col = grey(0.8), ylim = c(0, 1000))

# If end is < DOY100, add 365 so that the fitting doesn't get confused.
dat$END_SPAWN_DT[which(dat$END_SPAWN_DT < 100)] <- dat$END_SPAWN_DT[which(dat$END_SPAWN_DT < 100)] + 365
hist(dat$END_SPAWN_DT, breaks = seq(0, 465, 7), add= TRUE, border = NA, col = "#FF000050")


###############################################################################
# Summarize by location
###############################################################################

# The output by location is data that is used in the analysis of factors affecting
# spawn timing by Wilson and Peacock

dat2 <- data.frame(
  POPULATION_CU = sort(unique(dat$POPULATION_CU))
)

# Create index to pull variables from dat for each POPULATION_CU
ind <- match(dat2$POPULATION_CU, dat$POPULATION_CU)

dat2$POPULATION <- dat$POPULATION[ind]
dat2$POP_ID <- dat$POP_ID[ind]
dat2$SPECIES <- dat$SPECIES[ind]
dat2$Y_LAT <- dat$Y_LAT[ind]
dat2$X_LONGT <- dat$X_LONGT[ind]
dat2$CU_NAME <- dat$CU_NAME[ind]
dat2$CU_ACRO <- dat$CU_ACRO[ind]
dat2$FULL_CU_IN <- dat$FULL_CU_IN[ind]
dat2$cuid <- dat$cuid[ind]
dat2$IS_INDICATOR <- dat$IS_INDICATOR[ind]
dat2$REGION <- dat$REGION[ind]

dat2$year_start <- tapply(dat$ANALYSIS_YR, dat$POPULATION_CU, min)
dat2$year_final <- tapply(dat$ANALYSIS_YR, dat$POPULATION_CU, max)

#------------------------------------------------------------------------------
# Summarize start
#------------------------------------------------------------------------------
calc_nYears <- function(x){
  sum(!is.na(x))
}


dat2$START_SPAWN_DT <- tapply(dat$START_SPAWN_DT, dat$POPULATION_CU, mean, na.rm = TRUE)
dat2$nYears_START_SPAWN_DT <- tapply(dat$START_SPAWN_DT, dat$POPULATION_CU, calc_nYears)

# Calculate number of years when the start of spawning was estimated prior to the 
# start of the surveying (i.e., "part of run missed" for DQ score)
dat$START_SPAWN_DT_outside <- dat$START_SPAWN_DT < dat$START_DTT
dat2$nYearsOutside_START_SPAWN_DT <- tapply(dat$START_SPAWN_DT_outside, dat$POPULATION_CU, calc_nYears)

# Check:
# dat2[1:10, c("START_SPAWN_DT", "nYears_START_SPAWN_DT", "nYearsOutside_START_SPAWN_DT")]
# dat[dat$POPULATION_CU == dat2$POPULATION_CU[2],]

#------------------------------------------------------------------------------
# Summarize peak
#------------------------------------------------------------------------------
dat2$PEAK_SPAWN_DT <- tapply(dat$PEAK_SPAWN_DT, dat$POPULATION_CU, mean, na.rm = TRUE)
dat2$nYears_PEAK_SPAWN_DT <- tapply(dat$PEAK_SPAWN_DT, dat$POPULATION_CU, calc_nYears)

# Calculate number of years when the start of spawning was estimated prior to the 
# start of the surveying (i.e., "part of run missed" for DQ score)
dat$PEAK_SPAWN_DT_outside <- dat$PEAK_SPAWN_DT < dat$START_DTT | dat$PEAK_SPAWN_DT > dat$END_DTT
dat2$nYearsOutside_PEAK_SPAWN_DT <- tapply(dat$PEAK_SPAWN_DT_outside, dat$POPULATION_CU, calc_nYears)

# Check:
# dat2[1:10, c("PEAK_SPAWN_DT", "nYears_PEAK_SPAWN_DT", "nYearsOutside_PEAK_SPAWN_DT")]

#------------------------------------------------------------------------------
# Summarize end
#------------------------------------------------------------------------------
dat2$END_SPAWN_DT <- tapply(dat$END_SPAWN_DT, dat$POPULATION_CU, mean, na.rm = TRUE)
dat2$nYears_END_SPAWN_DT <- tapply(dat$END_SPAWN_DT, dat$POPULATION_CU, calc_nYears)

# Calculate number of years when the start of spawning was estimated prior to the 
# start of the surveying (i.e., "part of run missed" for DQ score)
dat$END_SPAWN_DT_outside <- dat$END_SPAWN_DT > dat$END_DTT
dat2$nYearsOutside_END_SPAWN_DT <- tapply(dat$END_SPAWN_DT_outside, dat$POPULATION_CU, calc_nYears)

# Check:
# dat2[1:10, c("END_SPAWN_DT", "nYears_END_SPAWN_DT", "nYearsOutside_END_SPAWN_DT")]

#------------------------------------------------------------------------------
# Assign data quality score
#------------------------------------------------------------------------------

# 1 = good (>=5 years of direct counts) 
# 2 = limited evidence (<5 years of direct counts) 
# 3 = 1 -2 years of evidence but with low numbers, or part of the run was missed

dat2$DQ_START <- 3
dat2$DQ_START[which((dat2$nYears_START_SPAWN_DT - dat2$nYearsOutside_START_SPAWN_DT) >= 5)] <- 1
dat2$DQ_START[which((dat2$nYears_START_SPAWN_DT - dat2$nYearsOutside_START_SPAWN_DT) < 5 & (dat2$nYears_START_SPAWN_DT - dat2$nYearsOutside_START_SPAWN_DT) > 0)] <- 2
hist(dat2$DQ_START)


dat2$DQ_PEAK <- 3
dat2$DQ_PEAK[which((dat2$nYears_PEAK_SPAWN_DT - dat2$nYearsOutside_PEAK_SPAWN_DT) >= 5)] <- 1
dat2$DQ_PEAK[which((dat2$nYears_PEAK_SPAWN_DT - dat2$nYearsOutside_PEAK_SPAWN_DT) < 5 & (dat2$nYears_PEAK_SPAWN_DT - dat2$nYearsOutside_PEAK_SPAWN_DT) > 0)] <- 2
hist(dat2$DQ_PEAK)


dat2$DQ_END <- 3
dat2$DQ_END[which((dat2$nYears_END_SPAWN_DT - dat2$nYearsOutside_END_SPAWN_DT) >= 5)] <- 1
dat2$DQ_END[which((dat2$nYears_END_SPAWN_DT - dat2$nYearsOutside_END_SPAWN_DT) < 5 & (dat2$nYears_END_SPAWN_DT - dat2$nYearsOutside_END_SPAWN_DT) > 0)] <- 2
hist(dat2$DQ_END)

write.csv(dat2, file = paste("output/life-cycle-timing_spawning_byLocation_", Sys.Date(), ".csv"))

###############################################################################
# Summarize by CU
###############################################################################
n <- length(unique(sort(unique(dat2$FULL_CU_IN))))

# Merge on FULL_CU_IN, because there are a few CUs with cuid = NA, and we 
# might as well keep that info for now
dat3 <- data.frame(
  species = NA,
  region = NA,
  FULL_CU_IN = sort(unique(dat2$FULL_CU_IN)),
  cuid = NA,
  start_spawn_025 = NA,
  start_spawn_10 = NA,
  start_spawn_mean = NA,
  end_spawn_975 = NA,
  end_spawn_90 = NA,
  end_spawn_mean = NA,
  n_years = NA,
  n_loc = NA,
  dq = NA
)

# Loop through each CU and summarize locations
for(i in 1:nrow(dat3)){
  boop <- which(dat2$FULL_CU_IN == dat3$FULL_CU_IN[i])
  dat3$species[i] <- dat2$SPECIES[boop[1]]
  dat3$region[i] <- dat2$REGION[boop[1]]
  dat3$cuid[i] <- dat2$cuid[boop[1]]
  
  # Summarize timing
  boop_start <- which(dat2$FULL_CU_IN == dat3$FULL_CU_IN[i] & !is.na(dat2$START_SPAWN_DT))
  if(length(boop_start) > 1){
    t025 <- sort(sample(dat2$START_SPAWN_DT[boop_start], size = 10000, replace = TRUE, prob = (4- dat2$DQ_START[boop_start])))
  # hist(t025); abline(v = t025[250], col = 2)
  dat3$start_spawn_025[i] <- t025[250]
  dat3$start_spawn_10[i] <- t025[1000]
  } else if(length(boop_start == 1)){
    dat3$start_spawn_025[i] <- dat2$START_SPAWN_DT[boop_start]
    dat3$start_spawn_10[i] <- dat2$START_SPAWN_DT[boop_start]
  } 
  dat3$start_spawn_mean[i] <- weighted.mean(
    x = dat2$START_SPAWN_DT[boop_start], 
    w = (4 - dat2$DQ_START[boop_start]))
  
  boop_end <- which(dat2$FULL_CU_IN == dat3$FULL_CU_IN[i] & !is.na(dat2$END_SPAWN_DT))
  if(length(boop_end) > 1){
    t975 <- sort(sample(dat2$END_SPAWN_DT[boop_end], size = 10000, replace = TRUE, prob = (4 - dat2$DQ_END[boop_end])))
  # hist(t975); abline(v = t975[9000], col = 2)
  dat3$end_spawn_975[i] <- t975[9750]
  dat3$end_spawn_90[i] <- t975[9000]
  } else if(length(boop_end == 1)){
    dat3$end_spawn_975[i] <- dat2$END_SPAWN_DT[boop_end]
    dat3$end_spawn_90[i] <- dat2$END_SPAWN_DT[boop_end]
  }
  dat3$end_spawn_mean[i] <- weighted.mean(
    x = dat2$END_SPAWN_DT[boop_end], 
    w = (4 - dat2$DQ_END[boop_end]))
  
  # Number of years and DQ score
  dat3$n_years[i] <- sum(apply(dat2[boop, c("nYears_START_SPAWN_DT", "nYears_END_SPAWN_DT")], 1, min))
  dat3$n_loc[i] <- length(which(apply(dat2[boop, c("nYears_START_SPAWN_DT", "nYears_END_SPAWN_DT")], 1, sum) > 0))
  dat3$dq[i] <- min(apply(dat2[boop, c("DQ_START", "DQ_END")], 1, mean))
} # end CUs
  
write.csv(dat3, file = paste0("output/life-cycle-timing_byCU_", Sys.Date(), ".csv"))

###############################################################################
# Plot
###############################################################################
rCol <- c("Yukon" = "#FF6212", "Transboundary"="#D55E00", "Nass"="#E69F00", "Skeena"="#F0E442","Haida Gwaii" = "#009E73", "Central Coast" = "#0072B2", "Vancouver Island & Mainland Inlets" = "#56b4e9", "Fraser"="#cc79a7", "Columbia" = "#8D8D8D")
regionNames <- names(rCol)
regionNames[7] <- "VIMI"

species <- unique(dat3$species)

for(i in 1:length(species)){
  pdf(file = paste0("output/figures/regions_", species[i], ".pdf"), width = 6, height = 7)
  par(mar = c(12, 4, 3, 1))
  dat3.i <- dat3[which(dat3$species == species[i]), ]
  dat3.i <- dat3.i[order(as.numeric(factor(dat3.i$region, levels = names(rCol)))), ]
  ni <- nrow(dat3.i)
  plot(c(min(dat3.i$start_spawn_025, na.rm = TRUE), max(dat3.i$end_spawn_975, na.rm = TRUE)), c(1, ni), "n", xlab = 'Day of Year', ylab = "Conservation Unit", yaxt = "n")
  segments(x0 = dat3.i$start_spawn_025, x1 = dat3.i$end_spawn_975, y0 = ni:1, y1 = ni:1, col = paste0(rCol[dat3.i$region], 30), lwd = 5)
  segments(x0 = dat3.i$start_spawn_10, x1 = dat3.i$end_spawn_90, y0 = ni:1, y1 = ni:1, col = paste0(rCol[dat3.i$region], 50), lwd = 3)
  segments(x0 = dat3.i$start_spawn_mean, x1 = dat3.i$end_spawn_mean, y0 = ni:1, y1 = ni:1, col = rCol[dat3.i$region], lwd = 1)
  mtext(side = 3, species[i])
  u <- par('usr')
  legend(u[1], u[3] - (u[4]-u[3])*0.25, fill = rCol, border = NA, legend = regionNames, xpd = NA, ncol = 3, title = "Regions")
  dev.off()
}


# Check certain CUs against data

s <- "Chinook"
r <- "HaidaGwaii"

pdf(file = paste0("output/figures/start-end-data_", Sys.Date(), ".pdf"), width = 6, height = 5)
for(s in species){
  for(r in unique(dat3$region[dat3$species == s])){
    ind0 <- which(dat3$species == s & dat3$region == r)
if(length(ind0) > 0){
    dat.i <- dat3[ind0, ]
    for(i in 1:nrow(dat.i)){
      ind <- which(dat$FULL_CU_IN == dat.i$FULL_CU_IN[i])
      ind2 <- which(dat2$FULL_CU_IN == dat.i$FULL_CU_IN[i])
      if(sum(!is.na(c(dat$START_SPAWN_DT[ind], dat$END_SPAWN_DT[ind]))) > 0){
      rng <- c(min(dat$START_SPAWN_DT[ind], na.rm = TRUE), max(dat$END_SPAWN_DT[ind], na.rm = TRUE))
      plot(rng, c(1, 3), "n", yaxt = "n", xlab = "DOY", ylab = "", ylim = c(0.5, 3.5))
      abline(h = c(1:3), col = grey(0.8))
      polygon(
        x = c(dat.i$start_spawn_025[i], dat.i$end_spawn_975[i], dat.i$end_spawn_975[i], dat.i$start_spawn_025[i]), 
        y = c(0, 0, 5, 5), 
        col = paste0(rCol[r], 20), border = NA)
      polygon(
        x = c(dat.i$start_spawn_10[i], dat.i$end_spawn_90[i], dat.i$end_spawn_90[i], dat.i$start_spawn_10[i]), 
        y = c(0, 0, 5, 5), 
        col = paste0(rCol[r], 20), border = NA)
      polygon(
        x = c(dat.i$start_spawn_mean[i], dat.i$end_spawn_mean[i], dat.i$end_spawn_mean[i], dat.i$start_spawn_mean[i]), 
        y = c(0, 0, 5, 5), 
        col = paste0(rCol[r], 20), border = NA)
      
      
      points(dat$START_SPAWN_DT[ind], jitter(rep(1, length(ind)), factor = 6), pch = 2, col = paste0(rCol[r], 60))
      points(dat2$START_SPAWN_DT[ind2], rep(1, length(ind2)), pch = 2, col = rCol[r], cex = 2, lwd = 2)
      
      points(dat$PEAK_SPAWN_DT[ind], jitter(rep(2, length(ind)), factor = 6), pch = 1, col = paste0(rCol[r], 60))
      points(dat2$PEAK_SPAWN_DT[ind2], rep(2, length(ind2)), pch = 1, col = rCol[r], cex = 2, lwd = 2)
      
      points(dat$END_SPAWN_DT[ind], jitter(rep(3, length(ind)), factor = 6), pch = 6, col = paste0(rCol[r], 60))
      points(dat2$END_SPAWN_DT[ind2], rep(3, length(ind2)), pch = 6, col = rCol[r], cex = 2, lwd = 2)
      
      mtext(side = 3, paste0(dat2$CU_NAME[ind2[1]], " (", dat2$FULL_CU_IN[ind2[1]], ")"))
      axis(side = 2, at = c(1:3), labels = c("start", "peak", "end"))
      
      legend("bottomright", pch = c(1,1), lty = NA,  lwd = c(1,2), pt.cex = c(1,2), col = rCol[r], c("raw", "location means"), bg = "white")
    }}}}}
    dev.off()