###############################################################################
# Compare outputs for spawn timing window from distribution fitting and
# start/end summaries
# May 31, 2023
###############################################################################

# Read in both spawn timing (st) estimate types
st1 <- read.csv("output/spawn-timing_CUs.csv") # Based on mean and sd
st2 <- read.csv("output/life-cycle-timing_byCU.csv") # Based on average start and end
str <- read.csv("output/spawn-timing_regions.csv") # Based on mean and sd
  
st1$start_spawn_normal <- st1$mu_spawn - 1.96 * st1$sd_spawn
st1$end_spawn_normal <- st1$mu_spawn + 1.96 * st1$sd_spawn

sp_lookup <- data.frame(
  SPECIES_QUALIFIED = sort(unique(st1$SPECIES_QUALIFIED)), 
  species = sort(unique(str$species))[c(1, 2, 3, 4, 4, 5, 5)]
)

###############################################################################
# Plot timing comparison for specific regions/species
###############################################################################

rCol <- c("Yukon" = "#FF6212", "AlaskaTransboundary"="#D55E00", "Nass"="#E69F00", "Skeena"="#F0E442","HaidaGwaii" = "#009E73", "Central Coast" = "#0072B2", "Vancouver Island & Mainland Inlets" = "#56b4e9", "Fraser"="#cc79a7", "Columbia" = "grey")

r <- "Central Coast"
s <- "SEL"

cuid <- sort(st1$cuid[which(st1$region == r & st1$SPECIES_QUALIFIED == s & !is.na(st1$cuid))])
match1 <- match(cuid, st1$cuid)
match2 <- match(cuid, st2$cuid)

n <- length(cuid)

xDate <- as.Date(paste(2001, c(1:365), sep = "-"), format = "%Y-%j")

quartz(width = 12, height = n/4+1, pointsize = 10)
par(mar = c(4, 20, 2, 1))
plot(range(xDate), c(1,n), "n", xaxs = "i", yaxt = "n", ylab = "", xlab = "")
axis(side = 2, at = c(1:n), labels = st1$SQ_CU_NAME[match1], las = 1)
abline(v = xDate[round(str$mu_spawn[str$region == r & str$species == sp_lookup$species[sp_lookup$SPECIES_QUALIFIED == s]])])
segments(
  x0 = xDate[round(st1$start_spawn_normal[match1])], 
  x1 = xDate[round(st1$end_spawn_normal[match1])], 
  y0 = 1:n, 
  y1 = 1:n, 
  col = paste0(rCol[r], 30), lwd = 10)

segments(
  x0 = xDate[round(st2$start_spawn_10[match2])], 
  x1 = xDate[round(st2$end_spawn_90[match2])], 
  y0 = 1:n, 
  y1 = 1:n, 
  col = paste0(rCol[r], 50), lwd = 3)

points(xDate[round(st1$mu_spawn[match1])], 1:n, col = rCol[r])
points(xDate[round(st2$start_spawn_mean[match2])], 1:n, col = rCol[r], pch = 24)
points(xDate[round(st2$end_spawn_mean[match2])], 1:n, col = rCol[r], pch = 25)

# Cu #4: 
st1[which(st$cuid == cuid[4]),]
# End spawn is wonky (less than start spawn)
# Look at data