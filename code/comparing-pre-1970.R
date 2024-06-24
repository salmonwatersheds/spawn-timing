###############################################################################
# Comparing CU timing with and without data pre-1970 for Chinook, coho, and 
# sockeye in the Central Coast, VIMI, Haida Gwaii and Nass regions.
#
# June 19, 2024
#
###############################################################################

# Background: 
# - Previously we removed pre-1970s data for populations that had a break of >80 days year to year
# - Despite this check, there were several Cus with suspicious run timing, either all pre-1970s or a change that didn't quite meet the 80-day threshold.
# - TO dig into this further, we created two subsets of the data: 
#.    1) All data, with only those populations that had significant shifts year-to-year removed; and 
#     2) also removing pre-1970 data across all regions and species.
# This code compares CU-level outcomes under those two scenarios
###############################################################################

# Load two sets of output for comparison
out1 <- read.csv("output/spawn-timing_CUs_brks_2024-06-20.csv")
out2 <- read.csv("output/spawn-timing_CUs_brksremoved_2024-06-20.csv")

# Merge output (x = full data, y = pre-1970 removed)
out <- full_join(out1, out2 %>% select(SQ_CU_NAME, arrival_50, mu_spawn_50, sd_spawn_50, start_spawn_50, end_spawn_50, n.arrival, n.sd, n.peak), join_by(SQ_CU_NAME))
  
# Calculate difference in peak spawn timing
out$diff <- out$mu_spawn_50.y - out$mu_spawn_50.x

# Plot Difference in peak spawn timing
breaks <- seq(-29.5, 80.5, 1)
hist(out$diff[round(out$diff) != 0], main = "Difference in peak spawn date\n between datasets", xlab = "Days", breaks = breaks, col = c(rep(2, length(which(breaks < -14))), rep(grey(0.8), length(which(breaks >= -14 & breaks <= 14))), rep(2, length(which(breaks > 14)))))
abline(v = c(-14.5, 14.5), lty = 2)

#------------------------------------------------------------------------------
# Identify CUs that show a shift in spawn timing of > 14 days or have no post-1970s data
#------------------------------------------------------------------------------

remove <- out %>%
  filter(abs(diff) > 14 | is.na(diff)) %>%
  # select(SQ_CU_NAME, cuid, diff, diff.n, n.peak.x, n.peak.y) %>%
  arrange(diff)

# Write to .csv for stage 2 removal
write.csv(remove, file = "data/nuseds-pre1970-shift.csv", row.names = FALSE)


#------------------------------------------------------------------------------
# Plot
#------------------------------------------------------------------------------
n <- dim(remove)[1]
pdf(file = "output/ignore/diff1970_brks.pdf", width = 7, height = 24)
par(mar = c(4,15,2,7))
DOY <- c(1:365)
xDate <- as.Date(paste(2003, DOY, sep = "-"), format = "%Y-%j")
plot(xDate, DOY, "n", ylim = c(0.5, n + 3 + 0.5), xlim = c(as.Date("2003-06-01"), as.Date("2003-12-15")), yaxt = "n", ylab = "", xlab = "", yaxs = "i")
axis(side = 2, at = 1:n, remove$SQ_CU_NAME, las = 1)
legend("top", fill = c("#0000FF50", "#FF000050"), legend = c("all", "remove pre-1970"), bg = "white")
u <- par("usr")
abline(h = seq(0.5, n + 0.5, 1), lwd = 0.5)
abline(v = as.Date(paste(2003, c(1:12), "01", sep = "-")), col = grey(0.8))
abline(v = as.Date(paste(2003, c(1:12), "15", sep = "-")), lty = 2, col = grey(0.8))
for(i in 1:n){
  segments(x0 = xDate[round(remove$start_spawn_50.x[i])], x1 = xDate[round(remove$end_spawn_50.x[i])], y0 = i +0.1, y1 = i+0.1, col = "#0000FF50", lwd = 3)
  points(xDate[round(remove$mu_spawn_50.x[i])], i+0.1, col = "#0000FF")
  
  segments(x0 = xDate[round(remove$start_spawn_50.y[i])], x1 = xDate[round(remove$end_spawn_50.y[i])], y0 = i-0.1, y1 = i-0.1, col = "#FF000050", lwd = 3)
  points(xDate[round(remove$mu_spawn_50.y[i])], i-0.1, col = "#FF0000")
  
  
  text(as.Date(u[2], origin = "1970-01-01"), i, pos = 4, paste0(remove$diff.n[i], " (", remove$n.peak.x[i], " to ", remove$n.peak.y[i], ")"), cex = 0.8, xpd = NA)
  
}
dev.off()

