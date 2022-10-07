


xDate <- as.Date(as.Date("2021-01-01"):as.Date("2021-12-31"), origin = "1970-01-01")
DOY <- c(1:365)

data.frame(xDate, DOY)

# Parameters to be estimated:
mu <- 240 
sd <- 4

obs <- c(230, 238)
quartz(width = 8, height = 4)
par(mfrow = c(1, 2))
plot(xDate[c(220:260)], dnorm(DOY[c(220:260)], mu, sd), "l", xlab = "Date", ylab = "Spawn timing")

# Cumulative distribution
plot(xDate[c(220:260)], pnorm(DOY[c(220:260)], mu, sd), "l", xlab = "Date", ylab = "Cumulative spawn")

# 
abline(v = xDate[obs], col = 2)

abline(h = pnorm(obs, mu, sd), col = 2)

# Likelihood 
diff(pnorm(obs, mu, sd))

# if obs are closer to peak?
obs <- c(256,258)
diff(pnorm(obs, mu, sd))

# Peak spawning used to estimate mean
# 
# Start and end used to estimate sd?
# START_SPAWN_DT_FROM -> START_SPAWN_DT_TO
# END_SPAWN_DT_FROM -> END_SPAWN_DT_TO

qnorm(p = 0.025, mu, sd)

# 