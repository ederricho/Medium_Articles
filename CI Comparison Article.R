# ================ CI Comparison Function ================
ci.comparison <- function(x = rnorm(1000), iterations = 10000, sampleSize = 30, alpha = 0.05){
  
  library(dplyr)
  
  pMean <- mean(x) # Population Mean for Coverage Probability
  
  # =============================================================
  # ================= Normality Assumption CI ===================
  nSamp <- sample(x, size = sampleSize)
  s <- sd(nSamp) # Sample Standard Dev.
  xBar <- mean(nSamp) # Mean of the Sample
  zVal <- qnorm(1 - alpha / 2) # Critical Value
  normalCI <- c(
    xBar - zVal * (s / sqrt(sampleSize)), # Left CI
    xBar + zVal * (s / sqrt(sampleSize)) # Right CI
  )
  
  
  # ================================================
  # ===================== BS CI ====================
  dist <- numeric(iterations)
  for(i in 1:iterations){
    samp <- sample(x, size = sampleSize, replace = TRUE)
    dist[i] <- mean(samp)
  }
  
  quants <- quantile(dist, probs = c(alpha/2, 1 - alpha/2)) # MC Confidence Interval
  
  # Results
  df <- data.frame(
    sampleSize = sampleSize,
    normalIntSize = normalCI[2] - normalCI[1],
    normalCoverage = ifelse(between(pMean, normalCI[1], normalCI[2]), 1, 0),
    BSIntSize = quants[2] - quants[1],
    BSCoverage = ifelse(between(pMean, quants[1], quants[2]), 1, 0)
  )
  
  return(df)
}

kable(ci.comparison())

# ================= Compare Using 100 Simulations ==========================
library(dplyr)

simulations <- 100
sim.df <- bind_rows(lapply(1:simulations, function(i) ci.comparison()))

kable(head(sim.df))

plot.f <- function(df){
  # Print Results
  cat(
    "\nNormal Coverage Probabilities: ", mean(df$normalCoverage),
    "\nVariance of Normal CI Length: ", var(df$normalIntSize),
    "\n------------------------",
    "\nBS Coverage Probabilities: ", mean(df$BSCoverage),
    "\nVariance of BS CI Length: ", var(df$BSIntSize)
  )
  
  # Plot Graphs
  plot(df$normalIntSize, main = "CI Length Comparison", lty = 1, type = "l", ylab = "CI Length")
  lines(df$BSIntSize, lty = 1, col = "red")
  legend("topleft", legend = c("Normal-based CI", "Bootstrap CI"), lty = c(1, 1), col = c("black", "red"),bty = "n"
  )
}

plot.f(sim.df)

# ================ Compare Using London Housing Dataset =============================
simulations <- 100
sim.df <- bind_rows(lapply(1:simulations, function(i) ci.comparison(x = HousingLondon$Price)))

kable(head(sim.df))

plot.f(sim.df)