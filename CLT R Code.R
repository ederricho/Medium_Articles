# ---------- CLT Article ----------

# ----------------------------------------------------------------------
# ---------- Population and Monte Carlo Simulation Parameters ----------
# ----------------------------------------------------------------------

n <- 10000 # Population Size
iterations <- 10000 # MC Iterations
sampleSize <- c(30, 100) # Sample Sizes - CLT Calls for a minimum of 30, let's observe the convergence for a much larger sample size (100)

# -- Create Data Frame --
norm <- rnorm(n)
exp <- rexp(n)
uniform <- runif(n)
unknown <- errorDist[5:(n+4)]

# -- DF ==
distribution_df <- data.frame(norm, exp, uniform, unknown)
colnames(distribution_df) <- c("Normal", "Exponential", "Uniform", "Unknown") # Rename Data Frame
head(distribution_df)

# -------------------------------------------------
# ---------- Function for MC Simulations ----------
# -------------------------------------------------

# -- Explanation of Function --
# This function is designed to run Monte Carlo simulations for samples of 30 and 100 for mean, variance,
# kurtosis, three-day average, and three-day avriance
# -----------------------------

# Make the function take the population AND its label
mcHistogram <- function(population, pop_name) {
  # -- Empty Lists for Statistics --
  meanList <- numeric(iterations) 
  medianList <- numeric(iterations)
  kurtList <- numeric(iterations)
  varList <- numeric(iterations)
  threeDayAvList <- numeric(iterations)
  threeDayVarList <- numeric(iterations)
  
  # -- MC Simulation over sample sizes --
  for (i in sampleSize) {
    for (j in seq_len(iterations)) {
      samp <- sample(x = population, size = i, replace = FALSE)
      meanList[j]      <- mean(samp)
      medianList[j]    <- median(samp)
      kurtList[j]      <- kurtosis(samp)
      varList[j]       <- sd(samp)^2
      threeDayAvList[j]  <- mean(roll_mean(samp,  width = 3), na.rm = TRUE)
      threeDayVarList[j] <- mean(roll_var(samp,   width = 3), na.rm = TRUE)
    }
    
    df <- data.frame(
      Mean = meanList, Median = medianList, Kurtosis = kurtList,
      Variance = varList, `Three Day Average` = threeDayAvList,
      `Three Day Variance` = threeDayVarList
    )
    
    lapply(seq_along(df), function(idx) {
      stat_name <- names(df)[idx]
      hist(df[[idx]],
           main = paste("Histogram of", stat_name, "| n =", i, "|", pop_name),
           xlab = stat_name,
           col = sample(c("lightblue","violet","red"), 1))
    })
  }
  invisible(NULL)
}

# If your populations are in a named data.frame/list `distribution_df`
# (e.g., names(distribution_df) = c("Normal(0,1)", "Uniform(0,1)", ...)):
mapply(mcHistogram, as.list(distribution_df), names(distribution_df), SIMPLIFY = FALSE)

# -----------------------------------------------
# ---------- Thank you for Downloading ----------
# -----------------------------------------------