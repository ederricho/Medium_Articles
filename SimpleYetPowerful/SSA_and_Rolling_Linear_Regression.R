# ------------------------------------------------------------------
#  This is the simple stock algorithm code where the model is:
#  price = movingAverage(3-day) + (coef)*movingStandardDeviation(3-day)
#  We will optimize the coefficient above with gradient descent and anylize
#  the model results. For a detailed explanation, go to the html file in the
#  github repository.
# -----------------------------------------------------------------

# Libraries and Data:
library(ggplot2)
library(quantmod)
library(roll)
library(lmtest)
library(tidyr)
library(readxl)
library(kableExtra)

apple_df <- getSymbols('AAPL',src='yahoo',auto.assign = F) # from quantmod package

# --------------------------------------------------------------------------
# ------------------ Important Functions -----------------------------------
# --------------------------------------------------------------------------

# Define the Prediction Function
pred.price <- function(moving.avg,moving.sd,a = 1){
  return(moving.avg + a * moving.sd)
}

# ---------------------------------------------------------
# Define the Loss Function:

# We will use the sum squares error:
loss.function <- function(a,prices,moving.avg,moving.standard.d){
  #error <- prices - moving.avg

  # Predict future Prices
  pred.prices <- moving.avg + moving.standard.d

  # Calculate the sum of squared errors (SSE)
  sse <- sum((prices - pred.prices)^2)

  return(sse)
}

# -----------------------------------------------------------------------
# ---------------- Simple Stock Algorithm --------------------------------
# -----------------------------------------------------------------------

left <- 1000
right <- 1150
prices <- apple_df$AAPL.Close[left:right]


simple.algo <<- function(data,omega = 1,min,max, window = 3,iterations=1000,learning.rate=0.01){
  library(roll)
  prices <- data[min:max]
  # ----------------------------------------------------------------------------
  # --------------------------- Feature Engineering ----------------------------
  # ----------------------------------------------------------------------------
  # 3-day MA
  moving.average <- stats::filter(prices,rep(1/window, window), sides = 1)
  # 3-day MSD
  moving.standard.d <- roll_sd(prices,window)
  # 3-day MA + 3-day MSD
  func.orig <- moving.average + omega * moving.standard.d
  # error
  error <- func.orig - prices

  # Remove NA values
  prices <- prices[!is.na(moving.average)]
  moving.average <- moving.average[!is.na(moving.average)]
  moving.standard.d <- moving.standard.d[!is.na(moving.standard.d)]
  func.orig <- func.orig[!is.na(func.orig)]
  error <- round(error[!is.na(error)],3)
  error.orig <- error
  #-----------------------------------------------------------------------------
  # ------------------------ Loss Function -------------------------------------
  # ----------------------------------------------------------------------------
  # Predict future Prices
  pred.prices <- moving.average + moving.standard.d

  # Calculate the sum of squared errors (SSE)
  sse <- sum((prices - pred.prices)^2)
  # ----------------------------------------------------------------------------
  # ----------------------------- Gradient Descent -----------------------------
  # ----------------------------------------------------------------------------
  # Number of Training Examples:
  m <- length(prices)

  # Initiate omega
  omega <- 0

  omega.values <- numeric(iterations)

  # Gradient Descent Loop:
  for(i in 1:iterations){
    #print(omega)
    # Compute the Predictions Based on Current Data:
    predictions <- moving.average + omega * moving.standard.d

    # Calculate the Error:
    error <- predictions - prices

    # Compute Gradient:
    gradient <- (1/m) * sum(error * moving.standard.d)
    #cat("Gradient: ",gradient)

    # Update Omega
    omega <- omega - learning.rate * gradient

    omega.values[i] <- omega
  }
  omega <- omega
  # ----------------------------------------------------------------------------
  # ------------------- Calculate New Optimized Function -----------------------
  # ----------------------------------------------------------------------------
  func.optim <- moving.average + omega * moving.standard.d
  error.optim <- func.optim - prices
  #Let us Make a new dataframe with all of the values:
  new.df <- as.data.frame(cbind(prices,
                                moving.average,
                                moving.standard.d,
                                func.orig,
                                func.optim,
                                error.orig,
                                error.optim))
  # -----------------------------------------------------------
  # ---------------------------- Output -----------------------
  # -----------------------------------------------------------
  mseOriginal <- mean(error.orig^2)
  mseOptimized <- mean(error.optim^2)


  colnames(new.df) <- c("Prices",
                        "MovingAverage",
                        "MovingStandardDev",
                        "Original Func",
                        "Optimized Func",
                        "Original Error",
                        "Optimized Error")

  # ---------------------- Print -----------------------------
  cat("Optimized Coefficient: ", omega,"\n",
      "Mean Squared Error of Original Algo: ", mseOriginal,"\n",
      "Mean Squared Error of Optimized Algo: ", mseOptimized,"\n",
      "Summary of Error: \n",summary(error.optim)
      )

  return(new.df)
}

origional <- simple.algo(prices,1,0,length(prices))

# ------------------------------------------------------------------------
# ---------------------- Gradient Descent --------------------------------
# ------------------------------------------------------------------------

gradient.descent <- function(data,learning.rate=0.01,iterations=1000){
  # Naming Variables
  price <- data$Price
  moving.average <- data$MovingAverage
  moving.standard.d <- data$MovingStandardDev

  # Number of Training Examples:
  m <- length(price)

  # Initiate omega
  omega <- 0

  omega.values <- numeric(iterations)

  # Gradient Descent Loop:
  for(i in 1:iterations){
    #print(omega)
    # Compute the Predictions Based on Current Data:
    predictions <- moving.average + omega * moving.standard.d

    # Calculate the Error:
    error <- predictions - price

    # Compute Gradient:
    gradient <- (1/m) * sum(error * moving.standard.d)
    #cat("Gradient: ",gradient)

    # Update Omega
    omega <- omega - learning.rate * gradient

    omega.values[i] <- omega
  }
  # Return Optimized Values:
  return(omega)
}

# GD Function
time.a <- proc.time()[3]
omega <- gradient.descent(origional,0.01,100)
time.b <- proc.time()[3]

# Time and Coefficient:
cat("Time to Complete: ",time.b - time.a,"\n",
    "Coefficient: ",omega)

# New Model with New Omega
optim.df <- simple.algo(prices,omega,0,length(prices))

# ------------------------------------------------------------------------
# -------------------- Iterative Gradient Descent ------------------------
# ------------------------------------------------------------------------

# This will return a list of omega values for each iteration of our model:
iterative.gradient.descent <- function(data,learning.rate=0.01,iterations=1000){
  # Naming Variables
  price <- data$Price
  moving.average <- data$MovingAverage
  moving.standard.d <- data$MovingStandardDev

  # Omega Values:
  omega.vals <- numeric(length(price))

  # Initiate omega (parameters) as a Vector of Zeros
  omega <- 0

  # Gradient Descent Loop:
  for(t in 1:length(price)){
    #print(omega)
    # Compute the Predictions Based on Current Data:
    predictions <- moving.average[t] + omega * moving.standard.d[t]

    # Calculate the Error:
    error <- predictions - price[t]

    # Compute Gradient:
    gradient <- error * moving.standard.d
    #cat("Gradient: ",gradient)

    # Update Omega every n iterations:
    n = 3
    if(t %% n == 0){
      omega <- omega - learning.rate * gradient
    }else{
      omega <- omega
    }


  }
  # Return Optimized Values:
  return(omega)
}

# GD
time.a <- proc.time()[3]
iter.omega <- iterative.gradient.descent(origional)
time.b <- proc.time()[3]
it.time <- time.b - time.a

# Observe Omega Values
head(iter.omega)

# Final Omega Value:
f.omega <- iter.omega[length(iter.omega)]

# --------------------------------------------------------------------
# -------------------------- Confidence Interval ---------------------
# --------------------------------------------------------------------

# Iterative Model:
iter.model2 <- simple.algo(prices,iter.omega,0,length(prices))

# Calculate MSE
cat("Origional Model MSE: ",mean(origional$`Original Error`^2),"\n",
    "Optimized Model MSE: ",mean(optim.df$`Optimized Error`^2),"\n",
    "Iterative Optimized Model MSE: ",mean(iter.model2$`Optimized Error`^2),"\n")

library(boot)
error <- iter.model2$`Optimized Error`

# Define a function for the statistic of interest:
mean.stat <- function(data,indices){
  d <- data[indices]
  return(mean(abs(d))) # Return Mean Absolute Error
}

# Bootstrap Loop:
bootstrap.results <- boot(error,statistic = mean.stat,R = 1000)
cat("Bootstrap Mean: ",mean(bootstrap.results$t),"\n","Sample Mean",mean(error),"\n")

# Calculate CI:
ci <-boot.ci(bootstrap.results, type = "perc", conf = 0.95) # Using Percentile Method
ci

# -------------------------------------------------------------------
# Confidence Error Visualization

# Retrieve Bootstrap Mean and Confidence Intervals:
ci.lower <- ci$percent[4] # 2.5th Percentile for 95% ci
ci.upper <- ci$percent[5] # 97.5th percentile for 95% ci
bootstrap.mean <- mean(bootstrap.results$t)
data <- abs(error)

# Plot the Data and Confidence Band:
plot(data, main = "Data with Bootstrap Confidence Interval for the Mean",
     ylab = "Value", xlab = "Index", col = "blue", pch = 16, ylim = c(min(data) - 5, max(data) + 5))
abline(h = bootstrap.mean, col = "red", lwd = 2, lty = 2)  # Bootstrapped mean line
abline(h = ci.lower, col = "darkgreen", lwd = 2, lty = 2)  # Lower CI
abline(h = ci.upper, col = "darkgreen", lwd = 2, lty = 2)  # Upper CI
polygon(c(1, length(data), length(data), 1), c(ci.lower, ci.lower, ci.upper, ci.upper),
        col = rgb(0.1, 0.9, 0.1, 0.2), border = NA)  # Shaded confidence band
legend("topright", legend = c("Bootstrap Mean", "95% CI"), col = c("red", "darkgreen"),
       lty = 2, lwd = 2, bty = "n")


# -----------------------------------------------------------------------
# ---------------------- Prediction Interval ----------------------------
# -----------------------------------------------------------------------

# We will use the best model's error to bootstrap: Optimized Model
error <- optim.df$`Optimized Error`

# Define a function for the statistic of interest:
sd.stat <- function(data,indices){
  d <- data[indices]
  return(sd(d)) # Return Mean Absolute Error
}

# Bootstrap Loop:
bootstrap.sd <- boot(error,statistic = sd.stat,R = 1000)
bs.sd <- sd(bootstrap.sd$t)


# Critical Value for 95% Confidence Interval
alpha <- 0.05
z <- qnorm(1 - alpha/2)

# Construct Confidence Interval for Predicted Prices
lower.ci <- optim.df$`Optimized Func` - z * bs.sd
upper.ci <- optim.df$`Optimized Func` + z * bs.sd

# Display Results:
pred.int <- data.frame(origional$Prices,optim.df$`Original Func`,lower.ci,upper.ci)

cat("Bootstrap Standard Deviation: ",bs.sd,"\n",
    "Sample Standard Deviation",sd(error),"\n")

head(pred.int)

# Plot Prediction Interval

# Load necessary library
library(ggplot2)

# Assuming `predicted`, `lower_pi`, and `upper_pi` are vectors with the same length
# and you have a data frame `data` containing the actual prices (for context)

# Create a data frame for plotting
plot_data <- data.frame(
  Actual = pred.int$origional.Prices,         # Actual prices
  Original = pred.int$optim.df..Original.Func., # Original Model
  Predicted = pred.int$optim.df..Original.Func.,   # Predicted prices from your model
  Lower_PI = pred.int$lower.ci,     # Lower bound of the prediction interval
  Upper_PI = pred.int$upper.ci      # Upper bound of the prediction interval
)

# Plot
ggplot(plot_data, aes(x = 1:nrow(plot_data))) +
  geom_line(aes(y = Actual), color = "red", size = 1, linetype = "dashed") +  # Actual prices
  geom_line(aes(y = Predicted), color = "blue", size = 1) +
  #geom_line(aes(y = Original), color = "gray", size = 1)+ # Original Model
  # Predicted prices
  geom_ribbon(aes(ymin = Lower_PI, ymax = Upper_PI), fill = "lightgrey", alpha = 0.5) +  # Prediction interval
  labs(title = "Prediction Interval for Stock Prices",
       x = "Time",
       y = "Price") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, nrow(plot_data), by = 20)) +  # Adjust x-axis for better visibility
  theme(legend.position = "none")


# ------------------------------------------------------------------------------
# ------------------------- Rolling Linear Regression --------------------------
# ------------------------------------------------------------------------------

rollingLinear <- function(vector, window = 3, res = TRUE, cDist = betaCoeffDist, sdDist = residualStDev, volTime = 1){
  # -- Empty Vectors --
  prices <- c()
  betaCoeff <- c()
  intercepts <- c()
  standardDeviations <- c()
  coeffPercentile <- c()
  sdPercentile <- c()
  volatility <- c()
  
  # -- Rolling LR Model --
  for(i in 1:(length(vector)-window)){
    left <- i
    right <- i + window
    model <- lm(vector[left:right] ~ c(left:right))
    if(res == TRUE){
      stDev <- sd(model$residuals)
    }else if(res == FALSE){
      stDev <- NA
    }
    
    #print(model$coefficients[2])
    # -- Percentiles --
    percBeta <- sum(model$coefficients[2] >= cDist ) / length(cDist)
    percRes <- sum(stDev >= sdDist) / length(sdDist)
    
    # -- Coefficients and Standard Deviations 
    prices <- append(prices, vector[i+window])
    betaCoeff <- append(betaCoeff, model$coefficients[2])
    intercepts <- append(intercepts, model$coefficients[1])
    standardDeviations <- append(standardDeviations,stDev)
    coeffPercentile <- append(coeffPercentile, percBeta)
    sdPercentile <- append(sdPercentile, percRes)
    volatility <- append(volatility, sd(vector[i:(i+window)]) * sqrt(volTime))
    
  }
  # -- Dataframe of Information --
  df <- data.frame(prices,
                   betaCoeff,
                   intercepts, 
                   standardDeviations,
                   coeffPercentile,
                   sdPercentile,
                   volatility)
  
  df$index <- c(1:length(df$prices))
  
  return(df)
}

head(rollingLinear(rnorm(500)))