# ---------- Normality Tester Function ----------
normalityTester <- function(data){
  # -- Libraries --
  library(nortest)
  library(tseries)
  library(e1071)
  
  # -- Minor cleaning --
  data <- as.numeric(na.omit(data)) # Omit NA Values and Create Numeric
  
  
  # -- Tests --
  sw <- shapiro.test(data)
  ad <- nortest::ad.test(data)
  ks <- ks.test(data, "pnorm")
  jb <- tseries::jarque.bera.test(data)
  
  # -- Store Results --
  shapiroRes <- ifelse(sw$p.value >= 0.05,
                       "Normal",
                       "Not Normal") # Shapiro Wilk
  andersonRes <- ifelse(ad$p.value >= 0.05,
                        "Normal",
                        "Not Normal") # Anderson-Darling
  kolmogorvRes <- ifelse(ks$p.value >= 0.05,
                         "Normal",
                         "Not Normal")
  jarqueRes <- ifelse(jb$p.value >= 0.05,
                      "Normal",
                      "Not Normal")
  skewnessRes <- ifelse(skewness(data) > -0.5 & skewness(data < 0.5),
                        "Within Bounds",
                        "Outside of Bounds")
  kurtosisRes <- ifelse(skewness(data) > -2 & skewness(data < 2),
                        "Within Bounds",
                        "Outside of Bounds")
  
  # -- Print Results --
  cat(
    "      Normality Tester Function         ", "\n", 
    "               ----------             ","\n",
    "     Shapiro-Wilk Test: ", shapiroRes, "\n",
    "     Anderson-Darling Test: ", andersonRes, "\n",
    "     Kolmogrov-Smirnov Test: ", kolmogorvRes, "\n",
    "     Jarque-Bera Test: ", jarqueRes, "\n",
    "     Skewness: ", skewnessRes, "\n",
    "     Kurtosis: ", kurtosisRes, "\n",
    "               ----------             ", "\n", "\n"
  )
  
  
  print(ifelse(shapiroRes == "Normal" &
                 andersonRes == "Normal" &
                 kolmogorvRes == "Normal" &
                 jarqueRes == "Normal" &
                 kurtosisRes == "Within Bounds" &
                 skewnessRes == "Within Bounds",
               "Data Appears to be Normal",
               "Data is not Normal"))
  
  # -- Visual Tests --
  # Histogram
  hist(data, main = "Histogram of Data", col = "lightblue")
  
  # Boxplot
  boxplot(data, main = "Boxplot of Data", col = "lightblue")
  
  # Q-Q Plot
  qqnorm(data, main = "Q-Q Plot of Data")
  qqline(data, col = "red", lwd = 2)
  
  # -- Data Frame --
  result <- list(
    Shapiro_Wilk_PValue = sw$p.value,
    Anderson_Darling_PValue = ad$p.value,
    Kolmogrov_Smirnov_PValue =  ks$p.value,
    Jarque_Bera_PValue = jb$p.value,
    Skewness = skewness(data),
    Kurtosis = kurtosis(data)
  )
  
  return(result)
}

test <- normalityTester(rnorm(500))