set.seed(123)
library(MASS)

compare_correlations <- function(n = 300, mc = 1000, rho = 0.8, plots = TRUE){
  
  ratios_uncor <- numeric(mc)
  ratios_cor   <- numeric(mc)
  
  Sigma_cor <- matrix(c(1, rho, rho, 1), 2, 2)
  
  for(i in 1:mc){
    
    # ----- Uncorrelated -----
    X_unc <- rnorm(n)
    Y_unc <- rnorm(n)
    ratios_uncor[i] <- mean(X_unc) / mean(Y_unc)
    
    # ----- Correlated -----
    XY <- mvrnorm(n, mu = c(0,0), Sigma = Sigma_cor)
    X_cor <- XY[,1]
    Y_cor <- XY[,2]
    ratios_cor[i] <- mean(X_cor) / mean(Y_cor)
  }
  
  if(plots == TRUE){
    
    # Boxplot
    df <- data.frame(
      ratios = c(ratios_cor, ratios_uncor),
      group  = c(rep("Correlated",mc), rep("Uncorrelated",mc))
    )
    
    boxplot(ratios ~ group, data = df, col = c("lightblue","red"), ylim = c(-10,10))
    
    # Histograms
    par(mfrow = c(2,1))
    hist(ratios_cor, col = "lightblue", main = "Correlated Ratios")
    hist(ratios_uncor, col = "lavender", main = "Uncorrelated Ratios")
    
    # QQ-Plots
    par(mfrow = c(1,2))
    qqnorm(ratios_cor); qqline(ratios_cor)
    qqnorm(ratios_uncor); qqline(ratios_uncor)
    
    cat("\nKS Test P-Value:\n")
  }
  
  test <- ks.test(ratios_uncor, ratios_cor)
  return(test$p.value)
}
