# Explore Function

explore <- function(df, checkNorm = TRUE, imputation = "Mean", printTest = TRUE){
  # ----- Empty Vectors -----
  tTestNames <- c()
  tTestResults <- c()
  testPrint <- c()
  
  # ----- Functions -----
  numericChange <- function(x){ # Change DF Columns to Numeric
    new <- as.numeric(x)
    return(new)
  }
  
  impute <- function(x, imputation){ # Imputes Mean or Median
    if(imputation == "Mean"){
      x[is.na(x)] <- mean(x, na.rm = TRUE)
    } else if(imputation == "Median"){
      x[is.na(x)] <- median(x, na.rm = TRUE)
    }
    return(x)
  }
  
  # ----- Clean Data -----
  newDataFrame <- as.data.frame(sapply(df, numericChange)) # Change to Numeric
  newDataFrame2 <- as.data.frame(sapply(newDataFrame, impute, imputation = imputation))
  
  # ----- Analysis -----
  
  colorArray <- c("lightblue", "forestgreen", "lavender", "orange", "yellow", "turquoise", "red")
  
  # ----- Histograms -----
  for(i in 1:length(newDataFrame2)){
    hist(newDataFrame2[,i], main = paste("Histogram of ", names(newDataFrame2[i])),
         col = sample(colorArray, 1, replace = T))
  }
  
  # ----- T-Tests and Boxplots -----
  n <- ncol(newDataFrame2)  # number of columns
  combinations <- expand.grid(var1 = 1:n, var2 = 1:n)
  
  # ----- Full Boxplot -----
  boxplot(newDataFrame2, use.cols = T, main = paste("Full Boxplot"))
  
  # ----- Boxplots -----
  for(i in 1:nrow(combinations)) {
    # ----- Make Combinations -----
    col1 <- combinations[i, "var1"]
    col2 <- combinations[i, "var2"]
    
    # ----- Boxplots and T-Tests -----
    if (col1 < col2) {
      # t-test, boxplot, readline
      test <- t.test(x = newDataFrame2[[col1]],
                     y = newDataFrame2[[col2]])
      
      # ----- Print T-Test -----
      if(printTest == TRUE){
        print(test)  # optional
      }
      
      # ----- Append T-Test Dataframe -----
      
      tTestNames <- append(tTestNames, paste(names(newDataFrame2[col1]),"and",names(newDataFrame2[col2])))
      tTestResults <- append(tTestResults, test$p.value)
      
      greater <- ifelse(test$estimate[1]>test$estimate[2],
                        paste(names(newDataFrame2[col1])," is greater"),
                        paste(names(newDataFrame2[col2])," is greater"))
      
      testPrint <- append(testPrint, greater)
      
      # ----- Print Boxplots -----
      boxplot(newDataFrame2[[col1]],
              newDataFrame2[[col2]],
              names = c(paste(names(newDataFrame[col1])), paste(names(newDataFrame[col2]))),
              main = paste("Boxplot: ", names(newDataFrame[col1]), "vs ", names(newDataFrame[col2])),
              col = sample(colorArray, 1, replace = T))
      #readline(prompt = "Press [Enter] to see the next boxplot...")
    }
  }
  
  # ----- T-Test Results -----
  testDataFrame <- data.frame(tTestNames, tTestResults, testPrint) # Data Frame of Results
  colnames(testDataFrame) <- c("Test", "p-Values for T-Test", "Result") # Change Names
  
  return(testDataFrame)
}