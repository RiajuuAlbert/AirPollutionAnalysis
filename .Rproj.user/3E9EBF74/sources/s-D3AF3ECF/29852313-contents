k = 1
id <- 1 : 332
nobs <- numeric()
cor <- numeric()

corr <- function(directory, threshold = 0){
  ## 'directory' is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'threshold' is a number vecetor of length 1 indicating the 
  ## number of completely observed observations (on all 
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  dataList <- list.files(directory, pattern = "*.csv", full.names = TRUE)
  
  dataSelect <- lapply(dataList[id], read.csv)
  
  for (i in id){
    nobs[i] <- nrow(dataSelect[[i]][complete.cases(dataSelect[[i]]), ])
  }
  
  df <- data.frame(id, nobs)
  
  idNobs <- df[nobs > threshold, 1]
  
  for (j in idNobs){
    dataClean <- na.omit(dataSelect[[j]])
    cor[k] <- cor(dataClean[["sulfate"]], dataClean[["nitrate"]])
    k = k + 1
  }
  return(cor)
}

## function call
corr(directory, threshold)

