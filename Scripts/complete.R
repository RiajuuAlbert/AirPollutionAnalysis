nobs <- numeric()

complete <- function(directory, id = 1 : 332){
  ## 'directory' is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID 
  ## numbers to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is th emonitor ID number and 'nobs' is the
  ## number of complete cases
  
  dataList <- list.files(directory, pattern = "*.csv", full.names = TRUE)
  
  dataSelect <- lapply(dataList[id], read.csv)
  
  for (i in 1:length(id)){
    nobs[i] <- nrow(dataSelect[[i]][complete.cases(dataSelect[[i]]), ])
  }
  
  report <- data.frame(id, nobs)
  
  ## returns the mean of the desired pollutant mean of particular datasets, 
  ## without the NAs
  return(report)
}

## function call
complete(directory, id)

