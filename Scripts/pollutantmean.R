pollutantmean <- function(directory, pollutant, id = 1 : 332){
  ## 'directory' is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating 
  ## the name of the pollutant for which we will calculate 
  ## the mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID 
  ## numbers to be used
  
  ## Return the mean of the pollutan across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  ## Listing all the data source from the directory in a single list to 
  ## be accessed in a batch
  ## 'directory': determining the data source directory
  ## 'pattern': to match the file names that are going to be listed, use '*' 
  ## for all files
  ## 'full.names': choosing whether to keep the full directory path for 
  ## the files in the file list
  dataList <- list.files(directory, pattern = "*.csv", full.names = TRUE)
  
  ## 'do.call(func/name, args)'  takes a function as input and splatters its 
  ## other arguments to the function
  ## 'lapply' applies a function over a list or vector and returning exactly 
  ## the same length as the list or vector length
  dataSelect <- do.call(rbind,lapply(dataList[id], read.csv))
  
  ## 'complete.cases(row, column)' ensures only data with values are kept
  dataProcessed <- dataSelect[complete.cases(dataSelect), ]
  
  ## returns the mean of the desired pollutant mean of particular datasets, 
  ## without the NAs
  return(mean(dataProcessed[[pollutant]]))
}

## function call
pollutantmean(directory, pollutant, id)

