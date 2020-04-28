# Function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
#       across a specified list of monitors. The function 'pollutantmean' 
#       takes three arguments: 'directory', 'pollutant', and 'id'.
#       Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
#       particulate matter data from the directory specified in the 'directory' 
#       argument and returns the mean of the pollutant across all of the monitors, 
#       ignoring any missing values coded as NA.

source('readdata.R')
#loading dataf function from readdata.R to read the desired csv file  
#      from the required directory


pollutantmean <- function(directory = 'specdata', pollutant, id = 1:332){
      summ <- 0
      len <- 0
      for(i in id){
            x =  dataf(dir = 'specdata', id = i)
            #calling the dataf function to load the desired files
            summ = summ + sum(x[,pollutant], na.rm = T)
            len = len + length(x[,pollutant][!is.na(x[,pollutant])])
      }
      #print(paste('sum = ', summ))
      #print(paste('len =', len))
      print(paste('mean =', summ/len))
}