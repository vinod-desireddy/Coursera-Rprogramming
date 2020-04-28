# Function that takes a directory of data files and a threshold for 
#       complete cases and calculates the correlation between sulfate and 
#       nitrate for monitor locations where the number of completely observed 
#       cases (on all variables) is greater than the threshold. The function 
#       should return a vector of correlations for the monitors that meet the 
#       threshold requirement. If no monitors meet the threshold requirement, 
#       then the function should return a numeric vector of length 0



source('readdata.R')
#loading dataf function from readdata.R to read the desired csv file  
#      from the required directory

source('complete.R')
#loading 'complete' function which returns the number of complete
#      cases for a given csv files


corr <- function(directory = 'specdata', threshold = 0){
      y <- complete()[complete()$nobs > threshold,]
      #filtering for the csv files, in the specified directory, 
      #which have no. of complete cases above the threshold specified.
      co <- numeric()
      b <- 1
      for(j in y$id){
            z = dataf(id = j)
            w = z[complete.cases(z),]
            #filtering for only observation with complete cases
            co[b] <- cor(x = w$sulfate, y = w$nitrate)
            #computing the correlation
            b = b + 1
      }
      co     
}