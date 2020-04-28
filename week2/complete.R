# Function that reads a directory full of files and reports 
#       the number of completely observed cases in each data file. 
#       The function should return a data frame where the first column is 
#       the name of the file and the second column is the number of complete cases


source('readdata.R')
#loading dataf function from readdata.R to read the desired csv file  
#      from the required directory


complete <- function(directory = 'specdata', id = 1:332){
      b <- numeric(length = length(id))
      I <- numeric(length = length(id))
      a = 1
      for(i in id){
            b[a] <-  nrow(dataf(directory, id = i)[complete.cases(dataf(directory, id = i)),])
            ##loading the files using dataf function and calculating the returning the rows which do not have NAs.
            I[a] <- i
            a = a + 1
      }
      cc <- as.data.frame(cbind(I, b))
      colnames(cc) <- c('id', 'nobs')
      cc
}