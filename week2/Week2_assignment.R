#coursera - john hopkins university - R Programming - week2 assignment

# # For this first programming assignment you will write three functions that are meant to 
#       interact with dataset that accompanies this assignment. The dataset is contained in a 
#       zip file specdata.zip that you can download from the Coursera web site.



# Although this is a programming assignment, you will be assessed using a separate quiz.


# Data
# The zip file containing the data can be downloaded here:
#       
#       specdata.zip [2.4MB]

# # The zip file contains 332 comma-separated-value (CSV) files containing pollution 
#       monitoring data for fine particulate matter (PM) air pollution at 332 locations 
#       in the United States. Each file contains data from a single monitor and the ID number 
#       for each monitor is contained in the file name. For example, data for monitor 200 is 
#       contained in the file "200.csv". Each file contains three variables:

#       Date: the date of the observation in YYYY-MM-DD format (year-month-day)
#     sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
#     nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)


# For this programming assignment you will need to unzip this file and create the 
#      directory 'specdata'. Once you have unzipped the zip file, do not make any 
#     modifications to the files in the 'specdata' directory. In each file you'll 
#     notice that there are many days where either sulfate or nitrate (or both) are 
#     missing (coded as NA). This is common with air pollution monitoring data in the United States.

#Function to read the desired csv files in the specified directory
dataf <- function(dir = 'specdata', id = 2){
      # here 'dir' is the directory from which we want to read the data and
      #     'id', a 3 digit number, is the name of the file (which is in id.csv format) we want to load
      direc <- paste(getwd(), dir, sep = '/')
      if(id<=9){
            assign(paste('data', id, sep = ''), read.csv(paste(direc, paste(0, 0, id, '.csv', sep = ''), sep = '/')))
      } else if(id<=99){
            assign(paste('data', id, sep = ''), read.csv(paste(direc, paste(0, id, '.csv', sep = ''), sep = '/')))
      } else{
            assign(paste('data', id, sep = ''), read.csv(paste(direc, paste(id, '.csv', sep = ''), sep = '/')))
      }
      return(eval(parse(text = paste('data', id, sep = ''))))
}

# Part 1
# # Write a function named 'pollutantmean' that calculates the mean of a 
#       pollutant (sulfate or nitrate) across a specified list of monitors. 
#       The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#       Given a vector monitor ID numbers, 'pollutantmean' reads that monitors 
#       ' particulate matter data from the directory specified in the 'directory' 
#       argument and returns the mean of the pollutant across all of the monitors, 
#       ignoring any missing values coded as NA.

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

# Part 2
# Write a function that reads a directory full of files and reports the number 
#      of completely observed cases in each data file. The function should return 
#      a data frame where the first column is the name of the file and the second column 
#      is the number of complete cases. A prototype of this function follows

complete <- function(directory = 'specdata', id = 1:332){
      b <- numeric(length = length(id))
      I <- numeric(length = length(id))
      a = 1
      for(i in id){
            b[a] <-  nrow(dataf(directory, id = i)[complete.cases(dataf(directory, id = i)),])
            #loading the files using dataf function and calculating the returning the rows which do not have NAs.
            I[a] <- i
            a = a + 1
      }
      cc <- as.data.frame(cbind(I, b))
      colnames(cc) <- c('id', 'nobs')
      cc
}


# Part 3
# # Write a function that takes a directory of data files and a threshold for 
#       complete cases and calculates the correlation between sulfate and nitrate 
#       for monitor locations where the number of completely observed cases 
#       (on all variables) is greater than the threshold. The function should 
#       return a vector of correlations for the monitors that meet the threshold 
#       requirement. If no monitors meet the threshold requirement, then the function 
#       should return a numeric vector of length 0. A prototype of this function follows

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