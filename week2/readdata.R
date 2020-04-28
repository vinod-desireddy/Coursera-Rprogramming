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