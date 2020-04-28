#3. Ranking hospitals in all states

# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
# The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
# hospitals when deciding the rankings.


# Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
# that the rankhospital function handles ties.

# NOTE: For the purpose of this part of the assignment (and for efficiency), your function should NOT call
# the rankhospital function from the previous section.

# The function should check the validity of its arguments. If an invalid outcome value is passed to rankall,
# the function should throw an error via the stop function with the exact message "invalid outcome". The num
# variable can take values "best", "worst", or an integer indicating the ranking (smaller numbers are better).
# If the number given by num is larger than the number of hospitals in that state, then the function should
# return NA.

rankall <- function(outcome, num = 'best'){
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      data <- data[,c(1:8,11,17,23)]
      colnames(data)[c(2, 9, 10, 11)] <- c("hosp_name", "heart_attack", "heart_failure", "pneumonia")
      
      if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
         #checking the validity of the argument passed
            stop('invalid outcome')
      } else{
            outcome2 <- gsub(" ", "_", outcome)
            data <- data[,c('hosp_name', 'State', outcome2)]
            data[,outcome2] <- suppressWarnings(as.numeric(data[,outcome2]))
            #suppressing the warning as na's will be introduced
            data <- data[!(is.na(data[,outcome2])),]
            
            h_name <- c()
            s_name <- c()
            statess <- unique(data[['State']])
            
            for(j in statess){
                  datas <- data[data[,'State'] == j,]
                  
                  if(num == 'best'){
                        rank <- 1
                  } else if(num == 'worst'){
                        rank <- nrow(datas)
                  } else if(num <= nrow(datas)){
                        rank <- num
                  } else {
                        rank <- NA
                  }
                  #datas[['State']] <- as.factor(datas[['State']])
                  if(!(is.na(rank))){
                        s_name <- c(s_name, j)
                        h_name <- c(h_name, datas[order(datas[,outcome2], datas[,'hosp_name']),][rank,'hosp_name'])
                  } else{
                        s_name <- c(s_name, j)
                        h_name <- c(h_name, NA)
                  }
            }
            
            fdata <- as.data.frame(cbind(h_name, s_name))
            colnames(fdata) <- c('hospital', 'state')
            rownames(fdata) <- s_name
            fdata <- fdata[order(fdata[,'state']),]
            return(fdata)
      }
}