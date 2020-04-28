##3. Ranking hospitals by outcome in a state

# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call
#
# rankhospital("MD", "heart failure", 5)
# 
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values "best", "worst", or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.

# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name.

# The function should check the validity of its arguments. If an invalid state value is passed to rankhospital,
# the function should throw an error via the stop function with the exact message "invalid state". If an invalid
# outcome value is passed to rankhospital, the function should throw an error via the stop function with
#the exact message "invalid outcome".

rankhospital <- function(state, outcome, num){
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      data <- data[,c(1:8,11,17,23)]
      colnames(data)[c(2, 9, 10, 11)] <- c("hosp_name", "heart_attack", "heart_failure", "pneumonia")
      if(!(state %in% data$State)){
            #checking the validity of the argument passed
            stop('invalid state')
      } else if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
            stop('invalid outcome')
      } else{
            outcome2 <- gsub(' ', '_', outcome)
            data <- data[data$State == state,c('hosp_name', 'State', outcome2)]
            data[,outcome2] <- suppressWarnings(as.numeric(data[,outcome2]))
            #suppressing the warning as na's will be introduced
            data <- data[!(is.na(data[,outcome2])),]
            if(num == 'best'){
                  rank <- 1
            } else if(num == 'worst'){
                  rank <- nrow(data)
            } else if(num <= nrow(data)){
                  rank <- num
            } else {
                  rank <- NA
            }
      }
      if(!(is.na(rank))){
            return(data[order(data[,outcome2], data[,'hosp_name']),][rank,1])
      } else{
            return(NA)
      }
}