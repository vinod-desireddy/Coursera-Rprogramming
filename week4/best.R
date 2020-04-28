##1. Finding the best hospital in a state

# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.

# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals "b", "c",
# and "f" are tied for best, then hospital "b" should be returned).

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message "invalid state". If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message "invalid outcome".

best <- function(state, outcome){
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
            return(data[order(data[,outcome2], data[,'hosp_name']),][1,1])
      }
}