# coursera R Programming week4 assignment

# Introduction
# Download the file ProgAssignment3-data.zip file containing the data for Programming Assignment 3 from
# the Coursera web site. Unzip the file in a directory that will serve as your working directory. 

#The zip file for this assignment contains three files

# . outcome-of-care-measures.csv: Contains information about 30-day mortality and readmission rates
# for heart attacks, heart failure, and pneumonia for over 4,000 hospitals.
# . hospital-data.csv: Contains information about each hospital.
# . Hospital_Revised_Flatfiles.pdf: Descriptions of the variables in each file (i.e the code book).

# A description of the variables in each of the files is in the included PDF file named Hospital_Revised_Flatfiles.pdf.
# This document contains information about many other files that are not included with this programming
# assignment. You will want to focus on the variables for Number 19 ("Outcome of Care Measures.csv") and
# Number 11 ("Hospital Data.csv"). You may find it useful to print out this document (at least the pages for
# Tables 19 and 11) to have next to you while you work on this assignment. In particular, the numbers of
# the variables for each table indicate column indices in each table (i.e. "Hospital Name" is column 2 in the
# outcome-of-care-measures.csv file).

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
            #checking the validity of the argument
            stop('invalid state')
      } else if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
            stop('invalid outcome')
      } else{
            outcome2 <- gsub(' ', '_', outcome)
            #making the 'outcome' string consitent with col names
            data <- data[data$State == state,c('hosp_name', 'State', outcome2)]
            data[,outcome2] <- suppressWarnings(as.numeric(data[,outcome2]))
            #suppressing the warning as na's will be introduced
            data <- data[!(is.na(data[,outcome2])),]
            return(data[order(data[,outcome2], data[,'hosp_name']),][1,1])
      }
}


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
            #checking the validity of the arguments
            stop('invalid state')
      } else if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
            stop('invalid outcome')
      } else{
            outcome2 <- gsub(' ', '_', outcome)
            #making the 'outcome' string consitent with col names
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
            #checking the validity of the arguments passed
            stop('invalid outcome')
      } else{
            outcome2 <- gsub(" ", "_", outcome)
            #making the 'outcome' string consitent with col names
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

