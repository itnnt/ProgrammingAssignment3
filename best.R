## Write a function called best that take two arguments: 
## the 2-character abbreviated name of a state and an outcome name. 

## The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. 

## The hospital name is the name provided in the Hospital.Name variable. 
## The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 

## Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.

## Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
## be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals "b", "c",
## and "f" are tied for best, then hospital "b" should be returned).

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message "invalid state". If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message "invalid outcome".

load_data<-function() {
    ## Read outcome data
    outcome <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    ## select examing columns
    examing_data <- outcome[,c(
        'Hospital.Name',
        'State',
        'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
        'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
        'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')]
    
    ## rename columns
    colnames(examing_data)<-c('Hospital.Name','State', 'Heart.Attack','Heart.Failure','Pneumonia')
    ## covert digits to numeric
    examing_data$Heart.Attack<-as.numeric(examing_data$Heart.Attack)
    examing_data$Heart.Failure<-as.numeric(examing_data$Heart.Failure)
    examing_data$Pneumonia<-as.numeric(examing_data$Pneumonia)
    examing_data
}

examing_data<-load_data()

## Finding the best hospital in a state
best <- function(state, outcome_name) {
    ## state: the 2-character abbreviated name of a state, for example: "TX", "MD", "BB", "NY" 
    ## outcome: an outcome name
    
    
    
    ## outcome_name should not contain any space character
    ## all characters will be replaced by period
    outcome_name<-paste(strsplit(outcome_name,' ')[[1]], collapse = '.')
    # print(outcome_name)
    # print(colnames(examing_data))
    # print(grepl(outcome_name, colnames(examing_data), ignore.case = T))
    selected_column <-
        (colnames(examing_data)[grepl(outcome_name, colnames(examing_data), ignore.case = T)])
    if (length(selected_column) == 0) {
        message("invalid outcome")
        stop ("invalid outcome")
    }
    # print(C)
    filter_data <-
        (subset(
            examing_data,
            examing_data$State == state & !is.na(examing_data[,selected_column])
        ))
    if (nrow(filter_data) == 0) {
        stop ("invalid state")
    }
    result <-
        subset(
            filter_data$Hospital.Name,
            filter_data[,selected_column] == min(filter_data[,selected_column])
        )
    print(result)
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}

