## Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome_name, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    ## 
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
    if (num == 'best') {
        result <-
            subset(
                filter_data$Hospital.Name,
                filter_data[,selected_column] == min(filter_data[,selected_column])
            )
    } else if (num == 'worst') {
        result <-
            subset(
                filter_data$Hospital.Name,
                filter_data[,selected_column] == max(filter_data[,selected_column])
            )
    } else {
        # sorting data
        sorted_filter_data<-filter_data[order(filter_data[,selected_column],filter_data$Hospital.Name),]
        View(sorted_filter_data)
        result<-sorted_filter_data[num,'Hospital.Name']
        
    }
    print(result)
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}
