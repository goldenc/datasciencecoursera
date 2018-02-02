best <- function(state, outcome) {
        rankhospital(state, outcome, "best")
}

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        states <- unique(data$State)
        orderedStates <- states[order(states)]
        results <- lapply(orderedStates, rankhospital, outcome, num, data)
        
        setNames(data.frame(unlist(results), orderedStates), c("hospital", "state"))
}

rankhospital <- function(state, outcome, num = "best", data = NULL) {
        if (is.null(data)) {
                data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        }
        
        ordered <- getOrderedStateValues(data, state, outcome)
        ordered[1, "Hospital.Name"]
        
        index <- numeric()
        if(num == "best") {
                index = 1
        } else if (num == "worst") {
                index = nrow(ordered)
        } else {
                index = as.numeric(num)
        }
        
        if (is.na(index)) {
                stop("invalid num")
        }
        
        if (index > nrow(ordered)) {
                NA
        } else {
                ordered[index, "Hospital.Name"]
        }
}

getOrderedStateValues <- function(data, state, outcome) {
        stateValues <- data[data$State == state, ]
        
        if(nrow(stateValues) == 0) {
                stop("invalid state")
        }
        
        columnName <- character()
        if(outcome == "heart failure") {
                columnName = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "heart attack") {
                columnName = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "pneumonia") {
                columnName = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        stateValues[, columnName] <- suppressWarnings(sapply(stateValues[, columnName], as.numeric))
        ordered <- order(stateValues[[columnName]], stateValues$Hospital.Name)
        reduced <- stateValues[ordered, c("Hospital.Name", columnName)]
        reduced[complete.cases(reduced), ]
}