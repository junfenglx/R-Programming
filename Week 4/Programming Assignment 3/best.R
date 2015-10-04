
#Define the function best with two arguments.
best <- function(state, outcome) {
# Read in the outcome data
        data <- read.csv("~/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
# Create variables that contain acceptable values for state and outcome.
        validstate <- unique(data$State)
        validoutcome <- c("heart attack", "heart failure","pneumonia")
        
# Check that state and outcome arguments are acceptable, return error if not.
        if (!state %in% validstate) {stop("invalid state")}
        if (!outcome %in% validoutcome) {stop("invalid outcome")}

# Subset the data for requested state.
        state.data <- subset(data, State == state)

# Subset the data based on requested outcome.
        if (outcome == "heart attack") {
                final.data <- state.data[,c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
        }
        else if (outcome == "heart failure") {
                final.data <- state.data[,c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
        }
        else {
                final.data <- state.data[,c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
        }
        
             
# Make outcome values numeric for correct sorting.
        final.data[,2] <- as.numeric(final.data[,2])

# Order the data first by outcome value, then by name. 
        sorted_data <- final.data[order(final.data[,2], final.data[,1]), ]
        
# Return the name of the hospital with the best outcome. 
        return(sorted_data[1,1])

# All done.
}