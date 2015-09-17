#-------------------------------------------------------------------------------
#
# Calculates the mean of a pollutant (sulfate or nitrate) across a specified 
# list of monitors.
#
# v 1.1 Tim Vigers 9/15/15
#
#-------------------------------------------------------------------------------


# Define the pollutantmean function and specify 3 arguments.
pollutantmean <- function(directory, pollutant, id = 1:332) {
# Create a list of files in "directory," and includes the directory in the 
# file name.
        files <- list.files(directory, full.names = TRUE)
# Create an empty dataframe for storing data from multiple csv files in 
# "directory".
        dataframe <- data.frame()
# Create a for loop that iterates through each file in "directory" specified 
# by "id," and binds the rows together in the variable dataframe
        for (i in id) {
                dataframe <- rbind(dataframe, read.csv(files[i]))
        }
# Calculcate and return the mean of the column specified by "pollutant"
        mean(dataframe[, pollutant], na.rm = TRUE)
}

# All done.