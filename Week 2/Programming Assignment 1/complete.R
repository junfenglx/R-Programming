#-------------------------------------------------------------------------------
#
# Reads a directory full of files and reports the number of completely observed 
# cases in each data file.
#
# v 1.1 Tim Vigers 9/15/15
#
#-------------------------------------------------------------------------------


# Define the "complete" function and specify 2 arguments
complete <- function(directory, id = 1:332) {
# Create a list of files in "directory," and include the directory in the 
# file name.
        files_list <- list.files(directory, full.names = TRUE)
# Create an empty dataframe in which to store data.
        dataframe <- data.frame()
# Iterate through each number specified in "id" argument.
        for(i in id) {
# For each file, make x a logical vector where each "TRUE" represents a row in
# the file's data with no NA values.
                x <- complete.cases(read.csv(files_list[i]))
# Make x the sum of all "TRUE" values (i.e. the number of rows with no missing
# data).
                x <- sum(x[x==TRUE])
# Row bind the file id number and number of complete cases to the empty 
# dataframe.
                dataframe <- rbind(dataframe, c(i, x))
        }
# Name the columns of the dataframe.
colnames(dataframe) <- c('id', "nobs")
# Return the final data frame containing the id number and number of complete
# cases for each file specified.
dataframe
}

# All done.
        