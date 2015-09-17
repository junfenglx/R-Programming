#-------------------------------------------------------------------------------
#
# Takes a directory of data files and a threshold for complete cases and 
# calculates the correlation between sulfate and nitrate for monitor 
# locations where the number of completely observed cases (on all variables) 
# is greater than the threshold.
#
# v 1.1 Tim Vigers 9/16/15
#
#-------------------------------------------------------------------------------


# Define the "corr" function and specify 2 arguments.
corr <- function(directory, threshold = 0) {
# Create a list of files in "directory," and include the directory in the 
# file name.
        files_list <- list.files(directory, full.names = TRUE)
# Create an empty numeric vector for storing correlations.
        correlations <- numeric()
# Iterate through each file in the directory.
        for(i in files_list) {
# Store the contents of the file in the dataframe "file."
                file <- read.csv(i)
# Make x a logical vector where each "TRUE" represents a row in the file's data 
# with no NA values.
                x <- complete.cases(file)
# Make x the sum of all "TRUE" values (i.e. the number of rows with no missing
# data).
                x <- sum(x[x==TRUE])
# If the number of complete cases is above threshold, add the correlation 
# between sulfate and nitrate to the vector correlations. Because use = 
# complete.obs, missing values are handled by casewise deletion.
                if(x > threshold) {
                        correlations <- c(correlations, cor(file$sulfate, file$
                                                                    nitrate, 
                                                            use = 
                                                                "complete.obs"))
                }
        }
# Return the vector correlations. Each item in the vector is a correlation from
# a file where the number of complete observations was greater then the 
# threshold.
        correlations
}

# All done.