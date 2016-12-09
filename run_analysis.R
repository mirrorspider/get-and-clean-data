# run_analysis.R
# This script contains the functions required to process
# the UCI Human Activity Recognition (HAR) dataset for the Coursera
# getting and cleaning data.
# The script will obtain the data; merge the test and training datasets;
# subset and summarise the required data and write the results to file and global variables

# The script uses the dplyr package for dealing with data tables
if (!require(dplyr)){
        stop("The dplyr package is required to run this .R file")
}

# The DetermineBaseDirectory function looks for the HAR Dataset in the working directory
# if it finds the data the function returns a reference to the working directory
# otherwise it returns the string "./data"
DetermineBaseDirectory <- function(){
        
        if(file.exists("./UCI HAR Dataset.zip") | dir.exists("./UCI HAR Dataset")){
                base.dir <- "."
        }
        else {
                base.dir <- "./data"
        }
        
        base.dir
}

# The GetData function checks for the existence of the required data in the supplied directory.
# It downloads and/or unzips the data as required.
# The function returns the name of the unzipped directory.
GetData <- function(base.dir){
        # create the supplied directory if it does not exist
        if (!dir.exists(base.dir)) {
                dir.create(base.dir)
                message(paste(base.dir, "directory created"))
        }
        # check if there's already an instance of the data
        if (!dir.exists(paste0(base.dir,"/UCI HAR Dataset"))) {
                data.file <- paste0(base.dir, "/UCI HAR Dataset.zip")
                # check whether the zip file exists and if not download it
                if (!file.exists(data.file)){
                        zip.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                        download.file(zip.url, data.file)
                        message("UCI HAR Data set downloaded")
                }
                # unzip the data
                unzip(data.file, exdir = base.dir)
                message("UCI HAR Dataset unzipped")
        }
        # return the name of the directory containing the data
        data.dir <- paste0(base.dir, "/UCI HAR Dataset/")
        data.dir
}

# The ReadLabels function takes as parameters the top-level data directory and the name of a file.
# It produces a data.frame with 2 dimensions (key and value) from thet file.
# The function is used to obtain the data from the HAR actvity_label and features files
ReadLabels <- function(data.dir, label.file.name){
        label.file <- paste0(data.dir, label.file.name)
        labels.frame <- read.table(label.file, col.names = c("key", "value"))
        labels.frame
}

# Given a data.frame the ExtractColumns returns a data.frame including only those columns with
# std or mean in the title
ExtractColumns <- function(telemetry){
        # pattern for grep matching names that contain std or mean
        column.pattern <- "(std|mean)"
        # use grep to define the subset of columns to be returned
        output <- telemetry[, grep(column.pattern, colnames(telemetry), value = TRUE)]
        output
        
}

# FormatHeading formats a column heading in line with "Clean Data" principles
# i.e. all lower case and with fullstops (.) removed
FormatHeading <- function(column.heading){
        output <- tolower(column.heading)
        output <- gsub("\\.", "", output)
        output
}

# FormatAllHeadings uses sapply to apply the FormatHeading function to a vector of column names
FormatAllHeadings <- function(column.headings){
        output <- sapply(column.headings, FormatHeading, simplify = TRUE, USE.NAMES = FALSE)
        output
}

# FormatActivityLabel takes in a value, looks it up in the supplied labels.frame
# and returns the matching value.
# This is used to change the activity values 1-6 into their lower case natural langauge counterparts'
# underscores are removed according to "Clean Data" principles'
FormatActivityLabel <- function(labels.frame, value.to.format){
        output <- labels.frame[value.to.format,"value"]
        output <- tolower(output)
        output <- gsub("_", "", output)
        output
}


# FormatAllActivityLabels uses sapply to apply the FormatActivityLable function to a vector of values
FormatAllActivityLabels <- function(labels.frame, frame.to.format){
        output <- sapply(frame.to.format, FUN = function(x){ y <- FormatActivityLabel(labels.frame, x); y})
        output
}

# ReadTestSet takes in the name of the top-level data directory, two data.frames one containing
# the activity labels, the other the names of the features and a string representing the test set to 
# be read.
# The function reads the 3 files that makes up the set, provides the correct col.names,
# extracts only the required columns from the telemetry data and concatenates the information together
# into a data.frame
ReadTestSet <- function(data.dir, features.frame, labels.frame, test.set){
        test.set.dir <- paste0(data.dir, test.set, "/")
        if (!dir.exists(test.set.dir)){
                stop("No such test set")
        }
        # set up the file names
        activities.file <- paste0(test.set.dir, "y_", test.set, ".txt")
        subjects.file <- paste0(test.set.dir, "subject_", test.set, ".txt")
        telemetry.file <- paste0(test.set.dir, "X_", test.set, ".txt")
        
        # read the files into data.frames
        activities.frame <- read.table(activities.file, col.names = c("activity"))
        subjects.frame <- read.table(subjects.file, col.names = c("subject"))
        # the telemetry file uses the feature names from the features.txt file as its col.names
        telemetry.frame <- read.table(telemetry.file, col.names = features.frame$value)
        
        # extract only those columns containing either mean or std
        telemetry.frame <- ExtractColumns(telemetry.frame)
        
        # format the column names in line with "Clean Data" principles
        colnames(telemetry.frame) <- FormatAllHeadings(colnames(telemetry.frame))
        
        # format the activity labels to turn them into natural language labels
        activities.frame$activity <- FormatAllActivityLabels(labels.frame, activities.frame$activity)
        
        # concatenate the information into a single data.frame
        test.set.frame <- data.frame(activities.frame, subjects.frame, telemetry.frame)
        
        test.set.frame
}

# CombineTestSets reads both the test and train test sets and combines them into a single frame
CombineTestSets <- function(data.dir, features.frame, labels.frame){
        test.frame <- ReadTestSet(data.dir, features.frame, labels.frame, "test")
        train.frame <- ReadTestSet(data.dir, features.frame, labels.frame, "train")
        
        # merge matches on all columns (all the columns should be shared by both sets) and returns
        # all the rows from both frames using all = TRUE
        collated.frame <- merge(test.frame, train.frame, all = TRUE)

        collated.frame
        
}

# ProduceSummaryData processes the collated data set to return summary information.
# The function uses dplyr's group_by_ and summarise_each function to group the data
# by activity and subject and return the mean of each of the telemetry columns for that group
ProduceSummaryData <- function(data.set){
        tbl.data.set <- as.tbl(data.set)
        # The data.frame has been passed into the function as a parameter so it's necessary to
        # define the group by columns first as strings and then turn those into symbols to use
        # in the .dots parameter
        grp.cols <- c("activity", "subject")
        dots <- lapply(grp.cols, as.symbol)
        # The group_by_ function is used because the data set has been passed in as a parameter
        # summarise_each performs the mean function on each of the feature columns
        out.data.set <- group_by_(tbl.data.set, .dots = dots) %>% summarise_each(funs(mean))
        out.data.set <- as.data.frame(out.data.set)
}

# WriteOutputFile takes in the working directory, a file stem, a data.frame to write and a
# boolean representing whether csv output is required.
# It writes the contents of the data.frame to the file.
WriteOutputFile <- function(base.dir, out.file.name, data.set, csv){
        output.dir <- paste0(base.dir, "/output")
        # create the output directory if it doesn't exist
        if (!dir.exists(output.dir)){
                dir.create(output.dir)
        }
        # create the full file stem
        output.file <- paste0(output.dir, "/", out.file.name)
        if (csv) {
                # if csv output is required append the .csv suffix and use write.csv
                output.file <- paste0(output.file, ".csv")
                # row.names are not required and quote is set to FALSE to prevent quotes beign written
                # around strings
                write.csv(data.set, file = output.file, quote = FALSE, row.names = FALSE)
        }
        else {
                # if plain text is required append the .txt suffix and use write.table
                output.file <- paste0(output.file, ".txt")
                # row.names are not required and quote is set to FALSE to prevent quotes beign written
                # around strings
                write.table(data.set, file = output.file, quote = FALSE, row.names = FALSE)
        }
}

# The Main function forms the program flow and logic. It executes all the other functions.
Main <- function(){
        # check where the data is to be held
        base.dir <- DetermineBaseDirectory()
        # make sure the data is available
        data.dir <- GetData(base.dir)
        # get the activity label information
        labels.frame <- ReadLabels(data.dir, "activity_labels.txt")
        # get the feature name information
        features.frame <- ReadLabels(data.dir, "features.txt")
        # create the combined data set in the required format, store it in a global variable for later access
        combined.har.data <<- CombineTestSets(data.dir, features.frame, labels.frame)
        # create the summary data in the required format, store it in a global variable for later access
        summary.har.data <<- ProduceSummaryData(combined.har.data)
        # Write the data to output txt files
        WriteOutputFile(base.dir, "uci_har_mean_std", combined.har.data, csv = FALSE)
        WriteOutputFile(base.dir, "summary_uci_har", summary.har.data, csv = FALSE)
}

Main()