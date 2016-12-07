# Header information
get_data <- function(){
        if (!dir.exists("./data")) {
                dir.create("./data")
                message("./data directory created")
        }
        if (!dir.exists("./data/UCI HAR Dataset")) {
                zipurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                datafile <- "./data/UCIHAR.zip"
                download.file(zipurl, datafile)
                unzip(datafile, exdir = "./data")
                message("UCI HAR Dataset imported")
        }
        datadir <<- "./data/UCI HAR Dataset/"
}

read_labels <- function(){
        labelfile <- paste0(datadir, "activity_labels.txt")
        featurefile <- paste0(datadir, "features.txt")
        
        labelsframe <<- read.table(labelfile, col.names = c("key", "value"))
        featuresframe <<- read.table(featurefile, col.names = c("key", "value"))
        
}

read_test_set <- function(testset){
        testsetdir <- paste0(datadir, testset, "/")
        if (!dir.exists(testsetdir)){
                stop("No such test set")
        }
        activitiesfile <- paste0(testsetdir, "y_", testset, ".txt")
        subjectsfile <- paste0(testsetdir, "subject_", testset, ".txt")
        telemetryfile <- paste0(testsetdir, "X_", testset, ".txt")
        
        activitiesframe <- read.table(activitiesfile, col.names = c("activity"))
        subjectsframe <- read.table(subjectsfile, col.names = c("subject"))
        telemetryframe <- read.table(telemetryfile, col.names = featuresframe$value)
        
        testsetframe <- data.frame(activitiesframe, subjectsframe, telemetryframe)
        
        testsetframe
}

collate_test_sets <- function(){
        testframe <- read_test_set("test")
        trainframe <- read_test_set("train")

        collatedframe <- merge(testframe, trainframe, all = TRUE)

        collatedframe
        
}

main <- function(){
        get_data()
        read_labels()
        cl <- collate_test_sets()
        cl
}

main()