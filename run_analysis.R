# Header information

get_data <- function(basedir){
        if (!dir.exists(basedir)) {
                dir.create(basedir)
                message(paste(basedir, "directory created"))
        }
        if (!dir.exists(paste0(basedir,"/UCI HAR Dataset"))) {
                zipurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                datafile <- paste0(basedir, "/UCIHAR.zip")
                download.file(zipurl, datafile)
                unzip(datafile, exdir = basedir)
                message("UCI HAR Dataset imported")
        }
        datadir <- paste0(basedir, "/UCI HAR Dataset/")
        datadir
}

read_labels <- function(datadir){
        labelfile <- paste0(datadir, "activity_labels.txt")
        featurefile <- paste0(datadir, "features.txt")
        
        labelsframe <- read.table(labelfile, col.names = c("key", "value"))
        featuresframe <- read.table(featurefile, col.names = c("key", "value"))
        
        output <- list(labelsframe, featuresframe)
        output
        
}

extract_req_columns <- function(telemetry){
        colpattern <- "(std|mean)"
        output <- telemetry[, grep(colpattern, colnames(telemetry), value = TRUE)]
        output
        
}

format_heading <- function(colheading){
        output <- tolower(colheading)
        output <- gsub("\\.", "", output)
        output
}

format_all_headings <- function(colheadings){
        output <- sapply(colheadings, format_heading, simplify = TRUE, USE.NAMES = FALSE)
        output
}

format_activity_label <- function(labelsframe, value_to_format){
        output <- labelsframe[value_to_format,"value"]
        output <- tolower(output)
        output <- gsub("_", "", output)
        output
}

format_all_activity_labels <- function(labelsframe, frame_to_format){
        output <- sapply(frame_to_format, FUN = function(x){ y <- format_activity_label(labelsframe, x); y})
        output
}

read_test_set <- function(datadir, featuresframe, labelsframe, testset){
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
        
        telemetryframe <- extract_req_columns(telemetryframe)
        
        colnames(telemetryframe) <- format_all_headings(colnames(telemetryframe))
        
        activitiesframe$activity <- format_all_activity_labels(labelsframe, activitiesframe$activity)
        
        testsetframe <- data.frame(activitiesframe, subjectsframe, telemetryframe)
        
        testsetframe
}

collate_test_sets <- function(datadir, featuresframe, labelsframe){
        testframe <- read_test_set(datadir, featuresframe, labelsframe, "test")
        trainframe <- read_test_set(datadir, featuresframe, labelsframe, "train")

        collatedframe <- merge(testframe, trainframe, all = TRUE)

        collatedframe
        
}

main <- function(){
        basedir <- "./data"
        datadir <- get_data(basedir)
        labellist <- read_labels(datadir)
        labelsframe <- labellist[[1]]
        featuresframe <- labellist[[2]]
        cl <- collate_test_sets(datadir, featuresframe, labelsframe)
        cl
}

dt <- main()