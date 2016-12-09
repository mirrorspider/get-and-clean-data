# Header information

if (!require(dplyr)){
        stop("The dplyr package is required to run this .R file")
}

GetData <- function(base.dir){
        if (!dir.exists(base.dir)) {
                dir.create(base.dir)
                message(paste(base.dir, "directory created"))
        }
        if (!dir.exists(paste0(base.dir,"/UCI HAR Dataset"))) {
                zip.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                data.file <- paste0(base.dir, "/UCIHAR.zip")
                download.file(zip.url, data.file)
                unzip(data.file, exdir = base.dir)
                message("UCI HAR Dataset imported")
        }
        data.dir <- paste0(base.dir, "/UCI HAR Dataset/")
        data.dir
}

ReadLabels <- function(data.dir, label.file.name){
        label.file <- paste0(data.dir, label.file.name)
        labels.frame <- read.table(label.file, col.names = c("key", "value"))
        labels.frame
}

ExtractColumns <- function(telemetry){
        column.pattern <- "(std|mean)"
        output <- telemetry[, grep(column.pattern, colnames(telemetry), value = TRUE)]
        output
        
}

FormatHeading <- function(column.heading){
        output <- tolower(column.heading)
        output <- gsub("\\.", "", output)
        output
}

FormatAllHeadings <- function(column.headings){
        output <- sapply(column.headings, FormatHeading, simplify = TRUE, USE.NAMES = FALSE)
        output
}

FormatActivityLabel <- function(labels.frame, value.to.format){
        output <- labels.frame[value.to.format,"value"]
        output <- tolower(output)
        output <- gsub("_", "", output)
        output
}

FormatAllActivityLabels <- function(labels.frame, frame.to.format){
        output <- sapply(frame.to.format, FUN = function(x){ y <- FormatActivityLabel(labels.frame, x); y})
        output
}

ReadTestSet <- function(data.dir, features.frame, labels.frame, test.set){
        test.set.dir <- paste0(data.dir, test.set, "/")
        if (!dir.exists(test.set.dir)){
                stop("No such test set")
        }
        activities.file <- paste0(test.set.dir, "y_", test.set, ".txt")
        subjects.file <- paste0(test.set.dir, "subject_", test.set, ".txt")
        telemetry.file <- paste0(test.set.dir, "X_", test.set, ".txt")
        
        activities.frame <- read.table(activities.file, col.names = c("activity"))
        subjects.frame <- read.table(subjects.file, col.names = c("subject"))
        telemetry.frame <- read.table(telemetry.file, col.names = features.frame$value)
        
        telemetry.frame <- ExtractColumns(telemetry.frame)
        
        colnames(telemetry.frame) <- FormatAllHeadings(colnames(telemetry.frame))
        
        activities.frame$activity <- FormatAllActivityLabels(labels.frame, activities.frame$activity)
        
        test.set.frame <- data.frame(activities.frame, subjects.frame, telemetry.frame)
        
        test.set.frame
}

CombineTestSets <- function(data.dir, features.frame, labels.frame){
        test.frame <- ReadTestSet(data.dir, features.frame, labels.frame, "test")
        train.frame <- ReadTestSet(data.dir, features.frame, labels.frame, "train")

        collated.frame <- merge(test.frame, train.frame, all = TRUE)

        collated.frame
        
}

ProduceSummaryData <- function(data.set){
        tbl.data.set <- as.tbl(data.set)
        grp.cols <- c("subject", "activity")
        dots <- lapply(grp.cols, as.symbol)
        ds <- group_by_(tbl.data.set, .dots = dots) %>% summarise_each(funs(mean))
        ds <- as.data.frame(ds)
}

WriteOutputFile <- function(base.dir, out.file.name, data.set){
        output.dir <- paste0(base.dir, "/output")
        if (!dir.exists(output.dir)){
                dir.create(output.dir)
        }
        output.file <- paste0(output.dir, "/", out.file.name)
        write.csv(data.set, file = output.file, quote = FALSE, row.names = FALSE)
}

Main <- function(){
        base.dir <- "./data"
        data.dir <- GetData(base.dir)
        labels.frame <- ReadLabels(data.dir, "activity_labels.txt")
        features.frame <- ReadLabels(data.dir, "features.txt")
        combined.har.data <<- CombineTestSets(data.dir, features.frame, labels.frame)
        summary.har.data <<- ProduceSummaryData(combined.har.data)
        WriteOutputFile(base.dir, "uci_har_mean_std.csv", combined.har.data)
        WriteOutputFile(base.dir, "summary_uci_har.csv", summary.har.data)
}

Main()