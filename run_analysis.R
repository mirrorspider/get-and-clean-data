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
}

main <- function(){
        get_data()
}

main()