## Coursera:    Getting and Cleaning Data (Course 3 - Data Science)
## Assignment:  Getting and Cleaning Data Course Project
## File Name:   run_analysis.R
## Programmer:  Clara A. ("clarablara" on GitHub)
## Date:        01/14/2017
##
## Peer-Graded Assignment: Getting and Cleaning Data Course Project
##
## You should create one R script called run_analysis.R that does the following:
## 
##  1. Merges the training and the test sets to create one data set
##  2. Extracts only the measurements on the mean and standard deviation for each measurement
##  3. Uses descriptive activity names to name the activities in the data set
##  4. Appropriately labels the data set with descriptive variable names
##  5. From the data set in step 4, creates a second, independent tidy data set with the average 
##      of each variable for each activity and each subject


## 1. merge_data() - Merges the training and the test sets to create one data set

merge_data <- function() {
    
    ## Message to user that data sets are being read and merged
    message("... Working on Step 1: Reading and merging Samsung data sets from \"data\" folder ...")
    
    ## Read "test" data: Test Subjects, Test Labels, Test Set
    subject_test <- read.table("data/test/subject_test.txt", header = FALSE)
    Y_test <- read.table("data/test/Y_test.txt", sep = " ", header = FALSE)
    X_test <- read.table("data/test/X_test.txt", header = FALSE)
    
    ## Column bind all "test" data sets
    data_test <- cbind(subject_test, Y_test, X_test)
    
    ## Read "train" data: Train Subjects, Train Labels, Train Set
    subject_train <- read.table("data/train/subject_train.txt", header = FALSE)
    Y_train <- read.table("data/train/Y_train.txt", sep = " ", header = FALSE)
    X_train <- read.table("data/train/X_train.txt", header = FALSE)
    
    ## Column bind all "train" data sets
    data_train <- cbind(subject_train, Y_train, X_train)
    
    ## Row bind "test" and "train" data sets to create one master set of raw data
    data_raw <- rbind(data_test, data_train)
    
    ## Read "features" labels
    features <- read.table("data/features.txt", header = FALSE)

    ## Create vector of column names for "Subject", "Activity", and "Features"
    features_names <- c("Subject", "Activity", as.character(features$V2))

    ## Set column names for master set of raw data
    colnames(data_raw) <- features_names
    
    ## Return master set of raw data
    data_raw
    
}


## 2. extract_data() - Extracts the measurements on the mean and standard deviation for each measurement

extract_data <- function() {
    
    ## Get master set of raw data from merge_data() function
    data_raw <- merge_data()
    
    ## Select columns that contain mean and standard deviation data
    x <- grepl("mean\\(\\)|std\\(\\)", colnames(data_raw))
    
    ## Sets "Subject" and "Activity" column names as "TRUE"
    x[1] = TRUE
    x[2] = TRUE
    
    ## Create data frame with extracted mean and standard deviation data columns from full data set
    data_meanstd <- data.frame(data_raw[x])
    
    ## Message to user that mean and std measurments are being extracted
    message("... Working on Step 2: Extracting mean and standard deviation measurements from raw data ...")
    
    ## Return data frame with mean and standard deviation data
    data_meanstd
    
}


## 3. activity_names() - Sets descriptive activity names to name the activities in the data set

activity_names <- function() {
    
    ## Get mean and standard deviation data from extract_data() function
    data_meanstd <- extract_data()
    
    ## Give descriptive activity names to the activities in the data set
    for (i in 1:nrow(data_meanstd)) {
        if (data_meanstd$Activity[i] == 1) {
            data_meanstd$Activity[i] = "Walking"
        } else if (data_meanstd$Activity[i] == 2) {
            data_meanstd$Activity[i] = "WalkingUpstairs"
        } else if (data_meanstd$Activity[i] == 3) {
            data_meanstd$Activity[i] = "WalkingDownstairs"
        } else if (data_meanstd$Activity[i] == 4) {
            data_meanstd$Activity[i] = "Sitting"
        } else if (data_meanstd$Activity[i] == 5) {
            data_meanstd$Activity[i] = "Standing"
        } else if (data_meanstd$Activity[i] == 6) {
            data_meanstd$Activity[i] = "Laying"
        }
    }
    
    ## Message to user descriptive activity names are being generated
    message("... Working on Step 3: Labelling data set with descriptive activity names ...")
    
    ## Return data frame with descriptive activity names
    data_meanstd
    
}


## 4. variable_names() - Labels the data set with descriptive variable names (for features)

variable_names <- function() {
    
    ## Get compiled data from activity_names() function
    data <- activity_names()
    
    ## Remove dot (".") and capitalize "Mean" and "Std" to make variable names cleaner
    names(data) <- sub("\\.m", "M", names(data))
    names(data) <- sub("\\.s", "S", names(data))
    
    ## Remove dots ("...") to make variable names cleaner
    names(data) <- sub("\\...", "", names(data))
    
    ## Remove dots ("..") from end of some variable names to make them cleaner
    names(data) <- sub("\\..", "", names(data))
    
    ## Change "t" to "time and "f" to "freq" to make variable names more descriptive
    names(data) <- sub("^t", "time", names(data))
    names(data) <- sub("^f", "freq", names(data))
    
    ## Message to user descriptive variable names are being generated
    message("... Working on Step 4: Labelling data set with descriptive variable names ...")
    
    ## Return data frame with descriptive variable names
    data
    
}


## 5. tidy_data() - Creates tidy data set with the average of each variable for each activity and subject
##      NOTE: This function makes use of {dplyr} package

tidy_data <- function() {
    
    ## Load {dplyr} package
    library(dplyr)
    
    ## Get compiled data from variable_names() function, set as dplyr table (for manipulation)
    data <- tbl_df(variable_names())
    
    ## Set "Subject" and "Activity" as factor variables
    data$Subject = factor(data$Subject)
    data$Activity = factor(data$Activity)
    
    ## Manipulate data to group by "Subject" & "Activity" and summarize all variable columns by mean
    ## Get data
    tidy_data <- data %>% 
        ## Group data by Subject, Activity
        group_by(Subject, Activity) %>%
        ## Summarize all columns by mean
        summarize_all(mean)
    
    ## Return tidy data set with with the average of each variable for each activity and subject
    tidy_data
    
    ## Write tidy data set to text file ("tidy_data.txt")
    write.table(tidy_data, file = "tidy_data.txt", row.names = FALSE)
    
    ## Message to user that "tidy_data.txt" has been created
    message("Success! The file \"tidy_data.txt\" has been created in your working directory.")
    
}


## Message to user that program is now running
message("Running run_analysis.R program ...")

## Call tidy_data() function to get wearable computing data and return a tidy data set
tidy_data()

