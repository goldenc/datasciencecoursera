require(dplyr)

getMeasurementAveragesByActivityAndSubject <- function() {
        ds <- getmergeddataset()
        ds %>% group_by(subject, activity) %>% summarise_all(funs(mean))
}

getmergeddataset <- function() {
        #Download the Data
        filesPath <- "C:/dev/play/datasciencecoursera/data cleaning/week 4"
        setwd(filesPath)
        if(!file.exists("./data")) {
                dir.create("./data")
        }
        if (!file.exists("./data/Dataset.zip")) {
                fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                download.file(fileUrl,destfile="./data/Dataset.zip")
                unzip(zipfile="./data/Dataset.zip",exdir="./data")
        }
        
        #load the test database
        testDataset <- getdataset("test")
        #load the train database
        trainingDataset <- getdataset("train")
        
        #return the merged table
        arrange(rbind(testDataset, trainingDataset), subject, activity)
}

#function that returns the merged table for a given directory (train or test)
getdataset <- function(directory) {
        #get the names of the measurements
        measurementNames <- read.table("./data/features.txt", col.names = c("id", "name"))
        #get the names of the activities
        activitynames <- read.table("./data/activity_labels.txt", col.names=c("id", "activityName"))
        #load in the measurements, passing in the names of each measurement
        measurements <- read.table(paste0("./data/", directory, "/x_", directory, ".txt"), col.names = measurementNames$name, check.names = FALSE)
        #only interested in measurments for mean and standard deviation
        validNames <- as.character(measurementNames[grep("-mean\\(\\)|-std\\(\\)", measurementNames$name), "name"])
        #reduce measurements to only contain these values
        meanAndStdMeasures <- measurements[, names(measurements) %in% validNames]
        
        #read in the subject for each measurement
        subject <- read.table(paste0("./data/", directory, "/subject_", directory, ".txt"), col.names = c("subject"))
        
        #read in the activity for each measurement
        activities <- read.table(paste0("./data/", directory, "/y_", directory, ".txt"), col.names = c("activityId"))
        
        #merge activities and activity names, joining on the id column
        activities <- merge(activities, activitynames, by.x = "activityId", by.y = "id")
        
        #only keep the activity name
        activities <- activities[, names(activities) == "activityName"]
        
        #join all 3 tables together
        result <- cbind(subject, activities, meanAndStdMeasures)
        
        #rename activity column
        colnames(result)[2] = "activity"
        
        #tidy up column names for better readability and clearer purpose
        names(result)<-gsub("std()", "SD", names(result))
        names(result)<-gsub("mean()", "MEAN", names(result))
        names(result)<-gsub("^t", "time", names(result))
        names(result)<-gsub("^f", "frequency", names(result))
        names(result)<-gsub("Acc", "Accelerometer", names(result))
        names(result)<-gsub("Gyro", "Gyroscope", names(result))
        names(result)<-gsub("Mag", "Magnitude", names(result))
        names(result)<-gsub("BodyBody", "Body", names(result))
        
        #return dataset
        result
}