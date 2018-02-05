require(dplyr)

getMeasurementAveragesByActivityAndSubject <- function() {
        ds <- getmergeddataset()
        ds %>% group_by(subject, activityName) %>% summarise_all(funs(mean))
}

getmergeddataset <- function() {
        testDataset <- getdataset("test")
        trainingDataset <- getdataset("train")
        rbind(testDataset, trainingDataset)
}

getdataset <- function(directory) {
        measurementNames <- read.table("./data/features.txt", col.names = c("id", "name"))
        activitynames <- read.table("./data/activity_labels.txt", col.names=c("id", "activityName"))
        measurements <- read.table(paste0("./data/", directory, "/x_", directory, ".txt"), col.names = measurementNames$name)
        meanAndStdMeasures <- measurements[, measurementNames[grep("-mean\\(\\)|-std\\(\\)", measurementNames$name), "name"]]
        
        subject <- read.table(paste0("./data/", directory, "/subject_", directory, ".txt"), col.names = c("subject"))
        activities <- read.table(paste0("./data/", directory, "/y_", directory, ".txt"), col.names = c("activityId"))
        activities <- merge(activities, activitynames, by.x = "activityId", by.y = "id")
        
        cbind(subject, meanAndStdMeasures, activities)
}