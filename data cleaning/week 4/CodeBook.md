
# CODEBOOK.md
## Getting and Cleaning Data Course Project
Instructions for project The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Description of the DATA
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix ‘t’ to denote time) were captured at a constant rate of 50 Hz. and the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) – both using a low pass Butterworth filter.

The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

A Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the ‘f’ to indicate frequency domain signals).

Description of abbreviations of measurements
leading t or f is based on time or frequency measurements.
Body = related to body movement.
Gravity = acceleration of gravity
Acc = accelerometer measurement
Gyro = gyroscopic measurements
Jerk = sudden movement acceleration
Mag = magnitude of movement
mean and SD are calculated for each subject for each activity for each mean and SD measurements.
The units given are g’s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

These signals were used to estimate variables of the feature vector for each pattern:
‘-XYZ’ is used to denote 3-axial signals in the X, Y and Z directions. They total 33 measurements including the 3 dimensions - the X,Y, and Z axes.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag
The set of variables that were estimated from these signals are:
mean(): Mean value
std(): Standard deviation
Data Set Information:
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Load required packages
library(dplyr)

### Download the Data
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

Files in data folder that will be used are
SUBJECT FILES
test/subject_test.txt
train/subject_train.txt
ACTIVITY FILES
test/X_test.txt
train/X_train.txt
DATA FILES
test/y_test.txt
train/y_train.txt
features.txt - Names of column variables in the dataTable
activity_labels.txt - Links the class labels with their activity name.

# Create helper function to read from test or train
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

# merge tables together
#load the test database
testDataset <- getdataset("test")
#load the train database
trainingDataset <- getdataset("train")

#return the merged table
rbind(testDataset, trainingDataset)

# Output
head(str(merged), 2)
'data.frame':	10299 obs. of  68 variables:
 $ subject                                       : int  1 1 1 1 1 1 1 1 1 1 ...
 $ activity                                      : Factor w/ 6 levels "LAYING","SITTING",..: 4 4 4 4 4 4 4 4 4 4 ...
 $ timeBodyAccelerometer-MEAN()-X                : num  0.289 0.278 0.28 0.279 0.277 ...
 $ timeBodyAccelerometer-MEAN()-Y                : num  -0.0203 -0.0164 -0.0195 -0.0262 -0.0166 ...
 $ timeBodyAccelerometer-MEAN()-Z                : num  -0.133 -0.124 -0.113 -0.123 -0.115 ...
 $ timeBodyAccelerometer-SD()-X                  : num  -0.995 -0.998 -0.995 -0.996 -0.998 ...
 $ timeBodyAccelerometer-SD()-Y                  : num  -0.983 -0.975 -0.967 -0.983 -0.981 ...
 $ timeBodyAccelerometer-SD()-Z                  : num  -0.914 -0.96 -0.979 -0.991 -0.99 ...
 $ timeGravityAccelerometer-MEAN()-X             : num  0.963 0.967 0.967 0.968 0.968 ...
 $ timeGravityAccelerometer-MEAN()-Y             : num  -0.141 -0.142 -0.142 -0.144 -0.149 ...
 $ timeGravityAccelerometer-MEAN()-Z             : num  0.1154 0.1094 0.1019 0.0999 0.0945 ...
 $ timeGravityAccelerometer-SD()-X               : num  -0.985 -0.997 -1 -0.997 -0.998 ...
 $ timeGravityAccelerometer-SD()-Y               : num  -0.982 -0.989 -0.993 -0.981 -0.988 ...
 $ timeGravityAccelerometer-SD()-Z               : num  -0.878 -0.932 -0.993 -0.978 -0.979 ...
 $ timeBodyAccelerometerJerk-MEAN()-X            : num  0.078 0.074 0.0736 0.0773 0.0734 ...
 $ timeBodyAccelerometerJerk-MEAN()-Y            : num  0.005 0.00577 0.0031 0.02006 0.01912 ...
 $ timeBodyAccelerometerJerk-MEAN()-Z            : num  -0.06783 0.02938 -0.00905 -0.00986 0.01678 ...
 $ timeBodyAccelerometerJerk-SD()-X              : num  -0.994 -0.996 -0.991 -0.993 -0.996 ...
 $ timeBodyAccelerometerJerk-SD()-Y              : num  -0.988 -0.981 -0.981 -0.988 -0.988 ...
 $ timeBodyAccelerometerJerk-SD()-Z              : num  -0.994 -0.992 -0.99 -0.993 -0.992 ...
 $ timeBodyGyroscope-MEAN()-X                    : num  -0.0061 -0.0161 -0.0317 -0.0434 -0.034 ...
 $ timeBodyGyroscope-MEAN()-Y                    : num  -0.0314 -0.0839 -0.1023 -0.0914 -0.0747 ...
 $ timeBodyGyroscope-MEAN()-Z                    : num  0.1077 0.1006 0.0961 0.0855 0.0774 ...
 $ timeBodyGyroscope-SD()-X                      : num  -0.985 -0.983 -0.976 -0.991 -0.985 ...
 $ timeBodyGyroscope-SD()-Y                      : num  -0.977 -0.989 -0.994 -0.992 -0.992 ...
 $ timeBodyGyroscope-SD()-Z                      : num  -0.992 -0.989 -0.986 -0.988 -0.987 ...
 $ timeBodyGyroscopeJerk-MEAN()-X                : num  -0.0992 -0.1105 -0.1085 -0.0912 -0.0908 ...
 $ timeBodyGyroscopeJerk-MEAN()-Y                : num  -0.0555 -0.0448 -0.0424 -0.0363 -0.0376 ...
 $ timeBodyGyroscopeJerk-MEAN()-Z                : num  -0.062 -0.0592 -0.0558 -0.0605 -0.0583 ...
 $ timeBodyGyroscopeJerk-SD()-X                  : num  -0.992 -0.99 -0.988 -0.991 -0.991 ...
 $ timeBodyGyroscopeJerk-SD()-Y                  : num  -0.993 -0.997 -0.996 -0.997 -0.996 ...
 $ timeBodyGyroscopeJerk-SD()-Z                  : num  -0.992 -0.994 -0.992 -0.993 -0.995 ...
 $ timeBodyAccelerometerMagnitude-MEAN()         : num  -0.959 -0.979 -0.984 -0.987 -0.993 ...
 $ timeBodyAccelerometerMagnitude-SD()           : num  -0.951 -0.976 -0.988 -0.986 -0.991 ...
 $ timeGravityAccelerometerMagnitude-MEAN()      : num  -0.959 -0.979 -0.984 -0.987 -0.993 ...
 $ timeGravityAccelerometerMagnitude-SD()        : num  -0.951 -0.976 -0.988 -0.986 -0.991 ...
 $ timeBodyAccelerometerJerkMagnitude-MEAN()     : num  -0.993 -0.991 -0.989 -0.993 -0.993 ...
 $ timeBodyAccelerometerJerkMagnitude-SD()       : num  -0.994 -0.992 -0.99 -0.993 -0.996 ...
 $ timeBodyGyroscopeMagnitude-MEAN()             : num  -0.969 -0.981 -0.976 -0.982 -0.985 ...
 $ timeBodyGyroscopeMagnitude-SD()               : num  -0.964 -0.984 -0.986 -0.987 -0.989 ...
 $ timeBodyGyroscopeJerkMagnitude-MEAN()         : num  -0.994 -0.995 -0.993 -0.996 -0.996 ...
 $ timeBodyGyroscopeJerkMagnitude-SD()           : num  -0.991 -0.996 -0.995 -0.995 -0.995 ...
 $ frequencyBodyAccelerometer-MEAN()-X           : num  -0.995 -0.997 -0.994 -0.995 -0.997 ...
 $ frequencyBodyAccelerometer-MEAN()-Y           : num  -0.983 -0.977 -0.973 -0.984 -0.982 ...
 $ frequencyBodyAccelerometer-MEAN()-Z           : num  -0.939 -0.974 -0.983 -0.991 -0.988 ...
 $ frequencyBodyAccelerometer-SD()-X             : num  -0.995 -0.999 -0.996 -0.996 -0.999 ...
 $ frequencyBodyAccelerometer-SD()-Y             : num  -0.983 -0.975 -0.966 -0.983 -0.98 ...
 $ frequencyBodyAccelerometer-SD()-Z             : num  -0.906 -0.955 -0.977 -0.99 -0.992 ...
 $ frequencyBodyAccelerometerJerk-MEAN()-X       : num  -0.992 -0.995 -0.991 -0.994 -0.996 ...
 $ frequencyBodyAccelerometerJerk-MEAN()-Y       : num  -0.987 -0.981 -0.982 -0.989 -0.989 ...
 $ frequencyBodyAccelerometerJerk-MEAN()-Z       : num  -0.99 -0.99 -0.988 -0.991 -0.991 ...
 $ frequencyBodyAccelerometerJerk-SD()-X         : num  -0.996 -0.997 -0.991 -0.991 -0.997 ...
 $ frequencyBodyAccelerometerJerk-SD()-Y         : num  -0.991 -0.982 -0.981 -0.987 -0.989 ...
 $ frequencyBodyAccelerometerJerk-SD()-Z         : num  -0.997 -0.993 -0.99 -0.994 -0.993 ...
 $ frequencyBodyGyroscope-MEAN()-X               : num  -0.987 -0.977 -0.975 -0.987 -0.982 ...
 $ frequencyBodyGyroscope-MEAN()-Y               : num  -0.982 -0.993 -0.994 -0.994 -0.993 ...
 $ frequencyBodyGyroscope-MEAN()-Z               : num  -0.99 -0.99 -0.987 -0.987 -0.989 ...
 $ frequencyBodyGyroscope-SD()-X                 : num  -0.985 -0.985 -0.977 -0.993 -0.986 ...
 $ frequencyBodyGyroscope-SD()-Y                 : num  -0.974 -0.987 -0.993 -0.992 -0.992 ...
 $ frequencyBodyGyroscope-SD()-Z                 : num  -0.994 -0.99 -0.987 -0.989 -0.988 ...
 $ frequencyBodyAccelerometerMagnitude-MEAN()    : num  -0.952 -0.981 -0.988 -0.988 -0.994 ...
 $ frequencyBodyAccelerometerMagnitude-SD()      : num  -0.956 -0.976 -0.989 -0.987 -0.99 ...
 $ frequencyBodyAccelerometerJerkMagnitude-MEAN(): num  -0.994 -0.99 -0.989 -0.993 -0.996 ...
 $ frequencyBodyAccelerometerJerkMagnitude-SD()  : num  -0.994 -0.992 -0.991 -0.992 -0.994 ...
 $ frequencyBodyGyroscopeMagnitude-MEAN()        : num  -0.98 -0.988 -0.989 -0.989 -0.991 ...
 $ frequencyBodyGyroscopeMagnitude-SD()          : num  -0.961 -0.983 -0.986 -0.988 -0.989 ...
 $ frequencyBodyGyroscopeJerkMagnitude-MEAN()    : num  -0.992 -0.996 -0.995 -0.995 -0.995 ...
 $ frequencyBodyGyroscopeJerkMagnitude-SD()      : num  -0.991 -0.996 -0.995 -0.995 -0.995 ...
 
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##write to text file on disk
getMeasurementAveragesByActivityAndSubject <- function() {
        ds <- getmergeddataset()
        ds %>% group_by(subject, activity) %>% summarise_all(funs(mean))
}
