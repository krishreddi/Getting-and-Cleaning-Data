#Getting and Cleaning Data
#Coursera 
#Course Project
#03/19/2015---krishreddi'S
#This script obtains, cleans and summarises data from Samsung Galaxy S smartphone accelerometers.
# The source data was collected as described in the link below:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# To run the script, users will need internet access and the libraries 'dplyr' and 'tidyr' installed.
# The output of the script is a file "./data/tidy UCR HAR summary.txt".

library(dplyr)
library(tidyr)
# Create data directory if necessary
if (!file.exists("./data")) {
     dir.create("./data")
}
# Download and unzip dataset if necessary
#if (!file.exists("./data/UCI HAR Dataset.zip")) {
#     download.file(
#         url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
#          method="curl",
#          destfile="./data/UCI HAR Dataset.zip")
#     unzip("./data/UCI HAR Dataset.zip", exdir="./data")
#}
# Load column and activity names
featureNames  <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)[[2]]
activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", col.names=c("activityNum", "activity"))
# Load training data
xTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt", col.names=c("activityNum"))
sTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names=c("subjectId"))
# Load test data
xTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names=c("activityNum"))
sTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names=c("subjectId"))
##################################################################
# --- STEP 1 
# Merges the training and the test sets to create one data set.
##################################################################
allData <- rbind(xTrain, xTest)
colnames(allData) <- featureNames
rm(list=c("xTrain", "xTest"))

############################################################################################
# --- STEP 2 ---
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################
stdOrMeanCols <- featureNames[grepl("mean\\()|std\\()", featureNames)]
allData <- allData[,stdOrMeanCols]
rm(stdOrMeanCols)

###########################################################################
# --- STEP 3 ---
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################
activityData <- merge(rbind(yTrain, yTest), activityLabels, by="activityNum", sort=FALSE)[["activity"]]
allData["activity"] <- sub("_", " ", tolower(activityData))
rm(list=c("yTrain", "yTest", "activityData"))

##############################################################
# --- STEP 4 ---
# 4. Appropriately labels the data set with descriptive names.
##############################################################
allData["subjectId"] <- rbind(sTrain, sTest)
rm(list=c("sTrain", "sTest"))

######################################################################################################################
# --- STEP 5 ---
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################
cleanupVariableName <- function(name) {
     # Better variable names
     name <- sub("^f", "meanFrequency", name)
     name <- sub("^t", "meanTime", name)
     name <- sub("Acc", "Acceleration", name)
     name <- sub("Mag", "Magnitude", name)
     name <- sub("Gyro", "Gyroscope", name)
     name <- sub("-mean\\(\\)", "Mean", name)
     name <- sub("-std\\(\\)", "StandardDeviation", name)
     name
}
tidyData <- allData %>%
     gather(variable, value, -activity, -subjectId) %>%
     mutate(variable=cleanupVariableName(variable)) %>%
     group_by(activity, subjectId, variable) %>%
     summarise(value=mean(value)) %>%
     arrange(activity, subjectId, variable)

# Write out tidy summary dataset
write.table(tidyData, "./data/tidy UCR HAR summary.txt", row.names=FALSE)
