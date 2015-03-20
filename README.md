# Getting-and-Cleaning-Data
Course Project
Getting and Cleaning Data - Coursera
Repository Contents

#README.md
#run_analysis.R
#tidy UCR HAR summary.txt
#CodeBook.md

Introduction

This repository contains my submission for the course project of the "Data Science - Getting and Cleaning Data" course on Coursera.

The purpose of the project was to obtain, clean and summarise a real-life dataset. The input data set used was from the accelerometers and gyroscopes of a Samsung Galaxy S smartphone. This data was produced as part of the Human Activity Recognition Using Smartphones project.

#The actual data file downloaded is here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The brief for the project was to: 1. Merge the training and the test sets to create one data set. 2. Extract only the measurements on the mean and standard deviation for each measurement. 3. Use descriptive activity names to name the activities in the data set. 4. Appropriately label the data set with descriptive variable names. 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

run_analysis.R

The run_analysis.R script performs the following:

Downloads the raw data files to the ./data folder,
Extracts the raw data files,
Performs the analysis steps listed above, and
Outputs the "tidy UCR HAR summary.txt" file.
This script requires the libraries dplyr and tidyr to be installed.

Software versions used when running this script are:

R version 3.1.1 (2014-07-10) -- "Sock it to Me", Platform: x86_64-w64-mingw32/x64 (64-bit)
dplyr_0.3.0.2 built under R version 3.1.2
tidyr_0.1 built under R version 3.1.2
To run the script, copy to your R working directory then call:

source("run_analysis.R")
tidy UCR HAR summary.txt

run_analysis.R writes the tidy summary data as a space delimited .txt file. This file can be loaded into R by copying it to your working directory then calling:

data <- read.table("tidy UCR HAR summary.txt", header=TRUE)
CodeBook.md

CodeBook.md describes the contents of the tidy UCR HAR summary.txt data file. The source data for this code book was taken from the code book for the Samsung Galaxy data sets.
