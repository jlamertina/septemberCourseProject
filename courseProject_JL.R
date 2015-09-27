# Course Project for Getting and Cleaning Data
#   author: John Lamertina  
#   date:  September 2015
#
# Project Tasks
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with 
#       the average of each variable for each activity and each subject
# 6. Prepare to upload the tidy data set created in step 5. Use write.table() using row.name=FALSE
#
# Setup ====================================================================================================
setwd("C:/Users/JohnL/Documents/CourseraData/GetCleanData")
#
# 1. Read the data sets ====================================================================================
#
# Load the files into data tables  (see lesson 1-9 and Quiz 3 question 3)
install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)
#
# file of activity labels
activityLabels <- read.table("data/UCI HAR Dataset/activity_labels.txt")
head(activityLabels)
#
# file of features
features <- read.table("data/UCI HAR Dataset/features.txt")
head(features,10)
tail(features,10)
#
# 2. Setup to extract only the measurements on mean and standard deviation =================================
#
# select only the feature columns for mean and standard deviation
#   (use grep to pattern match on mean or std)
featureStats <- features %>% filter(grepl('mean|std', features[,2]))
head(featureStats,10)
# Create indices as factors to read only the mean and std from the training and test data sets
featureStats$Index <- featureStats[,"V1"]
head(featureStats,10)
#
# Read the training data
train_X_Data <- read.table("data/UCI HAR Dataset/train/X_train.txt")[featureStats[,"Index"]]
train_Y_Activity <- read.table("data/UCI HAR Dataset/train/Y_train.txt")
train_Subject <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
train <- cbind(train_Subject, train_Y_Activity, train_X_Data)
# Read the test data
test_X_Data <- read.table("data/UCI HAR Dataset/test/X_test.txt")[featureStats[,"Index"]]
test_Y_Activity <- read.table("data/UCI HAR Dataset/test/Y_test.txt")
test_Subject <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
test <- cbind(test_Subject, test_Y_Activity, test_X_Data)
#
# 3. Merge only the stats for mean and std, using activity names from the data set =======================
#
# Merge the training and test datas sets
mergedData <- rbind(train, test)
head(mergedData[,1:5],10)
# Label the data
colnames(mergedData) <- c("Subject_ID", "ActivityCode", as.character(featureStats[,2]))
head(mergedData[,1:5],10)
#
# 4. Appropriately label the data set with descriptive variable names. =========================
# Append activity name to correspond to the code
#
colnames(activityLabels) <- c("ActivityCode", "ActivityName")
activityLabels
result <- mergedData %>% left_join(activityLabels)
head(result[,1:5],7)
head(result[,80:82],7)
#
# 5. From the data set in step 4, creates a second, independent tidy data set with =============
#       the average of each variable for each activity and each subject
#
meanGroups <- result %>%
    group_by(Subject_ID,ActivityName) %>%
    summarise_each(funs(mean(.,na.rm=TRUE)))
#
# 6. Prepare to upload the tidy data set created in step 5. Use write.table() using row.name=FALSE 
write.table(meanGroups, "tidyMeansByGroup.txt", row.names = FALSE, quote = FALSE)