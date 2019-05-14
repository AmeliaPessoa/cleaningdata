# Dependencies 
library(dplyr)

# Set the working directory to current
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


# 0. Download and unzip the data

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFilename <- "UCI HAR Dataset.zip"
dataPath <- "UCI HAR Dataset"
if (!file.exists(zipFilename)) {
  download.file(url, zipFilename, mode = "wb")
}
if (!file.exists(dataPath)) {
  unzip(zipFile)
}


# 1. Merges the training and the test sets to create one data set.

trainSubject <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainX <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainY <- read.table(file.path(dataPath, "train", "y_train.txt"))
testSubject <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testX <- read.table(file.path(dataPath, "test", "X_test.txt"))
testY <- read.table(file.path(dataPath, "test", "y_test.txt"))

# merge dataset
fullDataset <- rbind(
  cbind(trainSubject, trainX, trainY),
  cbind(testSubject, testX, testY)
)

# read features and rename the columns
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
colnames(fullDataset) <- c("subject", features[, 2], "activity")


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

columnsToKeep <- grepl("subject|activity|mean|std", colnames(fullDataset))
dataset <- fullDataset[, columnsToKeep]


# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityDescription")

dataset$activity <- factor(dataset$activity, 
                        levels = activities[, 1], 
                        labels = activities[, 2])


# 4. Appropriately labels the data set with descriptive variable names.

columns <- colnames(dataset)

columns <- gsub("[\\(\\)-]", "", columns)
columns <- gsub("^f", "frequencyDomain", columns)
columns <- gsub("^t", "timeDomain", columns)
columns <- gsub("Acc", "Accelerometer", columns)
columns <- gsub("Gyro", "Gyroscope", columns)
columns <- gsub("Mag", "Magnitude", columns)
columns <- gsub("Freq", "Frequency", columns)
columns <- gsub("mean", "Mean", columns)
columns <- gsub("std", "StandardDeviation", columns)
columns <- gsub("BodyBody", "Body", columns)

colnames(dataset) <- columns


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

means <- dataset %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

write.table(means, "tidyDataset.txt", row.names = FALSE, quote = FALSE)
