# 1. Merge the training and the test sets to create one data set
# load libraries
library(dplyr)
library(tidyr)
library(data.table)
library(readr)

# download dataset and unzip it
#set working directory
setwd("/Users/lisa/Documents/Learning/R Programming/UCI HAR Dataset/")

if (!file.exists("./data")){dir.create("./data")}
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  data <- download.file(fileURL, destfile= "./data/dataset.zip", method="curl")
  filename <- "dataset.zip"
  unzip(filename) 
 
# load activity labels and features
activity_labels <- read.table("./activity_labels.txt")
features <- read.table("./features.txt")

# load training data 
train_data <- read_table("./train/X_train.txt", col_names = FALSE)
train_labels <- read_table("./train/y_train.txt", col_names = FALSE)
train_subjects <- read_table("./train/subject_train.txt", col_names = FALSE)

# assign column names to training data
colnames(activity_labels) = c('Activity','ActivityType')
colnames(train_subjects) = "Subject"
colnames(train_labels) = "Activity"
colnames(train_data) = features[,2]

# load test data
test_data <- read_table("./test/X_test.txt", col_names = FALSE)
test_labels <- read_table("./test/y_test.txt", col_names = FALSE)
test_subjects <- read_table("./test/subject_test.txt", col_names = FALSE)

# Assign column names to the test data 
colnames(test_subjects) = "Subject"
colnames(test_data) = features[,2] 
colnames(test_labels) = "Activity"

# merge training and test sets
train <- bind_cols(train_subjects, train_labels, train_data)
test <- bind_cols(test_subjects, test_labels, test_data)
all_data <- bind_rows(train, test)

# 2. Extract only the measurements on the mean and standard deviation for each measurement
std_mean_data <- all_data %>% select(grep("Subject", names(all_data)), grep("Activity", names(all_data)), 
                    grep("mean\\(\\)", names(all_data)), grep("std\\(\\)", names(all_data)))

# 3. Use descriptive activity names to name the activities in the data set
final_data <- std_mean_data %>% right_join(activity_labels, std_mean_data, by = "Activity") %>%
  select(Subject, ActivityType, everything(), -Activity) 

# 4. Appropriately labels the data set with descriptive variable names

#loop through the column names and replace with more descriptive ones using regular expressions
column_names <- colnames(final_data)

for(i in 1:length(column_names)){
  column_names[i] = gsub("-std..","-StDev",  column_names[i])
  column_names[i] = gsub("-mean..","-Mean", column_names[i])
  column_names[i] = gsub("^f","Frequency",  column_names[i])
  column_names[i] = gsub("^t","Time", column_names[i])
  column_names[i] = gsub("Gyro", "Gyroscope", column_names[i])
  column_names[i] = gsub("Mag","Magnitude",  column_names[i])
  column_names[i] = gsub("BodyBody","Body",  column_names[i])
  column_names[i] = gsub("Acc","Acceleration",  column_names[i])
}

colnames(final_data) = column_names

# 5. From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject

final_data$subjectId <- as.factor(final_data$Subject)
final_data$subjectId <- as.factor(final_data$ActivityType)

tidy_data <- data.table(final_data)
tidy_data <-aggregate(. ~Subject + ActivityType, tidy_data, mean)
tidy_data <- tidy_data[order(tidy_data$Subject,tidy_data$ActivityType),]

write.table(tidy_data, file = "tidy_data.txt", row.names = FALSE)




