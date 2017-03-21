#################################### ASSIGNMENT ####################################
# Create WD destination
if(!file.exists("C:/RWD/ASSIGNMENT")){
  dir.create("C:/RWD/ASSIGNMENT")
}

# Set working directory
setwd("C:/RWD/ASSIGNMENT")

# Get working directory
getwd()

# Load libraries
library(dplyr)


# Download and unzip the data:
if (!file.exists("dataset.zip")){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, "dataset.zip", mode="wb")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip("dataset.zip") 
}


#1 Merge the training and the test sets to create one data set.

  #1.1 Store feature names in a vector
  feature_names <- read.table("UCI HAR Dataset/features.txt")[,2]
  feature_names <- gsub("[()]", "", feature_names)
  feature_names <- gsub("-", "_", feature_names)
  feature_names <- gsub(",", "_", feature_names)

  #1.2 Read, rename variables and merge training set
  trainFeatures <- read.table("UCI HAR Dataset/train/X_train.txt")
  colnames(trainFeatures) <- feature_names
  trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
  colnames(trainActivities) <- "activity"
  trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
  colnames(trainSubjects) <- "id"
  
  train <- cbind(trainSubjects, trainActivities, trainFeatures)
  
  
  
  #1.3 Read, rename variables and merge test set
  testFeatures <- read.table("UCI HAR Dataset/test/X_test.txt")
  colnames(testFeatures) <- feature_names
  testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
  colnames(testActivities) <- "activity"
  testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
  colnames(testSubjects) <- "id"
  
  test <- cbind(testSubjects, testActivities, testFeatures)



  #1.4 Merge training and test sets into a singe dataset
  data <- rbind(train, test)
  View(data)

  
#2 Extract only the measurements on the mean and standard deviation for each measurement
  
  #2.1 Keep variables where "mean_" ("_" in order to avoid meanFreq) or "std" found
  
  features_std <- feature_names [grep("[Ss]td", feature_names)]
  features_mean <- feature_names [grep("[Mm]ean_|[Mm]ean$", feature_names)]
  features_wanted <- c("id", "activity", features_mean, features_std)
  
  #2.2 Subset from "data" all variables contained in "features_wanted"
  
  data_wanted <- data[, features_wanted]
  
  head(data_wanted)
  

#3 Use descriptive activity names to name the activities in the data set
  
  #3.1 Store activity codes ("V1") and their names ("V2")
  activity_names <- read.table("UCI HAR Dataset/activity_labels.txt")
  
  #3.2 Transform the activity variable in the "data_wanted" data frame to asign names to codes
  data_wanted$activity <- factor(data_wanted$activity, levels = activity_names$V1, labels = activity_names$V2 )
  
  
#4 Label the dataset with descriptive variable names
  
  ## Done in #1
  

#5 From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject
  
  tidy_data <- data_wanted %>%
               group_by(id, activity) %>%
               summarize_each(funs(mean))

#6 Write tidy.txt data file
write.table(tidy_data, "tidy_data.txt", row.names = FALSE, quote = FALSE)