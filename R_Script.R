# Week 4 Cleaning and Getting Data Assignment 

library(dplyr) 


# Read in the features and activity labels information into seperate vectors to be used later in the script 
features <- read.table("C:/Courses/R/UCI HAR Dataset/features.txt") 
activity <- read.table("C:/Courses/R/UCI HAR Dataset/activity_labels.txt") 
colnames(activity) <- c("ActivityID", "Activity") 
  
#  Read in the training set data and associated reference files 
File_Training_set <- read.table("C:/Courses/R/UCI HAR Dataset/train/X_train.txt") 
File_Training_ActivityLabels<- read.table("C:/Courses/R/UCI HAR Dataset/train/y_train.txt") 
File_Training_subjects <- read.table ("C:/Courses/R/UCI HAR Dataset/train/subject_train.txt") 
  

#  Read in the test set data and associated reference files 
File_Test_set <- read.table("C:/Courses/R/UCI HAR Dataset/test/X_test.txt") 
File_Test_Activitylabels <- read.table("C:/Courses/R/UCI HAR Dataset/test/y_test.txt") 
File_Test_subjects <- read.table ("C:/Courses/R/UCI HAR Dataset/test/subject_test.txt") 
  

# Merge the training and test data sets into one dataset. 
MergeData <- rbind(File_Training_set, File_Test_set) 
  

# Merge the activity and subject reference tables for the training and test sets into single reference tables. 
MergeActivity <- rbind(File_Training_ActivityLabels, File_Test_Activitylabels) 
MergeSubjects <- rbind(File_Training_subjects, File_Test_subjects) 

# Add in appropriate descriptive labels to the column variables in the merged dataset using the created 'features' vector.  Add in descriptive column labels to the activity and subject tables. 
colnames(MergeData) <- features[,2] 
colnames(MergeActivity) <- "ActivityID" 
colnames(MergeSubjects) <- "SubjectID" 

# Add in columns to the merged data set so appropriate activity IDs and subject IDs are assigned to each row. 
Merged_set_wlabels <- cbind(MergeSubjects, MergeActivity, MergeData) 
  

# Extract only the measurements relating to mean and standard deviation from the merged dataset. 
SelectedColumns <- grepl("*mean\\(\\)|*std\\(\\)|ActivityID|SubjectID", names(Merged_set_wlabels)) 
SelectedData <- Merged_set_wlabels[ , SelectedColumns] 
  

# Replace the actvity IDs with the descriptive names for the activity. 
LabelledData <- merge(SelectedData, activity, by="ActivityID")  
LabelledData <- LabelledData[, c(2,ncol(LabelledData), 3:(ncol(LabelledData)-1))] 


# Create a tidy data set where the average for each of the variables has been calculated for each activity and subject combination and shown on a single row. 
TidyData <- aggregate(.~SubjectID+Activity, LabelledData, mean) 
TidyData <- arrange(TidyData, SubjectID) 
  

# Copy tidy data set to a text file for uploading into GitHub 
write.table(TidyData, "TidyData.txt", row.names = FALSE, quote = FALSE) 
