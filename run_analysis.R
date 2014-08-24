#install.packages("plyr")
install.packages("reshape2")
library(plyr)
library(reshape2)
getData <- function(data_file_name, subject_file_name, activity_file_name) {
  
  df_data <- read.table(data_file_name)
  df_subject <-read.table(subject_file_name)
  df_activity <-read.table(activity_file_name)
  
  # Select only the desired columns of the test data
  df_data <- df_data[,col_mean_std[,1]]
  
  # Join the activity numbers in the activity table to the activity names.
  y <- join(df_activity,df_activities)
  
  # Join subjects to activities.
  z <- cbind(df_subject,y[,2])
  
  # Join the subjects and activities to the data.
  df_data <- cbind(z, df_data)
  
  return(df_data)
}

#  Link the activity numbers to the descriptive activity names.
activity_file_name <- "F:\\RWork\\DataClass\\Project\\UCI HAR Dataset\\activity_labels.txt"
df_activities <- read.table(activity_file_name)
# Read the activity names into vector activity_labels.
activity_labels <- df_activities[,2]

# Create descriptive column labels for the data.
col_labels_file_name <- "F:\\RWork\\DataClass\\Project\\UCI HAR Dataset\\features.txt"
col_labels <- read.table(col_labels_file_name)
# Select just the column names that relate to the mean or standard deviation.
# There is some question of the nature of some of these columns, but the assignment calls for all
# data that appears to be mean or standard deviation, so all columns that appear to meet this
# criteri based upon their names have been included.
colLabels <- col_labels[grep("mean|std", colLabels, ignore.case="T"),]
# To do - clean up the column names to make them clearer - remove puctuation, etc.
colLabels <- col_mean_std$V2
colLabels <- gsub("-", "", colLabels) # removes dashes
colLabels <- gsub("\\(\\)", "", colLabels) # removes empty parenthesis
colLabels <- gsub("[Mm][Ee][Aa][Nn]", "Mean", colLabels) # Standardize case of Mean
colLabels <- gsub("[Ss][Tt][Dd]", "Std", colLabels) # Standardize case of Std

# Data files - two sets, one set was training data, the second set was to be used for testing.
# This project does not differentiate between test and center data. The three files consist of one observation
# per line and the observations are joined across files by line number.

# Each set of data files includes 3 files:
#  Data file     - 561 columns of observations.
#  Activity file - The activity being observed for the current line. Each line is one of the six activities
#                  recorded in the activity file.
#  Subject file  - The subject (designated by an integer of 1 - 30) of the current line. Designates the individual
#                  on whom the observations were made.

# Test Data files
test_data_file_name <- "F:\\RWork\\DataClass\\Project\\UCI HAR Dataset\\test\\X_test.txt"
test_subject_file_name <- "F:\\RWork\\DataClass\\Project\\UCI HAR Dataset\\test\\subject_test.txt"
test_activity_file_name <- "F:\\RWork\\DataClass\\Project\\UCI HAR Dataset\\test\\y_test.txt"

# Training Data files
train_data_file_name <- "F:\\RWork\\DataClass\\Project\\UCI HAR Dataset\\train\\X_train.txt"
train_subject_file_name <- "F:\\RWork\\DataClass\\Project\\UCI HAR Dataset\\train\\subject_train.txt"
train_activity_file_name <- "F:\\RWork\\DataClass\\Project\\UCI HAR Dataset\\train\\y_train.txt"

df_output_data <- getData(test_data_file_name, test_subject_file_name, test_activity_file_name)
df_output_data <- rbind(df_output_data, getData(train_data_file_name, train_subject_file_name, train_activity_file_name))

# Set the data column labels to nice values
colnames(df_output_data) <- c("Subject", "Activity", colLabels)
                     
# Summarize the data - average all numberic fields grouped by subject and activity.
attach(df_output_data)
# Note - this command will generate warnings because it will try to calculate the mean
#        of the Activity column. This column (which will consist entirely of NA values) will
#        be discarded. The correct contents of the Activity column will be stored in the
#        Group.2 column, which will be renamed to Activity.
aggdata <- aggregate(df_output_data, by=list(Subject, Activity), FUN=mean)
detach(df_output_data)

# Column cleanup - delete duplicate columns, change order, rename.
aggdata <- subset(aggdata, select=-c(3, 4))
colnames(aggdata)[colnames(aggdata) == "Group.1"] <- "Subject"                  
colnames(aggdata)[colnames(aggdata) == "Group.2"] <- "Activity"

# Export the data and summary data.
write.table(df_output_data, file = "F:\\RWork\\DataClass\\Project\\tidy_data.txt", row.name=FALSE)
write.table(aggdata, file = "F:\\RWork\\DataClass\\Project\\tidy_summary_data.txt", row.name=FALSE)

                  