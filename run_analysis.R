#### Getting and Cleaning Data Project - Coursera
#### Date: 7 March 2015
#### Author: Antony Mapfumo

rm(list=ls())
require(plyr)
setwd("/home/tony/Getting-and-Cleaning-Data-Coursera-Project")
## files and directories
uci_hard_directory <- "UCI HAR Dataset"
feature_file <- paste(uci_hard_directory, "/features.txt", sep = "")
activity_labels_file <- paste(uci_hard_directory, "/activity_labels.txt", sep = "")
x_train_file <- paste(uci_hard_directory, "/train/X_train.txt", sep = "")
y_train_file <- paste(uci_hard_directory, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uci_hard_directory, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(uci_hard_directory, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_hard_directory, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_hard_directory, "/test/subject_test.txt", sep = "")

## Load the raw data
features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

#Merges the training and the test sets to create one data set.
# Binding sensor data
training_data <- cbind(cbind(x_train, subject_train), y_train)
test_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_data, test_data)

# Label columns
sensor_label <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_label

# Extracts only the measurements on the mean and standard deviation for each measurement.
sensor_mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]
# Use descriptive activity names to name the activities in the data set
sensor_mean_std <- join(sensor_mean_std, activity_labels, by = "ActivityId", match = "first")
sensor_mean_std <- sensor_mean_std[,-1]

# Appropriately labels the data set with descriptive names.
# Remove brackets
names(sensor_mean_std) <- gsub('\\(|\\)',"",names(sensor_mean_std), perl = TRUE)
# Make valid names
names(sensor_mean_std) <- make.names(names(sensor_mean_std))
# Make more descriptive names
names(sensor_mean_std) <- gsub('Acc',"Acceleration",names(sensor_mean_std))
names(sensor_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_mean_std))
names(sensor_mean_std) <- gsub('Gyro',"AngularSpeed",names(sensor_mean_std))
names(sensor_mean_std) <- gsub('Mag',"Magnitude",names(sensor_mean_std))
names(sensor_mean_std) <- gsub('^t',"TimeDomain.",names(sensor_mean_std))
names(sensor_mean_std) <- gsub('^f',"FrequencyDomain.",names(sensor_mean_std))
names(sensor_mean_std) <- gsub('\\.mean',".Mean",names(sensor_mean_std))
names(sensor_mean_std) <- gsub('\\.std',".StandardDeviation",names(sensor_mean_std))
names(sensor_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_mean_std))
names(sensor_mean_std) <- gsub('Freq$',"Frequency",names(sensor_mean_std))

# Creates a second tidy data set
sensor_avg_by_act_sub = ddply(sensor_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "tidy.txt", row.names=FALSE)