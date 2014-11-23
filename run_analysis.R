# run_analysis.R
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the 
#   average of each variable for each activity and each Subject.
#   Please upload your data set as a txt file created with write.table() using row.name=FALSE 



library(data.table)
library(reshape2)

# features that need to be grepped out.
what_to_get <- "mean|std"

#set constants pointing to the files we need (its easier to have them all together if path need to be changed)
activity_lables_file <- "./activity_labels.txt"
features_file <- "./features.txt"
x_test_file <- "./test/x_test.txt"
y_test_file <- "./test/y_test.txt"
subject_test_file <- "./test/subject_test.txt"
x_train_file <- "./train/x_train.txt"
y_train_file <- "./train/y_train.txt"
subject_train_file <- "./train/subject_train.txt"
tidy_data_file <- "./tidy_data.txt"

# Get activity lables from activity lables file
activity_labels <- read.table(activity_lables_file)[,2] 

# Get the data column names from the feature files
features <- read.table(features_file)[,2]

# Get only the mean and standard deviation
features_needed <- grepl(what_to_get, features)

# Load the x and y test files
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file )

# Load the Subject test file
subject_test <- read.table(subject_test_file)

#Name the columns in the x_test var
names(x_test) <- features

# Take only the mean and standard deviation
x_test = x_test[,features_needed]

# Get activity labels for test data
y_test[,2] = activity_labels[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Name")
names(subject_test) = "Subject"

# Merge the y and x test data
test_data <- cbind(as.data.table(subject_test), y_test, x_test)

# Load and process x_train & y_train data.
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)

subject_train <- read.table(subject_train_file)

names(x_train) = features

# Extract only the measurements on the mean and standard deviation for each measurement.
x_train = x_train[,features_needed]

# Get activity lables for train data
y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Name")
names(subject_train) = "Subject"

# merge the y and x train data
train_data <- cbind(as.data.table(subject_train), y_train, x_train)

# Merge test and train data
merged_data = rbind(test_data, train_data)

id_labels   = c("Subject", "Activity_ID", "Activity_Name")
data_labels = setdiff(colnames(merged_data), id_labels)
melt_data      = melt(merged_data, id = id_labels, measure.vars = data_labels)

# get the mean
tidy_data   = dcast(melt_data, Subject + Activity_Name ~ variable, mean)

#Save it as per project requirments. 
write.table(tidy_data, file = tidy_data_file, row.names=FALSE)