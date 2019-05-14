library(dplyr)

#load dataset
setwd("C:\\Users\\1546259648C\\Desktop\\Coursera R Code")
zippedfile <- "getdata_projectfiles_UCI_HAR_Dataset.zip"


#create individual data frames

df_features <- read.table(unz(zippedfile, "UCI HAR Dataset/features.txt"), col.names = c("n","functions"))
df_activities <- read.table(unz(zippedfile, "UCI HAR Dataset/activity_labels.txt"), col.names = c("code", "activity"))
df_subjecttest <- read.table(unz(zippedfile, "UCI HAR Dataset/test/subject_test.txt"), col.names = "subject")
df_subjecttrain <- read.table(unz(zippedfile, "UCI HAR Dataset/train/subject_train.txt"), col.names = "subject")
df_xtest <- read.table(unz(zippedfile, "UCI HAR Dataset/test/X_test.txt"), col.names = df_features$functions)
df_ytest <- read.table(unz(zippedfile, "UCI HAR Dataset/test/y_test.txt"), col.names = "code")
df_xtrain <- read.table(unz(zippedfile, "UCI HAR Dataset/train/X_train.txt"), col.names = df_features$functions)
df_ytrain <- read.table(unz(zippedfile, "UCI HAR Dataset/train/y_train.txt"), col.names = "code")

#Merges the training and the test sets to create one data set.

df_x <- rbind(df_xtrain, df_xtest)
df_y <- rbind(df_ytrain, df_ytest)
df_subject <- rbind(df_subjecttrain, df_subjecttest)
df_merged <- cbind(df_subject, df_y, df_x)

#Extracts only the measurements on the mean and standard deviation for each measurement.

df_tidy <- df_merged %>% select(subject, code, contains("mean"), contains("std"))

#Uses descriptive activity names to name the activities in the data set

df_tidy$code <- df_activities[df_tidy$code, 2]

#Appropriately labels the data set with descriptive variable names.

names(df_tidy)[2] = "activity"
names(df_tidy)<-gsub("Acc", "Accelerometer", names(df_tidy))
names(df_tidy)<-gsub("Gyro", "Gyroscope", names(df_tidy))
names(df_tidy)<-gsub("BodyBody", "Body", names(df_tidy))
names(df_tidy)<-gsub("Mag", "Magnitude", names(df_tidy))
names(df_tidy)<-gsub("^t", "Time", names(df_tidy))
names(df_tidy)<-gsub("^f", "Frequency", names(df_tidy))
names(df_tidy)<-gsub("tBody", "TimeBody", names(df_tidy))
names(df_tidy)<-gsub("-mean()", "Mean", names(df_tidy), ignore.case = TRUE)
names(df_tidy)<-gsub("-std()", "STD", names(df_tidy), ignore.case = TRUE)
names(df_tidy)<-gsub("-freq()", "Frequency", names(df_tidy), ignore.case = TRUE)
names(df_tidy)<-gsub("angle", "Angle", names(df_tidy))
names(df_tidy)<-gsub("gravity", "Gravity", names(df_tidy))

#From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.

df_tidyavg <- df_tidy %>%
        group_by(subject, activity) %>%
        summarise_all(list(mean))
write.table(df_tidyavg, "TidyAverageData.txt", row.name=FALSE)
