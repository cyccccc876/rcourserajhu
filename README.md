# rcourserajhu
tidydata
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
setwd("D://sbu//2021spring//eco321 self-study//getdata_projectfiles_UCI HAR Dataset//UCI HAR Dataset")
features <- read.table("features.txt", col.names = c("n","functions"))
activities <- read.table("activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("subject_test.txt", col.names = "subject")
x_test <- read.table("X_test.txt", col.names = features$functions)
y_test <- read.table("y_test.txt", col.names = "code")
subject_train <- read.table("subject_train.txt", col.names = "subject")
x_train <- read.table("X_train.txt", col.names = features$functions)
y_train <- read.table("y_train.txt", col.names = "code")
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)
#Step 2: Extracts only the measurements on 
#the mean and standard deviation for each measurement.
TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
#Step 3: Uses descriptive activity names to
#name the activities in the data set.
attach(TidyData)
TidyData$code[code == 1] <-1
TidyData$code[code ==2] <-2
TidyData$code[code == 3] <-3
TidyData$code[code == 4] <-4
TidyData$code[code ==5] <-5
TidyData$code[code == 6] <-6
TidyData$code <- factor(TidyData$code,    #转为factor形式，替换标签
                         levels=c(1,2,3,4,5,6),
                         labels=c("WALKING","WALKING_UPSTAIRS", "WALKING_UPSTAIRS",
                                  "SITTING",	
                                  "STANDING","LAYING"))

TidyData$code <- activities[,2]
#Step 4: Appropriately labels the data set with
#descriptive variable names.
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))
#Step 5: From the data set in step 4, creates a second, 
#independent tidy data set with the average of
#each variable for each activity and each subject.
FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)
str(FinalData)

