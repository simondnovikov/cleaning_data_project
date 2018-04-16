library("reshape2")

## Read raw data
# Read Data

X_train=read.table("UCI HAR Dataset//train//X_train.txt")
y_train=read.table("UCI HAR Dataset//train//y_train.txt")
X_test=read.table("UCI HAR Dataset//test//X_test.txt")
y_test=read.table("UCI HAR Dataset//test//y_test.txt")

# Read labels

activity_labels=read.table("UCI HAR Dataset//activity_labels.txt")
features=read.table("UCI HAR Dataset//features.txt")

# Read subjects
subject_test=read.table("UCI HAR Dataset//test//subject_test.txt")
subject_train=read.table("UCI HAR Dataset//train//subject_train.txt")

##Cleaning

names(X_test) <-features[,2]
names(X_train) <-features[,2]


## ANALYSIS

## 1 merge training and test
X=rbind(X_test,X_train)
y=rbind(y_test,y_train)
subject=rbind(subject_test,subject_train)
names(y)<-"activity"
names(subject)<-"subject"

## 2 Extracts only the measurements on the mean and standard deviation for each measurement.
columns_to_extract <- grep("mean|std", features[, 2]) 
selected_X=X[,columns_to_extract]

## 3 Uses descriptive activity names to name the activities in the data set


y$activity<-factor(y$activity,labels=activity_labels$V2)

## 4 Appropriately labels the data set with descriptive variable names.
names(selected_X)<-sapply(names(selected_X), function(x) {gsub("[()-]", "",x)})

## 5 creates a second, independent tidy data set with the average of each variable for each activity and each subject.

combined_data=cbind(subject,y,selected_X)

meltedData <- melt(combined_data,(id.vars=c("subject","activity")))
tidyData <- dcast(meltedData, subject + activity ~ variable, mean)
names(tidyData)[-c(1:2)] <- paste("[mean of]" , names(tidyData)[-c(1:2)] )
write.table(tidyData, "tidy_data.txt", sep = ",")
