library(dplyr)

# load data into R

trainY <- read.table("train/y_train.txt", quote = "\"")
trainX <- read.table("train/X_train.txt", quote = "\"")
trainSubject <- read.table("subject_train.txt", quote = "\"")


testY <- read.table("y_test.txt", quote = "\"")
testX <- read.table("X_test.txt", quote = "\"")
testSubject <- read.table("subject_test.txt", quote = "\"")

features <- read.table("features.txt", quote = "\"")
activity <- read.table("activity_labels.txt", quote = "\"")

# create column headers for activity file
colnames(activity) <- c("V1", "Activity")
 
# merge train Y with activity
subject <- rename(trainSubject, subject = V1)
train <- cbind(trainY, subject)
trainMerge <- merge(train, activity, by=("V1"))

# train X header to match header from features
colnames(trainX) <- features[, 2]

# merging trainY, trainX and activity
train2 <- cbind(trainMerge, trainX)

# remove column 1 (V1) from train2 to eliminate dubplicate column name error
train2_2 <- train2[, -1]

# select columns that contain mean and standard deviation (std)
trainMeanStd <- select(train2_2, contains("subject"), contains("Activity"), contains("mean"),
                       contains("std"))

# repeat similar efforts on Test data
colnames(activity) <- c("V1", "Activity")

# merge test Y with activity
subject2 <- rename(testSubject, subject=V1)

test <- cbind(testY, subject2)
testMerge <- merge(test, activity, by = "V1")

# test X header to match header from features
colnames(testX) <- features[, 2]

# merging testY, testX and activity
test2 <- cbind(testMerge, testX)

# remove column 1 (V1) from test 3 to eliminate duplicate column error
test2_2 <- test2[, -1]

# select columns that contain mean and standard deviation (std)
testMeanStd <- select(train2_2, contains("subject"), contains("Activity"), contains("mean"),
                   contains("std"))

# bring test and training data together
finalProject <- rbind(trainMeanStd, testMeanStd)

# labeling with descriptive variable names

names(finalProject) <- gsub("^t", "time", names(finalProject))
names(finalProject) <- gsub("Acc", "Accelerometer", names(finalProject))
names(finalProject) <- gsub("^f", "frequency", names(finalProject))
names(finalProject) <- gsub("Gyro", "Gyroscope", names(finalProject))
names(finalProject) <- gsub("Mag", "Magnitude", names(finalProject))
names(finalProject) <- gsub("BodyBody", "Body", names(finalProject))

# 4 variables had 'angle(tbody...' after previous gsub's.  Not all t's were addressed.
# the '(' is a special character, so made labeling easier by doing the change this way.
names(finalProject) <- gsub("tBody", "timeBody", names(finalProject))


finalProjectOutput <- (finalProject %>% group_by(subject, Activity) %>% summarise_each (funs(mean)))

write.table(finalProjectOutput, file = "finalProjectOutput.txt", row.name = FALSE)
