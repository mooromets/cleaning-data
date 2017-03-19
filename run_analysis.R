## function readTrainTest(path) reads both train- and test-files TXT files
## and merges results in the corresponding order
## params :
##      path - path to both files containing a key "<mode>" as a plug 
##              distinguishing the train-path from the test-path
## returns : dataset

readTrainTest <- function (path) 
{
    train <- read.table(gsub ("<mode>", "train", path), header = FALSE)
    test <- read.table(gsub ("<mode>", "test", path), header = FALSE)
    bind_rows(train, test)
}


## Read all measurements
x <- readTrainTest("UCI HAR Dataset/<mode>/X_<mode>.txt")

## Read feature names and assign them to variables
names(x) <- read.table("UCI HAR Dataset/features.txt", header = FALSE)[,2]

## Subset only columns containing means and standard deviations
x <- x[,grep("-mean[()]|-std[()]", names(x))]

## Read activity variable and add it to the set
x$activity <- readTrainTest("UCI HAR Dataset/<mode>/y_<mode>.txt")[,1]

## Read subject variable and add it to the set
x$subject <- readTrainTest("UCI HAR Dataset/<mode>/subject_<mode>.txt")[,1]

## Read activity names and replace activity variable values with their names
actLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
x <- x %>% mutate(activity = as.character( actLabels[activity,2]))

## Create a new data with means of each variable for each activity and each subject 
harData <- x %>% 
    group_by(activity, subject) %>% #group dataset on activity and subject
    summarise_each(funs(mean(.)), #apply mean() function on each variable
                   `tBodyAcc-mean()-X`:`fBodyBodyGyroJerkMag-std()`)#all measurements

write.table(harData, "output.txt", row.name=FALSE)