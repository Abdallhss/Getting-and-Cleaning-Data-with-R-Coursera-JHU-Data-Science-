#download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","Human_Activity_Recognition.zip")

#Read in the features names and activities labels
features_names <- read.csv("UCI HAR Dataset/features.txt",sep="",header = F)
activity_labels <- read.csv("UCI HAR Dataset/activity_labels.txt",sep = "",header = F)

#Create a function to read and bind data 
get_and_combin_data <- function(train = TRUE){
    # Check if train or test data
    label <- ifelse(train == T, "train","test")
    #read the features
    X <- read.csv(paste("UCI HAR Dataset/",label,"/X_",label,".txt",sep=""),sep="",header = F)
    # Assign column names
    names(X) <- features_names[,2]
    # read activity labels
    y <- read.csv(paste("UCI HAR Dataset/",label,"/y_",label,".txt",sep=""),header = F)
    # change numbers to their corresponding activity
    y $V1= factor(y$V1,levels=activity_labels[,1],labels = activity_labels[,2])
    # update column name
    names(y) <- "Activity"
    # read the subject and update its name
    subject <- read.csv(paste("UCI HAR Dataset/",label,"/subject_",label,".txt",sep=""),header = F)
    names(subject) <- "Subject"
    # bind all columns together
    cbind(subject,X,y)
}
#Get train and test data
train <- get_and_combin_data(train = TRUE)
test <- get_and_combin_data(train = FALSE)
#merge train and test data
all <- rbind(train,test)
#Get all names and keep those with mean or std.
#regular expression is modified not to include meanfrequency
all_names <- names(all)
keep_columns = c(grep("(mean|std)[^F]",all_names,value = TRUE),
                 "Subject","Activity")
#keep appropiate columns
#assigned names are appropiate. No need to modify
all <- all[ , (all_names %in% keep_columns)]
#check for null values
sum(is.na(all))
#check data summary
summary(all)
#use dplyr library to ease the data manipulation
library(dplyr)
all = tbl_df(all)
# create a pipe line to group the data and calcualte the mean
all_mean <- all %>% group_by(Subject,Activity) %>%
    summarise_all(mean)
# save the data to txt format
write.table(all_mean,file="Human_Activity_Recognition_Tidy.txt", row.names = FALSE)
