#------------------------------------------------------------------------------------------#
# title: Script for importing and cleaning smartphone accelerometer data (refer README)   #
# author: "dumsim"                                                                         #
# date: "Saturday, April 25, 2015"                                                         #
# notes:                                                                                   #
#------------------------------------------------------------------------------------------#

# Get packages
  require(data.table)

# Set data directory
  dir_data<-paste(getwd(),"/UCI HAR Dataset/",sep="")

# Load data
  # Load features data
  d_test_attrib <- read.table(paste(dir_data,"test\\x_test.txt",sep=""))
  d_train_attrib <- read.table(paste(dir_data,"train\\X_train.txt",sep=""))

  # Load activity data
  d_test_activity <- read.table(paste(dir_data,"test\\y_test.txt",sep=""))
  d_train_activity <- read.table(paste(dir_data,"train\\y_train.txt",sep=""))

  # Load subject data
  d_test_person <- read.table(paste(dir_data,"test\\subject_test.txt",sep=""))
  d_train_person <- read.table(paste(dir_data,"train\\subject_train.txt",sep=""))

  # Load label files
  labels_attrib<-read.table(paste(dir_data,"features.txt",sep=""))
  labels_activity<-read.table(paste(dir_data,"activity_labels.txt",sep=""))

# Check data 
  # Test that the datasets have the same column names
  cat("Test column names match\n",all(colnames(d_test_attrib)==colnames(d_train_attrib)))
  cat("Test column names match\n",identical(colnames(d_test_attrib),colnames(d_train_attrib)))

  # Test that the activity datasets and the attribute datasets contain the same number of observations
  cat("Length Test dataset: ",(length(d_test_attrib[,1])==length(d_test_activity[,1]))&(length(d_test_activity[,1])==length(d_test_person[,1])))
  cat("Length Train dataset: ",(length(d_train_attrib[,1])==length(d_train_activity[,1]))&(length(d_train_activity[,1])==length(d_train_person[,1])))

# Combine test and train data
  d_attrib<-rbind(d_test_attrib,d_train_attrib)
  d_activity<-rbind(d_test_activity,d_train_activity)
  d_person<-rbind(d_test_person,d_train_person)

# Apply column names  
  colnames(d_attrib)<-labels_attrib[,2]
  colnames(d_activity)<-c("activitynumber")
  colnames(labels_activity)<-c("activitynumber","activityname")
  colnames(d_person)<-c("subject")
      
# Filter for columns containing "mean(" or "std("
  colselect <-sapply(labels_attrib[,2],FUN=function(x){x %like% "mean\\("|x %like% "std\\("})
  d_attrib<- d_attrib[,c(colselect==TRUE)]

# Merge activity labels into activity data
  d_activity<-merge(d_activity,labels_activity)
  
# Bind activity, subject and attribute (switch to data.table)
  d_merged <- data.table(cbind(d_person,activityname=d_activity$activityname,d_attrib))
  
# Create tidy data set with summary by subject and activity
  d_tidy <- d_merged[,j=lapply(.SD,mean),keyby=c('subject','activityname')]
  
# Remove temp files
  rm(colselect,d_activity,d_attrib,d_merged, d_person,d_test_activity,d_test_attrib,d_test_person,d_train_activity,d_train_attrib,d_train_person,dir_data, labels_activity,labels_attrib)
  
# Output to terminal
  print(d_tidy)
  
# END OF PROJECT ------------------------------------------------------------------------------------------------------------#
  
  