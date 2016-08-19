# Author      : Brandon Bartell
# Date        : 8/19/2016
# Title       : run_analysis.R
# Description : Use wearable data to practice merging and cleaning to produce a tidy data set.

run_analysis<-function() {
  #read format files
  #setwd("C:/Users/BARTEB01/Desktop/Coursera/Getting and Cleaning Data")
  URL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(URL,"Dataset.zip")
  files<-unzip("Dataset.zip",list=TRUE)
  unzip("Dataset.zip")
  
  
  features<-read.table(files[2,1])
  activities<-read.table(files[1,1])
  
  #read data from the test folder
  x_test<-read.table(files[17,1])
  y_test<-read.table(files[18,1])
  subj_test<-read.table(files[16,1])
  
  #read data from the train folder
  x_train<-read.table(files[31,1])
  y_train<-read.table(files[32,1])
  subj_train<-read.table(files[30,1])
  
  #merge training and test data
  x_result<-rbind(x_test,x_train)
  y_result<-rbind(y_test,y_train)
  subj_result<-rbind(subj_test,subj_train)
  
  #use descriptive names for values in data set
  names(x_result)<-features[,2]
  
  #merge subject and activity with measurements
  fd<-cbind(subj_result,y_result,x_result)
  
  #name first two columns: subject and activity
  names(fd)[1:2]<-c("subject","activity")
  
  #select columns having to do with the mean and standard deviation of variables
  fdnames<-names(fd)
  std_ind<-grepl("-std()",fdnames)
  mean_ind<-grepl("-mean()",fdnames)
  
  #remove columns with substring "meanFreq"
  bad_mean_ind<-grepl("-meanFreq",fdnames)
  mean_ind[bad_mean_ind]<-FALSE
  
  #combine mean_ind and std_ind into one vector with all desired indices
  all_ind<-mean_ind | std_ind
  
  #add back in the subject and activity columns to label each observation
  all_ind[1:2]=TRUE
  
  #extract these columns from fd
  newdata<-fd[fdnames[all_ind]]
  
  #give activity appropriate name
  act<-as.matrix(newdata['activity'])
  act2<-activities[act,2]
  newdata['activity']<-act2
  
  #create second, indepedent tidy data set with the average of each variable
  #for each activity and each subject
  DF=as.data.frame(newdata)
  subject<-DF$subject
  activity<-DF$activity
  aggdata<-aggregate(DF,by=list(subject,activity),FUN=mean,na.rm=TRUE)
  aggdata$activity<-NULL
  aggdata$subject<-NULL
  names(aggdata)[1:2]<-c("subject","activity")
  
  
  
  #write data to the results folder
  write.table(aggdata,file="tidydata.csv",row.names=FALSE)
  aggdata
}
