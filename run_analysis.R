run_analysis <- function() 
{
  #Verifying the avilablility of test and train directory
  
  if(!file.exists("./test")) stop("test directory unavailable")
  if(!file.exists("./train")) stop("train directory unavailable")
  
  #loading the actitivies data frame
  
  activity <- readLines("activity_labels.txt")
  activity <- as.data.frame(activity)
  activity$id <- substr(activity$activity,1,1)
  
  #loading the names of the features
  
  features <- readLines("features.txt")
  
  #identifying the mean and std indexes
  featuressubsetindex <- c(grep("mean",features),grep("std",features))
  
  # reading and merging the test and train datasets
  
  testsubject <- read.table("test/subject_test.txt",header=F,sep="",col.names=c("subject"))
  trainsubject <- read.table("train/subject_train.txt",header=F,sep="",col.names=c("subject"))
  
  Xtest <- read.table("test/X_test.txt",header=F,sep="")
  Xtrain <- read.table("train/X_train.txt",header=F,sep="")
  
  Ytest <- read.table("test/Y_test.txt",header=F,sep="",col.names=c("activityC"))
  Ytrain <- read.table("train/Y_train.txt",header=F,sep="",col.names=c("activityC"))  
  
  msubject <-rbind(testsubject,trainsubject)
  mXdata <- rbind(Xtest,Xtrain)
  mYdata <- rbind(Ytest,Ytrain)
  
  colnames(mXdata) <- features
  
  #write the subset data to the destionation file  
  write.table(mXdata[,featuressubsetindex],file="subsetdata.txt",col.names=T,sep="")
  
  #column merging the activity and subject 
  mXdata <- cbind(mXdata,msubject)
  mXdata <-cbind (mXdata,mYdata)
  tidydata<-merge(mXdata,activity,by.x="activityC",by.y="activity",all.x=T)
  #names(tidydata)
  #melting the data
  #reshape2 package is required
  melttidydata <- melt(tidydata,id=c("activityC","subject"),measure.vars=as.character(features))
 
  #recasting to get the mean
  casttidydata <- dcast(melttidydata,activityC+subject~variable,mean)
  
  #use this to write it to destination file
  #write.table(casttidydata,file="meltdata.txt",col.names=T,sep="")
  
  
}