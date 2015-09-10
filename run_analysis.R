######################################
######     Christie Haskell     ######
######       Sept. 9, 2015      ######
######                          ######
######    run_analysis.R v.1    ######
######################################

#Revision history
#-- none --

#Script contains one function:
#run_analysis: 1 - reads in the data collected from the accelerometers from the Samsung Galaxy S smartphone
#              2 - tidys the data table and writes it to file
#              3 - calculates group means and writes it to file

#Input: dir - git repository directory
#Output: two files - dttidy.txt (the tidied data table)
#                  - dttidymeans.txt (group means)
run_analysis <- function(dir) {
  #Required packages
  library(dplyr)
  library(tidyr)
  
  #Read in labels
  features <- read.table(paste(dir,"/data/features.txt",sep=""),sep=" ")            #Features: column labels
  act.labels <- read.table(paste(dir,"/data/activity_labels.txt",sep=""),sep=" ")   #Activties: row labels
    
  #Read in test data
  #------------------
  
  #Test data with feature column names
  dt.test<-read.table(paste(dir,"/data/test/X_test.txt",sep=""),sep="",col.names=features$V2)
  
  #Map activity numbers to their word labels and add column to data table
  test.labels.num <- read.table(paste(dir,"/data/test/Y_test.txt",sep=""),sep=" ")
  dt.test$activity<-plyr::mapvalues(as.character((test.labels.num$V1)),from=as.character(act.labels$V1), to=as.character(act.labels$V2))
  
  #Add subject column
  dt.test$subject<-read.table(paste(dir,"/data/test/subject_test.txt",sep=""),sep=" ")$V1
  
  
  #Read in train data
  #------------------
  
  #Train data with feature column names
  dt.train<-read.table(paste(dir,"/data/train/X_train.txt",sep=""),sep="",col.names=features$V2)
  
  #Map activity numbers to their word labels and add column to data table
  train.labels.num <- read.table(paste(dir,"/data/train/Y_train.txt",sep=""),sep=" ")
  dt.train$activity<-plyr::mapvalues(as.character((train.labels.num$V1)),from=as.character(act.labels$V1), to=as.character(act.labels$V2))
  
  #Add subject column
  dt.train$subject<-read.table(paste(dir,"/data/train/subject_train.txt",sep=""),sep=" ")$V1
  
  
  #Merge the data tables and make tbl df
  dt <- tbl_df(rbind(dt.test,dt.train))
  
  #Create record variable
  dt$record <- 1:length(dt$activity)
  
  
  #Tidy the data
  #--------------
  
  dt.tidy<-dt %>%
    
    #Select the record, subject, activity, mean and standard deviation columns
    select(matches("record"),
           matches("subject"),
           matches("activity"),
           contains("mean"),
           contains("std"),
           -contains("meanFreq"),
           -contains("gravityMean"),
           -contains("tBodyAccMean"),
           -contains("tBodyAccJerkMean"),
           -contains("tBodyGyroMean"),
           -contains("tBodyGyroJerkMean")) %>%
    
    #Convert from wide to long
    gather(feature.metric.axis,value,tBodyAcc.mean...X:fBodyBodyGyroJerkMag.std..) %>%
    
    #Fix feature.metric.axis values
    mutate(feature.metric.axis = gsub("\\.{3}", ".", feature.metric.axis)) %>%
    mutate(feature.metric.axis = gsub("\\.{2}", ".NA", feature.metric.axis)) %>%
    
    #Create separate columns for feature, metric and axis
    extract(feature.metric.axis,into=c("feature","metric","axis"),regex="([[:alnum:]]+).([[:alnum:]]+).([[:alnum:]]+)") %>%
    
    #Make the mean and standard deviation in their own column
    spread(metric,value)
  
  #Write the table to file
  write.table(dt.tidy,paste(dir,"/dttidy.txt",sep=""),sep=" ",row.name=FALSE)
  
  
  #Calculate means
  #----------------

  dt.tidy.means<-dt.tidy %>%
    
    #Set groups to calculate means for
    group_by(subject,activity,feature,axis) %>%
    
    #Calculate the mean, mean and the mean standard deviation in each group
    summarise(avgMean=mean(mean),avgStd=mean(std))
  
  #Write the table to file
  write.table(dt.tidy.means,paste(dir,"/dttidymeans.txt",sep=""),sep=" ",row.name=FALSE)
}