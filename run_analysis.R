run_analysis<-function(){
  #Call in features list
  install.packages("plyr")
  library(plyr)
  features<-read.table("./UCI HAR Dataset/features.txt", sep="",header=FALSE)
  
  #Create vector full of feature names to later be used in naming data set columns and selects columns for mean and standard deviation calculations.
  feature_vector<-features[,2]
  mean_features<-grep("mean()", feature_vector)
  std_features<-grep("std()",feature_vector)
  desired_features<-sort(append(mean_features,std_features))
  col_names<-feature_vector[desired_features]

  #Call in activity list
  activities<-read.table("./UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
  
  #Create vector full of activity names to later be used in tidy data set
  activities_vector<-activities[,2]

  #Call in test data and test activity labels and eliminate all columns except for mean and standard deviation calculations.
  x_test<-read.table("./UCI HAR Dataset/test/X_test.txt",sep="", header=FALSE)
  x_test_scrub<-x_test[,desired_features]
  
  #Give correct column names to data set
  names(x_test_scrub)<-col_names

  y_test<-read.table("./UCI HAR Dataset/test/Y_test.txt",sep="",header=FALSE)
  subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
  
  #Add activity and subject information to dataset
  x_test_scrub$Activity<-y_test[,1]
  
  x_test_scrub$Subject<-subject_test[,1]


  #Repeat the process but for the training data set
  
  x_train<-read.table("./UCI HAR Dataset/train/X_train.txt",sep="", header=FALSE)
  x_train_scrub<-x_train[,desired_features]
  
  #Give correct column names to data set
  names(x_train_scrub)<-col_names
  
  y_train<-read.table("./UCI HAR Dataset/train/Y_train.txt",sep="",header=FALSE)
  subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
  
  #Add activity and subject information to dataset
  x_train_scrub$Activity<-y_train[,1]
  
  x_train_scrub$Subject<-subject_train[,1]

  #Now we combine the test and training datasets

  full_data<-rbind(x_test_scrub,x_train_scrub)
  
  #Adding the Activity labels to the data set
  full_data<-merge(full_data, activities, by.x="Activity", by.y="V1")
  
  #Arranging the columns to move Subject and Activity columns to the front of the data set. Also removed activity # column.
  full_data$Activity<-full_data$V1
  full_data_ordered<-full_data[,c(80,81,1:79)]
  
  #Orders data set by subject number
  full_data_ordered<-full_data_ordered[order(full_data_ordered$Subject),]
  names(full_data_ordered)[2]<-"Activity"
  
  write.table(full_data_ordered, file="output.txt", col.names=TRUE, sep=",", row.names=FALSE)
  }
