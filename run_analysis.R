##Script to download and tidy the giro data

library(dplyr)
library(tidyr)

run_analysis<- function(){
	download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","gyro.zip")
	unzip("gyro.zip")
	data1<- tbl_df(read.table("UCI HAR Dataset\\test\\X_test.txt"))
	data2<- tbl_df(read.table("UCI HAR Dataset\\train\\X_train.txt"))
	data<-bind_rows(data1,data2)
	names<-read.table("UCI HAR Dataset\\features.txt")
	colnames(data)<-names$V2
	valid_column_names <- make.names(names=names(data), unique=TRUE, allow_ = TRUE)
	names(data)<-valid_column_names
	data<-select(data, matches("mean|std"))

	## pulling in subjects
	subject1<- tbl_df(read.table("UCI HAR Dataset\\test\\subject_test.txt"))
	subject2<- tbl_df(read.table("UCI HAR Dataset\\train\\subject_train.txt"))
	subject<- bind_rows(subject1, subject2)
	names(subject)<-"Subject"
	

	## pulling in activity labels
	label1<- tbl_df(read.table("UCI HAR Dataset\\test\\y_test.txt"))
	label2<- tbl_df(read.table("UCI HAR Dataset\\train\\y_train.txt"))
	label<- bind_rows(label1, label2)

	## Changing activity labels to words
	label<-lapply(label, FUN= function(x) gsub("1","Walking",x))
	label<-lapply(label, FUN= function(x) gsub("2","Walking Upstairs",x))
	label<-lapply(label, FUN= function(x) gsub("3","Walking Downstairs",x))
	label<-lapply(label, FUN= function(x) gsub("4","Sitting",x))	
	label<-lapply(label, FUN= function(x) gsub("5","Standing",x))
	label<-lapply(label, FUN= function(x) gsub("6","Laying",x))
	

	## Converting label to data frame
	label<-tbl_df(data.frame(label[1]))
	names(label)<-"Activity"
	
	
	##joining labels and columns 
	data<- subject%>% 
		bind_cols(label)%>%
		bind_cols(data)
	data

	##Finding means by subject & activity
	means<-data %>% 
		group_by(Subject,Activity)%>% 
		summarize_each(funs(mean))

	write.csv(means, "UCI HAR_means.csv")
	}