xt<-read.table("test/X_test.txt",header=FALSE,na.strings="",sep="")
yt<-read.table("test/y_test.txt",header=FALSE,sep=" ")
a<-read.table("test/subject_test.txt",header=FALSE,sep=" ")

xtr<-read.table("train/X_train.txt",header=FALSE,na.strings="",sep="")
ytr<-read.table("train/y_train.txt",header=FALSE,sep=" ")
b<-read.table("train/subject_train.txt",header=FALSE,sep=" ")

fea<-read.table("features.txt",header=FALSE,na.strings="",sep="")

colnames(yt)<- "activityCode"
colnames(ytr)<- "activityCode"
colnames(a)<- "subject"
colnames(b)<- "subject"
colnames(xtest)<- fea$V2
colnames(xtrain)<- fea$V2

yt$activity[yt$activityCode==1]<-"WALKING"
yt$activity[yt$activityCode==2]<-"WALKING_UPSTAIRS"
yt$activity[yt$activityCode==3]<-"WALKING_DOWNSTAIRS"
yt$activity[yt$activityCode==4]<-"SITTING"
yt$activity[yt$activityCode==5]<-"STANDING"
yt$activity[yt$activityCode==6]<-"LAYING"

ytr$activity[ytrain$activityCode==1]<-"WALKING"
ytr$activity[ytrain$activityCode==2]<-"WALKING_UPSTAIRS"
ytr$activity[ytrain$activityCode==3]<-"WALKING_DOWNSTAIRS"
ytr$activity[ytrain$activityCode==4]<-"SITTING"
ytr$activity[ytrain$activityCode==5]<-"STANDING"
ytr$activity[ytrain$activityCode==6]<-"LAYING"

test<-cbind(a,xt)  ## I wanted to use merge but always got warnings about the memory.size
test<-cbind(yt,test)
train<-cbind(b,xtr)
train<-cbind(ytr,train)

sum<-rbind(train,test)

cleardata<-subset(sum,select=c("subject","activity","activityCode","tBodyAccMag-mean()",
                              "tGravityAccMag-mean()","tBodyAccJerkMag-mean()","tBodyGyroMag-mean()",
                              "tBodyGyroJerkMag-mean()","fBodyAccMag-mean()","fBodyBodyAccJerkMag-mean()",
                              "fBodyBodyGyroMag-mean()","fBodyBodyGyroJerkMag-mean()","tBodyAccMag-std()",
                              "tGravityAccMag-std()","tBodyAccJerkMag-std()","tBodyGyroMag-std()",
                              "tBodyGyroJerkMag-std()","fBodyAccMag-std()","fBodyBodyAccJerkMag-std()",
                              "fBodyBodyGyroMag-std()","fBodyBodyGyroJerkMag-std()"))

summary<-data.frame()

for(i in 1:30){
  subjectdata<-subset(cleardata,cleardata$subject==i)
  for(j in 1:6){
    activitydata<-subset(subjectdata,subjectdata$activityCode==j)
    a<-6*(i-1)+j
    summary[a,1]<-i
    summary[a,2]<-j
    if(j==1){summary[a,3]<-"WALKING"}
    if(j==2){summary[a,3]<-"WALKING_UPSTAIRS"}
    if(j==3){summary[a,3]<-"WALKING_DOWNSTAIRS"}
    if(j==4){summary[a,3]<-"SITTING"}
    if(j==5){summary[a,3]<-"STANDING"}
    if(j==6){summary[a,3]<-"LAYING"}
    for(p in 4:21){
      summary[a,p]<-mean(activitydata[,p])
    }
  }    
}

colnames(summary)<-c("subject","activity","activityCode","tBodyAccMag-mean()",
                     "tGravityAccMag-mean()","tBodyAccJerkMag-mean()","tBodyGyroMag-mean()",
                     "tBodyGyroJerkMag-mean()","fBodyAccMag-mean()","fBodyBodyAccJerkMag-mean()",
                     "fBodyBodyGyroMag-mean()","fBodyBodyGyroJerkMag-mean()","tBodyAccMag-std()",
                     "tGravityAccMag-std()","tBodyAccJerkMag-std()","tBodyGyroMag-std()",
                     "tBodyGyroJerkMag-std()","fBodyAccMag-std()","fBodyBodyAccJerkMag-std()",
                     "fBodyBodyGyroMag-std()","fBodyBodyGyroJerkMag-std()")
                     
                     
write.table(summary,"tidydata.txt",col.names=TRUE)
