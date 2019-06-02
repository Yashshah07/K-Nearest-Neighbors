rm(list=ls())   # Clears the environment , removes all the objects 

#Load the "breast-cancer-wisconsin.data.csv" from CANVAS (see the description below) 

cancerdataset<-read.csv("breast-cancer-wisconsin.data.csv")
View(cancerdataset)

#a.Remove the rows with missing values

cancerdataset1<-na.omit(cancerdataset)
cancerdataset1
View(cancerdataset1)

#b.Store every fifth record in a "test" dataset starting with the first record

index<-seq(1,nrow(cancerdataset1),5)
test<-cancerdataset1[index,]
View(test)

#c.  Store the rest in the "training" dataset
training<-cancerdataset1[-index,]
View(training)

#d.Use knn with k=1 and classify the test dataset.  
#e.Measure the performance of knnf.   
#f.Repeat the above steps with k=2, k=5, k=10

install.packages("kknn")
library(kknn)


# when k=1

k1prediction<-kknn(formula= Class~., training, test, k=1, kernel="rectangular")
k1prediction
fit<-fitted(k1prediction)
fit
table(test$Class,fit)

#measure the preformance when k=1

measure1<-test$Class == fit
perform_measure1<-100*sum(measure1)/length(measure1)
perform_measure1


#when k=2

k2prediction<-kknn(formula= Class~., training, test, k=2, kernel="rectangular")
k2prediction
fit<-fitted(k2prediction)
fit
table(test$Class,fit)

#measure the preformance when k=2

measure2<-test$Class == fit
perform_measure2<-100*sum(measure2)/length(measure2)
perform_measure2


#when k=5

k5prediction<-kknn(formula= Class~., training, test, k=5, kernel="rectangular")
k5prediction
fit<-fitted(k5prediction)
fit
table(test$Class,fit)

#measure the preformance when k=5

measure5<-test$Class == fit
perform_measure5<-100*sum(measure5)/length(measure5)
perform_measure5


#when k=10

k10prediction<-kknn(formula= Class~., training, test, k=10, kernel="rectangular")
k10prediction
fit<-fitted(k10prediction)
fit
table(test$Class,fit)

#measure the preformance when k=10

measure10<-test$Class == fit
perform_measure10<-100*sum(measure10)/length(measure10)
perform_measure10
