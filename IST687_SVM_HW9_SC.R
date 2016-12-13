#Surabhi Chouhan
#IST687 - Support Vector Machines Homework

#Step 1: Load the data  

airquality

#replacing NA's with mean values
airquality$Ozone[is.na(airquality$Ozone)] <- round(mean(airquality$Ozone, na.rm = TRUE))
airquality$Solar.R[is.na(airquality$Solar.R)] <- round(mean(airquality$Solar.R, na.rm = TRUE))
airquality


#Step 2: Creat test and train dataset

randIndex	<- sample(1:nrow(airquality))
head(randIndex)
nr<- nrow(airquality)

cutPoint2_3	<- floor(2*nr/3)
cutPoint2_3

trainAirquality <-airquality[randIndex[1:cutPoint2_3],]
testAirquality <-airquality[randIndex[(cutPoint2_3+1):nr],]


#Step 3: Build a Model using KSVM & visualize the results

install.packages("kernlab")
library("kernlab")
install.packages("ggplot2")
library("ggplot2")

#1
predictOzone <- function(a, airquality){
  predictedOzone <- predict(a, airquality)
  results1 <- table(predictedOzone, airquality$Ozone)
  print(results1)
  percentCorrect1 <- (results1[1,1]+results1[2,2])/(results1[1,1]+results1[1,2]+results1[2,1]+results1[2,2])*100
  round(percentCorrect1)  
  return(percentCorrect1)
}

modelKSVM <- ksvm(Ozone ~ ., data = airquality)
predictOzone(modelKSVM, airquality)

#2
root_square <- function(error)
{
  sqrt(mean(error^2))
}

modelKSVM.first <- predict(modelKSVM, airquality)
modelKSVM.error <- (airquality$Ozone - modelKSVM.first)
root_square(modelKSVM.error)

#3
#Creating new dataframe
dfnew <- data.frame(airquality$Wind,airquality$Temp,modelKSVM.error)

#altering column names
colnames(dfnew) <- c("Wind","Temp","Error")

#plotting points
plotdf <- ggplot(data = dfnew,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=modelKSVM.error), color = "red") + ggtitle("KSVM Model")
plotdf


#4

install.packages("e1071")
library(e1071)
modelSVM <- svm(Ozone ~ ., data = airquality)
predictOzone(modelSVM, airquality)

modelSVM.first <- predict(modelSVM, airquality)
modelSVM.error <- (airquality$Ozone - modelSVM.first)
root_square(modelSVM.error)

dfnew1 <- data.frame(airquality$Wind,airquality$Temp,modelSVM.error)
colnames(dfnew1) <- c("Wind","Temp","Error")
plotdf1 <- ggplot(data = dfnew1,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=modelSVM.error), color = "red") + ggtitle("SVM Model")
plotdf1

modelLM <- lm(Ozone ~., data=airquality)

modelLM.first <- predict(modelLM, airquality)
modelLM.error <- (airquality$Ozone - modelLM.first)
root_square(modelLM.error)

dfnew2 <- data.frame(airquality$Wind,airquality$Temp,modelLM.error)
colnames(dfnew2) <- c("Wind","Temp","Error")
plotdf2 <- ggplot(data = dfnew2,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=modelLM.error), color = "red") + ggtitle("LM Model")
plotdf2

#5

install.packages("gridExtra")
library(gridExtra)
grid.arrange(plotdf,plotdf1, plotdf2, ncol = 2)



#Step 4: Create a goodOzone variable 

goodOzone <-c()
for (i in 1:153) {
  if (airquality$Ozone[i] < mean(airquality$Ozone)){
    goodOzone<-append(goodOzone,0)
  }
  else goodOzone<-append(goodOzone,1)
}

#adding the column in the dataframe
airquality<-data.frame(airquality,goodOzone)



#Step 5: See if we can do a better job predicting good and bad days
#1

predictGoodozone <- function(m, airquality){
  predictedGoodozone <- predict(m, airquality)
  results1 <- table(predictedGoodozone, airquality$goodOzone)
  print(results1)
  percentCorrect1 <- (results1[1,1]+results1[2,2])/(results1[1,1]+results1[1,2]+results1[2,1]+results1[2,2])*100
  round(percentCorrect1)  
  return(percentCorrect1)
}

modelKSVM1 <- ksvm(goodOzone ~ ., data = airquality)
predictGoodozone(modelKSVM1, airquality)

#2

root_square1 <- function(error)
{
  sqrt(mean(error^2))
}

modelKSVM1.first <- predict(modelKSVM1, airquality)
modelKSVM1.error <- (airquality$goodOzone - modelKSVM1.first)
root_square1(modelKSVM1.error)

#3

dfnew11 <- data.frame(airquality$Wind,airquality$Temp,modelKSVM1.error)
colnames(dfnew11) <- c("Wind","Temp","Error")
plotdf11 <- ggplot(data = dfnew11,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=modelKSVM1.error), color = "red") + ggtitle("KSVM Model")
plotdf11

#4

modelSVM1 <- svm(goodOzone ~ ., data = airquality)
predictGoodozone(modelSVM1, airquality)

modelSVM1.first <- predict(modelSVM1, airquality)
modelSVM1.error <- (airquality$goodOzone - modelSVM1.first)
root_square1(modelSVM1.error)

dfnew12 <- data.frame(airquality$Wind,airquality$Temp,modelSVM1.error)
colnames(dfnew12) <- c("Wind","Temp","Error")
plotdf12 <- ggplot(data = dfnew12,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=modelSVM1.error), color = "red") + ggtitle("SVM Model")
plotdf12

#Naive Bayes
modelNB <- naiveBayes(goodOzone ~ ., data = airquality)
predictGoodozone(modelNB, airquality)

modelNB.error <- predict(modelNB, airquality)
tmp <- data.frame(airquality$goodOzone)
predictedNB <- predict(modelNB, tmp)
predictedNB


dfnew13 <- data.frame(airquality$Wind,airquality$Temp,modelNB.error)
colnames(dfnew13) <- c("Wind","Temp","Error")
plotdf13 <- ggplot(data = dfnew13,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=modelSVM1.error), color = "red") + ggtitle("SVM Model")
plotdf13


#5

grid.arrange(plotdf11,plotdf12, plotdf13, ncol = 2)

#Step 6: Which are the best Models for this data? 

#Even though the difference is not much, the KSVM is the better fit than SVM
#We cannot use SVM for non linear modelling

