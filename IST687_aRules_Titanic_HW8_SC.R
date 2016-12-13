#Surabhi Chouhan
#IST687 - aRULES Lab: The Titanic

install.packages("sqldf")
library("sqldf")
install.packages("plyr")
library("plyr")
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)

#importing dataset
load("D:/Syracuse_University/ist687/titanic.raw.rdata")
t <- titanic.raw
View(t)

#Step 1: Descriptive Stats

#percentage people survived
surv <- count(sqldf("select Survived from t where Survived=='Yes' "))
surv
percentage <- (surv$freq/2201)*100
percentage

#percentage of people that were children
child <- tapply(t$Age,t$Age,count)
child
percent <- (child$Child$freq/2201)*100
percent

#percentage of people that were female
female <- tapply(t$Sex,t$Sex,count)
female
percent1 <- (female$Female$freq/2201)*100
percent1

#percentage of people that were in first class
first <- tapply(t$Class,t$Class,count)
first
p_first <- (first$`1st`$freq/2201)*100
p_first

#Step 2: More Descriptive Stats

#percentage of children survived
child_surv <- sqldf("select count(*) from t where Age='Child' AND Survived='Yes' ")
child_surv
p1 <- (child_surv/2201)*100
p1

#percentage of female survived
female_surv <- sqldf("select count(*) from t where Sex='Female' AND Survived='Yes' ")
female_surv
p2 <- (female_surv/2201)*100
p2

#percentage of first class people survived
first_surv <- sqldf("select count(*) from t where Class='1st' AND Survived='Yes' ")
first_surv
p3 <- (first_surv/2201)*100
p3

#percentage of third class people survived
third_surv <- sqldf("select count(*) from t where Class='3rd' AND Survived='Yes' ")
third_surv
p4 <- (third_surv/2201)*100
p4


#Step 3: Writing a Function

myFunction <- function(class,sex,age,surv)
{
  i <- 0
  for (i in length(t)){
      if(class=='1st' && sex=='Female' && age=='Adult' && surv=='Yes'){
        df <- data.frame(class,sex,age,surv)
        return(df)
      }
  }
}

#Function calling with different arguments to check the functionality of it
myFunction('1st','Female','Adult','Yes')
myFunction('1st','Male','Child','Yes')
myFunction('3rd','Female','Adult','No')


percentFunction <- function()
{
  num <- length(df)
 return((num/2201)*100)
}

percentFunction()


#Step 4 Use aRules

ruleset <- apriori(t,parameter=list(support=0.07,confidence=0.4)) #76 rules generated
summary(ruleset)

inspect(ruleset)
plot(ruleset)

#most interesting and useful rules

#Contains a value for lift which is large enough to make this a good rule, and conditions which will be a valid condition to check
#[36] {Class=1st,Survived=Yes}  => {Age=Adult}    0.08950477 0.9704433  1.0210066 

#Contains large value for lift, and conditions which will be a valid condition to check
#[46] {Class=3rd,Survived=No}   => {Age=Adult}    0.21626533 0.9015152  0.9484870

#Contains maximum value for lift, and conditions which will be a valid condition to check
#[42] {Sex=Female,Age=Adult}    => {Survived=Yes} 0.14357110 0.7435294  2.3016993






