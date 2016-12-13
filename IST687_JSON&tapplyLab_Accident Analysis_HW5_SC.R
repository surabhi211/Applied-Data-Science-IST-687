#Surabhi Chouhan
#IST687 - JSON & tapply Lab: Accident Analysis

#Step 1

#install the required jsonlite and curl packages to read json and to read json from web url respectively
install.packages("jsonlite")
library("jsonlite")
install.packages("curl")
library("curl")

acc_data <- fromJSON("https://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD") #this command gets the json from given url

#Step 2

accidents <- data.frame(acc_data[['data']]) #as the json contains metadata as well as data, so this command extracts the data from json and stores it as a dataframe
accident_analysis <- accidents[,-(1:8)] #removing the first 8 columns and storng it into another variable

#this command is creating a vector for the column names of the data frame
namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")
View(namesOfColumns)
names(accident_analysis) <- namesOfColumns #naming the columns of dataframe
View(accident_analysis)
head(accident_analysis) #displaying the first few values of data frame to check that column names are reflected or not

#Step 3

#install the required sqldf package to use sql statements
install.packages("sqldf")
library("sqldf")

#remlving extra space from the day of week using gsub function, and as it removes the space, the factor gets converted to character, so next step makes it a factor again
accident_analysis$DAY_OF_WEEK <- gsub('\\s+','',accident_analysis$DAY_OF_WEEK)
accident_analysis$DAY_OF_WEEK<-as.factor(accident_analysis$DAY_OF_WEEK)

#this command counts the number of accidents that took place on Sunday
sqldf("select count(*) from accident_analysis where DAY_OF_WEEK == 'SUNDAY'")

#this command counts the number of accidents that had injuries
sqldf("select count(*) from accident_analysis where INJURY == 'YES' AND INJURY IS NOT NULL")

#this command gives the count of injuries according to the day of week 
sqldf("select DAY_OF_WEEK, count(*) from accident_analysis where INJURY == 'YES'  group by DAY_OF_WEEK")


#Step 4

#importing package for the count function
install.packages("plyr")
library(plyr)

#this command counts the number of accidents that happened on Sunday. The difference in output is that is shows the count for other days under FALSE and for Sunday under TRUE as I have given the group by condition specifying the value of DAY_OF_WEEK. Rest the count in both sql and tapply remains the same.
acc_on_sunday <- tapply(accident_analysis$DAY_OF_WEEK,accident_analysis$DAY_OF_WEEK =="SUNDAY",count)
acc_on_sunday

#this command gives the count of accidents which had injuries. The difference in output is that it gives the count of INJURY=NO under FALSE and for INJURY=YES under TRUE, as I have given the group by condition specifying the value of INJURY. Rest the count in both sql and tapply remains the same.
acc_with_injury <- tapply(accident_analysis$INJURY,accident_analysis$INJURY == "YES",count)
acc_with_injury

#this command gives the count of injuries according to the days, with output showing count of INJURY=NO under FALSE and count of INJURY=YES under TRUE as my group by condition contains a specific value of INJURY. Rest the count in both sql and tapply remains the same.
injury_list_day <- tapply(accident_analysis$DAY_OF_WEEK,accident_analysis$INJURY=="YES",count)
injury_list_day
