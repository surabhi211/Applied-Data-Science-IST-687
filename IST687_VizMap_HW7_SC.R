#Surabhi Chouhan
#IST687 - Viz Map HW 7: Median Income


#installing and loading the required packages
install.packages("gdata")
library("gdata")
install.packages("zipcode")
library("zipcode")
install.packages("ggplot2")
library("ggplot2")
install.packages("ggmap")
library("ggmap")


#Step 1
#reading the data set
incomeDf <- read.xls("C:/Users/Surabhi/Downloads/MedianZIP-3.xlsx", sheet=1, header=TRUE, perl="C:/Strawberry/perl/bin/perl.exe")

#renaming the column names
colnames(incomeDf) <- c("zip","median","mean","population")

#importing the zipcode package
data("zipcode")

#removing the states Alaska and Hawaii
newZip <- subset(zipcode,zipcode$state != "AK")
finalZip <- subset(newZip,newZip$state != "HI")


#Step 2

#Combining the data frames incomeDf and finalZip using the common sttribute zip between them
mergedDf <- merge(x=incomeDf,y=finalZip,by="zip")

#sorting the state abbreviations in mergedDf to put into the final dataframe
stateAbb <- sort(unique(mergedDf$state))

#finding out the average median income and sum of population in mergedDf
avgmedianDf <- tapply(as.numeric(mergedDf$median),mergedDf$state,mean)
sumPop <- tapply(as.numeric(mergedDf$population),mergedDf$state,sum)

#creating a datframe with average median income, total population and states
custDf <- data.frame(avgmedianDf,sumPop,stateAbb)

#mathing and putting the name of states in the final dataframe
custDf$stateNames <- state.name[match(custDf$stateAbb,state.abb)]

#finding out the map data for us from the inbuilt dataset
us_map <- map_data("state")

#creating a simple map
map.simple	<- ggplot()		
map.simple	<- map.simple +	geom_map(data=us_map,aes(x=us_map$long,y=us_map$lat,map_id=region),map=us_map,fill="white",	color="black")	
map.simple

#Map showing color with the average median income of each state
custDf$stateNames <- tolower(custDf$stateNames)
map.income	<- map.simple + geom_map(data=custDf,map=us_map,aes(fill=avgmedianDf,map_id=stateNames),color="black",na.rm=TRUE)
map.income

#Map with color representing the population of each state
map.pop <- map.simple + geom_map(data=custDf,map=us_map,aes(fill=sumPop,map_id=stateNames),color="black",na.rm=TRUE)
map.pop

#Step 3 Income per zip code

#map with points showing the median income for the state
map.zip <- map.simple + geom_point(data=mergedDf,aes(x=mergedDf$latitude,y=mergedDf$longitude,colour=median),na.rm=TRUE)
map.zip <- map.zip + ggtitle("Income per zip code")
map.zip


#Step 4

#map showing density distribution of zipcodes 
map.density <- map.simple + stat_density2d(aes(x=mergedDf$longitude, y=mergedDf$latitude), data=mergedDf, geom="polygon") +
                            scale_fill_gradient(low="black",high="green")+
                            scale_alpha(range=c(0.00,0.25))+
                            ggtitle("Density for all Zip codes in USA")+
                            theme (plot.title=element_text(lineheight=3.5,face="bold"))

map.density
