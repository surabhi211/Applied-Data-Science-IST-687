#Surabhi Chouhan
#IST687 - Viz Lab:  air quality Analysis

#This Rscript is analyzing the Air Quality dataset by plotting different types of charts and graphs

#installing the necessary package
install.packages("ggplot2")
library("ggplot2")

#Step 1
#Copying the dataset into a temporary one
temp_aq <- airquality
temp_aq

#Step 2
#Removing the NAs using "na.rm=TRUE" in the function itself, so that any data is not skipped except the NAs


#Step 3 : Understand the data distribution

#Making the histogram for Ozone in steps
g <- ggplot(temp_aq,aes(x=Ozone))
g <- g + geom_histogram(binwidth=10,color="red",fill="green",na.rm=TRUE)
g

#Making the histogram for Solar.R in steps
g <- ggplot(temp_aq,aes(x=Solar.R))
g <- g + geom_histogram(binwidth=10,color="red",fill="yellow",na.rm=TRUE)
g

#Making the histogram for Wind in steps
g <- ggplot(temp_aq,aes(x=Wind))
g <- g + geom_histogram(binwidth=1,color="red",fill="lightblue",na.rm=TRUE)
g

#Making the histogram for Temp in steps
g <- ggplot(temp_aq,aes(x=Temp))
g <- g + geom_histogram(binwidth=1,color="red",fill="pink",na.rm=TRUE)
g

#boxplot for Ozone
g <- ggplot(temp_aq, aes(x=Ozone,y=date))
g <- g + geom_boxplot(na.rm=TRUE)
g

#Boxplot for different wind values
wind<-factor(temp_aq$Wind)
g <- ggplot(temp_aq, aes(x=wind,y=date))
g <- g + geom_boxplot(na.rm=TRUE)
g


#Explore how the data changes over time

#combining the Month and Day column to get a column named "date"
#code courtesy- http://stackoverflow.com/questions/26334763/merge-three-different-columns-into-a-date-in-r
date <- as.Date(with(temp_aq, paste(1973, temp_aq$Month, temp_aq$Day,sep="-")), "%Y-%m-%d")

#Making the line chart for Ozone with date on y-axis
g_o <- ggplot(temp_aq, aes(x=reorder(Ozone,date),y=date,group=1))
g_o <- g_o + geom_line(color="red",na.rm=TRUE)+geom_point()
g_o <- g_o + theme(axis.text.x=element_text(angle=90,hjust=1))
g_o

#Making the line chart for Temp with date on y-axis
g_t <- ggplot(temp_aq, aes(x=reorder(Temp,date),y=date,group=1))
g_t <- g_t + geom_line(color="purple",na.rm=TRUE)+geom_point()
g_t <- g_t + theme(axis.text.x=element_text(angle=90,hjust=1))
g_t

#Making the line chart for Wind with date on y-axis
g_w <- ggplot(temp_aq, aes(x=reorder(Wind,date),y=date,group=1))
g_w <- g_w + geom_line(color="blue",na.rm=TRUE)+geom_point()
g_w <- g_w + theme(axis.text.x=element_text(angle=90,hjust=1))
g_w

#Making the line chart for Solar.R with date on y-axis
g_s <- ggplot(temp_aq, aes(x=reorder(Solar.R,date),y=date,group=1))
g_s <- g_s + geom_line(color="green",na.rm=TRUE)+geom_point()
g_s <- g_s + theme(axis.text.x=element_text(angle=90,hjust=1))
g_s

#One chart with 4 lines each for Ozone,Temp,Wind and Solar.R
g_line <- ggplot(temp_aq,aes(x=date),group=1)
g_line <- g_line + geom_line(aes(y=Ozone,color="Ozone"),na.rm=TRUE)
g_line <- g_line + geom_line(aes(y=Temp,color="Temp"),na.rm=TRUE)
g_line <- g_line + geom_line(aes(y=Wind,color="Wind"),na.rm=TRUE)
g_line <- g_line + geom_line(aes(y=Solar.R,color="Solar.R"),na.rm=TRUE)
g_line

#Step 4
#Heat Map

#Heat map in steps, with xaxis as day and yaxis as different variables Ozone, Temp, Wind and Solar.R
g_heat<- ggplot(temp_aq, aes(x=Day,group=Day)) 
g_heat<- g_heat + geom_tile(aes(y = Ozone), color="red",na.rm=TRUE)  
g_heat<- g_heat + geom_tile(aes(y = Temp), color="green",na.rm=TRUE)
g_heat<- g_heat + geom_tile(aes(y = Wind), color="purple",na.rm=TRUE)
g_heat<- g_heat + geom_tile(aes(y = Solar.R), color="orange",na.rm=TRUE)
g_heat

#Step 5
#Scatter plot

#Making a scatter plot with Wind as xaxis, Temp as yaxis, size of dot as Ozone and color as Solar.R
g_sc	<- ggplot(temp_aq,aes(x=Wind,y=Temp),na.rm=TRUE) 
g_sc	<- g_sc+geom_point(aes(size=Ozone,color=Solar.R),na.rm=TRUE) 
g_sc


#Step 6

#Most useful visualization
#Line chart, as it shows the variation typically as it is with the highs and lows, and it is easy to understand and interpret
