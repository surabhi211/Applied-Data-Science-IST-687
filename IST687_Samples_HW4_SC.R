#Surabhi Chouhan
#IST687 - Samples Homework

#Step 1
#importing and installing moments library
install.packages("moments")
library("moments", lib.loc="~/R/win-library/3.3")

printVecInfo <- function(vec) #creating a function as asked in the question
{
  #using the cat() function, displayed various values in the format asked for, the second argument is the separator which makes the values separated. "\n" gives new line and " " gives a space as separator 
  cat("mean:",mean(vec),"\n")
  cat("median:",median(vec),"\n")
  cat("min:",min(vec)," ")
  cat("max:",max(vec),"\n")
  cat("sd:",sd(vec),"\n")
  cat("quantile (0.05 - 0.95):",quantile(vec,probs=c(0.05,0.95)),"\n")
  cat("skewness:",skewness(vec))
}

test <- c(1,2,3,4,5,6,7,8,9,10,50) #creates the vector test
printVecInfo(test) #calling the function by passing test vector as argument


#Step 2
jar <- c() #creates the jar vector
jar[1:50] <- "Red" #the first 50 elements have value "Red"
jar[51:100] <- "Blue" #the last 50 elements have value "Blue"

#Reference taken from http://stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector
BlueRed <- table(jar) #to count the number of red we use the function table(), which craetes a tabular view of the elements in the vector jar
BlueRed[names(BlueRed)== "Red"] #when we compare the names(BlueRed) with "Red", we get the count of elements with value "Red" in table BlueRed (values of vector jar that have value "Red" stored in table)

set.seed(10) #setting seed of sampling to get the same result each time sampling is done
sample(jar, size=10, replace=TRUE) #sample jar 10 times and count the occurences of "Red"

#Simple print statements
print("Got 7 red marbles")
print("Percentage of red marbles:70%")

#sampling jar 10times and as jar contains strings, so table() makes it into a tabular format to get the numeric values, give the count of each red and blue sample, then replicate it 10 times
sampling10 <-replicate(10,table(sample(jar,size=10,replace=TRUE)),simplify=TRUE) 
printVecInfo(sampling10[1,]) #calling function for row with values of Blue
printVecInfo(sampling10[1,]) #calling function for row with values of Red
hist(sampling10) #generating the histogram for the sample created

#sampling jar 100times and as jar contains strings, so table() makes it into a tabular format to get the numeric values, give the count of each red and blue sample, then replicate it 10 times
sampling100 <-replicate(10,table(sample(jar,size=100,replace=TRUE)),simplify=TRUE)
printVecInfo(sampling100[1,]) #calling function for row with values of Blue
printVecInfo(sampling100[2,]) #calling function for row with values of Red
hist(sampling100) #generating the histogram for the sample created

#sampling jar 100times and as jar contains strings, so table() makes it into a tabular format to get the numeric values, give the count of each red and blue sample, then replicate it 100 times
replicate100 <-replicate(100,table(sample(jar,size=100,replace=TRUE)),simplify=TRUE)
printVecInfo(replicate100[1,]) #calling function for row with values of Blue
printVecInfo(replicate100[2,]) #calling function for row with values of Red
hist(replicate100)

#Step 3
temp_airquality <- airquality #copy the dataframe into a temporary one to be manipulated
temp_airquality #print the dataframe

without_na <- na.omit(temp_airquality) #omitting the NA values in dataframe
without_na #print the dataframe without NA

ozone <- printVecInfo(without_na[,1]) #calling function for the column Ozone
hist(without_na[,1]) #generating the histogram for column Ozone

wind <- printVecInfo(without_na[,3]) #calling function for the column Wind
hist(without_na[,3]) #generating the histogram for column Wind

temp <- printVecInfo(without_na[,4]) #calling function for the column Temp
hist(without_na[,4]) #generating the histogram for column Temp
