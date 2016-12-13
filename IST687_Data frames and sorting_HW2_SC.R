#Surabhi Chouhan
#IST687 - Homework #2 - Data frames and sorting

#Task 1
my_mtcars <- mtcars  # Copy original dataframe into a new one
my_mtcars$disppercyl <- my_mtcars$disp/my_mtcars$cyl #Adding a new column disppercyl to store the value of "engine displacement per cylinder"
my_mtcars
summary(my_mtcars) #Get the summary of my_mtcars with the new column dispercyl added
                   #Output for the last column is exactly same as the one given in question


#Task 2
pets <- c(0,1,1,1,2) #Creating a vector "pets" for the number of pets, 5 of my friends own
order <- c(2,2,1,2,2) #Creating a vector "order" for their birth order in the family
siblings <- c(1,1,0,2,1) #Creating a vector "siblings" for number of siblings each of them have
user_id <- c("Ups", "Pal", "Alk", "Sum", "Sur") #Creating a vector "user_id" to assign id to each row of the data frame, unique id for each of my friend

myFriends <- data.frame(user_id, pets, order, siblings) #Combining the vectors just created into a data frame "myFriends"
myFriends #Displaying myFriends to check how it looks like 
str(myFriends) #str function gives the structure of the data frame with the number of rows and columns as well as the data-type of each column ans the values present in it
summary(myFriends) #summary gives the minimum value in the column, median, mean and maximum value in the column
#Getting the values from each column
myFriends$user_id
myFriends$pets 
myFriends$order
myFriends$siblings
