#Surabhi Chouhan
#IST687 - Intro Lab Assignment

#Vectors to be used in the problem
height <- c(59,60,61,58,67,72,70)
weight <- c(150,140,180,220,160,140,130)
a <- 150 #a variable defined to be used later

#Average height
avg_height <- mean(height) #store the average into a variable called avg_height
avg_height #print the value of avg_height

#Average_weight
avg_weight <- mean(weight) #store the average into a variable called avg_weight
print(avg_weight) ##print the value of avg_weight

#Length of height and weight
length_height <- length(height) #find the length of the vector height
length_height #printing the length of height
length_weight <- length(weight) #find the length of the vector weight
length_weight #printing the length of weight

#Sum of heights
sum_height <- sum(height) #Calculating the sum of all elements in height
sum_height #Printing the sum

#Average height by dividing
avg_height_new <- sum_height/length_height #Calculate the average height 
avg_height_new #print the the avg_height_new
#Both average heights are equal

#Calculating max height and weight
maxH <- max(height) #Storing the maximum value of height in maxH
maxW <- max(weight) #Storing the maximum value of weight in maxW

#Calculating min height and weight
minH <- min(height) #Storing the minimum value of height in minH
minW <- min(weight) #Storing the minimum value of height in minW

#New weight vector 
new_weight <- c(weight+5) #new vector created by adding 5 to each element of weight vector
poundsperinch <- c(new_weight/height) #calculating the pounds/inch for each person using the new_weight vector

#Conditional if statements
if (maxH>60){print('yes')} else print('no') #checking the condition if maxH is greater than 60 or not, and prints "yes" if condition is satisfied else prints "no"
if (minW>a){print('yes')} else print('no') #checking the condition if maxW is greater than the variable "a" or not, and prints "yes" if condition is satisfied else prints "no"




