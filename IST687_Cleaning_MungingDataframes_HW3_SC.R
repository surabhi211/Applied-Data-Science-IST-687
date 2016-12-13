#Surabhi Chouhan

#IST687 Cleaning/munging Dataframes

#Step 1

#Storing the csv location into a variablecsv_loc <- "http://www.census.gov/popest/data/state/totals/2011/tables/NST-EST2011-01.csv"

readStates <- function(csv) #defining the function readStates with one argument{   return (read.csv(csv_loc)) #this function reads and returns the csv file}
  mycsv <- readStates(csv_loc) #function calling to read the given csv file#
  
  mycsv #display the fetched csv file
  #Step 2
  
  #making a dataframe for temporary use, to delete rows and columns, and to rename the columns
  
  population <- data.frame(mycsv)
  
  new_population <- population[-c(1,2,3,4,5,6,7,8,60,61,62,63,64,65,66),-c(6,7,8,9,10)] #deleting the rows which are not needed i.e. the one which are not states of                                                                                                                                          #USA andPuerto Rico                                                                                                                                                                                                                        #deleting the columns which contain NA value
  
  #Naming the data frame as given in question
  
  names(new_population) <- c("stateName","Jul2010","Jul2011","base2010","base2011")
  
  colnames(new_population) <- names(new_population)
  #Removing the comma from numbers in the population columns to enable them to be converted into numeric in R
  
  new_population$Jul2010 <- as.numeric((gsub(",", "",new_population$Jul2010)))
  
  new_population$Jul2011 <- as.numeric((gsub(",", "", new_population$Jul2011)))
  
  new_population$base2010 <- as.numeric((gsub(",", "", new_population$base2010)))
  
  new_population$base2011 <- as.numeric((gsub(",", "", new_population$base2011)))
  
  
  
  #Step 3
  #Making the data frame as asked in the question
  
  dfStates <- data.frame(new_population)
  
  mean(dfStates$Jul2011) #Mean gives the value as required
  
  
  
  #Step 4
  
  max(dfStates$Jul2011) #Max value
  
  which.max(dfStates$Jul2011) #gives which row has max value
  
  dfStates[5,] #gives the complete row with max value
  
  dfStates[order(dfStates$Jul2011, decreasing=FALSE),] #sorting the data frame in increasing order according to Jul2011
  
  #Step 5
  #creating the function percentage
  
  percentage <- function(vec,num) #2 parameters vector and number
    
  { 
    
    flag <- 0 #a temporary variable to count the number of times loop is executed 
    
    l <- length(vec) #stores the length of vector  for(i in l) #loop is executed till the length of vector   
    
    {     if(vec[i] < num) {flag <- flag+1} #if vector value is less than the number then flag is incremented by 1   
      
    } 
    
    return ((flag/length(vec))*100) #at the end of loop formula is used to calculate the value of percentage of values less than the number
    
  }
  
  #calling the function as in the question
  
  percentage(dfStates$Jul2011,37253956) 
  
  percentage(mean(dfStates$Jul2011),5000000)