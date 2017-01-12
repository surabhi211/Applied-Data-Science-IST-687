#Surabhi Chouhan
#IST 687- Text Mining Homework

#Step 1: Read in the positive and negative word files 

install.packages("tm")
library(tm)
install.packages("wordcloud")
library(wordcloud)

pos<-read.delim("D:/Syracuse_University/ist687/data_files/positive.txt")

neg<-read.delim("D:/Syracuse_University/ist687/data_files/negative.txt")


#Clean the dataset

pos <- pos[-1:-33,]
head(pos,33)
str(pos)
names(pos) <- c("Positive")
View(pos)
head(pos)

neg <- neg[-1:-33,]
head(neg,33)
str(neg)
names(neg) <- c("Negative")
View(neg)
head(neg)



#Step 2: Process in the MLK speech

mlk <- read.delim("D:/Syracuse_University/ist687/data_files/MLK-Speech.txt")

str(mlk)
tail(mlk)

#Create a term matrix

words.vec <- VectorSource(mlk)
words.corpus <- Corpus(words.vec)
words.corpus

tdm <- TermDocumentMatrix(words.corpus)


#	Create a list of counts for each word

m <- as.matrix(tdm)
wordCounts <- rowSums(m)
head(wordCounts)

words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

inspect(tdm)
m = as.matrix(tdm)
wordCounts = rowSums(m)


wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)
str(wordCounts)


total <- sum(wordCounts)
words <- names(wordCounts)
str(words)


#Step 3: Determine how many positive words were in the speech

countPos <- match(words,pos,nomatch=0)
countPos
matchCounts <- wordCounts[which(countPos	!=	0)]
length(matchCounts) #42 positive words

#Step 4: Determine how many negative words were in the speech

countNeg <- match(words,neg,nomatch=0)
countNeg
matchCounts <- wordCounts[which(countNeg	!=	0)]
length(matchCounts) #46 negative words





#Step 5: Redo the 'positive' and 'negative' calculations for each 25% of the speech

#dividing speech into 4 equal parts
newdataframes = split(words, sample(rep(1:2:3:4, 459)))

#now matching to get the positive words
#first 25 percent
matchedpos1 = match(newdataframes$`1`,pos,nomatch = 0)

#count of match words
matchcountspos1 = wordCounts[which(matchedpos1	!=	0)]
pos1 = length(matchcountspos1)


#second 25 percent
matchedpos2 = match(newdataframes$`2`,pos,nomatch = 0)

#count of match words
matchcountspos2 = wordCounts[which(matchedpos2	!=	0)]
pos2 = length(matchcountspos2)

#third 25 percent
matchedpos3 = match(newdataframes$`3`,pos,nomatch = 0)

#count of match words
matchcountspos3 = wordCounts[which(matchedpos3	!=	0)]
pos3 = length(matchcountspos3)

#fourth 25 percent
matchedpos4 = match(newdataframes$`4`,pos,nomatch = 0)

#count of match words
matchcountspos4 = wordCounts[which(matchedpos4	!=	0)]
pos4 = length(matchcountspos4)

#barchart for comparing the result
h = c(pos1,pos2,pos3,pos4)
barplot(h)
#Barplot clearly shows that the third part has got the most positive words

#similarly for the negative words 

#first 25 percent
matchedneg1 = match(newdataframes$`1`,neg,nomatch = 0)

#count of match words
matchcountsneg1 = wordCounts[which(matchedneg1	!=	0)]
neg1 = length(matchcountsneg1)

#second 25 percent
matchedneg2 = match(newdataframes$`2`,neg,nomatch = 0)

#count of match words
matchcountsneg2 = wordCounts[which(matchedneg2	!=	0)]
neg2 = length(matchcountsneg2)

#third 25 percent
matchedneg3 = match(newdataframes$`3`,neg,nomatch = 0)

#count of match words
matchcountsneg3 = wordCounts[which(matchedneg3	!=	0)]
neg3 = length(matchcountsneg3)

#fourth 25 percent
matchedneg4 = match(newdataframes$`4`,neg,nomatch = 0)

#count of match words
matchcountsneg4 = wordCounts[which(matchedneg4	!=	0)]
neg4 = length(matchcountsneg4)

#barchart for seeing the result
J = c(neg1,neg2,neg3,neg4)
barplot(J)
#Barplot shows that second part has got the most negative words 