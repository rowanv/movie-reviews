library(caret)
library(caTools)
library(tm)
library(SnowballC)
library(Metrics)
library(rpart)
library(rpart.plot)
library(qdap) #using polarity to approximate the seintment of text

file.dir = '/Users/rowanvasquez/Documents/Data\ Science/Kaggle/Movie\ Reviews/'

#Read in data

dfTrain <- as.data.frame(read.delim(paste0(file.dir, 'train.tsv'), header = TRUE, sep = '\t', stringsAsFactors = FALSE))

dfTest <- as.data.frame(read.delim(paste0(file.dir, 'test.tsv'), header = TRUE, sep = '\t', stringsAsFactors = FALSE))

save(dfTrain, file = paste0(file.dir,'MovieReviews_dfTrain.Rda'))
save(dfTest, file = paste0(file.dir, 'MovieReviews_dfTest.Rda'))

dfTrain.save.file <-paste0(file.dir,'MovieReviews_dfTrain.Rda')
dfTest.save.file <- paste0(file.dir, 'MovieReviews_dfTest.Rda')

load(dfTrain.save.file)
load(dfTest.save.file)

table(dfTrain$Sentiment)

#####################
#Adding a polarity score

dfTrain$Polarity <- counts(polarity(strip(dfTrain$Phrase[1:30])))[,'polarity']



#######################
#Creating the Corpus
######################

dfTrain.corpus <- Corpus(VectorSource(dfTrain$Phrase))
dfTrain.corpus[[1]]

#convert to lowercase
dfTrain.corpus <- tm_map(dfTrain.corpus, content_transformer(tolower)) #have to use content_transformer because tolower is not a canonical transformation -- see getTransformations()

#remove punctuation
dfTrain.corpus <- tm_map(dfTrain.corpus, removePunctuation)

#remove stop words
dfTrain.corpus <- tm_map(dfTrain.corpus, removeWords, stopwords('english'))

#stem words
dfTrain.corpus <- tm_map(dfTrain.corpus, stemDocument)
dfTrain.corpus[[1]]



###############
#Bag of Words
###############

dfTrain.frequencies <- DocumentTermMatrix(dfTrain.corpus)
dfTrain.corpus

dfTrain.sparse <- dfTrain.frequencies

#dfTrain.sparse <- removeSparseTerms(dfTrain.frequencies, 0.2)
#sparsity is 100%

dfTrain.sparse <- as.data.frame(as.matrix(dfTrain.sparse))

#make column names appropriate for R
colnames(dfTrain.sparse) <- make.names(colnames(dfTrain.sparse))


#Add DV
dfTrain.sparse$Sentiment <- dfTrain$Sentiment
str(dfTrain.sparse)


#add the Polarity score
dfTrain.sparse$Polarity <- dfTrain$Polarity

dfTrain.sparse.save.file <- paste0(file.dir, 'MovieReviews_dfTrain_sparse.Rda')
save(dfTrain.sparse, file = dfTrain.sparse.save.file)




######################
#Splitting the dataset
######################

dfTrain.sparse.split <- sample.split(dfTrain$Sentiment, SplitRatio = 0.7)
df.train <- subset(dfTrain.sparse, dfTrain.sparse.split == TRUE)
df.test <- subset(dfTrain.sparse, dfTrain.sparse.split == FALSE)

##################
#Models
#################

#Baseline Model
#Everything neutral

baseline_pred <- rep(2, each = length(df.test))

baseline.accur <- table(df.test$Sentiment)[3] / length(df.test$Sentiment) 
baseline.accur

#Using just the Polarity score
polarity_pred <- rep(2, each = length(df.test))


#CART model
cart1 <- rpart(Sentiment~., data = df.train)
prp(cart1)

pred.cart1 <- predict(cart1, newdata = df.test)

confus.cart1 <- confusionMatrix(pred.cart1, df.test$Sentiment)
confus.cart1

accur.cart1.improv <- confus.cart1$overall[1] - baseline.accur
accur.cart1.improv




