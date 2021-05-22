#importing the dataset

dataset_original = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE)

#clean the reviews
#install.packages('tm')
library(tm)
corpus = VCorpus(VectorSource(dataset$Review))
corpus = tm_map(corpus, content_transformer(tolower))
#as.character((corpus[[1]]))
corpus = tm_map(corpus, removeNumbers)
#as.character((corpus[[841]]))
corpus = tm_map(corpus, removePunctuation)
#as.character((corpus[[1]]))
#install.packages('SnowballC')
library(SnowballC)
corpus = tm_map(corpus, removeWords, stopwords())
#as.character((corpus[[1]]))
corpus = tm_map(corpus, stemDocument)
#as.character((corpus[[1]]))
corpus = tm_map(corpus, stripWhitespace)

#creating bag of words
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm,0.999)
dtm
dataset = as.data.frame(as.matrix(dtm))
dataset$liked = dataset_original$Liked
# Encoding the target feature as factor
dataset$Liked = factor(dataset$liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')

library(caTools)
set.seed(123)
split = sample.split(dataset$liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)
cm