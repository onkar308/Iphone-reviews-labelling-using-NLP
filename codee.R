#linguipediaNLP
train = read.csv("train.csv")
test = read.csv("test.csv")
test$label <- NA
combi= rbind(train,test)
submission = read.csv("sample_submission.csv")
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(combi$tweet))
corpus
#preProcessingDATA
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,c("iphone",stopwords("english")))
corpus = tm_map(corpus,stemDocument)
frequencies = DocumentTermMatrix(corpus)
frequencies
bar = findFreqTerms(frequencies,lowfreq = 25)
library(ggplot2)
ggplot(bar) + geom_bar(aes(x=bar,y=count),col = rainbow())
#sparse the matrix
sparse = removeSparseTerms(frequencies,0.9991)
sparse
#lets Use this sparse matrix to predict model
tweetSparse = as.data.frame(as.matrix(sparse))
colnames(tweetSparse) = make.names(colnames(tweetSparse))
train1 <- tweetSparse[1:7920,]
test1 <- tweetSparse[7921:9873,]
library(randomForest)
set.seed(923)
train$label = as.factor(train$label)
tweetrf = randomForest(train$label~.,data = train1,ntree=40)
predictrf = predict(tweetrf,newdata = test1)
table(predictrf)
submission$label = predictrf
write.csv(submission,"my_sub11.csv",row.names = F)