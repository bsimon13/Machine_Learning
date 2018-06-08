#read in the data
df<- read.csv("H://Coursera/Machine Learning/pml-training.csv", stringsAsFactors = TRUE)
#remove summary columns / missing values
df<- df[,c(1:11,37:49,60:68,84:86, 100:160)]
df<- df[,c(1:36,39,50:61,77,88:97)]

#remove Metadata Columns (times, Identifyers etc)

df<- df[,8:60]

#remove total columns as they can be established from others


library(caret)

inTrain <- createDataPartition(df$classe, p=0.7, list = FALSE)
training <- df[inTrain,]
testing <- df[-inTrain,]
rm(inTrain)

library(nnet)
library(rpart)
fit1<- multinom(classe~., data = training) # ~ 65% Accuracy (In sample)
fit2<- train(classe~., data = training, method = rpart) # around 50% accuracy (in sample)

library(randomForest)
fit3<- randomForest(classe~., data = training) #nearly 100% accurate! (out of sample!)

#build predictions for testing

pred1<- predict(fit1, newdata = training)
pred2<- predict(fit2, newdata = training)
pred3<- predict(fit3, newdata = training)

confusionMatrix(pred1, factor(training$classe))
confusionMatrix(pred2, factor(training$classe))
confusionMatrix(pred3, factor(training$classe))


