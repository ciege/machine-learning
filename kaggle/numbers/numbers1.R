source("/numbers/adjNumVector.R")
library(sda)
library(randomForest)
library(FNN)
library(kknn)
train <- read.csv("/Users/u78671/Downloads/train.csv", header=TRUE)
test <- read.csv("/Users/u78671/Downloads/test.csv", header=TRUE)


nameV <- colnames( test )
nameV <- nameV[1:784]

testP <- matrix(0, 0,288) 
colnames(testP) <- nameV[1:288]

trainP <- matrix(0, 0,288) 
colnames(trainP) <- nameV[1:288]

labels <- as.character(train[,1])
train <- train[,-1]

rowP <-  rep(0, 288) 

testM <- as.matrix(test)
for(i in 1:nrow(testM)){
  rowP <- createMatrix(testM[i,])
  testP <-  rbind(testP,as.vector(rowP))
}

trainM <- as.matrix(train)
for(i in 1:nrow(trainM)){
  rowP <- createMatrix(trainM[i,])
  trainP <-  rbind(trainP,as.vector(rowP))
}
testDF <- data.frame(testP)
trainDF <- data.frame(trainP)
labelsF <- as.factor(labels)
model <- kknn(labels ~ ., trainDF, testDF, k=9, kernel="triangular")$call
results <- model$fitted.values

# save the output as column vector
write(as.numeric(levels(results))[results], file="kknn_submission.csv", ncolumns=1)
