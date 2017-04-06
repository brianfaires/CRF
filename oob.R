# Prediction Accuracy using RF

# Config values
trainCSV = "train1.csv"
testCSV = "test1.csv"
nFeatures = 1026
nTrees = 20001
seed = 100

# Init
strt = Sys.time()
set.seed(seed)
library(randomForest)
library(party)

# Load data
trainData = read.csv(trainCSV, sep=",", header=TRUE)
testData = read.csv(testCSV, sep=",", header=TRUE)
featFileName = paste("f", nFeatures, ".csv", sep="")
features = as.character(read.csv(featFileName, header=FALSE)$V1)

trainLabels = trainData$V1
testLabels = testData$V1

reducedTrainData = data.frame(trainData[, features])
reducedTestData = data.frame(testData[, features])

rf = randomForest(trainLabels~., data=reducedTrainData)
table(testLabels, predict(rf, newdata=reducedTestData, OOB=TRUE))

print(Sys.time()-strt)