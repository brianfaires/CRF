# Config
nInitialFeatures = 1026
nTreesCRF = 2001
nTreesRF = 20001
seed = 100
percentToKeep = 0.8
roundDigits = 5

# Init
tStart = Sys.time()
set.seed(seed)
library(party)
library(randomForest)
library(foreach)

# Load data
featFileName = paste("f", nInitialFeatures, ".csv", sep="")
features = as.character(read.csv(featFileName, sep=",", header=FALSE)$V1)
trainData = read.csv("train1.csv", sep=",", header=TRUE)
testData = read.csv("test1.csv", sep=",", header=TRUE)

trainLabels = trainData$V1
testLabels = testData$V1
nTrainSubj = nrow(trainData)
nTestSubj = nrow(testData)

# Prepare for DR
oob = matrix(nrow=0, ncol=7)
dimnames(oob) = list(NULL, c("nFeatures", "CRF OOB%", "CRF Sens", "CRF Spec", "RF OOB%", "RF Sens", "RF Spec"))
dir.create("DRoutput", showWarnings=FALSE)

# Do DR
while(length(curFeatures) > 1) {
  reducedTrainData = data.frame(trainData[,features])
  crf = cforest(trainLabels~., data=reducedTrainData, controls=cforest_unbiased(ntree=nTreesCRF))
  convCRF = table(trainLabels, predict(crf, OOB=TRUE))
  
  # Create an RF model for testing data
  rf = randomforest(trainLabels~., data=reducedTrainData, ntree=nTreesRF)
  convRF = table(testLabels, predict(rf, newdata=reducedTestData))
  
  # Log and write OOB/Sens/Spec
  oobRF = round((convRF[1,2] + convRF[2,1]) / nTestSubj, roundDigits)
  sensRF = round(convRF[1,1] / (convRF[1,1] + convRF[1,2]), roundDigits)
  specRF = round(convRF[2,2] / (convRF[2,1] + convRF[2,2]), roundDigits)
  
  oobCRF = round((convCRF[1,2] + convCRF[2,1]) / nTrainSubj, roundDigits)
  sensCRF = round(convCRF[1,1] / (convCRF[1,1] + convCRF[1,2]), roundDigits)
  specCRF = round(convCRF[2,2] / (convCRF[2,1] + convCRF[2,2]), roundDigits)
  
  oob = rbind(oob, c(length(features), oobCRF, sensCRF, specCRF, oobRF, sensRF, specRF))
  write.table(oob, "DRoutput//oob.csv", sep=",", row.names=FALSE)
  
  # Get, sort, and write MDAs from CRF model
  MDA = varimp(crf, conditional=TRUE, OOB=TRUE)
  sortedMDA = sort(MDA, decreasing=TRUE)
  outFileName = paste("DRoutput//f", length(features), ".csv", sep="")
  write.table(sortedMDA, outFileName, sep=",", col.names=FALSE)
  
  # Reduce dimensionality
  maxFeatures = round(percentToKeep * length(features), 0)
  if(length(features) == maxFeatures)
  	maxFeatures = maxFeatures - 1

  # Ensure at least all of the negative MDAs are cut
  features = features[MDA >= 0]
  if(length(features) > maxFeatures)
    features = names(sortedMDA[1:maxFeatures])
}

print(Sys.time()-tStart)
