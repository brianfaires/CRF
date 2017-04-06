# Non-first Dimension Reduction of data; Loads a list of all reduced features and samples them into groups to run in parallel

# Config values
trainCSV = "train1.csv"
nGroups = 59
nFeatPerGroup = 121
nFeatRetained = 121

# Rarely changed
nTrees = 2001
seed = 100
nProcessors = 16


# Init
startTime = Sys.time()
set.seed(seed)
library(foreach)
library(doParallel)

# Load list of features
featFileName = paste("f", nFeatPerGroup*nGroups, ".csv", sep="")
features = as.character(read.csv(featFileName, header=FALSE)$V1)
trainData = read.csv(trainCSV, header=TRUE)
trainLabels <- trainData$V1
featureSets = matrix(sample(features), nrow=nGroups, ncol=nFeatPerGroup)


# Setup parallel backend
clust<-makeCluster(nProcessors, outfile="")
registerDoParallel(clust)

foreach(i = 1:nGroups) %dopar% {
	reducedTrainData = data.frame(trainData[, featureSets[i,]])
	library(party)
	set.seed(seed)
  
	crf = cforest(trainLabels~., data=reducedTrainData, controls=cforest_unbiased(ntree=nTrees))
	mda = varimp(crf, conditional=TRUE, OOB=TRUE)
	sorted = sort(mda, decreasing=TRUE)
	sorted[c(1:nFeatRetained)] # parallel print to output file
}

stopCluster(clust)
print(Sys.time() - startTime)
