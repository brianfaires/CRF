# First Dimension Reduction of data; Generates a list of all features and samples them into groups to run in parallel

# Config values
trainCSV = "train1.csv"
nGroups = 330
nFeatPerGroup = 73
nFeatRetained = 73

# Rarely changed
nTrees = 2001
seed = 100
nProcessors = 16


# Init
startTime = Sys.time()
set.seed(seed)
library(foreach)
library(doParallel)

# Generate list of features, load data
features = seq(2, 1 + nGroups*nFeatPerGroup, by=1) # first col is response
trainData = read.csv(trainCSV, header=TRUE)
trainLabels = trainData$V1
featureSets = matrix(sample(features), nrow=nGroups, ncol=nFeatPerGroup)

# Setup parallel backend
clust<-makeCluster(nProcessors, outfile="")
registerDoParallel(clust)

foreach(i = 1:nGroups) %dopar% {
  library(party)
  set.seed(seed)
  reducedTrainData = data.frame(trainData[, featureSets[i,]], trainLabels)
	
	crf = cforest(trainLabels~., data=reducedTrainData, controls=cforest_unbiased(ntree=nTrees))
	mda = varimp(crf, conditional=TRUE, OOB=TRUE)
	sorted = sort(mda, decreasing=TRUE)
	sorted[c(1:nFeatRetained)] # parallel print to output file
}

stopCluster(clust)
print(Sys.time() - startTime)
