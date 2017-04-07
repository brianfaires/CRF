# Sample 100 random features from each modality, use them to compute OOB and MDAs

# Config
maxOOBToLogResults = 0.4
nIterations = 10000
nFeaturesPerModality = 100
fcFileName = "MMC_fc.txt"
dtiFileName = "FINAL_DTI.csv"
anatFileName = "FINAL_ANAT_v2.csv"
outputDir = "output"
roundDigits = 5

# Rarely changed
ntree=2001
seed=100
nFC = 24090
nDTI = 192
nAnat = 397
nSubj = 93
skipColsFC = 1
skipColsDTI = 2
skipColsAnat = 2
nProcessors = 16

# Init
library(foreach)
library(doParallel)
dir.create(outputDir, showWarnings=FALSE)
strt = Sys.time()

# Load input files
allFC =   read.csv(fcFileName, sep=" ", header=FALSE)
allDTI =  read.csv(dtiFileName, sep=",", header=TRUE)
allAnat = read.csv(anatFileName, sep=",", header=TRUE)
labels = allFC$V1

# setup parallel backend
cl<-makeCluster(nProcessors, outfile="")
registerDoParallel(cl)

foreach(i=1:nIterations) %dopar% {
  set.seed(seed+i)
  library(party)
  
  # 100 from FC; from 2-24091
  # 100 from DTI;from 3-194
  # 100 from anat;from 3-399

  # pull 100 from each modality into data
  indexesFC = sample(skipColsFC + (1:nFC), nFeaturesPerModality, replace=FALSE)
  dataFC = allFC[, indexesFC]

  indexesDTI = sample(skipColsDTI + (1:nDTI), nFeaturesPerModality, replace=FALSE)
  dataDTI = allDTI[, indexesDTI]

  indexesAnat = sample(skipColsAnat + (1:nAnat), nFeaturesPerModality, replace=FALSE)
  dataAnat = allAnat[, indexesAnat]

  sampledData = data.frame(dataFC, dataDTI, dataAnat, labels)

  crf = cforest(labels~., data=sampledData, controls=cforest_unbiased(ntree = ntree))
  conv = table(labels, predict(crf, OOB=TRUE))
  
  # Log OOB
  oobRate = round((conv[1,2] + conv[2,1]) / nSubj, roundDigits)
  if(oobRate <= maxOOBToLogResults) {
    sens = round(conv[1,1] / (conv[1,1] + conv[1,2]), roundDigits)
    spec = round(conv[2,2] / (conv[2,1] + conv[2,2]), roundDigits)
    oobOutput = paste("OOB,", oobRate, "Sensitivity,", sens, "Specificity,", spec, "\n", sep="")
    
    # Get MDAs
    MDA = varimp(crf, conditional=TRUE, OOB=TRUE)
    sorted = sort(MDA, decreasing=TRUE)
    sorted = append(sorted, c(oobRate, sens, spec), after = 0)
    
    names(sorted)[1:3] = c("OOB error rate", "Sensitivity", "Specificity")
    
    outFileName = paste(outputDir, "//seed", seed+i, ".csv", sep="")
    write.table(sorted, outFileName, sep=",", col.names=FALSE)
  }
}

print(Sys.time()-strt)
stopCluster(cl)
