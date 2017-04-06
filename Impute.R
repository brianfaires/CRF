# Wrapper for missForest, which uses RF to impute missing values (for incomplete behavioral data)

# Config
inputFileName = "Combined_RawData.csv"
outputFileName = "Combined_ImputedData.csv"

library(missForest)

allData = read.csv(inputFileName)

# impute missing values, using all parameters as default values
imputed <- missForest(allData)

# output values and error
imputed$ximp
imputed$OOBerror

write.csv(imputed$ximp, outputFileName)