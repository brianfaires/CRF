# Randomly partitions ABIDE data into 5 sets of (roughly) equal size, keeping balanced numbers of ASD and TD in each

# Config
seed = 100
inputFileName = "discrete.252.2per.csv"

# Init
set.seed(seed)

# Load data
allData = read.csv(inputFileName, header=TRUE)
labels = allData$V1

# Get indexes and randomize the order
asd_idx = sample(which(labels=="ASD"))
td_idx = sample(which(labels=="TD"))

g1_idx = c(asd_idx[1:25], td_idx[1:25])
g2_idx = c(asd_idx[26:50], td_idx[26:50])
g3_idx = c(asd_idx[51:75], td_idx[51:75])
g4_idx = c(asd_idx[76:100], td_idx[76:100])
g5_idx = c(asd_idx[101:126], td_idx[101:126])

# Will run CRF dim reduct on each training set
train1 = allData[-g1_idx,]
train2 = allData[-g2_idx,]
train3 = allData[-g3_idx,]
train4 = allData[-g4_idx,]
train5 = allData[-g5_idx,]

# Will run RF classification on each testing set
test1 = allData[g1_idx,]
test2 = allData[g2_idx,]
test3 = allData[g3_idx,]
test4 = allData[g4_idx,]
test5 = allData[g5_idx,]

# Write files
write.table(train1, "train1.csv", sep=",", row.names=FALSE)
write.table(train2, "train2.csv", sep=",", row.names=FALSE)
write.table(train3, "train3.csv", sep=",", row.names=FALSE)
write.table(train4, "train4.csv", sep=",", row.names=FALSE)
write.table(train5, "train5.csv", sep=",", row.names=FALSE)

write.table(test1, "test1.csv", sep=",", row.names=FALSE)
write.table(test2, "test2.csv", sep=",", row.names=FALSE)
write.table(test3, "test3.csv", sep=",", row.names=FALSE)
write.table(test4, "test4.csv", sep=",", row.names=FALSE)
write.table(test5, "test5.csv", sep=",", row.names=FALSE)
