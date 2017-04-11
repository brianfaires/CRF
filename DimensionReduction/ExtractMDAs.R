# Parses output of DR#.R, extracts features and MDAs, sorts them and writes them

# Config
inputFileName = "DR2.set3.o4341"

# Load input file
allLines = readLines(inputFileName)
nLines = length(allLines)

names = vector()
values = vector()

# Search for [[1]], [[2]], etc, which is followed by several line pairs of data, followed by an empty line
for(i in 1:nLines) {
  if(regexpr("\\[\\[\\d*\\]\\]", allLines[i]) > 0) {
    # Found "[[#]]"
    i = i+1
    
    while(allLines[i] != "") {
      names <- c(names, strsplit(allLines[i], "\\s+")[[1]])
      values <- c(values, strsplit(allLines[i+1], "\\s+")[[1]])
      i = i+2
    }
  }
}

# Remove empty strings that show up from uneven lines
names = names[names != ""]
values = as.numeric(values[values != ""])

names = names[values >= 0]
values = values[values >= 0]

# Sort and write data
output = data.frame(names, values)
sortedOutput = output[order(output$values, decreasing=TRUE), ]

outputFileName = paste("f", nrow(sortedOutput), ".csv", sep="")
write.table(sortedOutput, outputFileName, sep=",", row.names=FALSE, col.names=FALSE)
