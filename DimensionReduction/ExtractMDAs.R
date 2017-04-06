# Parses output of DR#.R, extracts features and MDAs, sorts them and writes them

# Config
inputFileName = "DR6.o4254"
outputFileName = paste(inputFileName, ".MDAs.csv", sep="")

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
values = values[values != ""]

# Sort and write data
output = data.frame(names, values)
output[order(output$values, decreasing=TRUE), ]
write.table(output, outputFileName, sep=",", row.names=FALSE, col.names=FALSE)
