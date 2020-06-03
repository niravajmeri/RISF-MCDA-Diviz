#Setup log file
if(!endsWith(getwd(), "MAIA")) {
  setwd("../")
}
setwd("./output")
log_file <- "r-logs.txt"
file.remove(log_file)
file.create(log_file)
sink(log_file)
print("Log file loaded")
setwd("../")

# Load the required packages
print("Loading RXMCDA package and MAIA data...")
library(RXMCDA)
library(XML)
library("methods")
library("kulife")
library("ggplot2")
library(viridis)



# Below is where the criteria and alternatives are defined that will be used
# It is important that this list is in the same order as the exported data
harmCriteriaList <- list("Harms of vehicle related mortality",
                         "Harms of vehicle specific damage",
                         "Harms of vehicle related damage",
                         "Harms of vehicle system encroachment on human living",
                         "Harms of vehicle related occupational injuries",
                         "Harms of vehicle related lack of status",
                         "Harms of vehicle related loss of time/productivity",
                         "Harms of vehicle related loss of social engagement",
                         "Harms of vehicle related injury to others",
                         "Harms of vehicle related economic costs",
                         "Harms of vehicle related changes to community",
                         "Harms of vehicle related crime opportunities",
                         "Harms of vehicle related economic changes")

benefitCriteriaList <- list("Benefit of promoting societal value",
                            "Benefits of minimizing negative societal impacts",
                            "Protecting the interests of users",
                            "Advancing the preservation of the environment",
                            "Maximizing the progress of science and technology",
                            "Engaging relevant communities",
                            "Ensuring oversight and accountability",
                            "Recognizing appropriate governmental and policy roles")

harmAlternativesList <- list("In status quo",
                             "Unfettered AVs",
                             "Regulated, privately owned AVs",
                             "Regulated, fleet owned AVs")

benefitAlternativesList <- list("Unfettered AVs",
                                "Regulated, privately owned AVs",
                                "Regulated, fleet owned AVs")


# List of all the files used for input to the MCDA
# Assumes you are in the MASH home directory
fileNamesList <- list("harmAlternatives.xml",
                      "benefitAlternatives.xml",
                      "harmCriteria.xml",
                      "benefitCriteria.xml",
                      "harmPerformanceTable.xml",
                      "benefitPerformanceTable.xml",
                      "harmCriteriaWeights.xml",
                      "benefitCriteriaWeights.xml",
                      "harmOutput.xml",
                      "benefitOutput.xml")

# Root node for all xml files
xmcdaNode <- newXMLNode("xmcda:XMCDA",
                        attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.1.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.1.0.xsd"),
                        suppressNamespaceWarning=TRUE,
                        namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.1.0"))

# Whether benefits, harms, or both are in the data
type <- "harm"
# Whether to use default weights or qualtrics weights
defaultWeights <- FALSE
# Minimum and maximum possible values in the data
MINIMUM <- 1
MAXIMUM <- 10

# Create the alernatives and criteria xml files
setwd("./input")

# Remove old files
if(type == "harm" || type == "both") {
  for(i in c(1, 3)) {
    file.remove(fileNamesList[[i]])
  }
}
if(type == "benefit" || type == "both") {
  for(i in c(2, 4)) {
    file.remove(fileNamesList[[i]])
  }
}

# Creating harmAlternatives.xml
if(type == "harm" || type == "both") {
  harmAltDoc = newXMLDoc(node=xmcdaNode)
  harmAltRoot = xmlRoot(harmAltDoc, skip=FALSE)
  harmAltNode = newXMLNode("alternatives", parent=harmAltRoot)
  for(alternative in harmAlternativesList) {
    harmAltIDNode = newXMLNode("alternative", attrs=c(id=alternative), parent=harmAltNode)
  }
  saveXML(harmAltDoc, file=fileNamesList[[1]])
}

# Creating benefitAlternatives.xml
if(type == "benefit" || type == "both") {
  benefitAltDoc = newXMLDoc(node=xmcdaNode)
  benefitAltRoot = xmlRoot(benefitAltDoc, skip=FALSE)
  benefitAltNode = newXMLNode("alternatives", parent=benefitAltRoot)
  for(alternative in benefitAlternativesList) {
    benefitAltIDNode = newXMLNode("alternative", attrs=c(id=alternative), parent=benefitAltNode)
  }
  saveXML(benefitAltDoc, file=fileNamesList[[2]])
}

# Creating harmCriteria.xml
if(type == "harm" || type == "both") {
  harmCritDoc = newXMLDoc(node=xmcdaNode)
  harmCritRoot = xmlRoot(harmCritDoc, skip=FALSE)
  harmCritNode = newXMLNode("criteria", parent=harmCritRoot)
  for(criteria in harmCriteriaList) {
    harmCritIDNode = newXMLNode("criterion", attrs=c(id=criteria), parent=harmCritNode)
  }
  saveXML(harmCritDoc, file=fileNamesList[[3]])
}

# Creating benefitCriteria.xml
if(type == "benefit" || type == "both") {
  benefitCritDoc = newXMLDoc(node=xmcdaNode)
  benefitCritRoot = xmlRoot(benefitCritDoc, skip=FALSE)
  benefitCritNode = newXMLNode("criteria", parent=benefitCritRoot)
  for(criteria in benefitCriteriaList) {
    benefitCritIDNode = newXMLNode("criterion", attrs=c(id=criteria), parent=benefitCritNode)
  }
  saveXML(benefitCritDoc, file=fileNamesList[[4]])
}
setwd("../")

# Convert MAIA xml file to data frame
setwd("./QualtricsData")
data <- xmlToDataFrame("MASH-Final-Criteria.xml")
# Get the area of study associated with each response
responseFields <- data[, (ncol(data) - 1)]
if(!defaultWeights) {
  weightData <- xmlToDataFrame("MASH-Final-Weights.xml")
  weightResponseFields <- weightData[, (ncol(weightData) - 1)]
}


# Cols will be the number of criteria
cols = ncol(data) - 2
# Rows will be the number of alternatives
rows = nrow(data)
setwd("../")

# Build column name lists with criteria and alternatives
if(type == "harm" || type == "both") {
  harmColumnNames <- vector("list", (length(harmCriteriaList) * length(harmAlternativesList)))
  i <- 1
  for(criteria in harmCriteriaList) {
    j <- 1
    for(alternative in harmAlternativesList) {
      harmColumnNames[[length(harmAlternativesList) * (i - 1) + j]] <- paste(criteria, "-", alternative)
      j <- j + 1
    }
    i <- i + 1
  }
}
if(type == "benefit" || type == "both") {
  benefitColumnNames <- vector("list",(length(benefitCriteriaList) * length(benefitAlternativesList)))
  i <- 1
  for(criteria in benefitCriteriaList) {
    j <- 1
    for(alternative in benefitAlternativesList) {
      benefitColumnNames[[length(benefitAlternativesList) * (i - 1) + j]] <- paste(criteria, "-", alternative)
      j <- j + 1
    }
    i <- i + 1
  }
}


# Clean the data to remove unanswered questions and non data
# Remove the metadata from the front of the data frame
dataCols <- colnames(data)
metadataSplit <- 0
for(col in dataCols) {
  metadataSplit <- metadataSplit + 1
  if(startsWith(col, "QID")) {
    break
  }
}
data <- data[,metadataSplit:cols]
cols = cols - metadataSplit + 1

# Ensure all data is numeric
data <- lapply(data, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
data <- as.data.frame(matrix(unlist(data), ncol=length(data), byrow=F))

# Creates some graphs based on the distributions of the data (currently disabled)
if(type == "harm" && FALSE) {
  colnames(data) <- harmColumnNames
  offset <- 0
  cl=rainbow(length(harmCriteriaList))
  for(alternative in harmAlternativesList) {
    criteria <- data[seq(1, length(harmColumnNames), 4)]
    dev.new(width=15, height=10)
    plot(density(as.numeric(as.character(unlist(criteria[[1]])))), 
         main="", 
         col=cl[1], 
         xlim=range(c(1, 8)),
         ylim=range(c(0,1.5)),
         bty='L')
    for(i in 2:length(harmCriteriaList)) {
      lines(density(as.numeric(as.character(unlist(criteria[[i]])))), col=cl[i], xlim=range(c(1, 4)))
    }
    title(main="Data Distributions In Status Quo Harm Survey Response")
    legend("topright", legend=unique(harmCriteriaList), col=unique(cl), cex=0.6)
    pdf("In_Status_Quo_Harm_Survey_Distributions.pdf")
    frame()
    offset <- offset + 1
  }
} else if(type == "benefit" && FALSE) {
  colnames(data) <- benefitColumnNames
  offset <- 0
  cl=rainbow(length(benefitCriteriaList))
  for(alternative in benefitAlternativesList) {
    criteria <- data[seq(3, length(benefitColumnNames), 3)]
    dev.new(width=15, height=10)
    plot(density(as.numeric(as.character(unlist(criteria[[1]])))), 
         main="", 
         col=cl[1], 
         xlim=range(c(1, 4)),
         ylim=range(c(0,1.5)),
         bty='L')
    for(i in 2:length(criteria)) {
      lines(density(as.numeric(as.character(unlist(criteria[[i]])))), col=cl[i], xlim=range(c(1, 4)))
    }
    title(main="Data Distributions R, Fleet Benefit Survey Response")
    legend("topright", legend=unique(benefitCriteriaList), col=unique(cl), cex=0.6)
    pdf("RFleet_Benefit_Survey_Distributions.pdf")
    frame()
    offset <- offset + 1
  }
}

# Assign row names to the data
rownames(data) = make.names(responseFields, unique=TRUE)

# Clean weight data and manipulate into usable format
if(!defaultWeights) {
  dataCols <- colnames(weightData)
  metadataSplit <- 0
  for(col in dataCols) {
    metadataSplit <- metadataSplit + 1
    if(startsWith(col, "QID")) {
      break
    }
  }
  weightData <- weightData[,metadataSplit:(metadataSplit + length(harmCriteriaList) + length(benefitCriteriaList) - 1)]
  weightData <- lapply(weightData, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else x
  })
  weightData <- as.data.frame(matrix(unlist(weightData), ncol=length(weightData), byrow=F))
}

# Find the mean of each row (response) and if the mean is outside the possible
# response values, throw that row out
responseMeans <- rowMeans(data)
offset <- 0
for(i in c(1:rows)) {
  if(responseMeans[i] < MINIMUM || responseMeans[i] > MAXIMUM) {
    data <- data[-c(i + offset), ]
    offset = offset - 1
  }
}
data[data < 0] <- NA
rows = nrow(data)


if(!defaultWeights) {
  #normWeightData <- lapply(numWeightData, function(x) ((x-min(x))/(max(x)-min(x))))
  #normWeightData <- do.call(cbind, normWeightData)
  weightResponseMeans <- colMeans(weightData)
  offset <- 0
  for(i in c(1:nrow(weightData))) {
    if(is.na(weightResponseMeans[i]) || weightResponseMeans[i] < 0 || weightResponseMeans[i] > 100) {
      weightData <- weightData[-c(i + offset), ]
      offset = offset - 1
    }
  }
  weightData[weightData < 0] <- NA
}


# Data is now clean, now we will split it into harms and benefits

# Find the index where the data goes from harms to benefit
if(type == "both") {
  splitIndex <- (length(harmCriteriaList) * length(harmAlternativesList))
  # This data frame will hold all values for criteria involving harm
  harmData <- data[, 1:splitIndex]
  # This data frame will hold all values for criteria involving benefit
  benefitData <- data[, (splitIndex + 1):(splitIndex + length(benefitAlternativesList))]
} else if(type == "harm") {
  harmData <- data
} else {
  benefitData <- data
}

if(!defaultWeights) {
  weightSplitIndex <- length(harmCriteriaList)
  # This data frame will hold all values for criteria involving harm
  harmWeightData <- weightResponseMeans[1:weightSplitIndex]
  # This data frame will hold all values for criteria involving benefit
  benefitWeightData <- weightResponseMeans[(weightSplitIndex + 1):(weightSplitIndex + length(benefitCriteriaList))]
}

# Set the column names
if(type == "harm" || type == "both") {
  colnames(harmData) <- harmColumnNames
}
if(type == "benefit" || type == "both") {
  colnames(benefitData) <- benefitColumnNames
}


# Calculate the mean values for each criteria
if(type == "harm" || type == "both") {
  harmCriteriaMeans <- as.list(colMeans(harmData, na.rm = TRUE))
  harmTemp <- colMeans(harmData, na.rm = TRUE)
  harmFinal <- c()
  for(i in c(1:length(harmAlternativesList))) {
    harmFinal <- rbind(harmFinal, harmTemp[c(seq(from=i, to=(length(harmTemp)-length(harmAlternativesList)+i), by=length(harmAlternativesList)))])
  }
}
if(type == "benefit" || type == "both") {
  benefitCriteriaMeans <- as.list(colMeans(benefitData, na.rm = TRUE))
  benefitTemp <- colMeans(benefitData, na.rm = TRUE)
  benefitFinal <- c()
  for(i in c(1:length(benefitAlternativesList))) {
    benefitFinal <- rbind(benefitFinal, benefitTemp[c(seq(from=i, to=(length(benefitTemp)-length(benefitAlternativesList)+i), by=length(benefitAlternativesList)))])
  }
}

# Build performance tables as data frames (these can be used with MCDA package)
if(type == "harm" || type == "both") {
  harmPerformanceTable <- as.data.frame(harmFinal, 
                                        row.names=harmAlternativesList)
  colnames(harmPerformanceTable) <- harmCriteriaList
}
if(type == "benefit" || type == "both") {
  benefitPerformanceTable <- as.data.frame(benefitFinal, 
                                           row.names=benefitAlternativesList)
  colnames(benefitPerformanceTable) <- benefitCriteriaList
}

# The mean values for each criteria have been calculated so now
# we will save them as performance table to be used by Diviz
setwd("./input")

# Creates the performance table xml file with the calculated means of both
# harms and benefits
# Main XML document with XMCDA standard defined here: https://www.decision-deck.org/xmcda/?pk_vid=d995aa9f5e38a502158209393315000f
if(type == "harm" || type == "both") {
  harmDoc = newXMLDoc(node=xmcdaNode)
  harmRoot = xmlRoot(harmDoc, skip=FALSE)
  performanceTableNode = newXMLNode("performanceTable", parent=harmRoot)
  descriptionNode = newXMLNode("description", parent=performanceTableNode)
  titleNode = newXMLNode("title", "MASH Harm Performance Table", parent=descriptionNode)
  
  for(alternative in harmAlternativesList) {
    alternativePerformanceNode = newXMLNode("alternativePerformances", parent=performanceTableNode)
    alternativeNode = newXMLNode("alternativeID", alternative, parent=alternativePerformanceNode)
    performanceNode = newXMLNode("performance", parent=alternativePerformanceNode)
    criterionIndex <- 1
    for(j in seq(from=1, to=(length(harmCriteriaList) * length(harmAlternativesList) - 1), by=length(harmAlternativesList))) {
      criterionNode = newXMLNode("criterionID", harmCriteriaList[[criterionIndex]], parent=performanceNode)
      valueNode = newXMLNode("value", parent=performanceNode)
      realNode = newXMLNode("real", harmData[1, j], parent=valueNode)
      criterionIndex <- criterionIndex + 1
    }
  }
  
  saveXML(harmDoc, file="harmPerformanceTable.xml")
}

# Main XML document with XMCDA standard outlined here: https://www.decision-deck.org/xmcda/?pk_vid=d995aa9f5e38a502158209393315000f
if(type == "benefit" || type == "both") {
  benefitDoc = newXMLDoc(node=xmcdaNode)
  benefitRoot = xmlRoot(benefitDoc, skip=FALSE)
  benefitPerformanceTableNode = newXMLNode("performanceTable", parent=benefitRoot)
  benefitDescriptionNode = newXMLNode("description", parent=benefitPerformanceTableNode)
  benefitTitleNode = newXMLNode("title", "MASH Benefit Performance Table", parent=benefitDescriptionNode)
  
  for(alternative in benefitAlternativesList) {
    benefitAlternativePerformanceNode = newXMLNode("alternativePerformances", parent=benefitPerformanceTableNode)
    benefitAlternativeNode = newXMLNode("alternativeID", alternative, parent=benefitAlternativePerformanceNode)
    benefitPerformanceNode = newXMLNode("performance", parent=benefitAlternativePerformanceNode)
    criterionIndex <- 1;
    for(j in seq(from=1, to=(length(benefitCriteriaList) * length(benefitAlternativesList) - 1), by=length(benefitAlternativesList))) {
      benefitCriterionNode = newXMLNode("criterionID", benefitCriteriaList[[criterionIndex]], parent=benefitPerformanceNode)
      benefitValueNode = newXMLNode("value", parent=benefitPerformanceNode)
      benefitRealNode = newXMLNode("real", benefitData[1, j], parent=benefitValueNode)
      criterionIndex <- criterionIndex + 1
    }
  }
  
  saveXML(benefitDoc, file="benefitPerformanceTable.xml")
}
setwd("../")


# Loads all the files required for MCDA
print("Loading files...")
tryCatch( {
  if(type == "harm" || type == "both") {
    harmAlternativesFile <- fileNamesList[[1]]
    harmCriteriaFile <- fileNamesList[[3]]
    harmPerformanceTableFile <- fileNamesList[[5]]
    harmCriteriaWeightsFile <- fileNamesList[[7]]
    harmOutputFile <- fileNamesList[[9]]
    print("Harm files names assigned")
  }
  
  if(type == "benefit" || type == "both") {
    benefitAlternativesFile <- fileNamesList[[2]]
    benefitCriteriaFile <- fileNamesList[[4]]
    benefitPerformanceTableFile <- fileNamesList[[6]]
    benefitCriteriaWeightsFile <- fileNamesList[[8]]
    benefitOutputFile <- fileNamesList[[10]]
    print("Benefit files names assigned")
  }
}, warning = function(war) {
  # warning handler picks up where error was generated
  print(paste("FILE NAME ASSIGNMENT WARNING:  ", war))
}, error = function(err) {
  # error handler picks up where error was generated
  print(paste("FILE NAME ASSIGNMENT ERROR:  ", err))
}, finally = {
  print("Finished loading files.")
})


# Load the input files
# See the documentation of the XML package for R
# for further details on the xmlParse function
print("Parsing xml from files...")
setwd("./input")
tryCatch({
  if(type == "harm" || type == "both") {
    treeHarmAlternatives<-xmlTreeParse(harmAlternativesFile,useInternalNodes=TRUE)
    treeHarmCriteria<-xmlTreeParse(harmCriteriaFile,useInternalNodes=TRUE)
    treeHarmPerformanceTable<-xmlTreeParse(harmPerformanceTableFile,useInternalNodes=TRUE)
    treeHarmCriteriaWeights<-xmlTreeParse(harmCriteriaWeightsFile,useInternalNodes=TRUE)
    print("Harm files parsed")
  }
  if(type == "benefit" || type == "both") {
    treeBenefitAlternatives<-xmlTreeParse(benefitAlternativesFile,useInternalNodes=TRUE)
    treeBenefitCriteria<-xmlTreeParse(benefitCriteriaFile,useInternalNodes=TRUE)
    treeBenefitPerformanceTable<-xmlTreeParse(benefitPerformanceTableFile,useInternalNodes=TRUE)
    treeBenefitCriteriaWeights<-xmlTreeParse(benefitCriteriaWeightsFile,useInternalNodes=TRUE)
    print("Benefit files parsed")
  }
}, warning = function(war) {
  # warning handler picks up where warning was generated
  print(paste("FILE PARSING WARNING:  ",war))
}, error = function(err) {
  # error handler picks up where error was generated
  print(paste("FILE PARSING ERROR:  ",err))
}, finally = {
  print("Finished parsing files.")
})
setwd("../")

# Read the MCDA data from the files
# See the documentation of the RXMCDA package for R for further
# details concerning this step, and for the description
# of the output data

print("Reading file data....")
tryCatch({
  if(type == "harm" || type == "both") {
    harmCritIDs <- getCriteriaIDs(treeHarmCriteria)[[1]]
    harmAltIDs <- getAlternativesIDs(treeHarmAlternatives)[[1]]
    harmPerfTable <- getPerformanceTables(treeHarmPerformanceTable,altIDs = harmAltIDs, critIDs = harmCritIDs)[[1]]
    harmCritWeights <- getCriteriaValues(treeHarmCriteriaWeights, harmCritIDs, mcdaConcept="weights")[[1]]
    harmCritWeights <- harmCritWeights[,2]
    print("Harm file data extracted")
  }
  if(type == "benefit" || type == "both") {
    benefitCritIDs <- getCriteriaIDs(treeBenefitCriteria)[[1]]
    benefitAltIDs <- getAlternativesIDs(treeBenefitAlternatives)[[1]]
    benefitPerfTable <- getPerformanceTables(treeBenefitPerformanceTable,altIDs = benefitAltIDs, critIDs = benefitCritIDs)[[1]]
    benefitCritWeights <- getCriteriaValues(treeBenefitCriteriaWeights, benefitCritIDs)[[1]]
    benefitCritWeights <- benefitCritWeights[,2]
    print("Benefit file data extraced")
  }
}, warning = function(war) {
  # warning handler picks up where warning was generated
  print(paste("FILE LOADING WARNING:  ",war))
}, error = function(err) {
  # error handler picks up where error was generated
  print(paste("FILE LOADING ERROR:  ",err))
}, finally = {
  print("Finished reading files.")
})

# Calculate the weighted sum of the alternatives
print("Calculating weighted sums of alternatives...")
if(type == "harm" || type == "both") {
  harmOverallValues <- c()
  for (i in 1:dim(harmPerformanceTable)[1]){
    harmOverallValues <- rbind(harmOverallValues, c(i,sum(harmPerformanceTable[i,]*t(harmCritWeights))))
  }
}
if(type == "benefit" || type == "both") {
  benefitOverallValues <- c()
  for (i in 1:dim(benefitPerformanceTable)[1]){
    benefitOverallValues <- rbind(benefitOverallValues, c(i,sum(benefitPerformanceTable[i,]*t(benefitCritWeights))))
  }
}
print("Sums calculated.")

# Calculate the standard deviation and variation for each criteria and alternative
if(type == "harm" || type == "both") {
  harmSD <- apply(harmData, 2, sd)
  harmVar <- apply(harmData, 2, var)
}
if(type == "benefit" || type == "both") {
  benefitSD <- apply(benefitData, 2, sd)
  benefitVar <- apply(benefitData, 2, var)
}

# Function to normalize values
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Write the values to output xml files
setwd("./output")
if(type == "harm" || type == "both") {
  file.remove(harmOutputFile)
  harmOutTree = newXMLDoc(node=xmcdaNode)
  putAlternativesValues(harmOutTree, harmOverallValues, rownames(harmPerformanceTable), "harmOverallValues")
  saveXML(harmOutTree, file=harmOutputFile)
  
}
if(type == "benefit" || type == "both") {
  file.remove(benefitOutputFile)
  benefitOutTree = newXMLDoc(node=xmcdaNode)
  putAlternativesValues(benefitOutTree, benefitOverallValues, rownames(benefitPerformanceTable), "benefitOverallValues")
  saveXML(benefitOutTree, file=benefitOutputFile)
}

setwd("../")

# Plotting is done below
if(type == "harm" || type == "both") {
  harmSpecie <- c()
  harmValues <- c()
  for(alternative in harmAlternativesList) {
    harmSpecie <- c(harmSpecie, rep(alternative, length(harmCriteriaList)))
    harmValues <- c(harmValues, unlist(harmPerformanceTable[alternative, ]) * (harmWeightData / 100))
  }
  harmValues <- normalize(harmValues)
  harmCondition <- unlist(rep(harmCriteriaList, length(harmAlternativesList)), use.names=FALSE)
  harmDataForPlot <- data.frame(harmSpecie, harmCondition, harmValues)
  ggplotly(ggplot(harmDataForPlot, aes(fill=harmCondition, y=harmValues, x=harmSpecie)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, option="magma") +
    ggtitle("Harm Analysis of Different AV Public Policy Options") +
    theme_ipsum() +
    xlab("Alternative") +
    ylab("Mean sum of criteria values") +
    labs(fill="Criteria"))
  
#  statusQuoName <- c()
#  for(criteria in harmCriteriaList) {
#    statusQuoName <- c(statusQuoName, rep(criteria, nrow(harmData)))
#  }
#  statusQuoValue <- c()
#  for(i in c(1:length(harmCriteriaList))) {
#    statusQuoValue <- c(statusQuoValue, harmData[,i])
#  }
#  statusQuoHarmData <- data.frame(
#    name=statusQuoName,
#    value=statusQuoValue)
  
#  ggplotly(ggplot(statusQuoHarmData, aes(x=name, y=value, fill=name)) +
#    geom_boxplot() +
#    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
#    geom_jitter(color="black", size=0.4, alpha=0.9) +
#    theme_ipsum() +
#    theme(
#      legend.position="none",
#      plot.title = element_text(size=11),
#      axis.text.x = element_text(angle = 45, hjust = 1)
#    ) +
#    ggtitle("Status Quo criteria analysis") +
#    xlab("Criteria"))
}

if(type == "benefit" || type == "both") {
  benefitSpecie <- c()
  benefitValues <- c()
  for(alternative in benefitAlternativesList) {
    benefitSpecie <- c(benefitSpecie, rep(alternative, length(benefitCriteriaList)))
    benefitValues <- c(benefitValues, unlist(benefitPerformanceTable[alternative, ] * t(benefitCritWeights)))
  }
  benefitValues <- normalize(benefitValues)
  benefitCondition <- unlist(rep(benefitCriteriaList, length(benefitAlternativesList)), use.names=FALSE)
  benefitDataForPlot <- data.frame(benefitSpecie, benefitCondition, benefitValues)
  ggplotly(ggplot(benefitDataForPlot, aes(fill=benefitCondition, y=benefitValues, x=benefitSpecie)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T) +
    ggtitle("Benefit Analysis of Different AV Public Policy Options") +
    theme_ipsum() +
    xlab("Alternative") +
    ylab("Mean sum of criteria values") +
    labs(fill="Criteria"))
}

sink()

