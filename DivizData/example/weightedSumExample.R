#Setup log file
log_file <- "r-logs.txt"
output_file <- "output.xml"
file.remove(log_file)
file.create(log_file)
file.remove(output_file)
file.create(output_file)
sink(log_file)

# Load the RXMCDA package (which in turn
# will load the required XML package)
print("Loading RXMCDA package and MASH data...")
library(RXMCDA)
library("XML")
library("methods")


print("Packages and data loaded.")
# Get the list of arguments behind the --args option

args <- commandArgs(trailingOnly = TRUE)
# Get the filenames from the arguments
print("Loading files...")
tryCatch( {
     alternativesFile <- "alternatives.xml"
     print("Alternatives loaded.")
     criteriaFile <- "criteria.xml"
     print("Criteria loaded.")
     performanceTableFile <- "performanceTable.xml"
     print("Performance table loaded.")
     criteriaWeightsFile <- "criteriaWeights.xml"
     print("Criteria weights loaded")
     overallValuesFile <- "output.xml"
}, warning = function(war) {
     # warning handler picks up where error was generated
     print(paste("FILE LOADING WARNING:  ",war))
}, error = function(err) {
     # error handler picks up where error was generated
     print(paste("FILE LOADING ERROR:  ",err))
}, finally = {
     print("Finished loading files.")
})

print(alternativesFile)
print(criteriaFile)
print(performanceTableFile)
print(criteriaWeightsFile)

# Load the input files
# See the documentation of the XML package for R
# for further details on the xmlParse function
print("Parsing xml from files...")
tryCatch({
     print(alternativesFile)
     treeAlternatives<-xmlTreeParse(alternativesFile,useInternalNodes=TRUE)
     print("Alternatives parsed.")
     treeCriteria<-xmlTreeParse(criteriaFile,useInternalNodes=TRUE)
     print("Criteria parsed.")
     treePerformanceTable<-xmlTreeParse(performanceTableFile,useInternalNodes=TRUE)
     print("Performance table parsed.")
     treeCriteriaWeights<-xmlTreeParse(criteriaWeightsFile,useInternalNodes=TRUE)
     print("Criteria weights parsed.")
}, warning = function(war) {
     # warning handler picks up where error was generated
     print(paste("FILE PARSING WARNING:  ",war))
}, error = function(err) {
     # error handler picks up where error was generated
     print(paste("FILE PARSING ERROR:  ",err))
}, finally = {
     print("Finished parsing files.")
})

if(checkXSD(treeAlternatives) != 1) {
     print("The alternatives xml tree is not valid XMCDA schema")
}
print(treeAlternatives)
if(checkXSD(treeCriteria) != 1) {
     print("The criteria xml tree is not valid XMCDA schema")
}
print(treeCriteria)
if(checkXSD(treePerformanceTable) != 1) {
     print("The performance table xml tree is not valid XMCDA schema")
}
print(treePerformanceTable)
if(checkXSD(treeCriteriaWeights) != 1) {
     print("The criteria weights xml tree is not valid XMCDA schema")
}
print(treeCriteriaWeights)

# Read the MCDA data from the files
# See the documentation of the RXMCDA package for R for further
# details concerning this step, and for the description
# of the output data

print("Reading file data....")
tryCatch({
     critIDs <- getCriteriaIDs(treeCriteria)[[1]]
     print("Criteria data read.")
     altIDs <- getAlternativesIDs(treeAlternatives)[[1]]
     print("Alternative data read.")
     perfTable <- getPerformanceTables(treePerformanceTable,altIDs = altIDs, critIDs = critIDs)[[1]]
     print("Performance table read.")
     critWeights <- getCriteriaValues(treeCriteriaWeights, critIDs)[[1]]
     print("Criteria weights read.")
}, warning = function(war) {
     # warning handler picks up where error was generated
     print(paste("FILE LOADING WARNING:  ",war))
}, error = function(err) {
     # error handler picks up where error was generated
     print(paste("FILE LOADING ERROR:  ",err))
}, finally = {
     print("Finished reading files.")
})

# Calculate the weighted sum of the alternatives
print(critIDs)
print(altIDs)
print(perfTable)
print(critWeights)

print("Calculating weighted sums of alternatives...")
overallValues <- c()
for (i in 1:dim(perfTable)[1]){
     overallValues <- rbind(overallValues, c(i,sum(perfTable[i,]*t(critWeights[,2]))))
}
print("Sums calculated.")
print(overallValues)

# Write the result file
# We first create an empty XML tree

outTree = newXMLDoc()

# Then we specify the root node

newXMLNode("xmcda:XMCDA",
     attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.1.0 https://www.decision-deck.org/xmcda/_downloads/XMCDA-2.1.0.xsd"),
     suppressNamespaceWarning=TRUE,
     namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.1.0"),
     parent=outTree)

# And finally we write the output data in the XML tree
# Again, see the RXMCDA documentation for
# further details on the putAlternativesValues function and its arguments

putAlternativesValues(outTree, overallValues, rownames(perfTable), "overallValues")

saveXML(outTree, file=overallValuesFile)
sink()