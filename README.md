# RISFMCDADiviz

This project is a Multiple Criterion Decision Analysis (MCDA) software using Diviz and R. The R script in this project in the main program and is designed to take Qualtrics data and perform MCDA operations on it.

Diviz is an MCDA algorithmic components workflow design, execution and sharing tool (more information [here](http://www.diviz.org)) and is used in this project as an R package.

## Getting Started

To begin with running this project, download this github repository to your local system. You will also need R.

### Prerequisites

Instructions for installing R on your system can be found [here](https://cran.r-project.org/doc/manuals/r-release/R-admin.html). It is also necessary to download [R Studio](https://rstudio.com/products/rstudio/download/) as this will allow for easier installation of required packages and running of the script.

You will also need the **RXMCDA**, **XML**, and **kulife** packages as well as some packages used for plotting and color schemes. These can be installed by entering the following commands into the RStudio console:

```
install.packages("RXMCDA", dependencies=TRUE)
install.packages("XML")
install.packages("kulife")
install.packages("methods")
install.packages("ggplot2")
install.packages(viridis)
```

You can also install the packages from the command line if you have R installed on your system at this point. An example of this is below:

```
R
install.packages("RXMCDA", dependencies=TRUE)
install.packages("XML")
install.packages("kulife")
install.packages("methods")
install.packages("ggplot2")
install.packages(viridis)
```

The R command should open up an instance of R in your terminal. While in this instance type `q()` and the instance will close.

## Running the Software

### RStudio
  - Open [MASHQualtrics.R](DivizData/MAIA/MASHQualtrics.R) in RStudio.
  - From the top ribbon, select Session --> Set Working Directory --> Choose Directory and select the MAIA directory in DivizData.
  - Click the `Source` button in the top right of the editory window and the script will run (note to change which data is being analyzed, you much set the type variable on line 81 in the script to either harm, benefit, or both).
  - The graph of the analysis will be displayed in the bottom right view area. You can click the `Show in new window button` to view the graph in a larger window (looks like a window with an arrow pointing at it).
  - Instead of running the script all at once, you can go line by line by selecting the first line then pressing `Ctrl (or Cmd on Mac) + Enter` to run the currently selected line. Pressing it repeatedly moves through each line automatically.
  - The top right window allows you to view the variables in the environment and better see exactly how the data is being manipulated.
  


This program's input/output is as follows:

- ### Input (from Qualtrics)
  - [MAIA-Final-Criteria.xml](DivizData/MAIA/QualtricsData/MAIA-Final-Criteria.xml)
  - [MAIA-Final-Weights.xml](DivizData/MAIA/QualtricsData/MAIA-Final-Weights.xml)
- ### Output
  - [harmOutput.xml](DivizData/MAIA/output/harmOutput.xml)
  - [benefitOutput.xml](DivizData/MAIA/output/benefitOutput.xml)

The paths for the input files can be modified by changing the text in quotations on line 150 for criteria data and on line 154 for weight data. In order for the program to succeed, the user must make sure that the alternatives and criteria lists (beginning on line 26 and ending on line 70) contain all the criteria and alternatives in the data and in the correct order. The minimum and maximum values for the criteria must also be set on lines 82 and 83.

### Algorithm

Currently the software is using a weighted sum MCDA algorithm to determine the scores for each alternative. The values are normalized before the mean of the responses is calculated. There are a large variety of MCDA algorithms offered by the Divis suite of methods.

### Logs

The program will log updates on its progress to the **r-logs.txt** file. This file can be used to see any error or warning messages, the exact contents of files at each step of the software, and how the software progresses through its execution.


  



