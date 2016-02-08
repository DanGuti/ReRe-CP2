# Course Project 2

# Retrieve zip file
u <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(u,"StormData.csv.bz2") #download zip file

# Unzip file
sData <- read.csv("StormData.csv.bz2")

## Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful 
## with respect to population health?
str(sData) #get some idea what variables are available.



## Across the United States, which types of events have the greatest economic consequences?