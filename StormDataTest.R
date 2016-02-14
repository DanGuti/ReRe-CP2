# Course Project 2

#Load dependencies
library(plyr)

# Retrieve zip file
u <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(u,"StormData.csv.bz2") #download zip file

# Unzip file
sData <- read.csv("StormData.csv.bz2")

## Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful 
## with respect to population health?
str(sData) #get some idea what variables are available.

intvars <- c("EVTYPE","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP") #index of variables of interest
sData <- sData[intvars] #reduce data to only relevant variables

# Create filters for relevant data

## Harmful events - Injuries or Fatalities
filter1 <- (sData$FATALITIES >1 | sData$INJURIES > 1) #filter for Injury or Fatality related events.

## Economic consequences
### Replace levels for property damage exponential
levels(sData$PROPDMG) #look at the levels we have from property damage exponentials

replacing <- c("?" = NA, 
              "-" = 0, "+" = 0, 
              "h" = 2, "H" = 2, 
              "K" = 3,
              "m" = 6, "M" = 6,
              "B" = 9) #list of value equivalencies to transform letters to number exponential
cleanPDMGEXP <- revalue(sData$PROPDMGEXP, replacing) #transform letters to exponential values. The given number is assumed to be of the form 10^x.

lv <- levels(cleanPDMGEXP) #save levels of property damage exponential
lv[1] = NA #when no damage is reported, list as NA
levels(cleanPDMGEXP) = lv #Fix levels on cleanPDMGEXP
cleanPDMGEXP = as.numeric(levels(cleanPDMGEXP)[cleanPDMGEXP]) #converts factor variable to numeric
totPdmg <- sData$PROPDMG * 10^cleanPDMGEXP

### Replace levels for crop damage exponential
levels(sData$CROPDMGEXP) #look at the levels we have from property damage exponentials

replacing <- c("?" = NA, 
               "k" = 3, "K" = 3,
               "m" = 6, "M" = 6,
               "B" = 9) #list of value equivalencies to transform letters to number exponential
cleanCDMGEXP <- revalue(sData$CROPDMGEXP, replacing) #transform letters to exponential values. The given number is assumed to be of the form 10^x.

lv <- levels(cleanCDMGEXP) #save levels of property damage exponential
lv[1] = NA #when no damage is reported, list as NA
levels(cleanCDMGEXP) = lv #Fix levels on cleanPDMGEXP
cleanCDMGEXP = as.numeric(levels(cleanCDMGEXP)[cleanCDMGEXP])#converts factor variable to numeric, the -1 offsets the value to match the levels
totPdmg <- sData$CROPDMG * 10^cleanCDMGEXP

### generate filter for complete data cases on either prop. damage or crop damage
filter2 <- ()



## Across the United States, which types of events have the greatest economic consequences?