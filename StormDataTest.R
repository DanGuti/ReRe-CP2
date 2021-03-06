# Course Project 2

#Load dependencies
require(plyr)

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

# Cleaning & gathering the data

cData <- data.frame("EVTYPE" = sData$EVTYPE, "FATALITIES" = sData$FATALITIES, "INJURIES" = sData$INJURIES)

## Making the damage exponentials uniform numerical data
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
cData$totPdmg <- sData$PROPDMG * 10^cleanPDMGEXP

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
cData$totCdmg <- sData$CROPDMG * 10^cleanCDMGEXP

### Finally, we change NA's to 0 in the totCdmg and totPdmg variables, so that we only have dollar amounts and can come up with one single damage No.
cData$totPdmg[is.na(cData$totPdmg)] <- 0
cData$totCdmg[is.na(cData$totCdmg)] <- 0
cData$totDmg <- cData$totPdmg + cData$totCdmg

# Create data summaries

## Harmful events - Injuries or Fatalities
Harm <- aggregate(cData[,2:3], list(EVTYPE = cData$EVTYPE), sum) #create table with fatality and Injury aggregates
TopHarm <- head(arrange(Harm,desc(Harm$FATALITIES))) #retrieve 6 EVTYPEs with most fatalities

TH <- melt(TopHarm, id = "EVTYPE")

## Damage
Dam <- aggregate(cData[,4:6], list(EVTYPE = cData$EVTYPE), sum) #aggregate fatalities
TopDam <- head(arrange(Dam,desc(Dam$totDmg))) #retrieve 6 EVTYPEs with most total damage

TD <- melt(TopDam[,1:3], id = "EVTYPE")
## Results
  ##op <- par(mar = c(9,4,4,2) + 0.1) ## Enlong margins for better visualization
  ##
  ##barplot(FatSum$FATALITIES, names.arg = InjSum$EVTYPE, las = 2)
  ##barplot(InjSum$INJURIES, names.arg = InjSum$EVTYPE, las = 2)
ggplot(data=TH, aes(x=EVTYPE, y=value, fill=variable)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=60,hjust=0.9,vjust=0.9))+
  ggtitle("Weather Event Harm") +
  labs(x="Event Type",y="Fatalities or Injuries") 

ggplot(data=TD, aes(x=EVTYPE, y=value, fill=variable)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=60,hjust=0.9,vjust=0.9))+
  ggtitle("Weather Event Damages") +
  labs(x="Event Type",y="Value of Damages ($)") 
