#download the data file using the URL
if(!file.exists("stormData.csv.bz2")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                  destfile = "stormData.csv.bz2", method = "curl")
}

#StmData <- read.table(bzfile("stormData.csv.bz2"), sep=",", header=T)

StmData <- read.csv(file="C:/Users/skb67/Desktop/CourseraDS/Reproducible Research/Final_Assignment/repdata_data_StormData_m.csv", header=TRUE, sep=",")

#Only use the necessary variables from  Stmdata
tidyStmData <- StmData[,c('EVTYPE','FATALITIES','INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')]

library(ggplot2)

# harmful event type that links to fatalities
fatal <- aggregate(FATALITIES ~ EVTYPE, data=tidyStmData, sum)
fatal <- fatal[order(-fatal$FATALITIES), ][1:10, ]
fatal$EVTYPE <- factor(fatal$EVTYPE, levels = fatal$EVTYPE)

g <- ggplot(fatal, aes(x = EVTYPE, y = FATALITIES)) + 
    geom_bar(stat = "identity", fill = "purple") + 
    labs(title="Fatalities due to weather events",
         x ="Event", y = "Number of Fatalities")+
    theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))
#print(g)

# harmful event type that links to injuries
inj <- aggregate(INJURIES ~ EVTYPE, data=tidyStmData, sum)
inj <- inj[order(-inj$INJURIES), ][1:10, ]
inj$EVTYPE <- factor(inj$EVTYPE, levels = inj$EVTYPE)

gg2 <-ggplot(inj, aes(x = EVTYPE, y = INJURIES)) + 
    geom_bar(stat = "identity", fill = "purple") + 
    labs(title="Injuries due to weather events",
         x ="Event", y = "Number of Injuries")+
    theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))
#print(gg2)

#harmful event types that link to property and crop damage
damg <- aggregate(PROPDMG + CROPDMG ~ EVTYPE, data=tidyStmData, sum)
names(damg) = c("EVTYPE", "TOTALDAMAGE")
damg <- damg[order(-damg$TOTALDAMAGE), ][1:10, ]
damg$EVTYPE <- factor(damg$EVTYPE, levels = damg$EVTYPE)

gg3<-ggplot(damg, aes(x = EVTYPE, y = TOTALDAMAGE)) + 
    geom_bar(stat = "identity", fill = "purple") + 
    labs(title="Property and Crop damages due to weather events",
         x ="Event", y = "Number of Damages in Dollars")+
    theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))

print(gg3)