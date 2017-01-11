#setwd("C:/Users/snewns/Dropbox/RunningAnalysis/Data")
setwd("C:/Users/Nimz/Dropbox/RunningAnalysis/Data")

garmin <- read.csv("garmin.csv")
str(garmin)
head(garmin)

#remove unneccessary cols
keepCols <- NA
keepCols <- c("Activity.Id", "When", "Type", "Gear", "Name", "Dist.mi", "Elv.ft", "Elapsed.Time", "Moving.Time",
              "Speed.mph", "Pace..mi", "Max.Pace..mi", "Cad", "Heart", "Max.Heart", "Elev.Dist.ft.mi",
              "Elev.Time.ft.h", "Cal", "Segs", "PRs", "Kudos")
newGarmin <- garmin[keepCols]
str(newGarmin)
head(newGarmin)

'##PRIOR DATA CLEANING --> REMOVE 2 ROWS AT TOP OF STRAVA FILES

strava1 <- read.csv("strava1.csv")

#check for interesting cols from Strava to keep
str(strava1)
names(strava1)
#Look at them more closely
head(strava1)

#get most recent data
strava6 <- read.csv("strava6.csv")
head(strava6)

#load in rest of strava files and combine into 1 DF
strava2 <- read.csv("strava2.csv")
strava3 <- read.csv("strava3.csv")
strava4 <- read.csv("strava4.csv")
strava5 <- read.csv("strava5.csv")

stravaFull <- Reduce(function(...) merge(..., all=TRUE), list(strava1, strava2, strava3, strava4, 
                                                              strava5, strava6))

#2 random records throwing things off, delete in Excel and reload 
write.csv(stravaFull, file = "fullStrava.csv", row.names = FALSE)'
stravaFull <- read.csv("fullStrava.csv")

str(stravaFull)
summary(stravaFull)
#multiple runs on August 12? Maybe a bike ride?
#check type in newGarmin

#install.packages("stringr")
library(stringr)
newGarmin$Date <- str_split_fixed(newGarmin$When, " ", 2)[,1]
newGarmin$StartTime <- str_split_fixed(newGarmin$When, " ", 2)[,2]

which(newGarmin$Date == '8/12/2016')
newGarmin[95:96,]
#looks like I just ran twice that day in Sea Isle. Back to the Strava data cleaning
head(stravaFull)

#install.packages("tidyr")
library(tidyr)
# specify the new column names:
vars <- c("Date", "StartTime")
vars2 <- c("DOW", "Date")
# then separate the "Details" column according to regex and drop extra columns:
testStravaFull <- separate(stravaFull, Start, into = vars, sep = "(?<=6 )", extra = "merge", remove = TRUE)
testStravaFull <- separate(testStravaFull, Date, into = vars2, sep = ", ", extra = "merge", remove = TRUE)

testStravaFull[order(testStravaFull$Date),]

testStravaFull$monthNum <- ifelse(grepl("Jan",testStravaFull$Date),1,
                            ifelse(grepl("Feb",testStravaFull$Date),2,
                            ifelse(grepl("Mar",testStravaFull$Date),3,
                            ifelse(grepl("Apr",testStravaFull$Date),4,
                            ifelse(grepl("May",testStravaFull$Date),5,
                            ifelse(grepl("Jun",testStravaFull$Date),6,
                            ifelse(grepl("Jul",testStravaFull$Date),7,
                            ifelse(grepl("Aug",testStravaFull$Date),8,
                            ifelse(grepl("Sep",testStravaFull$Date),9,
                            ifelse(grepl("Oct",testStravaFull$Date),10,
                            ifelse(grepl("Nov",testStravaFull$Date),11,
                            ifelse(grepl("Dec",testStravaFull$Date),12,NA))))))))))))
head(testStravaFull)

vars3 <- c("Month", "Date")
testStravaFull <- separate(testStravaFull, Date, into = vars3, sep = " ", extra = "merge", remove = TRUE)
head(testStravaFull)
vars4 <- c("Day", "Year")
testStravaFull <- separate(testStravaFull, Date, into = vars4, sep = ", ", extra = "merge", remove = TRUE)
head(testStravaFull)

#testDates <- as.Date(with(testStravaFull, paste(Year, monthNum, Day,sep="-")), "%Y-%m-%d")
#as.Date(with(testStravaFull, paste(Year, monthNum, Day,sep="/")), "%m/%d/%Y")
#testStravaFull$Day <- as.numeric(testStravaFull$Day)
#testStravaFull$Year <- as.numeric(testStravaFull$Day)

testStravaFull$Year <- trimws(testStravaFull$Year)
testStravaFull$Date <- format(as.Date(with(testStravaFull, paste(Year, monthNum, Day,sep="-")), "%Y-%m-%d"), "%m/%d/%Y")
str(testStravaFull$Date)

keepColsStrava  <- NA
keepColsStrava <- c("DOW", "Month", "StartTime", "Time", "Distance", "Elevation.Gain", "Avg.Speed.Avg.Pace.", "Avg.HR", 
                    "Max.HR", "Calories", "Date", "monthNum")
newStrava <- testStravaFull[keepColsStrava]
head(newStrava)

newGarmin$Date <- as.Date(newGarmin$Date,"%m/%d/%Y")
newStrava$Date <- as.Date(newStrava$Date,"%m/%d/%Y")
str(newStrava$Date)
str(newGarmin$Date)

newStrava <- newStrava[order(newStrava$Date, decreasing = FALSE),]
newGarmin <- newGarmin[order(newGarmin$Date, decreasing = FALSE),] #3 extra runs in Jan 2016, remove
head(newGarmin)#$Gear)
head(newStrava)

summary(newGarmin$Date)

#newGarmin$Date <- format(newGarmin$Date, "%m/%d/%Y")


newGarmin <- newGarmin[!newGarmin$Date == c("2016-01-20","2016-01-30","2016-01-31")]
#newGarmin <- newGarmin[!newGarmin$Date == "2016-01-30",]
#newGarmin <- newGarmin[!newGarmin$Date == "2016-01-31",]
newGarmin <- newGarmin[!newGarmin$Date < "2016-07-18",]

nrow(newGarmin)
nrow(newStrava) #3 extra

tail(newGarmin)
tail(newStrava)

#write.csv(newStrava$Date, file = "fullStravaDates.csv", row.names = FALSE)
#write.csv(newGarmin$Date, file = "fullGarminDates.csv", row.names = FALSE)

#check in excel (how in R?)
#remove false runs
newStrava <- newStrava[!newStrava$Date == "2016-08-27",]
newStrava <- newStrava[!newStrava$Date == "2016-09-01",]
newStrava[newStrava$Date == "2016-10-23",]
#remove bike ride
newStrava <- newStrava[!(newStrava$Date == "2016-10-23" & newStrava$Elevation.Gain == 657),]

nrow(newGarmin)
nrow(newStrava) #match now

head(newGarmin)
head(newStrava)
#install.packages("plyr") #for rename cols
library(plyr)
newStrava <- rename(newStrava, c("Date"="date_strava"))
newStrava <- rename(newStrava, c("StartTime"="StartTime_AM_PM"))
newGarmin$When <- NULL

newFullData <- cbind(newGarmin,newStrava)
head(newFullData)
newFullData[,c("Date","date_strava")]
#dates match

newFullData <- rename(newFullData, c("Activity.Id"="ID"))
names(newFullData)
keepColsFull  <- NA
keepColsFull <- c("ID", "Gear", "Name", "Speed.mph", "Cad", "Date", "StartTime", "DOW", "Month",  "Time", "Distance", 
                  "Elevation.Gain", "Avg.Speed.Avg.Pace.", "Avg.HR", "Max.HR", "Calories", "monthNum")
newFullData <- newFullData[keepColsFull]
#rearrange columns
newFullData <- newFullData[, c("ID", "Name", "Gear", "Date", "Month", "monthNum", "DOW", "StartTime",  "Distance", 
                               "Time", "Avg.Speed.Avg.Pace.", "Speed.mph", "Cad",  "Elevation.Gain", "Avg.HR", "Max.HR", 
                               "Calories")]
newFullData$Type <- ?ifelse(?is.element('Middle Long' %in% newFullData$Name),"yes","no")
table(newFullData$Name)

newFullData$Name <- if (newFullData$Name == "MIddle Long Run") {
                              newFullData$Name <- "Middle Long Run"
                      }
newFullData$Name <- ifelse(grepl("ML Run",newFullData$Name),"Middle Long Run",newFullData$Name)

newFullData$Type <- ifelse(grepl(c("V02","Tempo","Tune Up","LT","Tempo"),testStravaFull$Date),"Workout",
                                  ifelse(grepl("Long",testStravaFull$Date),"Long",
                                         ifelse(grepl("Middle","MIddle","ML")testStravaFull$Date),"Run",
                                                ifelse(grepl("Recovery",testStravaFull$Date),"Recovery",
                                                       ifelse(grepl("May",testStravaFull$Date),5,
                                                              ifelse(grepl("Jun",testStravaFull$Date),6,
                                                                     ifelse(grepl("Jul",testStravaFull$Date),7,
                                                                            ifelse(grepl("Aug",testStravaFull$Date),8,
                                                                                   ifelse(grepl("Sep",testStravaFull$Date),9,
                                                                                          ifelse(grepl("Oct",testStravaFull$Date),10,
                                                                                                 ifelse(grepl("Nov",testStravaFull$Date),11,
                                                                                                        ifelse(grepl("Dec",testStravaFull$Date),12,NA))))))))))))
