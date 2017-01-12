setwd("C:/Users/snewns/Dropbox/RunningAnalysis/Data")
#setwd("C:/Users/Nimz/Dropbox/RunningAnalysis/Data")

strava <- read.csv("strava.csv")
str(strava)
head(strava)

#remove unneccessary cols
keepCols <- NA
keepCols <- c("Activity.Id", "When", "Type", "Gear", "Name", "Dist.mi", "Elv.ft", "Elapsed.Time", "Moving.Time",
              "Speed.mph", "Pace..mi", "Max.Pace..mi", "Cad", "Heart", "Max.Heart", "Elev.Dist.ft.mi",
              "Elev.Time.ft.h", "Cal", "Segs", "PRs", "Kudos")
newStrava <- strava[keepCols]
str(newStrava)
head(newStrava)
tail(newStrava)

##PRIOR DATA CLEANING --> REMOVE 2 ROWS AT TOP OF GARMIN FILES

'garmin1 <- read.csv("garmin1.csv")

#check for interesting cols from garmin to keep
str(garmin1)
names(garmin1)
#Look at them more closely
head(garmin1)
tail(garmin1)

#get most recent data
garmin6 <- read.csv("garmin6.csv")
head(garmin6)
tail(garmin6)

#load in rest of garmin files and combine into 1 DF
garmin2 <- read.csv("garmin2.csv")
garmin3 <- read.csv("garmin3.csv")
garmin4 <- read.csv("garmin4.csv")
garmin5 <- read.csv("garmin5.csv")

head(garmin5)
tail(garmin4)

garminFull <- Reduce(function(...) merge(..., all=TRUE), list(garmin1, garmin2, garmin3, garmin4, 
                                                              garmin5, garmin6))

#2 random records throwing things off, delete in Excel and reload 
#write.csv(garminFull, file = "fullGarmin.csv", row.names = FALSE)'
garminFull <- read.csv("fullGarmin.csv")

str(garminFull)
summary(garminFull)
#multiple runs on August 12? Maybe a bike ride?
#check type in Strava

#install.packages("stringr")
library(stringr)
newStrava$Date <- str_split_fixed(newStrava$When, " ", 2)[,1]
newStrava$StartTime <- str_split_fixed(newStrava$When, " ", 2)[,2]

which(newStrava$Date == '8/12/2016')
newStrava[95:96,]
#looks like I just ran twice that day in Sea Isle. Back to the Garmin data cleaning
head(garminFull)

#install.packages("tidyr")
library(tidyr)
# specify the new column names:
vars <- c("Date", "StartTime")
vars2 <- c("DOW", "Date")
# then separate the "Details" column according to regex and drop extra columns:
garminFull <- separate(garminFull, Start, into = vars, sep = "(?<=6 )", extra = "merge", remove = TRUE)
garminFull <- separate(garminFull, Date, into = vars2, sep = ", ", extra = "merge", remove = TRUE)

garminFull[order(garminFull$Date),]

garminFull$monthNum <- ifelse(grepl("Jan",garminFull$Date),1,
                            ifelse(grepl("Feb",garminFull$Date),2,
                            ifelse(grepl("Mar",garminFull$Date),3,
                            ifelse(grepl("Apr",garminFull$Date),4,
                            ifelse(grepl("May",garminFull$Date),5,
                            ifelse(grepl("Jun",garminFull$Date),6,
                            ifelse(grepl("Jul",garminFull$Date),7,
                            ifelse(grepl("Aug",garminFull$Date),8,
                            ifelse(grepl("Sep",garminFull$Date),9,
                            ifelse(grepl("Oct",garminFull$Date),10,
                            ifelse(grepl("Nov",garminFull$Date),11,
                            ifelse(grepl("Dec",garminFull$Date),12,NA))))))))))))
head(garminFull)

vars3 <- c("Month", "Date")
garminFull <- separate(garminFull, Date, into = vars3, sep = " ", extra = "merge", remove = TRUE)
head(garminFull)
vars4 <- c("Day", "Year")
garminFull <- separate(garminFull, Date, into = vars4, sep = ", ", extra = "merge", remove = TRUE)
head(garminFull)

garminFull$Year <- trimws(garminFull$Year)
garminFull$Date <- format(as.Date(with(garminFull, paste(Year, monthNum, Day,sep="-")), "%Y-%m-%d"), "%m/%d/%Y")
str(garminFull$Date)

keepColsGarmin  <- NA
keepColsGarmin <- c("DOW", "Month", "StartTime", "Time", "Distance", "Elevation.Gain", "Avg.Speed.Avg.Pace.", "Avg.HR", 
                    "Max.HR", "Calories", "Date", "monthNum")
newGarmin <- garminFull[keepColsGarmin]
head(newGarmin)

newGarmin$Date <- as.Date(newGarmin$Date,"%m/%d/%Y")
newStrava$Date <- as.Date(newStrava$Date,"%m/%d/%Y")
str(newGarmin$Date)
str(newStrava$Date)

newGarmin <- newGarmin[order(newGarmin$Date, decreasing = FALSE),] 
newStrava <- newStrava[order(newStrava$Date, decreasing = FALSE),]
head(newGarmin)
head(newStrava)

#3 extra Garmin runs in Jan 2016, remove
summary(newGarmin$Date)

newGarmin <- newGarmin[!newGarmin$Date == c("2016-01-20","2016-01-30","2016-01-31")]
#newGarmin <- newGarmin[!newGarmin$Date == "2016-01-30",]
#newGarmin <- newGarmin[!newGarmin$Date == "2016-01-31",]
newGarmin <- newGarmin[!newGarmin$Date < "2016-07-18",]
newStrava <- newStrava[!newStrava$Date < "2016-07-18",]

nrow(newGarmin) #3 extra
nrow(newStrava)

tail(newGarmin)
tail(newStrava)

##check in excel (how in R?)
#write.csv(newStrava$Date, file = "fullStravaDates.csv", row.names = FALSE)
#write.csv(newGarmin$Date, file = "fullGarminDates.csv", row.names = FALSE)

#remove false runs
newGarmin <- newGarmin[!newGarmin$Date == "2016-08-27",]
newGarmin <- newGarmin[!newGarmin$Date == "2016-09-01",]
newGarmin[newGarmin$Date == "2016-10-23",]
#remove bike ride
newGarmin <- newGarmin[!(newGarmin$Date == "2016-10-23" & newGarmin$Elevation.Gain == 657),]

nrow(newGarmin)
nrow(newStrava) #match now

head(newGarmin)
head(newStrava)
#install.packages("plyr") #for rename cols
library(plyr)
newGarmin <- rename(newGarmin, c("Date"="date_garmin"))
newGarmin <- rename(newGarmin, c("StartTime"="StartTime_AM_PM"))
newStrava$When <- NULL

newFullData <- cbind(newGarmin,newStrava)
head(newFullData)
newFullData[,c("Date","date_garmin")]
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
#newFullData$Type <- ?ifelse(?is.element('Middle Long' %in% newFullData$Name),"yes","no")
table(newFullData$Name) 

#write.csv(newFullData, file = "newFullData.csv", row.names = FALSE)
str(newFullData$ID)
#Fix typos in names
newFullData$Name[newFullData$Name == 'MIddle Long Run'] <- 'ML Run'
newFullData$Name[newFullData$Name == 'Middle Long Run'] <- 'ML Run'
newFullData[newFullData$Name == 'Morning Run',] #One recovery run and one ML run based off of distance and pace
newFullData$Name[newFullData$ID == 650512799] <- 'Recovery Run'
newFullData$Name[newFullData$ID == 655096239] <- 'ML Run'

#create new "run type" for run, recovery, long, workout etc.
newFullData$RunType <- ifelse(grepl('LT',newFullData$Name),'Workout',
                        ifelse(grepl('Tempo',newFullData$Name),'Workout',
                          ifelse(grepl('Tune',newFullData$Name),'Workout',
                            ifelse(grepl('V02',newFullData$Name),'Workout',
                              ifelse(grepl('Long',newFullData$Name),'Long Run',
                                ifelse(grepl('Recovery',newFullData$Name),'Recovery Run','Run'))))))
table(newFullData$RunType)