#install.packages("stringr")  - for manipulating string values
#install.packages("tidyr")    - data cleansing
#install.packages("plyr")     - for renaming columns

setwd("C:/Users/snewns/Dropbox/RunningAnalysis/R_Backups")
#setwd("C:/Users/Nimz/Dropbox/RunningAnalysis/R_Backups")

#load strava data
strava <- read.csv("strava.csv")
dim(strava)
head(strava)

#load garmin data
garmin1 <- read.csv("garmin1.csv")
dim(garmin1)
head(garmin1)

garmin6 <- read.csv("garmin6.csv")
dim(garmin1)
head(garmin6)

garmin2 <- read.csv("garmin2.csv")
garmin3 <- read.csv("garmin3.csv")
garmin4 <- read.csv("garmin4.csv")
garmin5 <- read.csv("garmin5.csv")

#Combine garmin data files into 1 data frame
garminFull <- Reduce(function(...) merge(..., all=TRUE), list(garmin1, garmin2, garmin3, garmin4, 
                                                              garmin5, garmin6))
dim(garminFull)
summary(garminFull)
head(garminFull)

#remove top 2 null rows
garminFull <- garminFull[-c(1,2),]
head(garminFull)

#remove unneccessary cols
keepCols <- NA
keepCols <- c("Activity.Id", "When", "Type", "Gear", "Name", "Dist.mi", "Elv.ft", "Elapsed.Time", "Moving.Time",
              "Speed.mph", "Pace..mi", "Max.Pace..mi", "Cad", "Heart", "Max.Heart", "Elev.Dist.ft.mi",
              "Elev.Time.ft.h", "Cal", "Segs", "PRs", "Kudos")
newStrava <- strava[keepCols]
str(newStrava)

#split When into Date and Time fields in Strava data
library(stringr)
newStrava$Date <- str_split_fixed(newStrava$When, " ", 2)[,1]
newStrava$StartTime <- str_split_fixed(newStrava$When, " ", 2)[,2]

str(newStrava)

library(tidyr)

#Specify the new column names:
vars <- c("Date", "StartTime")
vars2 <- c("DOW", "Date")

#Separate columns according to regex and/or delimiteres and proceed to drop remaining extra columns:
garminFull <- separate(garminFull, Start, into = vars, sep = "(?<=6 )", extra = "merge", remove = TRUE)
garminFull <- separate(garminFull, Date, into = vars2, sep = ", ", extra = "merge", remove = TRUE)

#Create MonthNumber field based on what is in Date field
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
head(garminFull,2)

#create dy, mth, yr fields}
vars3 <- c("Month", "Date")
vars4 <- c("Day", "Year")
garminFull <- separate(garminFull, Date, into = vars3, sep = " ", extra = "merge", remove = TRUE)
garminFull <- separate(garminFull, Date, into = vars4, sep = ", ", extra = "merge", remove = TRUE)
head(garminFull,2)

#remove whitespace
garminFull$Year <- trimws(garminFull$Year)

#Create Date field from 3 components: Day, Month, Year
garminFull$Date <- format(as.Date(with(garminFull, paste(Year, monthNum, Day,sep="-")), "%Y-%m-%d"), "%m/%d/%Y")
str(garminFull$Date)

#remove Garmin Cols
keepColsGarmin  <- NA
keepColsGarmin <- c("DOW", "Month", "StartTime", "Time", "Distance", "Elevation.Gain", "Avg.Speed.Avg.Pace.", "Avg.HR", 
                    "Max.HR", "Calories", "Date", "monthNum")
newGarmin <- garminFull[keepColsGarmin]
head(newGarmin,2)

#format date field
newGarmin$Date <- as.Date(newGarmin$Date,"%m/%d/%Y")
newStrava$Date <- as.Date(newStrava$Date,"%m/%d/%Y")

#Check data type if Dates
str(newGarmin$Date)
str(newStrava$Date)

#Sort data frames from earliest to last date
newGarmin <- newGarmin[order(newGarmin$Date, decreasing = FALSE),] 
newStrava <- newStrava[order(newStrava$Date, decreasing = FALSE),]
head(newGarmin$Date)
head(newStrava$Date)

#remove excess strava runs
newStrava <- newStrava[!newStrava$Date < "2016-07-18",]

#check row counts
nrow(newGarmin)
nrow(newStrava)

#check garmin runs 
table(newGarmin$Distance)  

#check garmin wierd distances
newGarmin[newGarmin$Distance=="0.12",]  
newGarmin[newGarmin$Distance=="0.16",]  
newGarmin[newGarmin$Distance=="1.42",]  

#remove spin sessionts
newGarmin <- newGarmin[!newGarmin$Date == "2016-08-27",]
newGarmin <- newGarmin[!newGarmin$Date == "2016-09-01",]

#check garmin run dates
table(newGarmin$Date) > 1

#check august garmin runs
which(newStrava$Date == '2016-08-12') #runs 95 and 96
newStrava[22:23,]

#remove random run
newGarmin <- newGarmin[!newGarmin$Distance=="1.42",]
newStrava <- newStrava[!newStrava$Activity.Id=="677460614",]

#check october garmin runs
newGarmin[newGarmin$Date == "2016-10-23",]

#remove last bike ride
newGarmin <- newGarmin[!(newGarmin$Date == "2016-10-23" & newGarmin$Elevation.Gain == 657),]
#check row counts
nrow(newGarmin)
nrow(newStrava)

#check date match up
head(newGarmin)
head(newStrava)

#rename garmin cols
library(plyr)
newGarmin <- rename(newGarmin, c("Date"="date_garmin"))
newGarmin <- rename(newGarmin, c("StartTime"="StartTime_AM_PM"))
newStrava$When <- NULL #removes column

#combine data sets}
newFullData <- cbind(newGarmin,newStrava)
#inspect
head(newFullData,3)
#check dates
head(newFullData[,c("Date","date_garmin")])
tail(newFullData[,c("Date","date_garmin")])

#keep full dataset cols
newFullData <- rename(newFullData, c("Activity.Id"="ID"))
keepColsFull  <- NA
keepColsFull <- c("ID", "Gear", "Name", "Speed.mph", "Cad", "Date", "StartTime", "DOW", "Month",  "Time", "Distance", 
                    "Elevation.Gain", "Avg.Speed.Avg.Pace.", "Avg.HR", "Max.HR", "Calories", "monthNum")
newFullData <- newFullData[keepColsFull]

#rearrange columns
newFullData <- newFullData[, c("ID", "Name", "Gear", "Date", "Month", "monthNum", "DOW", "StartTime",  "Distance", 
                            "Time", "Avg.Speed.Avg.Pace.", "Speed.mph", "Cad",  "Elevation.Gain", "Avg.HR", "Max.HR", 
                            "Calories")]
#check run names
table(newFullData$Name) 

#fix ml runs
newFullData$Name[newFullData$Name == 'MIddle Long Run'] <- 'ML Run'
newFullData$Name[newFullData$Name == 'Middle Long Run'] <- 'ML Run'

#check morning runs
newFullData[newFullData$Name == 'Morning Run',] 

#rename morning runs
newFullData$Name[newFullData$ID == 650512799] <- 'Recovery Run'
newFullData$Name[newFullData$ID == 655096239] <- 'ML Run'

#new run categories
newFullData$RunType <- ifelse(grepl('LT',newFullData$Name),'Workout',
                        ifelse(grepl('Tempo',newFullData$Name),'Workout',
                          ifelse(grepl('Tune',newFullData$Name),'Workout',
                            ifelse(grepl('VO2',newFullData$Name),'Workout',
                              #ifelse(grepl('MP',newFullData$Name),'Workout',
                                ifelse(grepl('Long',newFullData$Name),'Long Run',
                                  ifelse(grepl('Recovery',newFullData$Name),'Recovery Run',
                                    ifelse(grepl('Marathon',newFullData$Name),'Race','Run')))))))#)
table(newFullData$RunType)

#install.packages("chron")
#library(chron)
#times(fu)

#Convert average Heart rate, max heart rate, calories, and elevation gain to numeric
newFullData$Avg.HR <- as.numeric(as.character(newFullData$Avg.HR, stringsAsFactors = FALSE))
newFullData$Max.HR <- as.numeric(as.character(newFullData$Max.HR, stringsAsFactors = FALSE))
newFullData$Calories <- as.numeric(gsub(",","",newFullData$Calories))
newFullData$Elevation.Gain <- as.numeric(as.character(newFullData$Elevation.Gain, stringsAsFactors = FALSE))


#Fix Cadence figures
newFullData$Cad <- newFullData$Cad*2


?strptime(as.POSIXct(newFullData$Avg.Speed.Avg.Pace., format = '%M:%S'), format = '%T')

install.packages("lubridate")
library(lubridate)
as.numeric(format(as.POSIXct(newFullData$Avg.Speed.Avg.Pace., format = '%M:%S'), "%M")) + 
  as.numeric(format(as.POSIXct(newFullData$Avg.Speed.Avg.Pace., format = '%M:%S'), "%S"))


as.character(minute(as.POSIXct(newFullData$Avg.Speed.Avg.Pace., format = '%M:%S'))) &
               ":" &  
  as.character(second(as.POSIXct(newFullData$Avg.Speed.Avg.Pace., format = '%M:%S')))


plot(newFullData$Date,as.POSIXct(newFullData$Avg.Speed.Avg.Pace., format = '%M:%S'))

library(ggplot2)

ggplot(data = newFullData, aes(x = Date, y = as.POSIXct(newFullData$Avg.Speed.Avg.Pace., format = '%M:%S'))) + 
  geom_point()

#fix ones less than 8 digits
newFullData$Time
newFullData$testTime <- as.character(newFullData$Time)
newFullData$testTime[3] <- "0:32:40"
newFullData$testTime[5] <- "0:48:25"
newFullData$testTime[9] <- "0:23:41"
newFullData$testTime[11] <- "0:49:29"
nchar(newFullData$testTime[82])

#fix total times
for (i in 1:nrow(newFullData)) {
  if (nchar(newFullData$testTime[i]) == 4) {
    print(paste("00:0", newFullData$testTime[i], sep=""))
    #print(newFullData$testTime[i])
  } else if (nchar(newFullData$testTime[i]) == 8) {
    print(paste("00:",substring(newFullData$testTime[i],1,5), sep=""))
  }
}

nchar(newFullData$testTime[71])
nchar(newFullData$testTime[2],1,1))

if (nchar(newFullData$testTime[i]) == 4) {
  lapply(newFullData$testTime, function(x) paste("00", x, sep=":"))
}









#write data to file
write.csv(newFullData, file = "cleanedMarathonTrainingData.csv", row.names = TRUE)