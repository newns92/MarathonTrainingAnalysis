library(tidyverse)
library(magrittr)

#load strava data
strava <- read.csv("../Data/2017/activities.csv", stringsAsFactors = F)

glimpse(strava)
summary(strava)

#remove unneccessary cols
keepCols_strava <- c("Activity.Id", "When", "Type", "Gear", "Name", "Dist.mi", "Elv.ft", "Elapsed.Time", 
              "Moving.Time", "Speed.mph", "Pace..mi", "Max.Pace..mi", "Cad", "Heart", "Max.Heart",
              "Elev.Dist.ft.mi","Elev.Time.ft.h")
strava <- strava[keepCols_strava]
glimpse(strava)

#split When into Date and Time fields in Strava data
library(lubridate)
strava  %<>% separate(col = When, into = c("Date", "StartTime"), sep = " ") %>%
  mutate(Date = as.POSIXct(Date),
         Month = month(Date), # number,
         Day = day(Date),
         Weekday = weekdays(Date))
str(strava)

# load garmin data
garmin <- read.csv("../Data/2017/garmin2017.csv", stringsAsFactors = F)

glimpse(garmin)
tail(garmin)
summary(garmin)

keepCols_garmin <- c("Activity.Type","Date","Title","Distance","Calories","Time","Avg.HR","Max.HR",
                     "Avg.Cadence", "Max.Cadence","Avg.Pace", "Best.Pace", "Elev.Gain", "Elev.Loss",
                     "Avg.Stride.Length")
garmin <- garmin[keepCols_garmin]
str(garmin)

#Specify the new column names:
garmin  %<>% separate(col = Date, into = c("Date", "StartTime"), sep = " ") %>%
  mutate(Date = as.POSIXct(Date),
         Month = month(Date), # number,
         Day = day(Date),
         Weekday = weekdays(Date))
str(garmin)

#Sort data frames from earliest to last date
garmin <- garmin[order(garmin$Date, decreasing = F),] 
strava <- strava[order(strava$Date, decreasing = F),]
head(garmin$Date)
head(strava$Date)
 
# remove excess runs
strava %<>% filter(Date >= "2017-07-17" & Date <= "2017-11-19")
garmin %<>% filter(Date >= "2017-07-17" & Date <= "2017-11-19")

# #check row counts
nrow(garmin)
nrow(strava)

# MISSING SOME GARMIN RUNS DUE TO LOST WATCH?

## WORK UP TO 10/17 IN MISSING




# check garmin runs 
table(garmin$Distance)  
# 
# #check garmin wierd distances
garmin[70:73,]  
strava[70:73,]  
# newGarmin[newGarmin$Distance=="0.16",]  
garmin <- garmin[!garmin$Distance==".5",] # remove injury
strava[(which(strava$Date == '2017-10-20')-1):(which(strava$Date == '2017-10-20')+3),] 
garmin[which(garmin$Date == '2017-10-20'),] # combine into 1 run

garmin[120,c("Activity.Type","Date","Distance", "Calories")] <-  c("running", as.POSIXct("2017-10-20"), 
                                                                   (1.5+9.05+1), NA,)
garmin[120,"StartTime"] <- "05:48:40",  
garmin[120,"Title"] <- "Running"
garmin[120,"Title"] <- "Running", Distance = (1.5+9.05+1), Calories = NA, "Time" = "1:23:10", Avg.HR = NA,
                           Max.HR = NA, Avg.Cadence = (183+177)/2, Max.Cadence = 192, Avg.Pace = "7:07", 
                           Best.Pace = NA, Elev.Gain = NA, Elev.Loss = NA, Avg.Stride.Length = NA, Month = 10,
                           Day = 20, Weekday = "Friday")
# #remove spin sessionts
# newGarmin <- newGarmin[!newGarmin$Date == "2016-08-27",]
# newGarmin <- newGarmin[!newGarmin$Date == "2016-09-01",]
# 
# #check garmin run dates
# table(newGarmin$Date) > 1
# 
# #check august garmin runs
# which(newStrava$Date == '2016-08-12') #runs 95 and 96
# newStrava[22:23,]
# 
# #remove random run
# newGarmin <- newGarmin[!newGarmin$Distance=="1.42",]
# newStrava <- newStrava[!newStrava$Activity.Id=="677460614",]
# 
# #check october garmin runs
# newGarmin[newGarmin$Date == "2016-10-23",]
# 
# #remove last bike ride
# newGarmin <- newGarmin[!(newGarmin$Date == "2016-10-23" & newGarmin$Elevation.Gain == 657),]
# #check row counts
# nrow(newGarmin)
# nrow(newStrava)
# 
# #check date match up
# head(newGarmin)
# head(newStrava)
# 
# #rename garmin cols
# library(plyr)
# newGarmin <- rename(newGarmin, c("Date"="date_garmin"))
# newGarmin <- rename(newGarmin, c("StartTime"="StartTime_AM_PM"))
# newStrava$When <- NULL #removes column
# 
# #combine data sets}
# newFullData <- cbind(newGarmin,newStrava)
# #inspect
# head(newFullData,3)
# #check dates
# head(newFullData[,c("Date","date_garmin")])
# tail(newFullData[,c("Date","date_garmin")])
# 
# #keep full dataset cols
# newFullData <- rename(newFullData, c("Activity.Id"="ID"))
# keepColsFull  <- NA
# keepColsFull <- c("ID", "Gear", "Name", "Speed.mph", "Cad", "Date", "StartTime", "DOW", "Month",  "Time", "Distance", 
#                     "Elevation.Gain", "Avg.Speed.Avg.Pace.", "Avg.HR", "Max.HR", "Calories", "monthNum")
# newFullData <- newFullData[keepColsFull]
# 
# #rearrange columns
# newFullData <- newFullData[, c("ID", "Name", "Gear", "Date", "Month", "monthNum", "DOW", "StartTime",  "Distance", 
#                             "Time", "Avg.Speed.Avg.Pace.", "Speed.mph", "Cad",  "Elevation.Gain", "Avg.HR", "Max.HR", 
#                             "Calories")]
# #check run names
# table(newFullData$Name) 
# 
# #fix ml runs
# newFullData$Name[newFullData$Name == 'MIddle Long Run'] <- 'ML Run'
# newFullData$Name[newFullData$Name == 'Middle Long Run'] <- 'ML Run'
# 
# #check morning runs
# newFullData[newFullData$Name == 'Morning Run',] 
# 
# #rename morning runs
# newFullData$Name[newFullData$ID == 650512799] <- 'Recovery Run'
# newFullData$Name[newFullData$ID == 655096239] <- 'ML Run'
# 
# #new run categories
# newFullData$RunType <- ifelse(grepl('LT',newFullData$Name),'Workout',
#                         ifelse(grepl('Tempo',newFullData$Name),'Workout',
#                           ifelse(grepl('Tune',newFullData$Name),'Workout',
#                             ifelse(grepl('VO2',newFullData$Name),'Workout',
#                               #ifelse(grepl('MP',newFullData$Name),'Workout',
#                                 ifelse(grepl('Long',newFullData$Name),'Long Run',
#                                   ifelse(grepl('Recovery',newFullData$Name),'Recovery Run',
#                                     ifelse(grepl('Marathon',newFullData$Name),'Race','Run')))))))#)
# table(newFullData$RunType)
# 
# #fix month ordering
# newFullData$Month <- factor(newFullData$Month, ordered = TRUE, levels = c("Jul","Aug","Sep","Oct","Nov"))
# class(newFullData$Month)
# 
# #Convert average Heart rate, max heart rate, calories, and elevation gain to numeric
# newFullData$Avg.HR <- as.numeric(as.character(newFullData$Avg.HR, stringsAsFactors = FALSE))
# newFullData$Max.HR <- as.numeric(as.character(newFullData$Max.HR, stringsAsFactors = FALSE))
# newFullData$Calories <- as.numeric(gsub(",","",newFullData$Calories))
# newFullData$Elevation.Gain <- as.numeric(as.character(newFullData$Elevation.Gain, stringsAsFactors = FALSE))
# 
# #Fix Cadence figures
# newFullData$Cad <- newFullData$Cad*2
# str(newFullData)
# 
# #fix Time fields and add week number
# library(lubridate)
# newFullData$weekNumber <- 0
# newFullData$weekNumber[1:6]     <- 1
# newFullData$weekNumber[7:12]    <- 2         
# newFullData$weekNumber[13:18]   <- 3
# newFullData$weekNumber[19:22]   <- 4
# newFullData$weekNumber[23:29]   <- 5
# newFullData$weekNumber[30:35]   <- 6
# newFullData$weekNumber[36:41]   <- 7
# newFullData$weekNumber[42:47]   <- 8
# newFullData$weekNumber[48:54]   <- 9
# newFullData$weekNumber[55:60]   <- 10
# newFullData$weekNumber[61:67]   <- 11
# newFullData$weekNumber[68:74]   <- 12
# newFullData$weekNumber[75:81]   <- 13
# newFullData$weekNumber[82:88]   <- 14
# newFullData$weekNumber[89:95]   <- 15
# newFullData$weekNumber[96:102]  <- 16
# newFullData$weekNumber[103:109] <- 17
# newFullData$weekNumber[110:115] <- 18
# newFullData$weekNumber[116]     <- 19
# 
# #fix Average Pace
# newFullData$Avg.Speed.Avg.Pace. <- as.POSIXct(newFullData$Avg.Speed.Avg.Pace., format = '%M:%S')
# plot(newFullData$Date,newFullData$Avg.Speed.Avg.Pace.)
# newFullData$Avg.Pace <- newFullData$Avg.Speed.Avg.Pace.
# newFullData$Avg.Speed.Avg.Pace. <- NULL
# 
# #fix total time
# newFullData$Time <- as.character(newFullData$Time)
# 
# for (i in 1:nrow(newFullData)) {
#   if (nchar(newFullData$Time[i]) == 4) {
#     newFullData$Time[i] <- paste("0:0", newFullData$Time[i], sep="")
#     #print(paste("00:0", newFullData$Time[i], sep=""))
#     #print(newFullData$Time[i])
#   } else if (nchar(newFullData$Time[i]) == 8) {
#     newFullData$Time[i] <- paste("0:",substring(newFullData$Time[i],1,5), sep="")
#     #print(paste("00:",substring(newFullData$Time[i],1,5), sep=""))
#   } else if (nchar(newFullData$Time[i]) == 5) {
#     newFullData$Time[i] <- paste("0:",newFullData$Time[i], sep="")
#     #print(paste("00:",substring(newFullData$Time[i],1,5), sep=""))
#   }  
# }
# 
# newFullData$Time <- as.POSIXct(newFullData$Time, format = '%H:%M:%S')
# 
# #test plot
# plot(newFullData$Date,newFullData$Time)
# 
# #put month name labels to monthNum to display names in numerical in order
# newFullData$Month <- factor(newFullData$Month, ordered = TRUE, levels = c("Jul","Aug","Sep","Oct","Nov"))
# class(newFullData$Month)
# newFullData$Month <- factor(newFullData, ordered = TRUE, levels = c("Jul","Aug","Sep","Oct","Nov"))
# 
# #fix Start Time
# newFullData$StartTime <- as.POSIXct(newFullData$StartTime, format = '%H:%M')
# 
# #add time bucket
# #newFullData$morningNightBucket <- if (newFullData$StartTime < 1491562800) {
# for (i in 1:nrow(newFullData)) {
#     if (newFullData$StartTime[i] < 1491562800) {
#       newFullData$morningNightBucket[i] <- 'Sunrise'
#     } else if (newFullData$StartTime[i] < 1491580800) {
#       newFullData$morningNightBucket[i] <- 'Morning'
#     } else if (newFullData$StartTime[i] < 1491595200) {
#       newFullData$morningNightBucket[i] <- 'Afternoon'
#     } else {
#       newFullData$morningNightBucket[i] <- 'Evening'
#     }
# }
# 
# #write data to file
# write.csv(newFullData, file = "cleanedMarathonTrainingData.csv", row.names = TRUE)