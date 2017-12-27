library(tidyverse)
library(magrittr)

#load strava data
strava <- read.csv("../Data/2017/strava2017.csv", stringsAsFactors = F)

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
         StartTime = paste(substr(StartTime,1,6),"00",sep=""),
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
         StartTime = paste(substr(StartTime,1,6),"00",sep=""))#,
         #Month = month(Date), # number,
         #Day = day(Date),
        # Weekday = weekdays(Date))
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


# mistmach with missing watch and manual entry
garmin$StartTime[c(43:45,47:51,110:113,127:130)] <- strava$StartTime[c(43:45,47:51,110:113,127:130)]

# get full dataset
fullData2 <- garmin %>% left_join(strava, by = c("Date","StartTime")) %>% 
  select(Name, Date, StartTime, Distance, Time, Heart, Avg.HR, Max.HR, Cad, Avg.Cadence, Max.Cadence, Avg.Pace, Best.Pace, 
         Elev.Gain, Elev.Loss, Avg.Stride.Length)
head(fullData2)

# check garmin runs 
table(fullData2$Distance)  
table(strava$Distance)  

# new run categories
fullData2$Name[150] <- "dress rehearsal GA" 

workouts <- c("tune","tempo","vo2","0","mp","race","lt")
extra <- c("warm","cool","jog","hurt")
ml <- c("medium","middle")
ga <- c("gen","aer","ga","ge","short")

fullData3 <- fullData2 %>% 
  mutate(RunCat = ifelse(grepl(paste(workouts, collapse="|"),  tolower(Name)),'Workout',
                    ifelse(grepl("marathon", tolower(Name)),"Race",
                    ifelse(round(Distance) > 15, "Long", 
                    ifelse(grepl(paste(ga, collapse="|"),  tolower(Name)), "GA",                           
                    ifelse(round(Distance) %in% c(3,4,5,6),
                          ifelse(grepl(paste(extra, collapse="|"),  tolower(Name)), "Misc", "Recovery"),
                    ifelse(grepl("recovery",  tolower(Name)),"Recovery",                          
                    ifelse(round(Distance) %in% c(12:15), "ML",
                    ifelse(grepl(paste(ml, collapse="|"),  tolower(Name)), "ML", 
                    ifelse(grepl(paste(extra, collapse="|"),  tolower(Name)),'Misc',"GA"))))))))), "GA",
         RunType = ifelse(Distance > 25, "Race",
                      ifelse(Distance > 15, "Long", 
                          ifelse(grepl(paste(c("ML","GA"), collapse = "|"), RunCat), "Run", RunCat))))
                                 #ifelse(grepl(paste(c("Recovery", "Long", "Workout", "Misc"), collapse = "|") , RunCat), RunCat))

fullData3 %>%
  select(Name,Distance,RunCat,RunType)


table(fullData3$Name)

fullData3 %>%
  select(Name,Distance,RunType)
# #check garmin wierd distances
#garmin[70:73,]  
#strava[70:73,]  
# newGarmin[newGarmin$Distance=="0.16",]  
fullData2[fullData2$Date=="2017-09-13",] # remove injury

garmin[garmin$Distance==1.5,]
fullData2[fullData2$Date=="2017-10-20",]

garmin <- garmin[!garmin$Date=="2017-09-13",] 
strava <- strava[!strava$Date=="2017-09-13",]

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