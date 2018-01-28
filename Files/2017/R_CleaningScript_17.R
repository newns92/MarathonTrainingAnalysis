# load libraries
library(tidyverse)
library(magrittr)
library(lubridate)

# load strava data
strava <- read.csv("../Data/2017/strava2017.csv", stringsAsFactors = F)

## inspect
#glimpse(strava)
#summary(strava)

## remove unneccessary cols
strava %<>% select(Activity.Id, When, Type, Gear, Name, Dist.mi, Elv.ft,
                     Elapsed.Time, Moving.Time, Speed.mph, Pace..mi, 
                     Max.Pace..mi, Cad, Heart, Max.Heart, Elev.Dist.ft.mi,
                     Elev.Time.ft.h)
#glimpse(strava)

## split `When` into Date and Time fields in Strava data
strava  %<>% separate(col = When, into = c("Date", "StartTime"), sep = " ") %>%
  mutate(Date = as.POSIXct(Date),
         StartTime = paste(substr(StartTime,1,6),"00",sep=""),
         Month = month(Date), # number,
         Day = day(Date),
         Weekday = weekdays(Date))
#str(strava)

## load garmin data
garmin <- read.csv("../Data/2017/garmin2017.csv", stringsAsFactors = F)

#glimpse(garmin)
#tail(garmin)
#summary(garmin)

## remove cols
keepCols_garmin <- c("Activity.Type","Date","Title","Distance","Calories","Time","Avg.HR","Max.HR",
                     "Avg.Cadence", "Max.Cadence","Avg.Pace", "Best.Pace", "Elev.Gain", "Elev.Loss",
                     "Avg.Stride.Length")
garmin <- garmin[keepCols_garmin]
#str(garmin)

## Specify new column names:
garmin  %<>% separate(col = Date, into = c("Date", "StartTime"), sep = " ") %>%
  mutate(Date = as.POSIXct(Date),
         StartTime = paste(substr(StartTime,1,6),"00",sep=""))#,
         #Month = month(Date), # number,
         #Day = day(Date),
         #Weekday = weekdays(Date))
#str(garmin)

## Sort from earliest to last date
garmin <- garmin[order(garmin$Date, decreasing = F),] 
strava <- strava[order(strava$Date, decreasing = F),]
#head(garmin$Date)
#head(strava$Date)
 
## remove excess runs
strava %<>% filter(Date >= "2017-07-17" & Date <= "2017-11-19")
garmin %<>% filter(Date >= "2017-07-17" & Date <= "2017-11-19")

## check row counts
nrow(garmin)
nrow(strava)

## mismach with missing watch vs manual entry
garmin$StartTime[c(43:45,47:51,110:113,127:130)] <- strava$StartTime[c(43:45,47:51,110:113,127:130)]

## get full dataset
fullData2 <- garmin %>% left_join(strava, by = c("Date","StartTime")) %>% 
  select(Name, Date, Month, Day, Weekday, StartTime, Distance, Time, Heart,
         Avg.HR, Max.HR, Cad, Avg.Cadence, Max.Cadence, Avg.Pace, Best.Pace, Elev.Gain, 
         Elev.Loss, Avg.Stride.Length)
#head(fullData2)

## check garmin runs 
#table(fullData2$Distance)  
#table(strava$Distance)  

## new run categories
fullData2$Name[150] <- "dress rehearsal GA" 

workouts <- c("tune","tempo","vo2","0","mp","race","lt")
extra <- c("warm","cool","mental","jog","hurt")
ml <- c("medium","middle")
ga <- c("gen","aer","ga","ge","short")

## create new variables to categorize runs
fullData3 <- fullData2 %>% 
  mutate(RunCat =   ifelse(grepl(paste(extra, collapse="|"),  tolower(Name)),'Misc',                
                    ifelse(grepl(paste(workouts, collapse="|"),  tolower(Name)),'Workout',
                    ifelse(grepl("marathon", tolower(Name)),"Race",
                    ifelse(round(Distance) > 15, "Long", 
                    ifelse(grepl(paste(ga, collapse="|"),  tolower(Name)), "GA",                           
                    ifelse(round(Distance) %in% c(1:6),
                          ifelse(grepl(paste(extra, collapse="|"),
                                       tolower(Name)), "Misc", "Recovery"),
                    ifelse(grepl("recovery",  tolower(Name)),"Recovery",                          
                    ifelse(round(Distance) %in% c(12:15), "ML",
                    ifelse(grepl(paste(ml, collapse="|"),  tolower(Name)), "ML", "GA"))))))))),
         
           RunType = ifelse(Distance > 25, "Race",
                     ifelse(Distance > 15, "Long", 
                     ifelse(grepl(paste(c("ML","GA"), collapse = "|"), RunCat), "Run", RunCat))),
            
           WorkoutType = ifelse(RunCat == "Workout",
                                ifelse(grepl(paste(c("tempo","lt"), collapse = "|"),
                                             tolower(Name)), "Tempo",
                                       ifelse(grepl("mp", tolower(Name)), "Marathon Pace",
                                       ifelse(grepl("tune", tolower(Name)), "Tune Up Race",
                                       ifelse(grepl(paste(c("vo","v0"), collapse = "|"),
                                                    tolower(Name)), "vO2 Max", "NA")))),
                      "NA"))

#glimpse(fullData3)

## fix total time
fullData3$Time <- as.character(fullData3$Time)

for (i in 1:nrow(fullData3)) {
  if (nchar(fullData3$Time[i]) == 4) {
    fullData3$Time[i] <- paste("0:0", fullData3$Time[i], sep="")
    #print(paste("00:0", fullData3$Time[i], sep=""))
    #print(fullData3$Time[i])
  } else if (nchar(fullData3$Time[i]) == 8) {
    fullData3$Time[i] <- paste("0:",substring(fullData3$Time[i],1,5), sep="")
    #print(paste("00:",substring(fullData3$Time[i],1,5), sep=""))
  } else if (nchar(fullData3$Time[i]) == 5) {
    fullData3$Time[i] <- paste("0:",fullData3$Time[i], sep="")
    #print(paste("00:",substring(fullData3$Time[i],1,5), sep=""))
  }  
}

## final cleaning
fullData4 <- fullData3 %>%
  # fix month + weekday ordering
  mutate(Month = factor(Month, ordered = T, levels = c(7:11),
                        labels = c("Jul","Aug","Sep","Oct","Nov")),
         Weekday = factor(Weekday, ordered = T, 
                          levels = c("Monday","Tuesday","Wednesday","Thursday",
                                     "Friday","Saturday","Sunday")),
         # factor run type
         RunCat = factor(RunCat),
         RunType = factor(RunType),
         WorkoutType = factor(WorkoutType),
         # fix times
         StartTime = as.POSIXct(StartTime, format = '%H:%M'),
         Time = as.POSIXct(Time, format = '%H:%M:%S'),
         # Convert average Heart rate, max heart rate, calories, and elevation gain to numeric
         Avg.HR = as.numeric(as.character(Avg.HR, stringsAsFactors = F)),
         Max.HR = as.numeric(as.character(Max.HR, stringsAsFactors = F)),
         Avg.Cadence = as.numeric(as.character(Avg.Cadence, stringsAsFactors = F)),
         Max.Cadence = as.numeric(as.character(Max.Cadence, stringsAsFactors = F)),
         Elev.Gain = as.numeric(as.character(Elev.Gain, stringsAsFactors = F)),
         Elev.Loss = as.numeric(as.character(Elev.Loss, stringsAsFactors = F)),
         # fix cadence from Strava
         Cad = Cad*2,
         # add week numbers of program
         Week = week(Date) - 28,
         # fix Average Pace
         Avg.Pace = as.POSIXct(Avg.Pace, format = '%M:%S'),
         Best.Pace = as.POSIXct(Best.Pace, format = '%M:%S'),
         # new column to specify marathon
         Marathon = "ph17") %>%
  select(Name, Marathon, Date, Month, Week, Day, Weekday, RunCat, RunType, WorkoutType, StartTime, 
         Time, Distance, Avg.HR, Max.HR, Avg.Cadence, Max.Cadence, Avg.Pace, Best.Pace,
         Elev.Gain, Elev.Loss, Avg.Stride.Length)
 
glimpse(fullData4)

## impute missing data points
library(mice)
simple <- fullData4[c("Week","Distance","Avg.HR","Max.HR","Avg.Cadence","Max.Cadence"
                      ,"Elev.Gain","Elev.Loss","Avg.Stride.Length")]

set.seed(144)
imputedDataFrame <- (complete(mice(simple)))

fullData4 %<>%
  mutate(Avg.HR = imputedDataFrame$Avg.HR,
         Max.HR = imputedDataFrame$Max.HR,
         Avg.Cadence = imputedDataFrame$Avg.Cadence,
         Max.Cadence = imputedDataFrame$Max.Cadence,
         Elev.Gain = imputedDataFrame$Elev.Gain,
         Elev.Loss = imputedDataFrame$Elev.Loss)
summary(fullData4)
#which(is.na(fullData4$Time))
#fullData4[c(which(is.na(fullData4$Time))),]
# fix missing Time values
fullData4$Time[c(which(is.na(fullData4$Time)))] <- c("2017-12-30 00:08:14 EST",
                                                     "2017-12-30 00:04:36 EST",
                                                     "2017-12-30 00:08:32 EST",
                                                     "2017-12-30 00:08:57 EST",
                                                     "2017-12-30 00:08:01 EST")
summary(fullData4)

## write data to file
write.csv(fullData4, file = "../Data/2017/cleanedMarathonTrainingData_philly17.csv",
          row.names = T)