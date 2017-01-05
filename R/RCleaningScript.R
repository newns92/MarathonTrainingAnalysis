setwd("C:/Users/snewns/Dropbox/RunningAnalysis/Data")

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


##sOME DATA CLEANING --> REMOVE 2 ROWS AT TOP OF STRAVA FILES

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
str(stravaFull)
summary(stravaFull)
#multiple runs on August 12? Maybe a bike ride?
#check type in newGarmin

install.packages("stringr")
library(stringr)
newGarmin$Date <- str_split_fixed(newGarmin$When, " ", 2)[,1]
newGarmin$StartTime <- str_split_fixed(newGarmin$When, " ", 2)[,2]

which(newGarmin$Date == '9/12/2016')
newGarmin[68:71,]
#looks like I just ran twice that day. back to Strava
head(stravaFull)

install.packages("tidyr")
library(tidyr)
# specify the new column names:
vars <- c("Date", "StartTime")
# then separate the "Details" column according to regex and drop extra columns:
?separate(stravaFull, Start, into = vars, sep = "[d]:", extra = "drop", remove = TRUE)
#    ID      Description    gn            os
#1 id_1 box1_homodomain  box1  homo sapiens 
#2 id_2   sox2_plurinet   plu  mus musculus

stravaFull$Date <- str_split_fixed(stravaFull$Start, "2016 ", 2)[,1]
stravaFull$StartTime <- str_split_fixed(stravaFull$When, " ", 2)[,2]
stravaFull[order(stravaFull$Start, decreasing = TRUE),]


keepCols <- NA
keepCols <- c("Activity.Id", "When", "Type", "Gear", "Name", "Dist.mi", "Elv.ft", "Elapsed.Time", "Moving.Time",
              "Speed.mph", "Pace..mi", "Max.Pace..mi", "Cad", "Heart", "Max.Heart", "Elev.Dist.ft.mi",
              "Elev.Time.ft.h", "Cal", "Segs", "PRs", "Kudos")