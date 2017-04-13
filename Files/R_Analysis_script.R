#install.packages("stringr")  - for manipulating string values
#install.packages("tidyr")    - data cleansing
#install.packages("plyr")     - for renaming columns


runs <- read.csv("cleanedMarathonTrainingData.csv")
str(runs)

#remove 1st col
runs$X <- NULL
head(runs,2)

library(lubridate)

#make POSIXct
runs$Time <- as.POSIXct(runs$Time)#, format = '%H:%M:%S')
runs$Avg.Pace <- as.POSIXct(runs$Avg.Pace)#, format = '%H:%M:%S')
runs$Date <- as.POSIXct(runs$Date)
runs$StartTime <- hour(strptime(runs$StartTime, format = '%H:%M'))

#make Month an ordered 
runs$Month <- factor(runs$Month, ordered = TRUE, levels = c("Jul","Aug","Sep","Oct","Nov"))

#simple histogram of how many miles I ran in each of my runs.

library(ggplot2)
library(RColorBrewer)

#distance histogram
ggplot(data = runs, aes(x = Distance)) + 
  geom_histogram(binwidth = 2, aes(fill = ..count..), colour = "black", boundary = 2) +
  scale_x_continuous(limits=c(0, 30)) +
  xlab("Distance (mi)") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Miles Ran in All Runs") + 
  guides(fill=FALSE) 

summary(runs$Distance)

#miles by month bars
ggplot(data = runs, aes(x = Month, y = Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  xlab("") + 
  ylab("Total Miles") + 
  guides(fill=FALSE) +
  ggtitle("Total Miles by Month")

#peak week bars
ggplot(data = runs, aes(x = weekNumber, y = Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  xlab("Week of Plan") + 
  ylab("Total Miles") + 
  #guides(fill=FALSE) +
  ggtitle("Total Miles by Week of Plan")
                                       
#find peak week
runs[runs$weekNumber == 11,c('Name','weekNumber','Date','DOW','Distance')]

#avg. cadence grouped by month
ggplot(data = runs, aes(Month, Cad)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), 
    outlier.colour = "black", alpha = 0.5) + #geom_jitter()
  xlab("") + 
  ylab("Cadence (Steps/Min)") + 
  guides(fill=FALSE) +
  ggtitle("Cadence by Month") 

#heart rate over time
ggplot(data = runs, aes(x = Date, y = Avg.HR)) + 
  geom_line(aes(group=1)) +
  theme(axis.text.x = element_blank()) +
  xlab("Time") + 
  ylab("Average Heart Rate") + 
  ggtitle("Average Heart Rate Over Time")

#avg. HR grouped by month
ggplot(data = runs, aes(Month, Avg.HR)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), 
  outlier.colour = "black", alpha = 0.5) + #geom_jitter()
  xlab("Month") + 
  ylab("Avg. Heart Rate") + 
  ggtitle("Average Heart Rate per Month") 

#avg. cadence grouped by run type
ggplot(data = runs, aes(RunType, Cad)) + 
  geom_jitter(aes(colour = RunType)) +
  geom_boxplot(aes(fill = RunType), 
               outlier.colour = "black", alpha = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  xlab("Run Type") + 
  ylab("Cadence") + 
  guides(fill=FALSE) +
  coord_flip() + 
  ggtitle("Cadence per Run Type") 

#cadence by hr
ggplot(data = runs, aes(x=Cad, y=Avg.HR)) + 
  geom_point(aes(colour = RunType), na.rm = TRUE) +
  xlab("Cadence") + 
  ylab("Average Heart Rate") + 
  scale_y_continuous(limits = c(110, 160)) +
  ggtitle("Average Heart Rate by Cadence")

#avg. hr grouped by run type
ggplot(data = runs, aes(RunType, Avg.HR)) + 
  geom_jitter(aes(colour = RunType)) +
  geom_boxplot(aes(fill = RunType), 
  outlier.colour = "black", alpha = 0.5) +
  xlab("Run Type") + 
  ylab("Avg Heart Rate") + 
  guides(fill=FALSE) +
  ggtitle("Average Heart Rate per Run Type") 

#max heart rate
runs[which.max(runs$Avg.HR),]

#avg pace by runs
gplot(data = runs, aes(x = Date, y = Avg.Pace, colour = RunType)) + 
  geom_point() +
  geom_smooth(fill = NA) +  
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  xlab("Date") + 
  ylab("Average Pace (mile/minute)") + 
  ggtitle("Average Pace Over Time") + 
  labs(caption="")

#avg pace by runs - seperate
ggplot(data = runs, aes(x = Date, y = Avg.Pace, colour = RunType)) + 
  geom_point() +
  geom_smooth(fill = NA) +  
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  xlab("Date") + 
  ylab("Average Pace (mile/minute)") + 
  ggtitle("Average Pace Over Time") + 
  labs(caption="") + 
  facet_grid(RunType~.)

table(runs$RunType)

#bar of steps, echo = FALSE}
ggplot(data = runs, aes(RunType, (Cad*((hour(runs$Time)*60)+(minute(runs$Time))+(second(runs$Time)/60))))) + 
  geom_bar(aes(fill = RunType), stat = "identity") +
  xlab("Run Type") + 
  ylab("Steps") + 
  guides(fill=FALSE) +
  ggtitle("Total Steps Taken by Run Type") 

#elevation and avg hr
ggplot(data = runs, aes(x=Elevation.Gain, y=Avg.HR)) + 
  geom_point(aes(colour = RunType), na.rm = TRUE) +
  geom_smooth(fill = NA, na.rm = TRUE) +
  xlab("Elevation Gain (ft.)") + 
  ylab("Average Heart Rate (BPM)") +
  scale_y_continuous(limits = c(110, 160)) +
  ggtitle("Average Heart Rate by Elevation Gain")

#elevation and max hr, echo = FALSE}
ggplot(data = runs, aes(x=RunType, y=Max.HR)) + 
  geom_jitter(aes(colour = RunType)) +
  geom_boxplot(aes(fill = RunType), outlier.colour = "black", alpha = 0.5) +
  xlab("Run Type") + 
  ylab("Max Heart Rate (BPM)") + 
  ggtitle("Max Heart Rate by Run Type")