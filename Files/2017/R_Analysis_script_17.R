library(tidyverse)
library(magrittr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

phil17 <- read.csv("../Data/cleanedMarathonTrainingData2017.csv", stringsAsFactors = F)
str(phil17)

# remove 1st cols
phil17 <- phil17 %>%
  mutate(X = NULL)
glimpse(phil17)

# make POSIXct again
phil17 %<>%
  mutate(Time = as.POSIXct(Time),
         Avg.Pace = as.POSIXct(Avg.Pace),
         Best.Pace = as.POSIXct(Best.Pace),
         Date = as.POSIXct(Date),
         StartTime = as.POSIXct(StartTime, format = '%H:%M'))

str(phil17[,c("Time","Avg.Pace","Date","StartTime")])

# make Month an ordered factor
phil17 <- phil17 %>%
  mutate(Month = factor(Month, ordered = TRUE, levels = c("Jul","Aug","Sep","Oct","Nov")))

#simple histogram of how many miles I ran in each of my phil17.
#distance histogram
ggplot(phil17, aes(Distance)) + 
  geom_histogram(binwidth = 2, aes(fill = ..count..), colour = "black", boundary = 2) +
  scale_x_continuous(limits=c(0, 30)) +
  xlab("Distance (mi)") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Miles Ran in All Runs") + 
  guides(fill=FALSE) 

summary(phil17$Distance)

#miles by month bars
ggplot(phil17, aes(Month, Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  xlab("") + 
  ylab("Total Miles") + 
  guides(fill=FALSE) +
  ggtitle("Total Miles by Month")

# peak week bars
ggplot(phil17, aes(Week, Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  xlab("Week of Plan") + 
  ylab("Total Miles") + 
  ggtitle("Total Miles by Week of Plan")
                                       
# find peak week
phil17 %>%
  group_by(Week) %>%
  summarize(Miles = sum(Distance)) %>%
  filter(Week %in% c(11,13))

#avg. cadence grouped by month
ggplot(phil17, aes(Month, Avg.Cadence)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), outlier.colour = "black", alpha = 0.5) +
  xlab("") + 
  ylab("Cadence (Steps/Min)") + 
  guides(fill=FALSE) +
  ggtitle("Cadence by Month") 

#heart rate over time
ggplot(phil17, aes(Month, Avg.HR)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), outlier.colour = "black", alpha = 0.5) + 
  xlab("Month") + 
  ylab("Avg. Heart Rate") + 
  ggtitle("Average Heart Rate per Month") 

#avg. HR grouped by month
ggplot(phil17, aes(Month, Avg.HR)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), 
  outlier.colour = "black", alpha = 0.5) + #geom_jitter()
  xlab("Month") + 
  ylab("Avg. Heart Rate") + 
  ggtitle("Average Heart Rate per Month") 

#avg. cadence grouped by run type
ggplot(phil17, aes(RunType, Avg.Cadence)) + 
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
ggplot(phil17, aes(Avg.Cadence, Avg.HR)) + 
  geom_point(aes(colour = RunType), na.rm = TRUE) +
  #geom_smooth() +
  xlab("Cadence") + 
  ylab("Average Heart Rate") + 
  scale_y_continuous(limits = c(110, 160)) +
  ggtitle("Average Heart Rate by Cadence")

#avg. hr grouped by run type
ggplot(phil17, aes(RunType, Avg.HR)) + 
  geom_jitter(aes(colour = RunType)) +
  geom_boxplot(aes(fill = RunType), outlier.colour = "black", alpha = 0.5) +
  xlab("Run Type") + 
  ylab("Avg Heart Rate") + 
  guides(fill=FALSE) +
  coord_flip() + 
  ggtitle("Average Heart Rate per Run Type") 

#max heart rate
phil17[which.max(phil17$Avg.HR),c("Name","Date","Distance","Avg.HR","Max.HR")]

#avg pace by phil17
ggplot(phil17, aes(Date, Avg.Pace, colour = RunType)) + 
  geom_point() +
  geom_smooth(fill = NA) +  
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  xlab("Date") + 
  ylab("Average Pace (mile/minute)") + 
  ggtitle("Average Pace Over Time") + 
  labs(caption="")

#avg pace by phil17 - seperate
ggplot(data = phil17, aes(Date, Avg.Pace, colour = RunType)) + 
  geom_point() +
  geom_smooth(fill = NA) +  
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  xlab("Date") + 
  ylab("Average Pace (mile/minute)") + 
  ggtitle("Average Pace Over Time") + 
  labs(caption="") + 
  facet_grid(RunType~.)

table(phil17$RunType)

#bar of steps, echo = FALSE}
ggplot(phil17, aes(RunType, 
                   (Avg.Cadence*((hour(phil17$Time)*60)+(minute(phil17$Time))+
                                   (second(phil17$Time)/60))))) + 
  geom_bar(aes(fill = RunType), stat = "identity") +
  xlab("Run Type") + 
  ylab("Steps") + 
  guides(fill=FALSE) +
  ggtitle("Total Steps Taken by Run Type") 

#elevation and avg hr
ggplot(phil17, aes(Elev.Gain, Avg.HR)) + 
  geom_point(aes(colour = RunType), na.rm = TRUE) +
  geom_smooth(fill = NA, na.rm = TRUE) +
  xlab("Elevation Gain (ft.)") + 
  ylab("Average Heart Rate (BPM)") +
  scale_y_continuous(limits = c(110, 160)) +
  ggtitle("Average Heart Rate by Elevation Gain")

#elevation and max hr, echo = FALSE}
ggplot(phil17, aes(RunType, Max.HR)) + 
  geom_jitter(aes(colour = RunType)) +
  geom_boxplot(aes(fill = RunType), outlier.colour = "black", alpha = 0.5) +
  xlab("Run Type") + 
  ylab("Max Heart Rate (BPM)") + 
  ggtitle("Max Heart Rate by Run Type")