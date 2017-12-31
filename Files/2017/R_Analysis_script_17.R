library(tidyverse)
library(magrittr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

philly17 <- read.csv("../Data/2017/cleanedMarathonTrainingData_philly17.csv", stringsAsFactors = F)
philly16 <- read.csv("../Data/2016/cleanedMarathonTrainingData_philly16.csv", stringsAsFactors = F)
str(philly17)
str(philly16)


philly17 <- philly17 %>%
  mutate(# remove 1st col
         X = NULL,
         # make POSIXct again
         Time = as.POSIXct(Time),
         Avg.Pace = as.POSIXct(Avg.Pace),
         Best.Pace = as.POSIXct(Best.Pace),
         Date = as.POSIXct(Date),
         StartTime = as.POSIXct(StartTime),
         # make Month an ordered factor
         Month = factor(Month, ordered = TRUE, 
                        levels = c("Jul","Aug","Sep","Oct","Nov")))

#glimpse(philly17)

philly16 <- philly16 %>%
  mutate(# remove 1st col
    X = NULL,
    # make POSIXct again
    Time = as.POSIXct(Time),
    Avg.Pace = as.POSIXct(Avg.Pace),
    Best.Pace = as.POSIXct(Best.Pace),
    Date = as.POSIXct(Date),
    StartTime = as.POSIXct(StartTime),
    # make Month an ordered factor
    Month = factor(Month, ordered = TRUE, 
                   levels = c("Jul","Aug","Sep","Oct","Nov")))
philly16$Month[17] <- "Aug"

philly <- rbind(philly16,philly17)

# simple histogram of how many miles I ran in each of my philly17.
#distance histogram
ggplot(philly, aes(Distance)) + 
  geom_histogram(binwidth = 2, aes(fill = ..count..), colour = "black", boundary = 2) +
  scale_x_continuous(limits=c(0, 30)) +
  xlab("Distance (mi)") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Miles Ran in All Runs") + 
  guides(fill=FALSE) +
  facet_grid(~Marathon)

summary(philly17$Distance)
summary(philly16$Distance)

#miles by month bars
ggplot(philly, aes(Month, Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  xlab("") + 
  ylab("Total Miles") + 
  guides(fill=FALSE) +
  ggtitle("Total Miles by Month") + 
  facet_grid(~ Marathon)

# peak week bars
ggplot(philly, aes(Week, Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  xlab("Week of Plan") + 
  ylab("Total Miles") + 
  ggtitle("Total Miles by Week of Plan") + 
  facet_grid(~ Marathon)
                                       
# find peak week
philly %>%
  filter(Marathon == "ph17") %>%
  group_by(Week) %>%
  summarize(Miles = sum(Distance)) %>%
  filter(Week %in% c(9:16))
philly %>%
  filter(Marathon == "ph16") %>%
  group_by(Week) %>%
  summarize(Miles = sum(Distance)) %>%
  filter(Week %in% c(9:16))

#avg. cadence grouped by month
ggplot(philly17, aes(Month, Avg.Cadence)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), outlier.colour = "black", alpha = 0.5) +
  xlab("") + 
  ylab("Cadence (Steps/Min)") + 
  guides(fill=FALSE) +
  ggtitle("Cadence by Month") 

#heart rate over time
ggplot(philly17, aes(Month, Avg.HR)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), outlier.colour = "black", alpha = 0.5) + 
  xlab("Month") + 
  ylab("Avg. Heart Rate") + 
  ggtitle("Average Heart Rate per Month") 

#avg. HR grouped by month
ggplot(philly17, aes(Month, Avg.HR)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), 
  outlier.colour = "black", alpha = 0.5) + #geom_jitter()
  xlab("Month") + 
  ylab("Avg. Heart Rate") + 
  ggtitle("Average Heart Rate per Month") 

#avg. cadence grouped by run type
ggplot(philly17, aes(RunType, Avg.Cadence)) + 
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
ggplot(philly17, aes(Avg.Cadence, Avg.HR)) + 
  geom_point(aes(colour = RunType), na.rm = TRUE) +
  #geom_smooth() +
  xlab("Cadence") + 
  ylab("Average Heart Rate") + 
  scale_y_continuous(limits = c(110, 160)) +
  ggtitle("Average Heart Rate by Cadence")

#avg. hr grouped by run type
ggplot(philly17, aes(RunType, Avg.HR)) + 
  geom_jitter(aes(colour = RunType)) +
  geom_boxplot(aes(fill = RunType), outlier.colour = "black", alpha = 0.5) +
  xlab("Run Type") + 
  ylab("Avg Heart Rate") + 
  guides(fill=FALSE) +
  coord_flip() + 
  ggtitle("Average Heart Rate per Run Type") 

#max heart rate
philly17[which.max(philly17$Avg.HR),c("Name","Date","Distance","Avg.HR","Max.HR")]

#avg pace by philly17
ggplot(philly17, aes(Date, Avg.Pace, colour = RunType)) + 
  geom_point() +
  geom_smooth(fill = NA) +  
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  xlab("Date") + 
  ylab("Average Pace (mile/minute)") + 
  ggtitle("Average Pace Over Time") + 
  labs(caption="")

#avg pace by philly17 - seperate
ggplot(data = philly17, aes(Date, Avg.Pace, colour = RunType)) + 
  geom_point() +
  geom_smooth(fill = NA) +  
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  xlab("Date") + 
  ylab("Average Pace (mile/minute)") + 
  ggtitle("Average Pace Over Time") + 
  labs(caption="") + 
  facet_grid(RunType~.)

table(philly17$RunType)

#bar of steps, echo = FALSE}
ggplot(philly17, aes(RunType, 
                   (Avg.Cadence*((hour(philly17$Time)*60)+(minute(philly17$Time))+
                                   (second(philly17$Time)/60))))) + 
  geom_bar(aes(fill = RunType), stat = "identity") +
  xlab("Run Type") + 
  ylab("Steps") + 
  guides(fill=FALSE) +
  ggtitle("Total Steps Taken by Run Type") 

#elevation and avg hr
ggplot(philly17, aes(Elev.Gain, Avg.HR)) + 
  geom_point(aes(colour = RunType), na.rm = TRUE) +
  geom_smooth(fill = NA, na.rm = TRUE) +
  xlab("Elevation Gain (ft.)") + 
  ylab("Average Heart Rate (BPM)") +
  scale_y_continuous(limits = c(110, 160)) +
  ggtitle("Average Heart Rate by Elevation Gain")

#elevation and max hr, echo = FALSE}
ggplot(philly17, aes(RunType, Max.HR)) + 
  geom_jitter(aes(colour = RunType)) +
  geom_boxplot(aes(fill = RunType), outlier.colour = "black", alpha = 0.5) +
  xlab("Run Type") + 
  ylab("Max Heart Rate (BPM)") + 
  ggtitle("Max Heart Rate by Run Type")