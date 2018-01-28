library(tidyverse) # data cleaning and manipulation
library(ggplot2) # plotting
library(lubridate) # dates
library(RColorBrewer) # plotting colors
library(magrittr) # pipe operations
library(knitr) # kable()
library(gridExtra) # grid.arrange()

## load in data files for both races
philly17 <- read.csv("../../Data/2017/cleanedMarathonTrainingData_philly17.csv", stringsAsFactors = F)
philly16 <- read.csv("../../Data/2016/cleanedMarathonTrainingData_philly16.csv", stringsAsFactors = F)

## random missing month value?
philly16[17,"Month"] <- "Aug"

## combine into 1 dataset and remove redundant row
philly <- rbind(philly16,philly17)
philly %<>% select(-X) %>%
  mutate(MarathonName = if_else(Marathon == "ph16","Philly 2016","Philly 2017"),
         # make POSIXct again
         Time = as.POSIXct(Time),
         Avg.Pace = as.POSIXct(Avg.Pace),
         Best.Pace = as.POSIXct(Best.Pace),
         Date = as.POSIXct(Date),
         StartTime = as.POSIXct(StartTime),
         # make Month an ordered factor
         Month = factor(Month, ordered = TRUE, 
                        levels = c("Jul","Aug","Sep","Oct","Nov")))

## count total runs by marathon
philly %>%
  group_by(MarathonName) %>%
  summarize(Count = n()) %>%
  rename("Marathon Name" = MarathonName) %>%
  kable(caption = "Total Number of Runs")

## total miles by month histogram
ggplot(data = philly, aes(x = Distance)) + 
  geom_histogram(binwidth = 2, aes(fill = MarathonName), colour = "black", boundary = 2) +
  scale_fill_manual(values = c("red","#338dff")) + # custom colors
  theme_bw() + 
  scale_x_continuous(limits = c(0, 30)) +
  xlab("Distance (mi)") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Miles Ran in All Runs") + 
  guides(fill = F) +
  facet_grid(~ MarathonName)

## count of run types table
philly %>%
  group_by(MarathonName,RunCat) %>%
  summarize(Count = n()) %>%
  spread(key = RunCat, value = Count) %>%
  rename("Marathon Name" = MarathonName) %>%
  kable(caption = "Total Number of Runs by Run Type")

## misc runs
philly %>%
  filter(RunCat == "Misc") %>%
  select(Name,Date,RunCat) %>%
  rename("Run Name" = Name,
         "Run Category" = RunCat) %>%
  kable(caption = "Miscellaneous Runs")

## show missing runs in histogram by day
philly %>%
  filter(Marathon == "ph17") %>%
  ggplot(aes(Date, Distance)) + 
  geom_bar(stat="identity", fill = "#338dff", colour = "black") +
  theme_bw() + 
  xlab("Date") + 
  ylab("Miles") + 
  ggtitle("Runs Over Time: 2017") +
  guides(fill = F) +
  coord_cartesian(xlim = c(as.POSIXct("2017-09-05", format = "%Y-%m-%d"),
                           as.POSIXct("2017-10-25", format = "%Y-%m-%d")))

## summary stats
philly %>%
  group_by(MarathonName) %>%
  select(Distance) %>%
  summarize(Mean = mean(Distance), Median = median(Distance), StdDev = sd(Distance),
            Min = min(Distance), Max = max(Distance), Count = n()) %>%
  rename("Marathon Name" = MarathonName) %>%
  kable(caption = "Statistical Summaries of Training Blocks")

## total miles by month by marathon histogram
philly %>% 
  select(Month,MarathonName,Distance) %>%
  group_by(Month,MarathonName) %>% 
  summarise(sum = sum(Distance)) %>%
  ggplot(aes(x = Month, y = sum, fill = MarathonName)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  scale_fill_manual(values = c("red","#338dff")) + 
  theme_bw() + 
  xlab("") + 
  ylab("Total Miles") + 
  ggtitle("Total Miles by Month")

## count november runs
philly %>%
  filter(Month == "Nov") %>%
  group_by(MarathonName) %>%
  summarize(Count = n()) %>%
  rename("Marathon Name" = MarathonName) %>%
  kable(caption = "Total Number of Runs in November")

## avg. cadence by month
ggplot(data = philly, aes(Month, Avg.Cadence)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), outlier.colour = "black", alpha = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() + 
  xlab("") + 
  ylab("Avgerage Cadence (Steps/Min)") + 
  theme(legend.position = "none") + 
  ggtitle("Average Cadence by Month") + 
  facet_grid(~ MarathonName)

## 10 lowest average cadences
philly %>%
  filter(Month %in% c("Jul","Aug"),
         Marathon == "ph17") %>%
  select(Name,Date,Avg.Cadence) %>%
  arrange(Avg.Cadence) %>%
  head(10) %>%
  rename("Run Name" = Name,
         "Average Cadence" = Avg.Cadence) %>%
  kable(caption = "10 Runs with Lowest Average Cadence")

## avg. heart rate over time by marathon
hr16 <- philly %>%
  filter(Marathon == "ph16") %>%
  ggplot(data = , aes(Date, Avg.HR)) + 
  geom_line(aes(group=1, colour = "red")) +
  geom_smooth(method = "lm", se = T, colour = "red") + 
  theme(axis.text.x = element_blank(), legend.position = "none") +
  xlab("2016") + 
  ylab("Average Heart Rate (BPM)") + 
  ggtitle("Average Heart Rate Over Time") + 
  coord_cartesian(ylim = c(100, 160))

hr17 <- philly %>%
  filter(Marathon == "ph17") %>%
  ggplot(data = , aes(Date, Avg.HR)) + 
  geom_line(aes(group=1), colour = "#338dff") +
  geom_smooth(method = "lm", se = T, colour = "#338dff") + 
  theme(axis.text.x = element_blank(), legend.position = "none") +
  xlab("2017") + 
  ylab("Average Heart Rate (BPM)") + 
  coord_cartesian(ylim = c(100, 160))

grid.arrange(hr16, hr17, nrow = 2)

## avg heart rate by month
ggplot(data = philly, aes(Month, Avg.HR)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), outlier.colour = "black", alpha = 0.5) + 
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +  
  theme_bw() +
  theme(legend.position = "none") + 
  xlab("Month") + 
  ylab("Average Heart Rate (BPM)") + 
  ggtitle("Average Heart Rate per Month") +
  facet_grid(~ MarathonName)

## avg cadence by run type
philly %>%
  filter(RunType != "Race",
         RunType != "Misc") %>%
  ggplot(aes(RunType, Avg.Cadence)) + 
  geom_jitter(aes(colour = RunType)) +
  geom_boxplot(aes(fill = RunType), outlier.colour = "black", alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "none") + 
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  xlab("Run Type") + 
  ylab("Cadence (Steps/Min)") + 
  guides(fill=FALSE) +
  coord_flip() + 
  ggtitle("Cadence per Run Type") +
  facet_grid(~ MarathonName)

## average HR vs. average cadence
philly %>%
  filter(RunType != "Race",
         RunType != "Misc") %>%  
  ggplot(aes(Avg.Cadence, Avg.HR)) + 
  geom_point(aes(colour = RunType), na.rm = TRUE) +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +    
  theme_bw() +
  xlab("Cadence (Steps/Min)") + 
  ylab("Average Heart Rate (BPM)") + 
  scale_y_continuous(limits = c(110, 160)) +
  ggtitle("Average Heart Rate by Cadence") + 
  facet_grid(~ MarathonName)

##  avg heart rate by run type
philly %>%
  filter(RunType != "Race",
         RunType != "Misc") %>%
  ggplot(aes(RunType, Avg.HR)) + 
  geom_jitter(aes(colour = RunType)) +
  geom_boxplot(aes(fill = RunType), outlier.colour = "black", alpha = 0.5) +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +  
  theme_bw() +
  theme(legend.position = "none") + 
  xlab("Run Type") + 
  ylab("Average Heart Rate (BPM)") + 
  guides(fill=FALSE) +
  coord_flip() + 
  ggtitle("Average Heart Rate per Run Type") + 
  facet_grid(~ MarathonName)

## average pace by run type over time
philly %>%
  filter(RunType != "Race",
         RunType != "Misc") %>%
  ggplot(aes(x = as.POSIXct(paste(month(Date),'/',day(Date), sep=""),format="%m/%d"),
             y = as.numeric(format(Avg.Pace, "%M")) + as.numeric(format(Avg.Pace, "%S"))/60,
             colour = RunType)) + 
  scale_y_reverse(limits = c(9,7), breaks = c(9,8,7), labels = c("9:00","8:00","7:00")) +
  geom_point() +
  geom_smooth(fill = NA) +  
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +   
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  xlab("Time") + 
  ylab("Average Pace (mile/minute)") + 
  ggtitle("Average Pace Over Time") +
  facet_grid(~ MarathonName)

## elevation and avg hr
philly %>%
  filter(RunType != "Race",
         RunType != "Misc") %>%
  ggplot(aes(Elev.Gain, Avg.HR)) + 
  geom_point(aes(colour = RunType), na.rm = TRUE) +
  geom_smooth(fill = NA, na.rm = TRUE) +
  theme_bw() + 
  xlab("Elevation Gain (ft.)") + 
  ylab("Average Heart Rate (BPM)") +
  scale_y_continuous(limits = c(110, 160)) +
  ggtitle("Average Heart Rate by Elevation Gain") +
  facet_grid(~ MarathonName)

## elevation gain by distance
philly %>%
  filter(RunType != "Race",
         RunType != "Misc") %>%
  ggplot(aes(Elev.Gain,Distance)) + 
  geom_point(aes(color = MarathonName)) +
  geom_smooth(fill = NA, na.rm = TRUE) +
  scale_colour_manual(values = c("red","#338dff")) + 
  theme_bw() + 
  theme(legend.position = "none") +
  xlab("Distance (mi)") + 
  ylab("Elevation Gain (ft.)") +
  ggtitle("Elevation Gain by Total Distance") +
  facet_grid(~ MarathonName)