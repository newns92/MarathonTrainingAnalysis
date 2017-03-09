#install.packages("stringr")  - for manipulating string values
#install.packages("tidyr")    - data cleansing
#install.packages("plyr")     - for renaming columns

setwd("C:/Users/snewns/Dropbox/RunningAnalysis/R_Backups")
#setwd("C:/Users/Nimz/Dropbox/RunningAnalysis/R_Backups")

runs <- read.csv("cleanedMarathonTrainingData.csv")
str(runs)

#remove 1st col
runs$X <- NULL
head(runs,2)
#summary(runs)

#can't save POSIXct into CSV, must change here
runs$Time <- as.POSIXct(runs$Time)#, format = '%H:%M:%S')
runs$Avg.Pace <- as.POSIXct(runs$Avg.Pace)#, format = '%H:%M:%S')
str(runs)

#can't save ordered factors in CSV, must change here
runs$Month <- factor(runs$Month, ordered = TRUE, levels = c("Jul","Aug","Sep","Oct","Nov"))
class(runs$Month)

#load ggplot2
library(ggplot2)

#distance histogram
ggplot(data = runs, aes(x = Distance)) + 
  geom_histogram(binwidth = 2, aes(fill = ..count..)) + 
  xlab("Distance (mi)") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Miles Ran in All Runs") + 
  #labs(caption="*Majority of runs were between 9 and 11 miles, 
    #   with a suprisingly low number of runs between 7 and 9 miles")

#check data
tapply(runs$Distance,runs$Month,sum)

#tapply returns vectors, aggregate return DF
#hrByMonth <- aggregate(runs$Avg.HR,list(runs$Month),median)
#names(hrByMonth) <- c("MonthName", "MedianHR")

#runs <- merge(runs, hrByMonth, by.x = "Month", by.y = "MonthName")
#names(runs)[names(runs)=="MedianHR"] <- "monthlyMedianHr"
#str(runs)

#graph table above --> miles by month bars
ggplot(data = runs, aes(x = Month, y = Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  #geom_line(aes(x = Month, y = monthlyMedianHr), group = 1, size = 2) +
  xlab("Month") + 
  ylab("Total Miles") + 
  #coord_flip() + 
  #scale_x_discrete(limits = rev(levels(runs$Month))) +
  #geom_text(vjust=0, colour="red") +
  ggtitle("Sum of Miles by Month") + 
  labs(caption="*The largest number of miles ran, 308 miles, was in September")

#boxplot(Cad ~ monthNum, data = runs)

#avg. cadence grouped by month
ggplot(data = runs, aes(Month, Cad)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), 
               outlier.colour = "black", alpha = 0.5) + #geom_jitter()
  xlab("Month") + 
  ylab("Cadence") + 
  ggtitle("Cadence per Month") + 
  labs(caption="*Cadence generally increases month over month, due to more workouts, or improved form?")

#avg. HR grouped by month
ggplot(data = runs, aes(Month, Avg.HR)) + 
  geom_jitter(aes(colour = Month)) +
  geom_boxplot(aes(fill = Month), 
               outlier.colour = "black", alpha = 0.5) + #geom_jitter()
  xlab("Month") + 
  ylab("Cadence") + 
  ggtitle("Cadence per Month") + 
  labs(caption="*HR generally decreases month over month --> increased fitness or cooler weather?")
#tbl <- tapply(runs$Avg.HR,runs$Month,median)

#average heart rate over plan line
ggplot(data = runs, aes(x = Date, y = Avg.HR)) + 
  geom_line(aes(group=1)) +
  theme(axis.text.x = element_blank()) +
  xlab("Time") + 
  ylab("Average Heart Rate") + 
  ggtitle("Average Heart Rate Over Time") + 
  labs(caption="*Very slight general decrease, with outliers of about 100 in the 1st third of the plan and 
70 in the last 3rd of the plan")

#average pace over plan line
ggplot(data = runs, aes(x = Date, y = Avg.Pace)) + 
  geom_point() + # by color? 
  #?stat_smooth() + 
  theme(axis.text.x = element_blank()) +
  xlab("Time") + 
  ylab("Average Heart Rate") + 
  ggtitle("Average Heart Rate Over Time") + 
  labs(caption="*Very slight general decrease, but not linear")

#scatter plot of total times by distnace
#ggplot(data = runs, aes(x = Distance, y = Time)) + geom_point()
  #geom_line() +
  #theme(axis.text.x = element_text(size=0, angle=45)) +
  #xlab("Time") 
#graph table above --> miles by month bars

#medianCadence <- aggregate(runs$Cad,list(runs$RunType),median)
#runs <- merge(runs, medianCadence, by.x = "RunType", by.y = "Group.1")
#names(runs)[names(runs)=="x"] <- "medianCadence"


ggplot(data = runs, aes(x=RunType, y=Cad, fill = RunType)) + 
  geom_boxplot(alpha = 0.5) + 
  geom_jitter(aes(colour = RunType)) +
    #stat_summary(fun.y="median", geom="bar", alpha = 0.1) + 
  expand_limits(y = 130) + 
  xlab("Run Type") + 
  ylab("Cadence") + 
  coord_flip() + 
  ggtitle("Cadence by Run Type") + 
  labs(caption="*The actual marathon had the highest cadence, which makes sense. Workouts had widest variety - 
       inspect more, recovery runs abit slower, but less variant, long runs second slowest")


