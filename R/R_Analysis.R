#install.packages("stringr")  - for manipulating string values
#install.packages("tidyr")    - data cleansing
#install.packages("plyr")     - for renaming columns

setwd("C:/Users/snewns/Dropbox/RunningAnalysis/Data")
#setwd("C:/Users/Nimz/Dropbox/RunningAnalysis/Data")

runs <- read.csv("cleanedMarathonTrainingData.csv")
#str(runs)

#remove 1st col
runs$X <- NULL
#str(runs)
#summary(runs)

#put month name labels to monthNum to display names in numerical in order
runs$monthNum <- factor(runs$monthNum,labels = unique(runs$Month[order(runs$monthNum)]))

#load ggplot2
library(ggplot2)

#distance histogram
ggplot(data = runs, aes(x = Distance)) + 
  geom_histogram(binwidth = 2, aes(fill = ..count..)) + 
  xlab("Distance (mi)") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Miles Ran in All Runs") + 
  labs(caption="*Majority of runs were between 9 and 11 miles, with a suprisingly low number of runs between 7 and 9 miles")

#tapply(runs$Distance,runs$Month,sum)
#Aug    Jul    Nov    Oct    Sep 
#251.50 118.08 168.44 304.45 308.14

#ggplot(data = runs, aes(x = Month, y = Distance, fill = Month)) + 
 # geom_bar(stat="identity") +
 # xlab("Month") + 
#  ylab("Total Miles") + 
#  ggtitle("Sum of Miles by Month") + 
 # labs(caption="*The largest number of miles ran, 308 miles, was in September")
#most miles in September, not October, still more in November than in June

ggplot(data = runs, aes(x = monthNum, y = Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  xlab("Month") + 
  ylab("Total Miles") + 
  coord_flip() + 
  #geom_text(vjust=0, colour="red") +
  ggtitle("Sum of Miles by Month") + 
  labs(caption="*The largest number of miles ran, 308 miles, was in September")

#boxplot(Cad ~ monthNum, data = runs)

ggplot(data = runs, aes(monthNum, Cad)) + 
  geom_boxplot(aes(fill = factor(runs$Month, levels = runs$Month[order(runs$monthNum)], ordered = TRUE)), 
               outlier.colour = "red") + #geom_jitter()
  xlab("Month") + 
  ylab("Cadence") + 
  ggtitle("Cadence per Month") + 
  labs(caption="*Cadence generally increases month over month, due to more workouts, or improved form?")

ggplot(data = runs, aes(x = Date, y = Avg.HR)) + 
  geom_line(aes(group=1)) +
  theme(axis.text.x = element_text(size=0, angle=45)) +
  xlab("Time") + 
  ylab("Average Heart Rate") + 
  ggtitle("Average Heart Rate Over Time") + 
  labs(caption="*Loosk like a very slight general decrease, with an outlier of about 100 in the 1st third of the plan
      and 70 in the last 3rd of the plan")
 
  
  
  
  
  
table(mean(runs$Avg.HR),runs$monthNum)

ggplot(data = runs, aes(x = monthNum, y = mean(Avg.HR), fill = Month)) + 
    geom_bar(stat="identity") +
    xlab("Month") + 
    ylab("Total Miles") + 
    #geom_text(vjust=0, colour="red") +
    ggtitle("Sum of Miles by Month") + 
    labs(caption="*The largest number of miles ran, 308 miles, was in September")



head(runs[sort(runs$Cad),])

as.numeric(runs$Cad)

as.date(runs$Avg.Speed.Avg.Pace.,'m:s')
as.Date.(runs$Avg.Speed.Avg.Pace.,'m:s')

sum(runs$Distance)

ggplot(data = runs, aes(x = monthNum2, y = Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  xlab("Month") + 
  ylab("Total Miles") + 
  ggtitle("Sum of Miles by Month") + 
  labs(caption="*The largest number of miles ran, 308 miles, was in September")