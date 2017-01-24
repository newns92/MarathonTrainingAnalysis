#install.packages("stringr")  - for manipulating string values
#install.packages("tidyr")    - data cleansing
#install.packages("plyr")     - for renaming columns

setwd("C:/Users/snewns/Dropbox/RunningAnalysis/Data")
#setwd("C:/Users/Nimz/Dropbox/RunningAnalysis/Data")

runs <- read.csv("cleanedMarathonTrainingData.csv")
str(runs)

#remove 1st col
runs$X <- NULL
str(runs)
summary(runs)


library(ggplot2)

#?barplot(sum(runs$Distance),runs$monthNum)

ggplot(data = runs, aes(x = Distance)) + 
  geom_histogram(binwidth = 2, aes(fill = ..count..)) + 
  xlab("Distance (mi)") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Miles Ran in All Runs") + 
  labs(caption="*Majority of runs were between 9 and 11 miles, with a suprisingly low number of runs between 7 and 9 miles")

tapply(runs$Distance,runs$Month,sum)
#Aug    Jul    Nov    Oct    Sep 
#251.50 118.08 168.44 304.45 308.14

ggplot(data = runs, aes(x = monthNum, y = Distance, fill = monthNum)) + 
  geom_bar(stat="identity") +
  xlab("Month") + 
  ylab("Total Miles") + 
  ggtitle("Sum of Miles by Month") + 
  labs(caption="*The largest number of miles ran, 308 miles, was in September")
#most miles in August, not October, still more in November than in June


#boxplot(Cad ~ monthNum, data = runs)

ggplot(data = runs, aes(factor(monthNum), Cad)) + 
  geom_boxplot(aes(fill = factor(monthNum)), outlier.colour = "red") + #geom_jitter()
  xlab("Month") + 
  ylab("Cadence") + 
  ggtitle("Cadence per Month") + 
  labs(caption="*Cadence increases month over month, due to more workouts, or improved form?")

?sort(runs$Cad)

as.date(runs$Avg.Speed.Avg.Pace.,'m:s')
as.Date.(runs$Avg.Speed.Avg.Pace.,'m:s')

sum(runs$Distance)