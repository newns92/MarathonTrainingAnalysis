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


library(ggplot2)

#?barplot(sum(runs$Distance),runs$monthNum)

ggplot(data = runs, aes(x = Distance)) + geom_histogram(binwidth = 2,aes(fill = ..count..))
#majority of miles between 9-11 miles

ggplot(data = runs, aes(x = monthNum, y = Distance)) + geom_bar(stat="identity")
#most miles in August, not OCtober, still more in November than in June


as.date(runs$Avg.Speed.Avg.Pace.,'m:s')
as.Date.(runs$Avg.Speed.Avg.Pace.,'m:s')

sum(runs$Distance)