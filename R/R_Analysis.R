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

?barplot(sum(runs$Distance),runs$monthNum)

ggplot(data = runs, aes(x = Distance)) + geom_histogram(binwidth = 2)

sum(runs$Distance)