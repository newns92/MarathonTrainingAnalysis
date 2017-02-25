Marathon Training Plan Data Analysis
================
Steve Newns
February 24, 2017

*Initial Data Inspection*
=========================

Strava Data File
----------------

load in the CSV file containing the Strava data file created by and downloaded from **VeloViewer.com** and check out the dataset.

``` r
runs <- read.csv("cleanedMarathonTrainingData.csv")
str(runs)
```

    ## 'data.frame':    117 obs. of  19 variables:
    ##  $ X                  : int  12 15 94 4 6 8 13 16 95 5 ...
    ##  $ ID                 : int  646029304 647216014 648410753 649445500 650512799 651933678 653912805 655096239 656203325 657259723 ...
    ##  $ Name               : Factor w/ 13 levels "10k Tune Up Run",..: 2 7 10 3 10 4 3 7 10 7 ...
    ##  $ Gear               : Factor w/ 4 levels "ASICS dunno Black, Yellow, Red",..: 2 2 1 2 1 2 2 2 1 2 ...
    ##  $ Date               : Factor w/ 116 levels "2016-07-19","2016-07-20",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Month              : Factor w/ 5 levels "Aug","Jul","Nov",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ monthNum           : int  7 7 7 7 7 7 7 7 7 7 ...
    ##  $ DOW                : Factor w/ 7 levels "Fri","Mon","Sat",..: 6 7 5 1 3 4 6 7 5 1 ...
    ##  $ StartTime          : Factor w/ 90 levels "10:01","10:09",..: 58 51 67 46 71 76 46 37 64 36 ...
    ##  $ Distance           : num  9.01 11 4 11 6 ...
    ##  $ Time               : Factor w/ 115 levels "1:00:53","1:02:08",..: 4 23 81 28 98 60 13 41 72 37 ...
    ##  $ Avg.Speed.Avg.Pace.: Factor w/ 68 levels "6:32","6:54",..: 13 32 58 38 55 39 33 41 48 49 ...
    ##  $ Speed.mph          : num  3.72 3.54 3.29 3.49 3.33 ...
    ##  $ Cad                : num  84.2 84.2 85.6 88 86.9 85 82.9 87.4 85.8 85.1 ...
    ##  $ Elevation.Gain     : Factor w/ 92 levels "--","0","13",..: 37 56 1 58 15 80 48 61 1 60 ...
    ##  $ Avg.HR             : Factor w/ 39 levels "--","100","113",..: 34 35 24 35 17 37 35 31 19 34 ...
    ##  $ Max.HR             : Factor w/ 49 levels "--","118","122",..: 37 30 19 31 25 35 29 28 13 30 ...
    ##  $ Calories           : Factor w/ 113 levels "1,018","1,021",..: 110 19 71 18 94 46 11 28 62 22 ...
    ##  $ RunType            : Factor w/ 5 levels "Long Run","Race",..: 5 4 3 4 3 1 4 4 3 4 ...

remove 1st col
==============

``` r
runs$X <- NULL
str(runs)
```

    ## 'data.frame':    117 obs. of  18 variables:
    ##  $ ID                 : int  646029304 647216014 648410753 649445500 650512799 651933678 653912805 655096239 656203325 657259723 ...
    ##  $ Name               : Factor w/ 13 levels "10k Tune Up Run",..: 2 7 10 3 10 4 3 7 10 7 ...
    ##  $ Gear               : Factor w/ 4 levels "ASICS dunno Black, Yellow, Red",..: 2 2 1 2 1 2 2 2 1 2 ...
    ##  $ Date               : Factor w/ 116 levels "2016-07-19","2016-07-20",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Month              : Factor w/ 5 levels "Aug","Jul","Nov",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ monthNum           : int  7 7 7 7 7 7 7 7 7 7 ...
    ##  $ DOW                : Factor w/ 7 levels "Fri","Mon","Sat",..: 6 7 5 1 3 4 6 7 5 1 ...
    ##  $ StartTime          : Factor w/ 90 levels "10:01","10:09",..: 58 51 67 46 71 76 46 37 64 36 ...
    ##  $ Distance           : num  9.01 11 4 11 6 ...
    ##  $ Time               : Factor w/ 115 levels "1:00:53","1:02:08",..: 4 23 81 28 98 60 13 41 72 37 ...
    ##  $ Avg.Speed.Avg.Pace.: Factor w/ 68 levels "6:32","6:54",..: 13 32 58 38 55 39 33 41 48 49 ...
    ##  $ Speed.mph          : num  3.72 3.54 3.29 3.49 3.33 ...
    ##  $ Cad                : num  84.2 84.2 85.6 88 86.9 85 82.9 87.4 85.8 85.1 ...
    ##  $ Elevation.Gain     : Factor w/ 92 levels "--","0","13",..: 37 56 1 58 15 80 48 61 1 60 ...
    ##  $ Avg.HR             : Factor w/ 39 levels "--","100","113",..: 34 35 24 35 17 37 35 31 19 34 ...
    ##  $ Max.HR             : Factor w/ 49 levels "--","118","122",..: 37 30 19 31 25 35 29 28 13 30 ...
    ##  $ Calories           : Factor w/ 113 levels "1,018","1,021",..: 110 19 71 18 94 46 11 28 62 22 ...
    ##  $ RunType            : Factor w/ 5 levels "Long Run","Race",..: 5 4 3 4 3 1 4 4 3 4 ...

put month name labels to monthNum to display names in numerical in order
========================================================================

``` r
runs$monthNum <- factor(runs$monthNum,labels = unique(runs$Month[order(runs$monthNum)]))
```

load ggplot2
============

``` r
library(ggplot2)

#distance histogram
ggplot(data = runs, aes(x = Distance)) + 
  geom_histogram(binwidth = 2, aes(fill = ..count..)) + 
  xlab("Distance (mi)") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Miles Ran in All Runs") + 
  labs(caption="*Majority of runs were between 9 and 11 miles, with a suprisingly low number of runs between 7 and 9 miles")
```

![](R_Analysis_git_files/figure-markdown_github/plotting-1.png)

``` r
ggplot(data = runs, aes(x = monthNum, y = Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  xlab("Month") + 
  ylab("Total Miles") + 
  coord_flip() + 
  #geom_text(vjust=0, colour="red") +
  ggtitle("Sum of Miles by Month") + 
  labs(caption="*The largest number of miles ran, 308 miles, was in September")
```

![](R_Analysis_git_files/figure-markdown_github/miles%20by%20month-1.png)

``` r
ggplot(data = runs, aes(monthNum, Cad)) + 
  geom_boxplot(aes(fill = factor(runs$Month, levels = runs$Month[order(runs$monthNum)], ordered = TRUE)), 
               outlier.colour = "red") + #geom_jitter()
  xlab("Month") + 
  ylab("Cadence") + 
  ggtitle("Cadence per Month") + 
  labs(caption="*Cadence generally increases month over month, due to more workouts, or improved form?")
```

    ## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
    ## else paste0(labels, : duplicated levels in factors are deprecated

    ## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
    ## else paste0(labels, : duplicated levels in factors are deprecated

    ## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
    ## else paste0(labels, : duplicated levels in factors are deprecated

    ## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
    ## else paste0(labels, : duplicated levels in factors are deprecated

    ## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
    ## else paste0(labels, : duplicated levels in factors are deprecated

    ## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
    ## else paste0(labels, : duplicated levels in factors are deprecated

    ## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
    ## else paste0(labels, : duplicated levels in factors are deprecated

    ## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
    ## else paste0(labels, : duplicated levels in factors are deprecated

![](R_Analysis_git_files/figure-markdown_github/cadence%20by%20month-1.png)

``` r
ggplot(data = runs, aes(x = Date, y = Avg.HR)) + 
  geom_line(aes(group=1)) +
  theme(axis.text.x = element_text(size=0, angle=45)) +
  xlab("Time") + 
  ylab("Average Heart Rate") + 
  ggtitle("Average Heart Rate Over Time") + 
  labs(caption="*Looks like a very slight general decrease, with an outlier of about 100 in the 1st third of the plan
      and 70 in the last 3rd of the plan")
```

![](R_Analysis_git_files/figure-markdown_github/HR%20over%20time-1.png)
