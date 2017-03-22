Marathon Training Plan Data Analysis
================

*Initial Data Inspection*
=========================

First, I load in the CSV file created in the **R\_Cleaning\_Garmin\_Strava** script file, which contains the combined dataset of runs from my Philadelphia Marathon training plan from both Strava and Garmin Connect. I then inspect the data to make sure I have all the correct data in the correct data types.

    ## 'data.frame':    116 obs. of  18 variables:
    ##  $ X             : int  12 15 94 4 6 8 13 16 95 5 ...
    ##  $ ID            : int  646029304 647216014 648410753 649445500 650512799 651933678 653912805 655096239 656203325 657259723 ...
    ##  $ Name          : Factor w/ 13 levels "10k Tune Up Run",..: 2 7 10 3 10 4 3 7 10 7 ...
    ##  $ Gear          : Factor w/ 4 levels "ASICS dunno Black, Yellow, Red",..: 2 2 1 2 1 2 2 2 1 2 ...
    ##  $ Date          : Factor w/ 116 levels "2016-07-19","2016-07-20",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Month         : Factor w/ 5 levels "Aug","Jul","Nov",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ DOW           : Factor w/ 7 levels "Fri","Mon","Sat",..: 6 7 5 1 3 4 6 7 5 1 ...
    ##  $ StartTime     : Factor w/ 89 levels "10:01","10:09",..: 57 50 66 45 70 75 45 36 63 35 ...
    ##  $ Distance      : num  9.01 11 4 11 6 ...
    ##  $ Time          : Factor w/ 114 levels "2017-03-18 00:07:43",..: 50 69 14 74 31 103 59 87 6 83 ...
    ##  $ Avg.Pace      : Factor w/ 67 levels "2017-03-18 00:06:32",..: 13 32 57 38 55 39 33 41 48 49 ...
    ##  $ Speed.mph     : num  3.72 3.54 3.29 3.49 3.33 ...
    ##  $ Cad           : num  168 168 171 176 174 ...
    ##  $ Elevation.Gain: int  351 461 NA 468 230 572 400 485 NA 477 ...
    ##  $ Avg.HR        : int  149 150 139 150 131 154 150 146 134 149 ...
    ##  $ Max.HR        : int  167 160 149 161 155 165 159 158 142 160 ...
    ##  $ Calories      : int  943 1176 442 1170 721 1653 1090 1318 314 1241 ...
    ##  $ RunType       : Factor w/ 5 levels "Long Run","Race",..: 5 4 3 4 3 1 4 4 3 4 ...

Well, to start off, I have a column which seems to contain the row numbers from the CSv, so let's remove that before going back to the inspection.

``` r
runs$X <- NULL
head(runs,2)
```

    ##          ID           Name                   Gear       Date Month DOW
    ## 1 646029304 Classic LT Run Brooks Ghost Red/Black 2016-07-19   Jul Tue
    ## 2 647216014         ML Run Brooks Ghost Red/Black 2016-07-20   Jul Wed
    ##   StartTime Distance                Time            Avg.Pace Speed.mph
    ## 1      5:51     9.01 2017-03-18 01:04:59 2017-03-18 00:07:13    3.7179
    ## 2      5:36    11.00 2017-03-18 01:23:29 2017-03-18 00:07:35    3.5350
    ##     Cad Elevation.Gain Avg.HR Max.HR Calories RunType
    ## 1 168.4            351    149    167      943 Workout
    ## 2 168.4            461    150    160     1176     Run

Alright, cool. But it looks like my Date field, **Date**, total run time field, **Time**, and average pace (min/mile) field, **Avg.Pace**, are not in the POSIXct format needed. So let's put them back into the correct format.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
runs$Time <- as.POSIXct(runs$Time)#, format = '%H:%M:%S')
runs$Avg.Pace <- as.POSIXct(runs$Avg.Pace)#, format = '%H:%M:%S')
runs$Date <- as.POSIXct(runs$Date)
runs$StartTime <- hour(strptime(runs$StartTime, format = '%H:%M'))

str(runs)
```

    ## 'data.frame':    116 obs. of  17 variables:
    ##  $ ID            : int  646029304 647216014 648410753 649445500 650512799 651933678 653912805 655096239 656203325 657259723 ...
    ##  $ Name          : Factor w/ 13 levels "10k Tune Up Run",..: 2 7 10 3 10 4 3 7 10 7 ...
    ##  $ Gear          : Factor w/ 4 levels "ASICS dunno Black, Yellow, Red",..: 2 2 1 2 1 2 2 2 1 2 ...
    ##  $ Date          : POSIXct, format: "2016-07-19" "2016-07-20" ...
    ##  $ Month         : Factor w/ 5 levels "Aug","Jul","Nov",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ DOW           : Factor w/ 7 levels "Fri","Mon","Sat",..: 6 7 5 1 3 4 6 7 5 1 ...
    ##  $ StartTime     : int  5 5 6 5 7 8 5 5 6 5 ...
    ##  $ Distance      : num  9.01 11 4 11 6 ...
    ##  $ Time          : POSIXct, format: "2017-03-18 01:04:59" "2017-03-18 01:23:29" ...
    ##  $ Avg.Pace      : POSIXct, format: "2017-03-18 00:07:13" "2017-03-18 00:07:35" ...
    ##  $ Speed.mph     : num  3.72 3.54 3.29 3.49 3.33 ...
    ##  $ Cad           : num  168 168 171 176 174 ...
    ##  $ Elevation.Gain: int  351 461 NA 468 230 572 400 485 NA 477 ...
    ##  $ Avg.HR        : int  149 150 139 150 131 154 150 146 134 149 ...
    ##  $ Max.HR        : int  167 160 149 161 155 165 159 158 142 160 ...
    ##  $ Calories      : int  943 1176 442 1170 721 1653 1090 1318 314 1241 ...
    ##  $ RunType       : Factor w/ 5 levels "Long Run","Race",..: 5 4 3 4 3 1 4 4 3 4 ...

Looks like **Month** was also not loaded as on *ordered* factor like I need it. As it is now, if I used it in my graphs, it would be ordereded alphabetically, not chronologically like I need it. So let's convert it back to an ordered factor with the correctly-specified ordering via the **levels** argument.

``` r
runs$Month <- factor(runs$Month, ordered = TRUE, levels = c("Jul","Aug","Sep","Oct","Nov"))
```

Now its time to acually make some plots. I'll start off with a simple histogram of how many miles I ran in each of my runs.

![](R_Analysis_git_files/figure-markdown_github/plotting-1.png)

It looks like the majority of my runs (20 of 'em) were between 10 and 12 miles, with the next highest frequency being from 4 to 6 miles, which were mostly recovery runs, showing that there was indeed a lot decent amount of time for recovery in this plan. We can also see my one 24 mile run all the way to the right, my longest Long Run of the plan.

Now let's look at the total number of miles ran each month, with a pretty obvious expectation to see the total rise month-by-month.

![](R_Analysis_git_files/figure-markdown_github/miles%20by%20month-1.png)

    ##    Jul    Aug    Sep    Oct    Nov 
    ## 118.08 250.08 308.14 304.45 168.44

The largest number of miles ran, 308 miles, was in September, and then we see a *ever-so-slightly* drop to 304 miles in October. This could be possibly be so if my peak mileage week was in September. We then see a large drop from October to November, which was expected due tapering up the race and the fact that the race was on November 20th, so there's no more run data after that date.

``` r
runs[which.max(runs$Distance[runs$Date < "2016-11-20"]),]
```

    ##           ID     Name                   Gear       Date Month DOW
    ## 67 732206487 Long Run Brooks Ghost Red/Black 2016-10-02   Oct Sun
    ##    StartTime Distance                Time            Avg.Pace Speed.mph
    ## 67         8       24 2017-03-18 02:55:35 2017-03-18 00:07:19    3.6667
    ##    Cad Elevation.Gain Avg.HR Max.HR Calories  RunType
    ## 67 175            789    143    158     2129 Long Run

Welp, I was half wrong about that I guess. It seems peak week was actually both in September *and* October, with the final run of the week coinciding with the start of October, so that was where my total mileage started to decreased *just* a bit.

Another important metric for runners is their **cadence**, which is the number of steps taken per minute. The general consensus for a good cadence to run efficiently seems to be anywhere from 160 to 180 steps per minute, with more advanced and elite runners leaning towards the higher end.

![](R_Analysis_git_files/figure-markdown_github/cadence%20by%20month-1.png)

So, barring some outliers, I can see that my cadence tended to generally increases month over month, and ended up averagin between 170 and 180, barring those outliers. I would assume they were mainly recovery runs, as I was mainly concerned with keeping my heart rate down so I could actually recover on them than I was with making sure my cadence was optimal. I could investigate to see if this was due to having more speed workouts as the plan progressed, in which I would assume my cadence was typically higher), or could it have been due to improved form from an ever-increasing volume????

Another metric would be my average heart rate during runs over the course of the training. Compared to the rise of my cadence, we would expect that my average heart rate would have decreased over time, as my fitness would have increased due to higher mileage and an increasing number of workouts.

![](R_Analysis_git_files/figure-markdown_github/HR%20by%20month%20boxplot-1.png)

So we can see that my hear rate had generally decreased month over month. Now while I would hope it was due to increased fitness (and I still believe that to be the case, just by how I felt during my runs), the drops in temperature as we moved from summer to fall also may have played a part. Since it would have been cooler out, the body would not have been working as hard to cool itself down during my runs, so my heart rate would not have been as high. Maybe finding the historical temperature data and bringing it in could bring some insights into this question.

Now let's look at cadence and heart rate by Run Type as well for comparisons.

![](R_Analysis_git_files/figure-markdown_github/cadence%20by%20run%20type-1.png)

We can ignore the variability in the **Race** run type, since it's only on race. But we can still see that it was either my maximum cadence, or the second or third highest after those 2 quickest workout cadences. We can also see that my recovery runs had the least variability, and recovery runs as well as normal runs. I also expected my workouts to have the highest cadence, but it has the lowest median. I can only assume that this was due to recovery sections of interval runs, or warm-ups and cooldowns during tempo runs which were *much* slower and relaxed than even recovery runs. Now let's look at heart rate.

![](R_Analysis_git_files/figure-markdown_github/heart%20rate%20by%20run%20type-1.png)

As expected, my heart rate was lowest during recovery runs. We can also see that there were 2 runs were my heart rate monitor was off, since my average heart rate was around 70. Also, my average heart rate was not very variable for the other 3 types of runs, excluding the race. My Long runs also tended to have a lower average heart rate than my "normal" runs, which I personally did not expect. But this does a make a little sense, since I was supposed to run at a bit of a slower pace so that I could run longer. Also, I did expect my heart rate to be higher during workouts, since they were designed to to push my heart rate to the max during **V02 intervals**, and was supposed to be elevated during those "comfortably hard" **tempo runs**. I can also see that there was one single workout wherein it was higher than my average heart rate during the marathon.

``` r
runs[which.max(runs$Avg.HR),]
```

    ##           ID        Name                      Gear       Date Month DOW
    ## 73 738343078 Tune-Up Run Hoka One One Clifton Reds 2016-10-08   Oct Sat
    ##    StartTime Distance                Time            Avg.Pace Speed.mph
    ## 73        10     9.33 2017-03-18 01:00:53 2017-03-18 00:06:32    4.1107
    ##    Cad Elevation.Gain Avg.HR Max.HR Calories RunType
    ## 73 139            257    157    169      939 Workout

So we can see that my max heart rate was during my 10k Tune-up Race, which does not surprise me since it SUCKED.

`{ r heart rate over time}  ggplot(data = runs, aes(x = Date, y = Avg.HR)) +    geom_line(aes(group=1)) +   theme(axis.text.x = element_blank()) +   xlab("Time") +    ylab("Average Heart Rate") +    ggtitle("Average Heart Rate Over Time") +    labs(caption="*Very slight general decrease, with outliers of about 100 in the 1st third of the plan and  70 in the last 3rd of the plan")`

average pace over plan line
===========================

``` r
ggplot(data = runs, aes(x = Date, y = Avg.Pace)) + 
  geom_point() + # by color? 
  #?stat_smooth() + 
  theme(axis.text.x = element_blank()) +
  xlab("Time") + 
  ylab("Average Heart Rate") + 
  ggtitle("Average Heart Rate Over Time") + 
  labs(caption="*Very slight general decrease, but not linear")
```

![](R_Analysis_git_files/figure-markdown_github/avg%20pace-1.png)
