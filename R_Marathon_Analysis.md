rmd2md <- function( path_site = getwd(),
                    dir_rmd = "_rmd",
                    dir_md = "_posts",                              
                    #dir_images = "figures",
                    url_images = "figures/",
                    out_ext='.md', 
                    in_ext='.rmd', 
                    recursive=FALSE) {
  
  require(knitr, quietly=TRUE, warn.conflicts=FALSE)

  #andy change to avoid path problems when running without sh on windows 
  files <- list.files(path=file.path(path_site,dir_rmd), pattern=in_ext, ignore.case=TRUE, recursive=recursive)
  
  for(f in files) {
    message(paste("Processing ", f, sep=''))
    content <- readLines(file.path(path_site,dir_rmd,f))
    frontMatter <- which(substr(content, 1, 3) == '---')
    if(length(frontMatter) >= 2 & 1 %in% frontMatter) {
      statusLine <- which(substr(content, 1, 7) == 'status:')
      publishedLine <- which(substr(content, 1, 10) == 'published:')
      if(statusLine > frontMatter[1] & statusLine < frontMatter[2]) {
        status <- unlist(strsplit(content[statusLine], ':'))[2]
        status <- sub('[[:space:]]+$', '', status)
        status <- sub('^[[:space:]]+', '', status)
        if(tolower(status) == 'process') {
          #This is a bit of a hack but if a line has zero length (i.e. a
          #black line), it will be removed in the resulting markdown file.
          #This will ensure that all line returns are retained.
          content[nchar(content) == 0] <- ' '
          message(paste('Processing ', f, sep=''))
          content[statusLine] <- 'status: publish'
          content[publishedLine] <- 'published: true'
          
          #andy change to path
          outFile <- file.path(path_site, dir_md, paste0(substr(f, 1, (nchar(f)-(nchar(in_ext)))), out_ext))
                   
          #render_markdown(strict=TRUE)
          #render_markdown(strict=FALSE) #code didn't render properly on blog
          
          #andy change to render for jekyll
          render_jekyll(highlight = "pygments")
          #render_jekyll(highlight = "prettify") #for javascript
          
          opts_knit$set(out.format='markdown') 
                    
          # andy BEWARE don't set base.dir!! it caused me problems
          # "base.dir is never used when composing the URL of the figures; it is 
          # only used to save the figures to a different directory. 
          # The URL of an image is always base.url + fig.path"
          # https://groups.google.com/forum/#!topic/knitr/18aXpOmsumQ
                    
          opts_knit$set(base.url = "/")
          opts_chunk$set(fig.path = url_images)                     
          
          #andy I could try to make figures bigger
          #but that might make not work so well on mobile
          #opts_chunk$set(fig.width  = 8.5,
          #               fig.height = 5.25)
          
          try(knit(text=content, output=outFile), silent=FALSE)
          
        } else {
          warning(paste("Not processing ", f, ", status is '", status, 
                        "'. Set status to 'process' to convert.", sep=''))
        }
      } else {
        warning("Status not found in front matter.")
      }
    } else {
      warning("No front matter found. Will not process this file.")
    }
  }
  invisible()
}---
title: "Marathon Training Plan Data Cleaning"
author: "Steve Newns"
date: "February 21, 2017"
output:
  html_document: default
  word_document: default
---

# *Initial Data Inspection*

## Strava Data File

load in the CSV file containing the Strava data file created by and downloaded from **VeloViewer.com** and check out the dataset.

```{r load dataset}
runs <- read.csv("cleanedMarathonTrainingData.csv")
str(runs)
```

#remove 1st col
```{r remove 1st col }
runs$X <- NULL
str(runs)
```

#put month name labels to monthNum to display names in numerical in order
```{r fix monthNum}
runs$monthNum <- factor(runs$monthNum,labels = unique(runs$Month[order(runs$monthNum)]))
```


#load ggplot2
```{r plotting}
library(ggplot2)

#distance histogram
ggplot(data = runs, aes(x = Distance)) + 
  geom_histogram(binwidth = 2, aes(fill = ..count..)) + 
  xlab("Distance (mi)") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Miles Ran in All Runs") + 
  labs(caption="*Majority of runs were between 9 and 11 miles, with a suprisingly low number of runs between 7 and 9 miles")
```


```{r miles by month} 
ggplot(data = runs, aes(x = monthNum, y = Distance, fill = Month)) + 
  geom_bar(stat="identity") +
  xlab("Month") + 
  ylab("Total Miles") + 
  coord_flip() + 
  #geom_text(vjust=0, colour="red") +
  ggtitle("Sum of Miles by Month") + 
  labs(caption="*The largest number of miles ran, 308 miles, was in September")
```

```{r cadence by month} 
ggplot(data = runs, aes(monthNum, Cad)) + 
  geom_boxplot(aes(fill = factor(runs$Month, levels = runs$Month[order(runs$monthNum)], ordered = TRUE)), 
               outlier.colour = "red") + #geom_jitter()
  xlab("Month") + 
  ylab("Cadence") + 
  ggtitle("Cadence per Month") + 
  labs(caption="*Cadence generally increases month over month, due to more workouts, or improved form?")
```

```{r HR over time} 
ggplot(data = runs, aes(x = Date, y = Avg.HR)) + 
  geom_line(aes(group=1)) +
  theme(axis.text.x = element_text(size=0, angle=45)) +
  xlab("Time") + 
  ylab("Average Heart Rate") + 
  ggtitle("Average Heart Rate Over Time") + 
  labs(caption="*Looks like a very slight general decrease, with an outlier of about 100 in the 1st third of the plan
      and 70 in the last 3rd of the plan")
 
```
  
  